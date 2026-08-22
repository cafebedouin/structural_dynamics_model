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
 *   human_readable: Marriage Authority Under Christian Ecclesiastical Governance (Canonical Reading)
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   Under the Christian canonical reading (Catholic orthodoxy and many
 *   traditional Protestant denominations), marriage is a sacrament — a
 *   spiritually-binding covenant sealed by ecclesiastical authority. The
 *   Catholic tradition holds marriage as indissoluble; Protestant traditions
 *   vary but many restrict remarriage after divorce. The constraint extracts
 *   from spouses (indissolubility bounds their autonomy), most severely from
 *   divorced persons (denied remarriage), and from women where patriarchal
 *   authority is enforced. The coordination function is real: the church
 *   provides a stable institutional frame for marriage, community
 *   recognition, inheritance rules, and moral order. But the extraction is
 *   substantial and persistent: ecclesiastical authority over validity and
 *   dissolution is maintained even as civil law has largely assumed marriage
 *   governance, and the constraint continues to bind adherents through
 *   identity-lock (leaving the tradition costs family, professional, and
 *   communal belonging). The measurement series tracks rising theater-ratio
 *   (increasing share of enforcement devoted to maintaining authority rather
 *   than serving coordination function) as civil divorce normalizes;
 *   extractiveness rises then plateaus as the constraint adapts to partial
 *   compliance (annulment becomes a practical workaround for those with
 *   institutional access and resources).
 *
 * KEY AGENTS:
 *   - Ecclesiastical hierarchy: sets doctrine, adjudicates validity, enforces indissolubility as institutional law
 *   - Married individuals within tradition: receive sacramental status, bear indissolubility constraint via identity-lock
 *   - Divorced persons denied remarriage: powerless, trapped, targeted by full enforcement weight
 *   - Women under patriarchal enforcement: experience asymmetric constraint, identity-locked through family role
 *   - State legal systems: excluded from determining sacramental validity, occupy competing jurisdictional space
 *   - Civil partners (same-sex): excluded from both sacramental and civil ecclesiastical recognition
 *   - Reformist clergy: boundary-occupiers, observe full structure, constrained from internal advocacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.68).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Marriage Authority Under Christian Ecclesiastical Governance (Canonical Reading)").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious/legal/political").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, 'fa48001d-e14c-4897-a85c-a21c9bf0b1fe').
narrative_ontology:cs_kernel_codification('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', fixed_text).
narrative_ontology:cs_authority_grounding('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', lineage).
narrative_ontology:cs_interpretation_layer_present('fa48001d-e14c-4897-a85c-a21c9bf0b1fe').
narrative_ontology:cs_reading_relation('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', foundational, sacramental_permanence_doctrine).
narrative_ontology:cs_axiom_status(sacramental_permanence_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', sacramental_permanence_doctrine, deontological).
narrative_ontology:cs_axiom('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', foundational, ecclesiastical_authority_over_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_validity, holdable).
narrative_ontology:cs_axiom_grounding('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', ecclesiastical_authority_over_validity, deontological).
narrative_ontology:cs_reference_frame('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', apostolic_sacramental_authority).
narrative_ontology:cs_drift_state('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', contemporary_secular_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fa48001d-e14c-4897-a85c-a21c9bf0b1fe', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, married_individuals_within_tradition).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, divorced_persons_denied_remarriage).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, women_under_patriarchal_enforcement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, married_individuals_within_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Catholic magisterium and episcopal bodies set the rules governing sacramental validity, permanence, and dissolution. They adjudicate disputed marriages, grant annulments on canonical grounds, and enforce the doctrine of indissolubility as doctrine-law. They control the authority structure and have competing institutional interests: defending doctrine (civilizational time horizon, institutional reproduction) while managing pastoral care and regulatory pressure from secular states. Protestant denominations hold similar but denomin­ationally-variant authority, each governing marriage within its own flock differently.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Receive the sacramental status and divine blessing pronounced over their marriage; benefit from the institutional stability and community recognition the church confers. They also bear the constraint: indissolubility binds them to a vow the church enforces. Their exit from the marriage itself is foreclosed (Catholic doctrine) or constrained (Protestant denominations vary). Identity-locked because leaving the faith or the marriage carries identity rupture — professional status, family belonging, community identity are constituted through the religious frame.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, married_individuals_within_tradition, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, married_individuals_within_tradition, payer).

% Bear the full enforcement weight: denied access to sacramental remarriage (Catholic) or stigmatized/excluded from community life (many Protestant traditions). They have limited recourse — civil divorce is possible in most jurisdictions but carries no ecclesiastical weight in the tradition's eyes. Trapped because they cannot exit the tradition's authority without losing family, professional networks, and identity anchors built within it.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, divorced_persons_denied_remarriage, payer,
    powerless, biographical, trapped, global).

% Experience the constraint differentially: sacramental permanence and church authority over marital dissolution work asymmetrically in patriarchal family structures where male authority is presumed. Historically, women's dissolution options and grounds for annulment were narrower; remarriage penalties fell more heavily on women; church authority reinforced male household headship. Identity-locked through family role, religious identity, and economic dependency historically structured into the institution.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, women_under_patriarchal_enforcement, payer,
    powerless, biographical, identity_locked, global).

% Claim authority over marriage through civil law (no-fault divorce, secular grounds for dissolution, equal property division). They are excluded from determining validity under the ecclesiastical reading; their civil authority does not touch the sacrament. In jurisdictions with established churches or strong denominational influence, state authority is constrained by ecclesiastical prerogatives; in secular-state contexts, the church's authority applies only to self-identified adherents, creating a parallel jurisdiction.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, state_legal_systems, excluded,
    institutional, generational, constrained, national).

% Denied sacramental marriage under this reading (Catholic doctrine; many Protestant traditions similarly restrict); denied even the civil-contract recognition available to opposite-sex couples in secular jurisdictions that recognize same-sex civil union. Structurally excluded from both the ecclesiastical and secular frameworks where the sacramental reading applies; trapped because the tradition offers no legitimate status for their partnership.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, civil_partners_same_sex, excluded,
    powerless, biographical, trapped, global).

% Occupy the boundary: members of the tradition who question indissolubility doctrine, propose pastoral accommodation (annulment liberalization, internal-forum absolution), or advocate doctrinal revision. Constrained by institutional loyalty and fear of censure; their interventions shape interpretation of the constraint without overriding it. They see the full structure and resist it from within.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, reformist_clergy_and_theologians, observer,
    moderate, biographical, constrained, global).

% External analysis of the constraint's structure, history, and effects across traditions. No stake in outcomes; reads the competing readings (Christian canonical, Hindu dharmashastra, Muslim shariat, Parsi Zoroastrian, secular contractual) as distinct framings of the family-law kernel.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes marriage as a permanent, spiritually-binding covenant recognized by the community and the divine; solves the coordination problem of stable household formation, inheritance, and social belonging by treating the marital bond as sacramentally sealed and ecclesiastically witnessed.
% TRANSFER_FUNCTION: Transfers authority over marital validity, permanence, and dissolution from individual will or state law to ecclesiastical jurisdiction; moves spouses' autonomy over their own bond into ecclesiastical control; collects legitimacy-conferral authority for the institutional church; in patriarchal contexts, transfers women's autonomy into household-head authority backed by ecclesiastical doctrine.
% ABSENT_VOICES: Those excluded from sacramental marriage (same-sex partners, interfaith couples in strict traditions) have no seat at the table; civil-law authorities in secular states are structurally excluded from this reading's jurisdiction; divorced persons seeking remarriage are present only as objects of judgment, not as agents in the rule-making process.
% DISAPPEARANCE_RATIONALE: If ecclesiastical authority over marriage permanence disappeared overnight, millions of adherents would lose the sacramental status that grounds their marriage identity; clergy would lose authority to adjudicate validity and dissolution; tens of millions globally would gain access to remarriage denied under this constraint; inheritance, property, and child-custody arrangements in traditionally-Christian jurisdictions would reorganize; the institutional church would lose a primary lever of authority over lay life. The arrangements would rearrange substantially, though parallel secular law would already provide a partial frame.
% FOUNDING_PROBLEM: Early Christian theology posited marriage as a sacrament reflecting Christ's unbreakable covenant with the church; established ecclesiastical authority to guard against adultery, maintain sexual exclusivity, and stabilize social order through permanent vows; created mechanism (canon law) for determining validity and enforcing indissolubility as doctrine.
% FOUNDING_PROBLEM_CORROBORATION: The ecclesiastical hierarchy attests the problem is live: moral order, spiritual integrity, and family stability require permanent commitment backed by sacramental seal and church authority. Civil authorities, reformed Protestant denominations (which permit divorce), feminist scholars, divorced adherents, and secular observers attest the founding problem is substantially solved by civil law and that the constraint now persists as institutional authority-maintenance divorced from pastoral function. Demographic evidence from Vatican II era: Catholic divorce rates rose from <5% (1960s) to 30-50% (contemporary), contradicting doctrine. Canonical law scholarship documents annulment liberalization as administrative workaround. Comparative law studies (Brinig & Garnett on Catholic family law, Witte on Protestant variants) show the constraint persists through institutional inertia despite civil-law alternatives.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) because ecclesiastical authority over marital dissolution persists despite civil law having substantially solved the founding coordination problem (stable marriage, inheritance, property), and adherents face real costs for exit (divorced remarriage denial, family rupture, identity cost). Suppression is higher (0.72) because the constraint's enforcement depends on active institutional maintenance: clergy must refuse remarriage; institutions must process annulments selectively to maintain doctrine-appearance; adherents must internalize the doctrine as legitimate despite its autonomy costs. Theater-ratio rises from 0.18 to 0.41 because the constraint's operation increasingly defends institutional authority rather than coordination: early modernity (t=0) saw substantial coordination function (church providing marriage governance, record-keeping, moral framework); contemporary period (t=40) shows growing share of enforcement devoted to authority-maintenance (annulment processing, doctrine reiteration in face of defection) rather than solving actual coordination problems (which civil law now handles). Accessibility-collapse is moderate (0.58) because alternatives exist and are increasingly visible: civil divorce is accessible in most jurisdictions; secular marriage frames are culturally available; exit costs are identity-based rather than purely structural, making the collapse partial. Resistance is substantial (0.64) because married adherents increasingly divorce civilly despite doctrine (demographic data show 30-50% Catholic divorce rates in Western countries), reformist clergy push for pastoral accommodation, and secular states enforce civil divorce regardless of ecclesiastical objection.
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical hierarchy and married-within-tradition beneficiaries perceive the constraint differently from divorced persons and women under patriarchal enforcement. From the hierarchy's seat, the constraint is genuine coordination (providing marriage solemnity, stability, community recognition) that they maintain through institutional authority; from the divorced person's seat, the same structure is coercive extraction (denial of remarriage, identity rupture, no appeal). The engine should compute these divergent classifications: the hierarchy sits as a beneficiary (d near 0.0, low effective extraction), while divorced persons sit as targets (d near 1.0, high effective extraction amplified by identity-lock and spatial scope). Women's experience is intermediate: they receive coordination benefits (community recognition, inheritance protection) but bear asymmetrically higher suppression costs (patriarchal authority reinforced by ecclesiastical doctrine). Same-sex couples sit at the extreme: completely excluded, no coordination benefit, full target status.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical hierarchy: powerful institutional actor, arbitrage-level exit (they can reinterpret doctrine, declare annulments, permit divorce if they choose — their 'exit' is within the system), collects authority and rents. d ≈ 0.0 (full beneficiary). Married individuals within tradition: moderate power, identity-locked exit (leaving means family rupture, identity loss), receive coordination benefits but bear indissolubility cost. d ≈ 0.4–0.5 (symmetric). Divorced persons: powerless, trapped exit (cannot remarry within the tradition, cannot easily leave without identity cost), entirely targeted by the constraint. d ≈ 0.95 (full target). Women under patriarchal enforcement: powerless, identity-locked, face asymmetrically higher suppression burden; d ≈ 0.85 (near-full target, somewhat less trapped than divorced persons because they can remain married, but more suppressed). State legal systems: institutional power, constrained exit (they can change family law but encounter clerical resistance and adherent defection where the reading holds sway), excluded but not targeted. d ≈ 0.55 (neutral-to-slightly-extractive, constrained by the reading's institutional reach).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stable marriage, inheritance, moral order) is substantially solved by civil law in jurisdictions where secular marriage governance is established. Yet the constraint persists in ecclesiastical contexts because it serves not the founding problem but institutional self-maintenance: ecclesiastical authority over marriage is intrinsically valuable to the church's claim to civilizational relevance. The theater-ratio rising from 0.18 to 0.41 reflects this shift: as civil law handles marriage stability, more of the ecclesiastical machinery is devoted to defending the boundary (annulment as workaround that preserves doctrine-appearance, continued refusal to recognize civil remarriage, pastoral messaging that reiterates indissolubility despite non-compliance). This is mandatrophy candidate: the founding problem is dead (civil law solved it); the founding authority-claim (ecclesiastical jurisdiction over marriage) persists through institutional inertia and identity-maintenance. The constraint is not a snare (it is enforced through identity-lock and institutional legitimacy, not pure coercion), but it is degrading toward piton: more theater, less coordination function, persistence via authority-maintenance rather than beneficiary satisfaction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramentality_vs_contractuality,
    'Is marriage fundamentally sacramental (spiritually-binding, ecclesiastically adjudicated) or contractual (a covenant between individuals, civilly-adjudicated)? Does this distinction rest on empirically-falsifiable claims about marital outcomes or on deontological commitment to ecclesiastical authority?',
    'Comparative empirical analysis: do sacramental-marriage jurisdictions show measurably different divorce rates, stability outcomes, or well-being than contractual-marriage jurisdictions when controlling for secular factors? Does the sacramental framing produce measurable coordination benefits beyond what civil law provides, or is the empirical case merely post-hoc rationalization of institutional authority?',
    'If sacramental framing produces empirical coordination benefits, part of the measured extraction is the price of coordination; if not, the extraction is pure institutional authority-maintenance. If the distinction is deontological (not empirically-contingent), empirical evidence cannot resolve it, and the constraint''s persistence rests on commitment to ecclesiastical authority regardless of outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramentality_vs_contractuality, empirical, 'Whether sacramentality is empirically grounded or deontologically grounded.').

omega_variable(
    natural_law_vs_doctrine,
    'Does indissolubility derive from natural law (a feature of marriage as created by God, discoverable through reason) or from ecclesiastical doctrine (a claim the church authoritatively maintains)? If natural law, it should be accessible to non-adherents; if doctrine, it is internal to the tradition''s frame.',
    'Historical theology and comparative analysis: does the natural-law claim appear convincing to secular philosophers, Islamic scholars, Hindu theorists who share no commitment to ecclesiastical authority? If the claim is compelling only within the tradition''s hermeneutic frame, it is doctrine, not natural law.',
    'If natural law (discovered, not invented), the constraint has stronger legitimacy as a real coordination solution. If doctrine (invented, maintained by authority), the constraint is more clearly an extraction mechanism masked in naturalistic language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_doctrine, conceptual, 'Whether indissolubility is universal law or tradition-internal doctrine.').

omega_variable(
    patriarchal_authority_fusion,
    'Is the constraint''s suppression of women separable from the sacramental framing, or is patriarchal authority structurally fused with ecclesiastical authority in this tradition? If separable, could the constraint be reformed to maintain sacramentality while removing gendered suppression?',
    'Historical and comparative analysis: does the sacramental frame necessarily entail patriarchal household authority, or is the fusion a contingent feature of how specific traditions developed the doctrine? Where Protestant denominations have liberalized divorce and marriage authority (permitting remarriage, female clergy, egalitarian leadership), did they retain sacramental framing, or did sacramentality depend on patriarchal structure?',
    'If separable, women''s targeting by the constraint is a distinct extractive component; if fused, reforming the constraint to remove patriarchal authority would require doctrinal revision beyond divorce permissibility. Separability affects whether the constraint can be reformed from within or requires systemic replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patriarchal_authority_fusion, conceptual, 'Whether patriarchal authority is essential to sacramental framing or a contingent fusion.').

omega_variable(
    annulment_as_constraint_degradation,
    'Does the practice of annulment (declaring a marriage null from its inception) constitute a pragmatic workaround that allows married persons to exit while preserving the indissolubility doctrine, or does it represent the constraint''s degradation into theater (the doctrine says no divorce, but institutional practice permits exit for those with resources and clerical access)?',
    'Comparative access analysis: are annulments granted at equal rates to wealthy and poor adherents, to men and women, to those with institutional connections and those without? Rising annulment rates combined with declining divorce rates among traditionalist populations would suggest theater (doctrine maintained symbolically while practical exit is available to insiders).',
    'If annulment is genuine (applies equally to all), it is a legitimate exception to indissolubility. If annulment is stratified (available to the connected), the constraint degrades toward piton: formal doctrine persists but the enforcement applies unequally, and administrative theater maintains appearance of indissolubility while permitting practical exit for the privileged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(annulment_as_constraint_degradation, empirical, 'Whether annulment is constraint accommodation or constraint-degradation theater.').

omega_variable(
    ecclesiastical_authority_vs_secular_pluralism,
    'In secular jurisdictions where civil marriage and no-fault divorce are normative, does the ecclesiastical reading''s authority depend on voluntary adherence (those who remain in the tradition consent to its rules), or does it continue to extract from those who have partially defected (comply with doctrine despite valuing civil law)?',
    'Demographic and interview analysis: among Catholics who divorce civilly, how many identify as having ''broken their vows'' or feel spiritually bound despite legal dissolution? If they experience ongoing suppression and guilt despite secular law permitting remarriage, the constraint extracts from partial-defectors. If they experience relief and no residual binding, the constraint''s force is voluntary adherence only.',
    'If the constraint extracts from partial-defectors, it represents extractive reach beyond willing participants — a snare component. If it applies only to voluntary adherents, it is a negotiated tangled rope (coordination + asymmetric extraction accepted as part of the tradition). Pluralist jurisdictions complicate this: the state permits secular exit, but the constraint persists in identity and community (identity-lock).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_authority_vs_secular_pluralism, empirical, 'Whether ecclesiastical authority is consensual within secular pluralism or continues to extract from partial-defectors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fami_tr_t8, family_law_authority__christian_canonical_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(fami_tr_t16, family_law_authority__christian_canonical_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(fami_tr_t24, family_law_authority__christian_canonical_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(fami_tr_t32, family_law_authority__christian_canonical_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fami_be_t8, family_law_authority__christian_canonical_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(fami_be_t16, family_law_authority__christian_canonical_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(fami_be_t24, family_law_authority__christian_canonical_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(fami_be_t32, family_law_authority__christian_canonical_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(fami_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fami_su_t8, family_law_authority__christian_canonical_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(fami_su_t16, family_law_authority__christian_canonical_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(fami_su_t24, family_law_authority__christian_canonical_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(fami_su_t32, family_law_authority__christian_canonical_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(fami_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the family_law_authority kernel. The Christian canonical reading grounds marriage authority in ecclesiastical hierarchy and sacramental doctrine; it coexists with dharmashastra (authority in Vedic texts and customary practice), shariat (authority in Quranic injunctions and hadith), Zoroastrian law (authority in religious community practice), and secular contractual (authority in individual will and state law). Each reading instantiates a different constraint with different beneficiaries (ecclesiastical hierarchy vs. family elder councils vs. state apparatus), different victims (divorced persons vs. women under patriarchal authority vs. same-sex couples, depending on the reading), and different ε values. The readings are linked by network.affects_constraints edges because changes in one reading's legitimacy or institutional reach affect others: increasing secular authority over marriage constrains ecclesiastical authority; pluralist accommodation of one reading influences others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__christian_canonical_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
