% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Scripture Alone (Sola Scriptura) as Authoritative Standard
 *   domain: theological/religious
 *
 * SUMMARY:
 *   Sola scriptura is one reading of the contested kernel 'biblical
 *   authority' — the claim that Scripture alone, without tradition or
 *   magisterial adjudication, is sufficient and self-interpreting for
 *   determining Christian doctrine and practice. This reading emerged from
 *   16th-century Protestant Reformation polemic against Catholic magisterial
 *   claims and remains a live position across Reformed, evangelical, and many
 *   independent Protestant communities. The structural claim is that this
 *   reading benefits lay believer autonomy and congregational self-governance
 *   while imposing doctrinal fragmentation as the cost of refusing any
 *   overarching interpretive monopoly. The constraint operates with low
 *   extractiveness (0.38) because lay beneficiaries gain real authority
 *   without a concentrated rent-capturing institution; low suppression (0.22)
 *   because the constraint does not require coercive exclusion of alternative
 *   views, only the institutional refusal to grant magisterial bodies
 *   adjudicative power. The founding problem (magisterial monopoly over
 *   doctrine and lay textual access) is contested: Reformed tradition affirms
 *   it persists; Catholic/Orthodox traditions deny both the problem and the
 *   sola scriptura solution; secular scholarship affirms textual
 *   accessibility as solved but denies sufficiency. This story is ONE READING
 *   of the kernel; sibling readings (conciliar_reading,
 *   tradition_scripture_reading) instantiate different constraints with
 *   different ε values, different beneficiary structures, and different
 *   cardinal types.
 *
 * KEY AGENTS:
 *   - lay_believers: Gain interpretive authority and responsibility under sola scriptura; bear the cost of doctrinal uncertainty when congregations diverge in their readings.
 *   - congregational_communities: Gain institutional autonomy; bear doctrinal fragmentation and schism risk.
 *   - reformed_pastorate: Sets the interpretive standard within congregations; benefits from reduced extraction, bears burden of congregational dissent.
 *   - magisterial_clergy: Lose monopoly over doctrine; trapped exit (rejecting sola scriptura entirely alienates constituencies where the norm is embedded).
 *   - doctrinal_coherence (non-agent): Pays the cost of theological fragmentation across communities and time periods.
 *   - reformation_councils_and_synods: Codify sola scriptura and provide teaching guidance while maintaining Scripture's ultimate authority.
 *   - anabaptist_and_radical_reformers: Apply sola scriptura more radically than mainstream Reformed; excluded from institutional councils as heretical despite shared authority principle.
 *   - ecumenical_councils: Structurally excluded from adjudicating Reformed doctrine because sola scriptura denies them magisterial authority.
 *   - historical_textual_critics: Observe that Scripture's self-interpreting claim is incomplete — textual criticism, historical context, and original-language expertise are prerequisites for reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.38).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.22).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Scripture Alone (Sola Scriptura) as Authoritative Standard").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theological/religious").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, 'eba147d7-fd10-441a-8555-f124e4009464').
narrative_ontology:cs_kernel_codification('eba147d7-fd10-441a-8555-f124e4009464', fixed_text).
narrative_ontology:cs_authority_grounding('eba147d7-fd10-441a-8555-f124e4009464', practice).
narrative_ontology:cs_interpretation_layer_present('eba147d7-fd10-441a-8555-f124e4009464').
narrative_ontology:cs_reading_relation('eba147d7-fd10-441a-8555-f124e4009464', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_reading_relation('eba147d7-fd10-441a-8555-f124e4009464', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('eba147d7-fd10-441a-8555-f124e4009464', foundational, scripture_self_sufficiency_for_doctrine).
narrative_ontology:cs_axiom_status(scripture_self_sufficiency_for_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('eba147d7-fd10-441a-8555-f124e4009464', scripture_self_sufficiency_for_doctrine, deontological).
narrative_ontology:cs_axiom('eba147d7-fd10-441a-8555-f124e4009464', foundational, lay_interpretive_autonomy_without_magisterial_monopoly).
narrative_ontology:cs_axiom_status(lay_interpretive_autonomy_without_magisterial_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('eba147d7-fd10-441a-8555-f124e4009464', lay_interpretive_autonomy_without_magisterial_monopoly, deontological).
narrative_ontology:cs_reference_frame('eba147d7-fd10-441a-8555-f124e4009464', congregational_interpretive_autonomy).
narrative_ontology:cs_drift_state('eba147d7-fd10-441a-8555-f124e4009464', contemporary_historical_critical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('eba147d7-fd10-441a-8555-f124e4009464', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, congregational_communities).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, reformed_pastorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, anabaptist_and_radical_reformers).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, magisterial_clergy).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, perspicuity_of_scripture).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, universal_priesthood_of_believers).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, congregational_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under sola scriptura, gain direct interpretive authority and responsibility. Access to Scripture in vernacular languages enables personal reading and doctrinal reasoning without mediation through ordained clergy. Exit from the arrangement means accepting diminished interpretive agency in alternative frameworks (conciliar or magisterial traditions).
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, global).

% Gain institutional autonomy to set doctrine and practice by congregational consensus, grounded in shared Scripture reading rather than hierarchical adjudication. This independence comes at the cost of doctrinal fragmentation: no overarching authority to harmonize interpretation across congregations; schism risk rises when Scripture admits multiple readings.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, congregational_communities, beneficiary,
    organized, generational, constrained, regional).

% Sets the interpretive standard by teaching Scripture authoritatively within congregations, but does not hold magisterial monopoly on interpretation. Authority flows from pastoral competence, theological training, and congregational trust, not from sacramental ordination or hierarchical appointment. Benefits from reduced extraction from laity; bears the burden of congregational dissent when interpretations diverge.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, reformed_pastorate, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, reformed_pastorate, beneficiary).

% Lose the monopoly over doctrinal interpretation that sola scriptura denies them. The constraint's operation strips away the institutional claim that only ordained clergy can authoritatively declare doctrine. Their exit would require rejecting sola scriptura entirely and returning to tradition-mediated authority structures, but such a move would alienate their own constituencies where sola scriptura norms have become embedded.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, magisterial_clergy, payer,
    institutional, generational, trapped, global).

% Codify and defend sola scriptura as the standard for doctrine and practice. These councils (Heidelberg Catechism, Westminster Confession, etc.) provide interpretive guidance while maintaining that Scripture alone is the ultimate authority. They operate as teaching instruments, not as magisterial replacements.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, reformation_councils_and_synods, agenda_setter,
    organized, generational, constrained, regional).

% Embrace sola scriptura but apply it more radically than mainstream reformed churches, deriving different doctrines (adult baptism, pacifism, church-state separation) from Scripture. Are excluded from institutional Reformed councils as heretical while theoretically sharing the same authority principle; the exclusion reveals that sola scriptura does not resolve disagreement about Scripture's meaning.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, anabaptist_and_radical_reformers, beneficiary,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, anabaptist_and_radical_reformers, excluded).

% Would interpret Scripture through patristic consensus and living tradition; are structurally barred from adjudicating Reformed doctrine since sola scriptura denies them authority. Their exclusion is the mechanism of the constraint: the claim that councils lack magisterial binding power on doctrine.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_councils, excluded,
    institutional, civilizational, trapped, global).

% Analyze Scripture as historical document, identifying layers of composition, scribal variation, and redaction. From this seat, the self-interpreting claim appears incomplete: Scripture's meaning depends on resolving textual uncertainties, original intent, and historical context — interpretive tasks that require expertise beyond mere reading.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, historical_textual_critics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared canonical text (the 66-book Protestant canon) as the binding authority for doctrine and practice, enabling congregations to self-govern without mediation through hierarchical clerical structures or magisterial adjudication. The coordination problem solved: how do communities maintain doctrinal coherence and practice alignment without conciliar or papal hierarchy?
% TRANSFER_FUNCTION: Transfers interpretive authority from ordained magisterium (Catholic/Orthodox clergy claiming monopoly on tradition) to congregational leaders and informed laity. The lay believer gains the right and responsibility to read Scripture directly and reason toward doctrine; the magisterial clergy loses the exclusive claim that only they can authoritatively declare what Scripture means.
% ABSENT_VOICES: Ecumenical councils, magisterial traditions, and textual critics who would argue that Scripture's meaning is irreducibly contested, that councils serve necessary adjudicative functions, and that the self-interpreting claim obscures the role of tradition and expertise in making sense of the text. Anabaptist radicals share the sola scriptura principle but are excluded from mainstream Reformed institutional governance when their readings diverge.
% DISAPPEARANCE_RATIONALE: If sola scriptura as binding authority vanished (replaced by tradition-mediated or conciliar frameworks), Reformed congregations would either reorganize around alternative authorities (magisterium, ecumenical councils, creeds as interpreted by living hierarchies) or dissolve as unified bodies. The doctrinal autonomy, congregational self-governance, and lay interpretive standing that depend on sola scriptura would have to be reconstituted under a different principle.
% FOUNDING_PROBLEM: Medieval and early modern Catholicism concentrated doctrinal authority in the papacy and magisterium, using tradition as a parallel authority to Scripture to justify practices and doctrines not explicitly warranted by the text (indulgences, Marian veneration, clerical celibacy, sacramental efficacy claims). Lay believers had no direct access to Scripture (vernacular Bibles were prohibited) and no voice in doctrinal interpretation. Protestant Reformers asserted that Scripture alone is sufficient; printing and translation made the text accessible; and congregations should read and interpret for themselves.
% FOUNDING_PROBLEM_CORROBORATION: Reformed tradition affirms the founding problem remains live: Scripture is still contested by false traditions and unauthorized clergy; vernacular accessibility is still needed to prevent magisterial monopoly. Catholic/Orthodox traditions deny the problem's premise: tradition is not a competing authority but a living continuity with Scripture; councils and magisterium adjudicate rightly because they inherit apostolic authority, not because they usurp lay prerogatives. Secular historians and textual scholars affirm the accessibility problem (lay access to texts in the 16th century was genuinely novel) but dispute the sufficiency claim (they attest that Scripture's meaning is inherently ambiguous and requires scholarly apparatus beyond reading).
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.38 at interval end) because the constraint produces genuine lay beneficiaries without a concentrated rent-capturing seat. The reformed pastorate and reformation councils do exercise interpretive influence, but this is framed as teaching and guidance, not magisterial monopoly. The beneficiaries (lay believers, congregations) gain authority without paying extraction; the payers (magisterial clergy, doctrinal coherence) lose authority and pay the cost of fragmentation. Theater is low (0.18) because the constraint's primary function (establishing congregational autonomy and lay interpretive standing) remains directly functional — the doctrinal fragmentation that appears as a cost is structurally necessary to the coordination function. Suppression is very low (0.22) because the constraint does not require active coercion of dissenting voices; it operates by institutional denial of magisterial authority, not by silencing those who might claim it. Accessibility collapse is high (0.72) because once Scripture is the declared sole authority, alternatives (tradition, councils, magisterium) are structurally closed off from claiming adjudicative power in Reformed frameworks. Resistance is moderate (0.58) because Catholic/Orthodox traditions actively contest the foundational claim, and textual scholars resist the sufficiency thesis — the constraint meets steady, organized resistance even within Christianity. Measurements are authored on one shared grid (1517, 1650, 1800, 1900, 1980, 2024) so every metric appears at every time point. Early (1517) values are marked 'projected' (the constraint was not yet institutionalized); later values are 'observed' (historical record of Reformed institutional practice, doctrine disputes, and textual scholarship).
 *
 * PERSPECTIVAL GAP:
 *   From the lay believer seat, sola scriptura appears as genuine coordination: shared access to authoritative text, congregational voice, and interpretive autonomy. From the magisterial clergy seat, the same structure appears as foreclosure: loss of inherited authority, institutional power stripped away, replaced by congregational populism. From the doctrinal-coherence analytical seat, the constraint appears as cost-bearer: fragmentation is the price paid for lay autonomy. These divergences are structural asymmetries the engine captures through directionality derivation — they are not contradictions, but different positioning relative to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers sit at d near 0.25 (beneficiaries, mobile exit, moderate power — they gain interpretive authority and direct text access without paying extraction). Congregational communities sit at d near 0.30 (beneficiaries, constrained exit tied to regional institutions, organized power — they gain autonomy but are constrained by congregation membership and doctrinal fragmentation). Reformed pastorate sits at d near 0.40 (agenda-setter beneficiary, arbitrage exit, powerful institutional position — they gain from reduced extraction and congregational trust but depend on maintaining the sola scriptura framework). Magisterial clergy sit at d near 0.65 (payers, trapped exit, institutional power — they lose monopoly and cannot exit without alienating their own constituencies where sola scriptura norms have embedded). Doctrinal coherence sits at d = 1.0 (pure target, analytical power — it bears the cost of fragmentation). The engine computes directionality from this structural data; no override is needed because the beneficiary/victim declarations map cleanly to power atoms and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (magisterial monopoly and lay textual exclusion in late medieval Catholicism) drove the constraint's creation. The problem's status is 'contested': Reformed tradition affirms it persists (magisterial overreach continues; vernacular access is still needed as a safeguard); Catholic/Orthodox deny the problem's premise entirely (magisterium is not usurpation but apostolic continuity; tradition complements Scripture). Secular scholars affirm the *accessibility* problem was solved (printing, vernacular Bibles) but deny the *sufficiency* claim (Scripture's meaning requires expertise, tradition, and critical apparatus, not just reading). The constraint does NOT exhibit mandatrophy because the founding problem's live status — disputed but not dead — keeps the arrangement functional to its core constituencies. A mandatrophy signal would appear if (1) Reformed communities began claiming that doctrinal fragmentation is itself a problem requiring magisterial solution, OR (2) the barrier to magisterial reentry lowered enough that the institutional cost of maintaining sola scriptura exceeded the perceived benefit. Neither has occurred at scale. Theater is low enough (0.18) that the constraint's operation is not primarily performative — the doctrinal fragmentation and lay interpretive authority are real outcomes, not theatrical maintenance of a defunct principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_interpreting_claim_ambiguity,
    'What does ''self-interpreting'' mean for Scripture, and does the claim survive textual ambiguity, historical context requirements, and grammatical complexity?',
    'Examine historical practice: compare interpretive divergence within sola scriptura communities (Reformed vs. evangelical vs. fundamentalist readings of the same texts) against divergence within magisterial traditions (Catholic doctrine on salvation, Mary, purgatory across centuries). High divergence in both suggests ''self-interpreting'' is incomplete; if sola scriptura communities show dramatically less fragmentation, the claim is vindicated.',
    'If ''self-interpreting'' is overextended (does not account for genuine textual complexity), the constraint is more snare-like than rope-like: it uses the sufficiency claim to license congregational autonomy while obscuring that interpretation requires tradition or expertise. If vindicated, the constraint is genuine rope: coordination achieved through shared text without monopoly interpretive hierarchy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_interpreting_claim_ambiguity, empirical, 'Whether Scripture''s meaning can be accessed without tradition, expertise, or magisterial guidance.').

omega_variable(
    doctrinal_fragmentation_as_feature_or_flaw,
    'Is the doctrinal fragmentation produced by sola scriptura an acceptable cost of lay autonomy, or does the cost eventually demand institutional remedy?',
    'Historical trajectory analysis: track whether Reformed communities gravitate toward re-institutionalizing authority (creeds, confessions, synods) as doctrinal standards in practice if not in theory. If practice converges on binding creeds despite sola scriptura, fragmentation is a flaw the constraint cannot bear; if doctrinal diversity persists as acceptable, it is a feature of congregational autonomy.',
    'If fragmentation is eventually treated as flaw, the constraint slides toward theater: sola scriptura remains nominal authority while practical authority devolves to confessions and councils (piton-like degradation). If fragmentation persists as accepted feature, the constraint remains functional as genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_fragmentation_as_feature_or_flaw, empirical, 'Whether doctrinal diversity is the intended structure or an unacceptable cost.').

omega_variable(
    sola_scriptura_vs_conciliar_foreclosure,
    'Does the sola scriptura reading logically foreclose the conciliar reading (councils have binding authority mediated through tradition), or can both coexist in the same theological framework?',
    'Examine whether Lutheran, Reformed, and Anglican traditions (all affirming sola scriptura in principle) can simultaneously accept ecumenical councils as doctrinally binding (Nicaea, Constantinople). If yes, they coexist; if no, foreclosure is real.',
    'Foreclosure would make this reading structurally incompatible with conciliar authority; coexistence would allow overlapping commitments and reduce the sharpness of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sola_scriptura_vs_conciliar_foreclosure, conceptual, 'Whether sola scriptura and conciliar authority are logically compatible.').

omega_variable(
    magisterial_extraction_mechanism,
    'Is the low suppression (0.22) measurement accurate, or does sola scriptura require active suppression of magisterial claims to maintain its institutional position?',
    'Analyze historical enforcement: did Reformed churches prohibit preaching magisterial doctrine, burn Catholic books, exclude Catholic clergy? Or did sola scriptura persist mainly through institutional refusal to grant magisterial authority without active coercion? Records from Reformation era (active persecution) vs. post-Reformation era (institutional separation) would show whether suppression was front-loaded or sustained.',
    'If sola scriptura required sustained heavy suppression, the low suppression score is historically inaccurate and masks a more coercive constraint. If institutional refusal (rather than active coercion) sufficed, the low score is correct and the constraint is genuinely low-suppression rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magisterial_extraction_mechanism, empirical, 'Whether sola scriptura''s persistence depends on active suppression of magisterial voices.').

omega_variable(
    lay_autonomy_vs_clerical_reconcentration,
    'Does sola scriptura actually produce lay interpretive autonomy, or does reformed pastorate reconcentrate interpretive authority by becoming a new clergy class?',
    'Examine whether lay members of Reformed congregations in practice interpret Scripture independently and authoritatively, or whether they defer to pastoral/theological specialists while nominally accepting sola scriptura. Surveys of congregational Bible study vs. pastoral preaching patterns would show whether lay autonomy is real or theatrical.',
    'If lay autonomy is theatrical (specialists reconcentrate authority under a different name), extractiveness and theater_ratio are underestimated; the constraint is more snare-like (cover story for clerical authority relabeled as congregational leadership). If real, lay autonomy is genuine beneficiary position and the rope classification is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_autonomy_vs_clerical_reconcentration, empirical, 'Whether sola scriptura produces genuine lay interpretive authority or reconcentrates it in reformed clerics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 1517, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1517, biblical_authority__sola_scriptura_reading, theater_ratio, 1517, 0.08).
narrative_ontology:measurement_basis(bibl_tr_t1517, projected).
narrative_ontology:measurement(bibl_tr_t1650, biblical_authority__sola_scriptura_reading, theater_ratio, 1650, 0.12).
narrative_ontology:measurement_basis(bibl_tr_t1650, observed).
narrative_ontology:measurement(bibl_tr_t1800, biblical_authority__sola_scriptura_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t1800, observed).
narrative_ontology:measurement(bibl_tr_t1900, biblical_authority__sola_scriptura_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t1900, observed).
narrative_ontology:measurement(bibl_tr_t1980, biblical_authority__sola_scriptura_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t1980, observed).
narrative_ontology:measurement(bibl_tr_t2024, biblical_authority__sola_scriptura_reading, theater_ratio, 2024, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1517, biblical_authority__sola_scriptura_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement_basis(bibl_be_t1517, projected).
narrative_ontology:measurement(bibl_be_t1650, biblical_authority__sola_scriptura_reading, base_extractiveness, 1650, 0.28).
narrative_ontology:measurement_basis(bibl_be_t1650, observed).
narrative_ontology:measurement(bibl_be_t1800, biblical_authority__sola_scriptura_reading, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement_basis(bibl_be_t1800, observed).
narrative_ontology:measurement(bibl_be_t1900, biblical_authority__sola_scriptura_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(bibl_be_t1900, observed).
narrative_ontology:measurement(bibl_be_t1980, biblical_authority__sola_scriptura_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement_basis(bibl_be_t1980, observed).
narrative_ontology:measurement(bibl_be_t2024, biblical_authority__sola_scriptura_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(bibl_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1517, biblical_authority__sola_scriptura_reading, suppression_requirement, 1517, 0.1).
narrative_ontology:measurement_basis(bibl_su_t1517, projected).
narrative_ontology:measurement(bibl_su_t1650, biblical_authority__sola_scriptura_reading, suppression_requirement, 1650, 0.18).
narrative_ontology:measurement_basis(bibl_su_t1650, observed).
narrative_ontology:measurement(bibl_su_t1800, biblical_authority__sola_scriptura_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement_basis(bibl_su_t1800, observed).
narrative_ontology:measurement(bibl_su_t1900, biblical_authority__sola_scriptura_reading, suppression_requirement, 1900, 0.21).
narrative_ontology:measurement_basis(bibl_su_t1900, observed).
narrative_ontology:measurement(bibl_su_t1980, biblical_authority__sola_scriptura_reading, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement_basis(bibl_su_t1980, observed).
narrative_ontology:measurement(bibl_su_t2024, biblical_authority__sola_scriptura_reading, suppression_requirement, 2024, 0.22).
narrative_ontology:measurement_basis(bibl_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, information_standard).
narrative_ontology:boltzmann_floor_override(biblical_authority__sola_scriptura_reading, 0.06).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% Sola scriptura is one of three readings of the contested kernel 'biblical_authority'. The kernel is the Christian commitment to Scripture's authority; different parties read it as (1) Scripture-alone-self-interpreting (this story), (2) Scripture-within-conciliar-tradition (conciliar_reading), or (3) Scripture-requiring-magisterium (tradition_scripture_reading). Each reading instantiates a different constraint with different ε values and victim sets. Sola scriptura produces lay beneficiaries and doctrinal fragmentation; conciliar reading produces patristic beneficiaries and collegial authority; magisterial reading produces clerical monopoly and doctrinal coherence. They coexist in contemporary Christianity as competing institutional frameworks; each influences the others through competing claims of interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
