% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II as Organic Development Within Unbroken Tradition (Continuity Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   In the decades following the Second Vatican Council (1962-1965), a
 *   dominant interpretive framework emerged — most explicitly articulated by
 *   Pope Benedict XVI in his 2005 address to the Roman Curia — holding that
 *   the Council's documents must be read as continuous development of
 *   doctrine rather than rupture with prior magisterial teaching. This
 *   'hermeneutic of continuity' became the institutionally sanctioned lens
 *   for adjudicating disputes over implementation: whether vernacular
 *   liturgical practice exceeded SC's textual warrant, whether Dignitatis
 *   Humanae's religious freedom teaching contradicted the Syllabus of Errors,
 *   whether episcopal collegiality diminished papal primacy. The reading
 *   performs genuine coordination work — it lets a global, doctrinally
 *   unified institution absorb conciliar reform without triggering a crisis
 *   over magisterial indefectibility — but it also functions as a
 *   disciplinary tool against both progressive pastoral innovation and
 *   traditionalist claims of illegitimate rupture, and its authority is
 *   administered by the same institutional actors whose continuity is being
 *   vindicated.
 *
 * KEY AGENTS:
 *   - curial_hermeneutics_office: agenda_setter (institutional/arbitrage) — issues and enforces the authoritative interpretive standard
 *   - conservative_magisterial_theologians: beneficiary (organized/mobile) — professional and institutional standing built on the continuity framework
 *   - progressive_pastoral_reformers: payer (moderate/constrained) — pastoral initiatives recharacterized as unauthorized under the standard
 *   - vernacular_liturgy_practitioners: payer (powerless/constrained) — lived practice subordinated to textual Latin-preservation mandate
 *   - traditionalist_rupture_claimants: payer/excluded (organized/mobile) — factual rupture diagnosis foreclosed by the continuity reading's denial that any break occurred
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.42).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II as Organic Development Within Unbroken Tradition (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, 'fe524115-9834-46ac-8d7b-2ac92457da8c').
narrative_ontology:cs_kernel_codification('fe524115-9834-46ac-8d7b-2ac92457da8c', fixed_text).
narrative_ontology:cs_authority_grounding('fe524115-9834-46ac-8d7b-2ac92457da8c', lineage).
narrative_ontology:cs_interpretation_layer_present('fe524115-9834-46ac-8d7b-2ac92457da8c').
narrative_ontology:cs_reading_relation('fe524115-9834-46ac-8d7b-2ac92457da8c', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('fe524115-9834-46ac-8d7b-2ac92457da8c', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('fe524115-9834-46ac-8d7b-2ac92457da8c', foundational, magisterial_indefectibility_precludes_doctrinal_reversal).
narrative_ontology:cs_axiom_status(magisterial_indefectibility_precludes_doctrinal_reversal, holdable).
narrative_ontology:cs_axiom_grounding('fe524115-9834-46ac-8d7b-2ac92457da8c', magisterial_indefectibility_precludes_doctrinal_reversal, theological).
narrative_ontology:cs_axiom('fe524115-9834-46ac-8d7b-2ac92457da8c', foundational, conciliar_texts_bind_implementation_to_prior_magisterium).
narrative_ontology:cs_axiom_status(conciliar_texts_bind_implementation_to_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('fe524115-9834-46ac-8d7b-2ac92457da8c', conciliar_texts_bind_implementation_to_prior_magisterium, conventional).
narrative_ontology:cs_axiom('fe524115-9834-46ac-8d7b-2ac92457da8c', secondary, spirit_of_council_appeals_lack_magisterial_warrant).
narrative_ontology:cs_axiom_status(spirit_of_council_appeals_lack_magisterial_warrant, holdable).
narrative_ontology:cs_axiom_grounding('fe524115-9834-46ac-8d7b-2ac92457da8c', spirit_of_council_appeals_lack_magisterial_warrant, conventional).
narrative_ontology:cs_reference_frame('fe524115-9834-46ac-8d7b-2ac92457da8c', tridentine_magisterial_continuity).
narrative_ontology:cs_drift_state('fe524115-9834-46ac-8d7b-2ac92457da8c', post_conciliar_reception_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fe524115-9834-46ac-8d7b-2ac92457da8c', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, curial_hermeneutics_office).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, conservative_magisterial_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, episcopal_conference_continuity_faction).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_pastoral_reformers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, vernacular_liturgy_practitioners).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, traditionalist_rupture_claimants).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, magisterial_indefectibility_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, hermeneutic_of_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues authoritative interpretive guidance (post-conciliar instructions, CDF clarifications, papal addresses like Benedict XVI's 2005 Curia speech) establishing that conciliar texts must be read as continuous with prior magisterium. Adjudicates disputes about whether a given implementation (liturgical, ecumenical, doctrinal) is faithful development or unauthorized rupture. Controls which readings receive institutional sanction.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, curial_hermeneutics_office, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Academic and clerical theologians whose scholarly and institutional standing depends on demonstrating that conciliar documents (Lumen Gentium, Dei Verbum, Dignitatis Humanae) can be harmonized with Trent, Vatican I, and the Syllabus of Errors. Their careers, publications, and appointments are validated by the continuity framework; exit would mean abandoning a research program with institutional backing.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, conservative_magisterial_theologians, beneficiary,
    organized, generational, mobile, global).

% Bishops and conferences who govern dioceses by citing the continuity reading to restrain pastoral experimentation ('the Council did not authorize this') and to discipline priests or movements invoking 'the spirit of Vatican II.' Their governing authority is reinforced by the claim that only text-bound, magisterially-supervised implementation is legitimate.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, episcopal_conference_continuity_faction, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, episcopal_conference_continuity_faction, agenda_setter).

% Priests, religious, and lay movements who read the Council as licensing broader liturgical inculturation, collegial governance, and doctrinal development beyond the letter of the texts. Under the continuity reading their initiatives are recharacterized as unauthorized rupture and can be disciplined, defunded, or suppressed; leaving the institution means losing clerical status or ministry access.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressive_pastoral_reformers, payer,
    moderate, biographical, constrained, national).

% Parish communities and clergy who implemented vernacular and adapted liturgical practice in the years after the Council, relying on Sacrosanctum Concilium's opening toward vernacular use. The continuity reading's insistence that SC §36's Latin-preservation clause is binding subordinates their lived practice to a textual standard they did not control and cannot easily litigate against.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, vernacular_liturgy_practitioners, payer,
    powerless, biographical, constrained, local).

% Groups (e.g. SSPX-aligned communities) who agree with the rupture reading's premise that the Council broke with prior teaching but draw the opposite evaluative conclusion — treating the break as illegitimate and refusing implementation. The continuity reading forecloses their claim by denying any rupture occurred at all, leaving them structurally outside the accepted interpretive field even though their factual diagnosis overlaps with the rupture reading.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_rupture_claimants, payer,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, traditionalist_rupture_claimants, excluded).

% The broader body of Catholics who receive the continuity or rupture framing largely secondhand through catechesis, homilies, and media, without direct access to the interpretive dispute. Their practice is shaped by whichever reading their local ecclesial authority adopts.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, lay_faithful_general, observer,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, curial_hermeneutics_office).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpretive key allowing a global, multi-generational institution to implement conciliar reforms without each diocese, order, or theologian independently relitigating whether Vatican II broke with prior teaching — coordinating doctrinal stability across a body with no central enforcement mechanism beyond magisterial pronouncement.
% TRANSFER_FUNCTION: Moves interpretive authority and disciplinary legitimacy toward institutional actors (curial offices, continuity-aligned bishops, magisterial theologians) and away from pastoral innovators, vernacular practitioners, and traditionalist rupture-claimants, by defining the range of admissible readings of the Council's texts.
% ABSENT_VOICES: Progressive reformers who believe the Council licensed a genuine paradigm shift, and traditionalists who believe it constituted an illegitimate break, are structurally excluded from co-authoring the interpretive standard even though both groups' practice is directly regulated by it — the continuity reading's own institutional apparatus (curial offices, magisterial theology chairs) adjudicates the dispute in which it is also a party.
% DISAPPEARANCE_RATIONALE: If the continuity reading lost magisterial sanction overnight, disciplinary actions against 'spirit of Vatican II' pastoral initiatives would lose their textual warrant, SSPX-style traditionalist communities would gain a stronger claim to have correctly diagnosed rupture (even while disputing its legitimacy), and doctrinal disputes (religious freedom, ecumenism, collegiality) would re-open as live questions rather than settled developments — decades of catechetical and disciplinary practice built on the continuity framing would require re-justification.
% FOUNDING_PROBLEM: The Council needed retrospective interpretive framing that would allow its documents — many deliberately ambiguous compromise formulations reconciling competing conciliar factions — to be received without appearing to repudiate prior infallible or authoritative teaching, which would threaten the doctrine of magisterial indefectibility itself.
% FOUNDING_PROBLEM_CORROBORATION: Popes Benedict XVI and John Paul II attest the continuity reading as authoritative teaching from within the magisterium itself. Independent historians of the Council (e.g. the Bologna school associated with Alberigo, and secular intellectual historians of 20th-century Catholicism) attest, from outside the magisterial apparatus, that conciliar drafting records and floor debates show genuine doctrinal shifts on religious freedom and collegiality that the continuity framing retrospectively harmonizes rather than straightforwardly describes — corroboration for the founding-problem-as-live claim comes almost entirely from within the benefiting institutional structure.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).
:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the continuity reading performs a real coordination function (doctrinal stability across a global, non-centrally-enforced institution) alongside its disciplinary use, so this is authored as tangled_rope rather than snare. Suppression is higher (0.55) because maintaining the reading against both progressive and traditionalist counter-readings requires active magisterial intervention — encyclicals, CDF instructions, disciplinary actions against dissenting theologians and communities. Theater ratio is moderate (0.30): substantial catechetical and pastoral apparatus exists to perform continuity (anniversary celebrations, curial commentary) alongside genuine doctrinal governance. The temporal series shows extraction and suppression rising through the 1970s-2013 period (peak institutional assertion under John Paul II and Benedict XVI) with a slight relaxation by 2025 reflecting a somewhat more permissive posture under Francis toward pastoral latitude, though the continuity framework itself remains formally intact.
 *
 * PERSPECTIVAL GAP:
 *   From the curial office's seat, the continuity reading is a mountain-adjacent fact about doctrinal indefectibility — a natural consequence of what the Church is. From the vernacular liturgy practitioner's seat, the same textual apparatus (SC §36's Latin clause) operates as an actively enforced constraint on lived, decades-settled pastoral practice. The engine computing divergent seat classifications from the same structural data is the intended behavior here, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The curial hermeneutics office and continuity-aligned bishops sit at the beneficiary end: they administer the standard and their governing authority is reinforced by it (low d). Conservative magisterial theologians benefit indirectly through validated career and institutional standing. Progressive pastoral reformers and vernacular liturgy practitioners sit toward the target end: their practice is judged against a standard they did not author and cannot easily contest given constrained exit (clerical status, ministry access). Traditionalist rupture claimants are structurally unusual — they are payers who are also excluded, because the continuity reading does not merely regulate their practice, it denies the premise (that a rupture occurred) on which their entire position rests, foreclosing their claim rather than merely taxing it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling conciliar reform with magisterial indefectibility — remains genuinely live for the institution as a whole (doctrinal continuity is not a solved problem in any generation), which argues against calling this pure mandatrophy. But the specific disciplinary uses of the continuity reading against pastoral innovators and vernacular practitioners increasingly serve institutional control functions disconnected from the original doctrinal-coherence problem, which is why this is authored as tangled_rope: a genuine coordination function persists alongside an extraction function that has grown large enough to require its own justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vatican_ii_kernel_reading_selection,
    'Is the continuity reading the correct account of what Vatican II''s texts actually did, relative to the rupture reading (fundamental break with prior teaching) and the composite_overdetermination reading (ambiguous compromise texts encoding incompatible visions)?',
    'This is not resolvable by further textual analysis alone — drafting history (conciliar acta, floor debate records) supports partial claims for each reading; the disagreement is partly conceptual (what counts as ''development'' versus ''rupture'' is itself contested) and partly a matter of which interpretive authority is granted final adjudicating power. A sibling constraint exists for each reading; this omega documents that the selection among them is the committer''s, not a resolved empirical fact.',
    'If the rupture reading is structurally correct, the continuity reading''s enforcement apparatus is defending a false description of what happened, converting its tangled_rope coordination claim into something closer to pure extraction (snare) dressed as doctrinal fidelity. If the composite reading is correct, both continuity and rupture readings are each partially right about different textual strata, and the disciplinary use of either single reading against dissenters is itself the extractive move.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vatican_ii_kernel_reading_selection, conceptual, 'Kernel-reading selection ambiguity: continuity vs. rupture vs. composite readings of Vatican II.').

omega_variable(
    dh_syllabus_reconciliation_mechanism,
    'Does the thesis/hypothesis distinction (or the broader ''development of doctrine'' framework) genuinely reconcile Dignitatis Humanae''s religious freedom teaching with the Syllabus of Errors'' condemnation of religious liberty, or does it retroactively relabel a doctrinal reversal as development?',
    'Comparative analysis of the semantic content of 19th-century magisterial condemnations against DH''s positive teaching, cross-checked against how the continuity reading''s own architects (e.g. John Courtney Murray''s drafting influence) understood the shift at the time versus how it was characterized afterward.',
    'If the reconciliation mechanism is sound, the continuity reading''s coordination claim is well-founded and the tangled_rope''s coordination component is genuine rather than cover. If it is a retroactive relabeling, the continuity reading''s core distinguishing axiom is weaker than claimed, and its enforcement against rupture-claimants is harder to justify as anything but institutional self-protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dh_syllabus_reconciliation_mechanism, empirical, 'Whether the thesis/hypothesis distinction genuinely reconciles DH with the Syllabus or relabels a reversal.').

omega_variable(
    false_summit_indefectibility_claim,
    'Is magisterial indefectibility (the doctrine this reading vindicates) itself a naturally-arising theological necessity, or a constructed doctrinal commitment that happens to benefit the institutional actors who administer its interpretation?',
    'This constraint is authored as tangled_rope, not mountain, so FSM does not directly apply here — but the vindicated proposition (magisterial_indefectibility_doctrine) sits adjacent to mountain-like framing in Catholic theology. Comparative ecclesiology (how other apostolic traditions, e.g. Orthodox conciliarism, handle doctrinal continuity without an equivalent indefectibility claim) would help triangulate whether indefectibility is theologically necessary or institutionally convenient.',
    'If indefectibility is a constructed doctrine rather than a theological necessity, the continuity reading''s foundational axiom is less secure than its holdable status suggests, strengthening the case that this tangled_rope''s coordination component is thinner than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_indefectibility_claim, conceptual, 'Whether magisterial indefectibility is a natural theological necessity or a constructed, institutionally convenient doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement_basis(vati_tr_t1965, observed).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement_basis(vati_tr_t1975, observed).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1985, 0.24).
narrative_ontology:measurement_basis(vati_tr_t1985, observed).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(vati_tr_t2000, observed).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2013, 0.33).
narrative_ontology:measurement_basis(vati_tr_t2013, observed).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement_basis(vati_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1965, 0.25).
narrative_ontology:measurement_basis(vati_be_t1965, observed).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1975, 0.32).
narrative_ontology:measurement_basis(vati_be_t1975, observed).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1985, 0.36).
narrative_ontology:measurement_basis(vati_be_t1985, observed).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement_basis(vati_be_t2000, observed).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2013, 0.44).
narrative_ontology:measurement_basis(vati_be_t2013, observed).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(vati_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement_basis(vati_su_t1965, observed).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1975, 0.45).
narrative_ontology:measurement_basis(vati_su_t1975, observed).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement_basis(vati_su_t1985, observed).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement_basis(vati_su_t2000, observed).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement_basis(vati_su_t2013, observed).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(vati_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__continuity_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the vatican_ii_magisterial_authority kernel. continuity_reading (this file) holds organic development with no rupture; rupture_reading holds fundamental doctrinal break; composite_overdetermination_reading holds that the conciliar texts are ambiguous compromise formulations encoding multiple incompatible ecclesiologies simultaneously, making any single-reading claim (continuity or rupture) an overdetermination. Each reading has its own ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
