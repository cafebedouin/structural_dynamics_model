% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority — Rupture Reading
 *   domain: Ecclesiology / Institutional History / Hermeneutics
 *
 * SUMMARY:
 *   This constraint models the rupture reading of Vatican II's magisterial
 *   authority: the claim that the Council's texts encode a deliberate,
 *   substantive break with specific pre-conciliar teachings (notably on
 *   religious liberty, the theology of other religions, and liturgical form),
 *   such that post-conciliar institutional life is built on discontinuity
 *   rather than mere restatement. This is one of three sibling readings of
 *   the same kernel (vatican_ii_magisterial_authority); the
 *   continuity_reading and composite_overdetermination_reading are separate
 *   constraint stories with their own ε values, beneficiary/victim
 *   structures, and classifications. Do not average across them — this story
 *   is a clean, self-contained account of the rupture reading alone.
 *
 * KEY AGENTS:
 *   - progressive_episcopal_conferences: agenda_setter (institutional/arbitrage) — implements the discontinuity reading structurally
 *   - reform_oriented_theologians: beneficiary (organized/mobile) — professional authority derived from the rupture premise
 *   - post_conciliar_liturgical_establishment: beneficiary/agenda_setter (institutional/arbitrage) — institutional survival tied to discontinuity holding
 *   - traditionalist_clergy_and_laity: payer (moderate/constrained) — delegitimized formation and canonical precarity
 *   - religious_orders_disrupted_by_reform: payer (powerless/trapped) — charism reinterpreted without consent
 *   - pre_conciliar_formed_seminarians: payer (powerless/trapped) — formation rendered obsolete
 *   - roman_curia_doctrinal_offices: observer/agenda_setter (institutional/analytical) — partial, contested adjudicating authority
 *   - sspx_and_traditionalist_societies: excluded (moderate/trapped) — object to the premise itself, held outside normalized standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.52).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.44).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority — Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "Ecclesiology / Institutional History / Hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, 'e04dc61b-bf4f-45a3-abb4-07b69cc93f7f').
narrative_ontology:cs_kernel_codification('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', fixed_text).
narrative_ontology:cs_authority_grounding('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', lineage).
narrative_ontology:cs_interpretation_layer_present('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f').
narrative_ontology:cs_reading_relation('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', foundational, doctrinal_reversal_is_possible_and_occurred).
narrative_ontology:cs_axiom_status(doctrinal_reversal_is_possible_and_occurred, holdable).
narrative_ontology:cs_axiom_grounding('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', doctrinal_reversal_is_possible_and_occurred, conventional).
narrative_ontology:cs_axiom('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', secondary, error_has_no_rights_doctrine_superseded).
narrative_ontology:cs_axiom_status(error_has_no_rights_doctrine_superseded, overridden).
narrative_ontology:cs_axiom_grounding('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', error_has_no_rights_doctrine_superseded, empirically_contingent).
narrative_ontology:cs_reference_frame('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', pre_conciliar_propositional_magisterium).
narrative_ontology:cs_drift_state('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', post_conciliar_implementation_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('e04dc61b-bf4f-45a3-abb4-07b69cc93f7f', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, reform_oriented_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_liturgical_establishment).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy_and_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, religious_orders_disrupted_by_reform).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_formed_seminarians).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, doctrinal_development_can_include_genuine_reversal).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, historical_consciousness_supersedes_propositional_fixity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops' conferences that read the conciliar texts as license for structural and liturgical reinvention implement vernacular liturgy, collegial governance experiments, and revised catechetical content, treating the Council as a hinge that authorizes departure from pre-conciliar norms. They administer seminaries, publishing houses, and diocesan structures that now operate on the rupture premise.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_episcopal_conferences, agenda_setter,
    institutional, generational, arbitrage, global).

% Academic theologians whose careers, chairs, and publishing platforms are built on reading the Council as discontinuity with the pre-conciliar magisterium. They gain intellectual authority and institutional positions (theology faculties, curial advisory roles) precisely because the rupture reading legitimizes their revisionist scholarship as the Council's own self-understanding.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, reform_oriented_theologians, beneficiary,
    organized, generational, mobile, global).

% Liturgical commissions and publishing bodies that produced the new missal and catechetical materials derive ongoing institutional funding and authority from the premise that the old rite and its theology are superseded, not merely supplemented. Their continued relevance depends on the rupture framing holding.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_liturgical_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_liturgical_establishment, agenda_setter).

% Clergy and laity attached to the pre-conciliar liturgy and doctrinal formulations experience the rupture reading as delegitimizing their formation and, in periods of restriction, their access to the older rite and associated societies. Their canonical and sacramental standing has at times depended on episcopal discretion shaped by whichever reading of the Council prevails locally; exit means schism-adjacent status (SSPX-type arrangements) or quiet marginalization within structures.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy_and_laity, payer,
    moderate, biographical, constrained, global).

% Communities whose constitutions, habits, and apostolates were substantially restructured under mandates justified by the rupture reading of aggiornamento experienced defections, collapsed vocations, and loss of institutional identity in the following decades. Many had no meaningful voice in how their charism was reinterpreted and could not simply exit the reform process while remaining inside their order.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, religious_orders_disrupted_by_reform, payer,
    powerless, biographical, trapped, national).

% Clergy formed under the older theological curriculum found their formation treated as superseded almost overnight, with new ordinands trained under a discontinuous framework; those already ordained bore the cost of adaptation or found themselves professionally and pastorally sidelined by colleagues and superiors who had adopted the rupture reading as institutional policy.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_formed_seminarians, payer,
    powerless, biographical, trapped, regional).

% The offices charged with adjudicating doctrinal continuity (e.g., the doctrinal congregation) issue interpretive statements — some affirming continuity (Benedict XVI's hermeneutic-of-continuity address), some tolerating rupture-inflected implementation in practice — and thereby hold partial authority to resolve or perpetuate the interpretive contest without fully settling it.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, roman_curia_doctrinal_offices, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, roman_curia_doctrinal_offices, agenda_setter).

% Societies and communities that reject the rupture reading's premises entirely (and often the conciliar documents' authority on the contested points) are excluded from full communion or normalized canonical standing for extended periods; their objection that DH and collegiality contradict prior infallible teaching is precisely what the rupture reading affirms as true, which is also why their canonical position remains unresolved.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, sspx_and_traditionalist_societies, excluded,
    moderate, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative account of what the Council changed, allowing the institution to update liturgy, canon law, ecumenical posture, and catechesis coherently rather than leaving every diocese and order to improvise its own relationship to conciliar texts.
% TRANSFER_FUNCTION: Moves interpretive and institutional authority from formation, orders, and clergy shaped by the pre-conciliar magisterium to bishops, theological faculties, and publishing bodies whose authority derives from reading the Council as discontinuous; moves canonical standing and resources away from communities that reject the discontinuity premise.
% ABSENT_VOICES: Traditionalist clergy, disrupted religious communities, and societies like the SSPX object that the rupture reading treats a pastoral council's texts as reversing prior infallible or definitive teaching, but their objections are typically heard as disciplinary problems to be managed rather than as a live hermeneutical claim requiring adjudication by the same authority they are accused of resisting.
% DISAPPEARANCE_RATIONALE: If the rupture reading were formally abandoned by the offices with authority to adjudicate it, post-conciliar liturgical and theological institutions built on the discontinuity premise would need to re-justify their authority on other grounds, and traditionalist communities' standing would likely improve; but institutional actors dispute whether such abandonment is even coherent, since large parts of the post-conciliar Church's self-understanding are already built on the rupture premise having occurred.
% FOUNDING_PROBLEM: The Council itself was convened to address the Church's relationship to modernity — religious liberty, ecumenism, liturgical intelligibility, collegiality — and the rupture reading answers the genealogical question of what the Council actually did by asserting it deliberately broke with specific prior teachings (e.g., the denial of a natural right to religious liberty) rather than merely restating them in new language.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the rupture reading (reform theologians, some conciliar periti themselves, e.g. figures associated with the 'Bologna school' historiography) attest that discontinuity was the Council's actual achievement. Corroboration from outside the benefiting parties is mixed: Pope Benedict XVI, addressing the Roman Curia in 2005, explicitly named and rejected a 'hermeneutic of rupture' as a misreading, arguing for continuity — a corroborating voice against this reading from within the highest doctrinal authority itself. No neutral third party outside intra-Catholic doctrinal debate is positioned to adjudicate the historical claim independently.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that the rupture reading transfers real institutional authority, formation resources, and canonical legitimacy from pre-conciliar-aligned actors to post-conciliar reformist institutions — a genuine transfer, not merely a scholarly dispute, because canonical and financial consequences (parish closures, seminary curricula, order restructuring, and the decades-long irregular status of traditionalist societies) followed from which reading prevailed administratively. Suppression (0.44) is moderate: enforcement has been real (restrictions on the older liturgical form at various points, disciplinary actions against traditionalist clergy) but has also oscillated with different pontificates, unlike a constraint with a single stable coercive apparatus. Theater ratio (0.38) captures that a portion of ongoing institutional activity defending the rupture reading (commissions, symposia reaffirming 'the spirit of the Council') functions more to shore up post-conciliar institutional legitimacy than to resolve the underlying hermeneutical question. Accessibility collapse is moderate (0.4) — the pre-conciliar alternative was never fully eliminated (traditionalist societies, indult provisions, and later Summorum Pontificum kept it partially accessible), which is why this is not a snare with fully collapsed alternatives. Resistance is high (0.72) because organized, sustained pushback (traditionalist movements, Benedict XVI's own curial address rejecting rupture hermeneutics) has persisted for six decades without resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (episcopal conferences, liturgical establishment), the rupture reading looks like a rope: it solved a real coordination problem (updating a global institution's relationship to modernity) and the resulting authority is earned by having done the difficult reform work. From the payer seats (disrupted orders, traditionalist clergy), the same structure looks like enforced extraction: their formation and communal identity were declared obsolete by a reading they never accepted, and the coercive apparatus that followed (restricted access to the older rite, canonical marginalization) is what makes the reading operative rather than merely academic. The engine computes this divergence from the declared power/exit/beneficiary structure; this story does not adjudicate which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive episcopal conferences and the post-conciliar liturgical establishment sit near the beneficiary end: they administer the reading and derive ongoing institutional authority and resources from its being true. Reform theologians benefit similarly but with more exit mobility (academic careers can migrate across institutions). Traditionalist clergy, disrupted religious orders, and pre-conciliar-formed seminarians sit near the target end: they bear the cost of formation obsolescence and institutional marginalization with limited exit — leaving typically means schism-adjacent status, which is a severe, identity-costly exit rather than a a lateral move. The SSPX and similar societies are declared excluded rather than merely payer, because their core objection is to the premise itself, and the constraint's classification is exactly what keeps them outside rather than in dialogue as a contesting party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (renewing the Church's engagement with modernity) is genuinely contested as either still-live or already-resolved-and-superseded-by-new-problems; the rupture reading's proponents treat the discontinuity itself as the achievement, which makes the reading self-certifying in a way that resists falsification from inside its own institutional structure. Classifying this as tangled_rope rather than snare or mountain prevents two mislabelings: treating it as pure extraction would ignore the genuine coordination achievement of updating global liturgical and pastoral practice; treating it as natural/inevitable (mountain) would launder a contested historical-theological claim as settled fact, which is precisely the false-summit risk this reading's own beneficiaries have an interest in encouraging.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_versus_development_genealogy,
    'Is the discontinuity between Dignitatis Humanae and prior teaching on religious liberty (e.g., Pius IX''s Quanta Cura, Leo XIII''s Immortale Dei) a genuine doctrinal reversal, or a development that resolves apparent but not real contradiction by distinguishing levels of teaching authority and changed circumstances?',
    'Close textual-historical analysis of conciliar drafting history (the acta and relatio of the Council), combined with subsequent magisterial commentary (e.g., Benedict XVI''s 2005 curial address, and any future authoritative doctrinal clarification) that directly addresses whether DH''s teaching was intended and understood as a reversal.',
    'If genuine reversal is established as the authoritative reading, this constraint''s rupture_reading gains authoritative corroboration and the continuity_reading correspondingly weakens; if development-without-reversal is authoritatively established, this reading''s core premise is falsified and it would need reclassification or retraction as the dominant reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rupture_versus_development_genealogy, conceptual, 'Whether DH constitutes doctrinal reversal or compatible development — the central contested claim distinguishing this reading from its siblings.').

omega_variable(
    rupture_reading_self_certification_risk,
    'Does the rupture reading''s institutional dominance in post-conciliar liturgical and academic structures make it self-certifying — i.e., do the very institutions positioned to adjudicate the historical question have a structural interest in the rupture reading being true?',
    'Identify whether any adjudicating voice (magisterial or scholarly) corroborating either reading sits genuinely outside the institutional beneficiaries of that reading; track whether doctrinal offices with authority to settle the question have in fact done so definitively versus merely commented.',
    'If no non-beneficiary corroboration exists for the rupture reading, its classification risk shifts further toward tangled_rope-with-thin-legitimation or even toward a more purely extractive reading; strong non-beneficiary corroboration would support treating it as a defensible historical-theological claim rather than a self-serving institutional narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_reading_self_certification_risk, empirical, 'Whether the rupture reading is corroborated from outside the institutions that benefit from it being true.').

omega_variable(
    committer_framing_choice,
    'Given the SCOPE manifest lists three sibling readings (rupture, continuity, composite_overdetermination), is treating ''rupture'' as a single coherent reading itself defensible, or does the rupture position itself fracture further (e.g., a moderate rupture on religious liberty alone versus a total ecclesiological rupture)?',
    'Survey rupture-reading proponents (from the ''Bologna school'' historiographical tradition and progressive theological literature) for internal variation in scope of claimed discontinuity; if scope varies substantially, further decomposition into narrower reading-constraints may be warranted per the ε-invariance principle.',
    'If the rupture reading is itself internally heterogeneous with different ε profiles on different doctrinal points (religious liberty vs. liturgy vs. ecclesiology), a single constraint story may be under-decomposed; this would motivate splitting into narrower reading-constraints in a future revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_choice, conceptual, 'Whether the rupture reading requires further internal decomposition beyond the current three-way kernel split.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1988, 0.35).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2013, 0.37).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1962, 0.28).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1988, 0.5).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2013, 0.5).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1988, 0.58).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2013, 0.4).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2025, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the vatican_ii_magisterial_authority kernel, decomposed per the ε-invariance principle because 'what Vatican II did' is not a single structurally stable claim: the rupture reading, continuity reading, and composite_overdetermination reading assign different beneficiary/victim structures and different extractiveness profiles to what is colloquially called 'Vatican II's authority.' The rupture reading here is the most extractive of the three candidates authored (moderate-high ε, tangled_rope), reflecting that it is the reading most institutionally entrenched in post-conciliar administrative structures while remaining the most contested doctrinally. Each sibling file documents this same decomposition rationale from its own vantage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
