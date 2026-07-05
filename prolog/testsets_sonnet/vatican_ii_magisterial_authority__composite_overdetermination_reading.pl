% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II as Overdetermined Composite: Hermeneutical Control as the Locus of Authority
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the composite-overdetermination reading of the
 *   Vatican II magisterial authority kernel: the conciliar texts are read not
 *   as a single settled reinterpretation of Church teaching but as a
 *   negotiated composite that encodes multiple incompatible ecclesiological
 *   commitments simultaneously, engineered to clear supermajority thresholds.
 *   On this reading, the real locus of authority migrated from the text
 *   (which does not resolve the dispute) to whoever controls the
 *   hermeneutical apparatus — Roman dicasteries, episcopal conferences,
 *   seminary formation — that decides which reading counts as authoritative
 *   in practice. Two sibling constraints exist for the same kernel and are
 *   NOT this constraint: continuity_reading (organic development, no rupture)
 *   and rupture_reading (fundamental break, new and incompatible
 *   ecclesiology). Each sibling has its own epsilon, its own
 *   beneficiary/victim structure, and its own type; they are linked here only
 *   via network edges and kernel_context, per the epsilon-invariance
 *   decomposition rule.
 *
 * KEY AGENTS:
 *   - curial_hermeneutical_authorities: controls which reading is treated as authoritative without amending the underlying texts
 *   - episcopal_conferences_favoring_pastoral_reading: benefits from ambiguity-licensed local latitude
 *   - traditionalist_institutes_favoring_restrictive_reading: benefits from ambiguity-licensed restrictive latitude
 *   - parish_clergy_navigating_contradictory_directives: bears the pastoral cost of unresolved interpretive conflict
 *   - sspx_aligned_traditionalist_laity: bears canonical irregularity for treating the incompatibility as real
 *   - council_fathers_dissenting_minority: excluded voice whose formal objection registered the incompatibility at the time and was procedurally overridden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.47).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II as Overdetermined Composite: Hermeneutical Control as the Locus of Authority").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'a1d33365-11be-479c-a405-8d4594b60bd5').
narrative_ontology:cs_kernel_codification('a1d33365-11be-479c-a405-8d4594b60bd5', fixed_text).
narrative_ontology:cs_authority_grounding('a1d33365-11be-479c-a405-8d4594b60bd5', extraction).
narrative_ontology:cs_interpretation_layer_present('a1d33365-11be-479c-a405-8d4594b60bd5').
narrative_ontology:cs_reading_relation('a1d33365-11be-479c-a405-8d4594b60bd5', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1d33365-11be-479c-a405-8d4594b60bd5', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('a1d33365-11be-479c-a405-8d4594b60bd5', foundational, conciliar_texts_encode_genuine_incompatibility_by_design).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_genuine_incompatibility_by_design, holdable).
narrative_ontology:cs_axiom_grounding('a1d33365-11be-479c-a405-8d4594b60bd5', conciliar_texts_encode_genuine_incompatibility_by_design, empirically_contingent).
narrative_ontology:cs_axiom('a1d33365-11be-479c-a405-8d4594b60bd5', foundational, hermeneutical_control_constitutes_the_real_locus_of_magisterial_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_constitutes_the_real_locus_of_magisterial_authority, holdable).
narrative_ontology:cs_axiom_grounding('a1d33365-11be-479c-a405-8d4594b60bd5', hermeneutical_control_constitutes_the_real_locus_of_magisterial_authority, conventional).
narrative_ontology:cs_axiom('a1d33365-11be-479c-a405-8d4594b60bd5', secondary, implementation_divergence_is_structural_feature_not_correctable_defect).
narrative_ontology:cs_axiom_status(implementation_divergence_is_structural_feature_not_correctable_defect, holdable).
narrative_ontology:cs_axiom_grounding('a1d33365-11be-479c-a405-8d4594b60bd5', implementation_divergence_is_structural_feature_not_correctable_defect, empirically_contingent).
narrative_ontology:cs_reference_frame('a1d33365-11be-479c-a405-8d4594b60bd5', conciliar_promulgation_1965_settlement).
narrative_ontology:cs_drift_state('a1d33365-11be-479c-a405-8d4594b60bd5', post_synodal_era_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1d33365-11be-479c-a405-8d4594b60bd5', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_authorities).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_conferences_favoring_pastoral_reading).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_institutes_favoring_restrictive_reading).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, parish_clergy_navigating_contradictory_directives).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, sspx_aligned_traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_movements_post_conciliar).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful_seeking_doctrinal_clarity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_supermajority_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, hermeneutic_of_continuity_as_authoritative_frame).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls which interpretive key (continuity vs. rupture) is treated as authoritative in Roman documents, seminary formation, and disciplinary proceedings. Because the conciliar texts themselves do not resolve the ambiguity, whoever holds the interpretive office effectively holds the substantive authority the Council itself left underdetermined. Can promote a reading through appointments, catechetical guidance, and doctrinal notification without ever amending the texts.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Uses the compromise language on collegiality, liturgy, and religious liberty to implement locally adapted pastoral practice, citing conciliar authority for latitude the texts' ambiguity makes available. Benefits from the underdetermination because it licenses local variation that a clearer text would have foreclosed.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_conferences_favoring_pastoral_reading, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_conferences_favoring_pastoral_reading, agenda_setter).

% Reads the same compromise formulations as minimal concessions bounded tightly by continuity with prior magisterium, and uses the ambiguity to argue that expansive post-conciliar reforms exceed what the texts actually license. Periodically achieves canonical accommodation (e.g., traditional liturgy provisions) precisely because the underlying textual ambiguity keeps their claim technically arguable rather than settled.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_institutes_favoring_restrictive_reading, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_institutes_favoring_restrictive_reading, excluded).

% Receives conflicting formation, diocesan directives, and lay expectations depending on which regional or generational hermeneutic prevails, and bears the pastoral cost of adjudicating disputes the Council itself did not resolve. Cannot appeal to a single settled meaning of the texts because no such single meaning exists; exit means leaving ministry or aligning visibly with one faction at professional and relational cost.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, parish_clergy_navigating_contradictory_directives, payer,
    moderate, biographical, trapped, local).

% Experiences the ambiguity as an institution refusing to admit that the Council broke with prior teaching, is told their objections are already answered by 'proper' interpretation, and bears canonical irregularity or schism-adjacent status for treating the incompatibility as real rather than resolvable through interpretive discipline.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, sspx_aligned_traditionalist_laity, payer,
    powerless, biographical, trapped, national).

% Reads the same compromise texts as authorizing a decisive break — collegiality against curial centralism, religious liberty against prior condemnations — and repeatedly finds those readings checked, walked back, or reframed as never having been the Council's actual intent whenever curial authority reasserts the continuity frame.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_movements_post_conciliar, payer,
    moderate, generational, constrained, global).

% Wants a determinate answer to what the Church actually teaches on collegiality, religious liberty, ecumenism, or liturgy, and instead receives fifty years of dueling authoritative-sounding claims, each citing the same conciliar text. Bears the disorientation cost of an authority structure that cannot or will not name the incompatibility.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful_seeking_doctrinal_clarity, payer,
    powerless, biographical, constrained, global).

% The roughly 10-12% of council fathers who voted against key documents (e.g., Dignitatis Humanae, 70 negative votes out of ~2,300) registered formal objection that the final texts contained unresolved theological incompatibility with prior magisterial teaching. Their dissent was procedurally overridden by supermajority requirements designed to produce promulgable documents regardless of substantive resolution, and their objection is rarely treated as live evidence in subsequent interpretive disputes — it is filed as historical footnote rather than as the structural signal it is.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, council_fathers_dissenting_minority, excluded,
    powerless, civilizational, trapped, global).

% Documents the drafting history, the successive schema revisions, and the explicit compromise language negotiated to secure supermajorities, and can show textually where ambiguity was deliberately retained rather than resolved. Has no authority to adjudicate the theological dispute but can establish the historical fact of overdetermined drafting.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ecclesiastical_historians_and_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council needed to produce texts that a supermajority of bishops holding genuinely incompatible ecclesiological commitments (curial-centralist vs. collegial, exclusivist vs. dialogical, static vs. developmental) could all vote to promulgate, so that the Church would emerge with a single body of conciliar teaching rather than a formal schism at the Council itself.
% TRANSFER_FUNCTION: Moves interpretive authority away from the text itself (which does not settle the dispute) and toward whichever institutional actor currently controls catechesis, seminary formation, liturgical discipline, and doctrinal notification — at the cost of the clergy and laity who must live under directives that shift with who holds that interpretive office.
% ABSENT_VOICES: The dissenting minority of council fathers (roughly 10-12% on key votes) registered formal rejection precisely on the grounds of unresolved incompatibility, but their objection was procedurally absorbed by the supermajority mechanism and is now treated as settled history rather than live evidence. Lay faithful and parish clergy who bear the interpretive whiplash have no formal voice in hermeneutical adjudication at all.
% DISAPPEARANCE_RATIONALE: If the composite ambiguity were resolved overnight — if the texts were amended or authoritatively disambiguated to name one reading as exclusively correct — entire institutional formations (traditionalist institutes operating on canonical latitude, episcopal conferences citing conciliar warrant for pastoral adaptation, curial offices whose leverage depends on controlling interpretation) would lose their present footing, and the SSPX-aligned and progressive-reform populations currently in a holding pattern would face a forced reckoning rather than a permanent, manageable ambiguity.
% FOUNDING_PROBLEM: Bridging genuinely incompatible ecclesiological visions present among the council fathers (integralist/curial vs. reform/collegial, exclusivist vs. ecumenically open) well enough to secure the supermajority votes required to promulgate conciliar documents without a formal schism at the Council.
% FOUNDING_PROBLEM_CORROBORATION: Independent conciliar historians (e.g. work documenting the successive redrafting of Lumen Gentium's collegiality chapter and Dignitatis Humanae's religious liberty text under explicit pressure to secure minority-bloc votes) attest that the ambiguity was a negotiated drafting outcome, not incidental prose imprecision. The dissenting council fathers' own recorded interventions and negative votes corroborate from outside the Curia's own self-presentation that the incompatibility was recognized as real at the time, not manufactured later by partisans.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the composite's real function is now less about resolving genuine pastoral coordination and more about sustaining the interpretive leverage of whichever office currently controls the hermeneutic — a function that has grown, not shrunk, over six decades, hence the rising base_extractiveness series. Suppression (0.47) is moderate rather than severe: dissent is not physically coerced, but it is structurally absorbed — formal objection at the Council itself (the 10-12% negative votes) was procedurally overridden by supermajority design, and subsequent dissent is managed through doctrinal notification and canonical marginalization rather than open acknowledgment of unresolved incompatibility. Theater ratio (0.41) captures the substantial performative element in claims that 'proper hermeneutics' fully resolves the ambiguity when drafting history shows the ambiguity was retained by design. Accessibility collapse is comparatively low (0.35) because multiple readings remain genuinely available and contested in practice — the ambiguity has not collapsed into a single dominant interpretation, which is itself the diagnostic signature of an overdetermined composite rather than a resolved text. Resistance is high (0.72): both traditionalist and progressive factions actively contest the current hermeneutical settlement, each citing textual warrant.
 *
 * PERSPECTIVAL GAP:
 *   From the curial hermeneutical seat, the arrangement looks like legitimate exercise of magisterial interpretive authority resolving ambiguity through proper doctrinal development. From the parish clergy and lay faithful seats, the same structure delivers decades of contradictory formation and unresolved doctrinal status with no honest acknowledgment that the underlying texts do not settle the question. The engine computes these as structurally different because the positional atoms (power, exit options) genuinely differ — the curial seat has arbitrage-grade interpretive leverage; the parish and lay seats are trapped or constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (curial authorities, and both episcopal-conference and traditionalist-institute blocs) hold the compromise ambiguity as usable leverage — each can claim textual warrant for its preferred reading precisely because the text does not foreclose it. Victims (parish clergy, SSPX-aligned laity, progressive reform movements, ordinary lay faithful seeking clarity) bear the cost of an authority structure that will not name the incompatibility, each in a different register: clergy bear administrative whiplash, traditionalists bear canonical irregularity, progressives bear repeated walk-back, and ordinary laity bear simple disorientation. The dissenting council fathers are excluded rather than beneficiary or victim in the ordinary sense — their formal objection was procedurally absorbed rather than either rewarded or actively punished, which is why they sit in the excluded role rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing supermajority promulgation across genuinely incompatible episcopal factions — was live and arguably necessary in 1962-65. On this reading, however, the compromise mechanism that solved that one-time problem has not sunset; it has become the ongoing operating logic of magisterial interpretation itself, now serving a different function (interpretive leverage maintenance) than the one it was built for (avoiding schism at the Council). This is exactly the divergence the mandatrophy apparatus is built to catch: a mechanism whose founding problem is formally still 'live' by curial self-description, but whose actual operation increasingly resembles indefinite deferral rather than resolution-in-progress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drafting_intent_vs_retrospective_projection,
    'Was the ambiguity in the conciliar texts deliberately engineered by drafters to secure incompatible-bloc votes, or is the appearance of overdetermination a retrospective projection by later interpreters reading present-day disputes back into texts that were, at the time, reasonably determinate?',
    'Comparative analysis of successive schema drafts (e.g. the multiple redrafts of Lumen Gentium ch. III and Dignitatis Humanae) against the recorded interventions and amendment requests (modi) of specific episcopal blocs, to establish whether known incompatible factions each secured textual concessions traceable to their specific objections.',
    'If deliberate engineering is established, the composite-overdetermination reading is strongly corroborated and the hermeneutical-control-as-real-authority claim follows directly. If the ambiguity is better explained as ordinary compromise drafting that later factions have weaponized retrospectively, the constraint''s extractiveness is overstated and it more closely resembles a genuine, if imperfect, rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafting_intent_vs_retrospective_projection, empirical, 'Whether textual ambiguity was designed-in or retrospectively read-in.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel itself best framed as ''the conciliar texts'' (the fixed_text framing used here) or as ''the legitimacy of the interpretive office that adjudicates them'' (an authority-layered framing)? The two framings could support different cs_pattern classifications: fixed_text foregrounds textual ambiguity as the site of contest, while an authority-layered framing would foreground the interpretive office itself as the kernel, with the texts as merely the occasion for its exercise.',
    'Track whether disputes are actually resolved by textual exegesis (favoring fixed_text framing) or by appeal to which office currently holds interpretive authority regardless of textual argument (favoring authority-layered framing) — examine a sample of post-conciliar doctrinal controversies for which mode of resolution actually operated.',
    'Under the fixed_text framing (adopted here), authority_grounding is naturally read as extraction-through-institutional-control-of-a-fixed-canonical-text. Under an authority-layered framing, the kernel_codification itself might be better described as distributed, since no single interpretive office has uncontested final say. This would shift interpretation_layer_present and could change whether the constraint reads as tangled_rope or something closer to a snare centered purely on interpretive gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel is the text itself or the interpretive-authority layer above it.').

omega_variable(
    supermajority_dissent_significance,
    'Do the recorded 10-12% negative votes on key documents (e.g., 70 votes against Dignitatis Humanae) represent genuine, theologically serious minority dissent registering real incompatibility, or ordinary procedural dissent of the kind any large deliberative body produces regardless of substantive coherence?',
    'Content analysis of the recorded interventions and written objections (relationes) of the dissenting minority to determine whether their stated grounds concern substantive doctrinal incompatibility with prior magisterium (supporting this reading) or narrower procedural, stylistic, or prudential concerns (undermining this reading''s inference from vote counts alone).',
    'If dissent is substantively doctrinal, it corroborates the composite-overdetermination reading''s central claim that incompatibility was recognized and embedded, not merely alleged by later partisans. If dissent is primarily procedural, the vote-count evidence for embedded incompatibility weakens considerably and the constraint''s extractiveness estimate should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_dissent_significance, empirical, 'Whether conciliar dissent votes evidence doctrinal incompatibility or ordinary procedural friction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement_basis(vati_tr_t1962, observed).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.27).
narrative_ontology:measurement_basis(vati_tr_t1975, observed).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1988, 0.32).
narrative_ontology:measurement_basis(vati_tr_t1988, observed).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(vati_tr_t2000, observed).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement_basis(vati_tr_t2013, observed).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(vati_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.32).
narrative_ontology:measurement_basis(vati_be_t1962, observed).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.41).
narrative_ontology:measurement_basis(vati_be_t1975, observed).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1988, 0.47).
narrative_ontology:measurement_basis(vati_be_t1988, observed).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement_basis(vati_be_t2000, observed).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2013, 0.54).
narrative_ontology:measurement_basis(vati_be_t2013, observed).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(vati_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement_basis(vati_su_t1962, observed).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.36).
narrative_ontology:measurement_basis(vati_su_t1975, observed).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1988, 0.4).
narrative_ontology:measurement_basis(vati_su_t1988, observed).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement_basis(vati_su_t2000, observed).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2013, 0.45).
narrative_ontology:measurement_basis(vati_su_t2013, observed).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.47).
narrative_ontology:measurement_basis(vati_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_rupture_reading).

% DUAL FORMULATION NOTE:
% This story, continuity_reading, and rupture_reading form a three-member constraint family reading the same kernel (vatican_ii_magisterial_authority) differently. Each carries its own epsilon: continuity_reading is expected to score low extraction (genuine organic development, minimal identifiable victims), rupture_reading is expected to score moderate-to-high extraction concentrated on the pre-conciliar institutional forms it displaces, and this composite_overdetermination_reading scores moderate-high extraction distributed across the hermeneutical-control mechanism itself rather than any single doctrinal content. The three are not the same constraint measured three ways — each instantiates a distinct claim about what Vatican II structurally is, with its own beneficiary/victim sets and its own classification. Do not average or reconcile epsilon across the three; that reconciliation is exactly what the epsilon-invariance principle forbids.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
