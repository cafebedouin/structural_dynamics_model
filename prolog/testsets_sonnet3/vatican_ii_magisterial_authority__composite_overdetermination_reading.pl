% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II as Overdetermined Composite Text — Hermeneutical Control as Real Authority Locus
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the composite-overdetermination reading of the
 *   Vatican II magisterial-authority kernel: the claim that the conciliar
 *   texts are not a single coherent teaching awaiting correct interpretation
 *   but an engineered composite in which mutually incompatible
 *   ecclesiological visions were encoded side by side to secure a
 *   supermajority vote. On this reading, the real locus of ongoing
 *   magisterial authority is not the text but whoever currently controls the
 *   dominant hermeneutic applied to it — and the ~10-12% non-placet/iuxta
 *   modum votes are read as evidence of unresolved theological
 *   incompatibility surviving inside the final documents, not as marginal
 *   noise. This is a distinct constraint from the sibling continuity_reading
 *   (which holds the texts encode a single organic development and treats
 *   interpretive divergence as error or bad faith) and the sibling
 *   rupture_reading (which holds the texts encode a single coherent break and
 *   treats continuity-hermeneutic enforcement as suppression of the Council's
 *   true intent). All three readings describe the same historical texts but
 *   assign different epsilon: continuity_reading would assess low extraction
 *   (faithful development, contested only by dissenters in error);
 *   rupture_reading would assess extraction concentrated in curial
 *   suppression of the Council's actual break; this composite reading
 *   assesses extraction as distributed across whoever currently wins the
 *   interpretive contest, with the ambiguity itself functioning as the
 *   extractive mechanism because it is never resolved, only administered.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.52).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II as Overdetermined Composite Text — Hermeneutical Control as Real Authority Locus").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'f502b093-9e1a-45be-9ada-2fc617cb0f73').
narrative_ontology:cs_kernel_codification('f502b093-9e1a-45be-9ada-2fc617cb0f73', fixed_text).
narrative_ontology:cs_authority_grounding('f502b093-9e1a-45be-9ada-2fc617cb0f73', extraction).
narrative_ontology:cs_interpretation_layer_present('f502b093-9e1a-45be-9ada-2fc617cb0f73').
narrative_ontology:cs_reading_relation('f502b093-9e1a-45be-9ada-2fc617cb0f73', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f502b093-9e1a-45be-9ada-2fc617cb0f73', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('f502b093-9e1a-45be-9ada-2fc617cb0f73', foundational, conciliar_texts_are_engineered_composites).
narrative_ontology:cs_axiom_status(conciliar_texts_are_engineered_composites, holdable).
narrative_ontology:cs_axiom_grounding('f502b093-9e1a-45be-9ada-2fc617cb0f73', conciliar_texts_are_engineered_composites, empirically_contingent).
narrative_ontology:cs_axiom('f502b093-9e1a-45be-9ada-2fc617cb0f73', foundational, hermeneutical_control_is_the_real_authority_locus).
narrative_ontology:cs_axiom_status(hermeneutical_control_is_the_real_authority_locus, holdable).
narrative_ontology:cs_axiom_grounding('f502b093-9e1a-45be-9ada-2fc617cb0f73', hermeneutical_control_is_the_real_authority_locus, conventional).
narrative_ontology:cs_axiom('f502b093-9e1a-45be-9ada-2fc617cb0f73', secondary, rejection_votes_signal_embedded_incompatibility).
narrative_ontology:cs_axiom_status(rejection_votes_signal_embedded_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('f502b093-9e1a-45be-9ada-2fc617cb0f73', rejection_votes_signal_embedded_incompatibility, empirically_contingent).
narrative_ontology:cs_reference_frame('f502b093-9e1a-45be-9ada-2fc617cb0f73', supermajority_compromise_drafting_1965).
narrative_ontology:cs_drift_state('f502b093-9e1a-45be-9ada-2fc617cb0f73', post_conciliar_polarization_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f502b093-9e1a-45be-9ada-2fc617cb0f73', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_authorities).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_conferences_favoring_ambiguity).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, theological_schools_of_compromise_interpretation).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_communities_rejecting_texts).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_communities_denied_implementation).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_clergy_caught_between_readings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_clergy_caught_between_readings).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_supermajority_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control which reading of the ambiguous conciliar formulations counts as authoritative at any given moment — the 'hermeneutic of continuity' vs. competing readings. Because the texts themselves were drafted to secure supermajority votes by encoding multiple incompatible positions, the authority to declare which encoded position is 'the' Council's meaning becomes a live, renewable power rather than a settled fact recoverable from the text alone. This interpretive discretion is not incidental — it is the actual site where magisterial authority is exercised post-council.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_authorities, beneficiary).

% Regional bishops' conferences exploit the same textual ambiguity to license locally divergent liturgical and pastoral practice, citing the Council's own compromise language as warrant. They benefit from the composite structure precisely because no single reading can be imposed uniformly; their exit from Roman oversight is bounded by formal communion but practically wide given textual indeterminacy.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_conferences_favoring_ambiguity, beneficiary,
    organized, generational, constrained, national).

% Encounter contradictory catechesis, liturgical practice, and pastoral guidance depending on diocese, parish, or decade, all traceable to the same conciliar texts read differently by different authorities. Bear the cost of confusion, scandal, and inconsistent formation without any means to compel a single authoritative resolution; leaving the institution is the only real exit, and even that does not resolve the underlying interpretive dispute for those who remain believers.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% Read the compromise formulations as encoding genuine rupture with prior teaching and refuse full assent, some formally separating (SSPX and adjacent movements) and others remaining in uneasy, disciplined tension within communion. Their persistent ~10-12% rejection is treated by the hierarchy as marginal dissent rather than as evidence the texts embed unresolved theological incompatibility — a reading this story treats as diagnostically significant rather than dismissible.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_communities_rejecting_texts, payer,
    moderate, generational, constrained, regional).

% Read the same compromise formulations as licensing structural reforms (collegiality, liturgical vernacularization, expanded lay and ecclesial-ministry roles) and experience the subsequent decades of curial retrenchment as bad-faith reneging on textual commitments they consider plainly encoded. They bear the cost of promises embedded in the text but never permitted to fully cash out, because the same text also encodes the continuity reading that authorities selectively enforce against them.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_communities_denied_implementation, payer,
    moderate, generational, constrained, regional).

% Must administer sacraments and formation under whichever reading their bishop or local tradition enforces, often reversing practice across appointments or pontificates. They benefit from some interpretive latitude to manage local pastoral needs, but pay in career risk, accusations of disloyalty from both directions, and the impossibility of giving a single consistent account of what the Council actually requires.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_clergy_caught_between_readings, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, local_clergy_caught_between_readings, beneficiary).

% The historical body that drafted and voted on the texts is no longer available to adjudicate its own intent; the ~2,300 fathers who voted did so under drafting compromises engineered precisely to let mutually incompatible factions each vote yes for different reasons. Their actual, divided intent is structurally unrecoverable and is not represented by any living seat in the ongoing dispute.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, council_fathers_1962_1965, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__composite_overdetermination_reading, council_fathers_1962_1965).

% Study the drafting history, voting records, and successive schema revisions (e.g., the shift from the rejected preparatory schemas to the final texts) and document that ambiguity in passages like Lumen Gentium 8, 22, and Gaudium et Spes 22 was a negotiated feature required to secure votes from both progressive and conservative blocs, not an accident of translation or later misreading.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, academic_conciliar_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, curial_hermeneutical_authorities).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The compromise formulations solved a genuine and urgent coordination problem: assembling a supermajority of bishops from mutually suspicious theological factions (nouvelle theologie progressives, Roman curial conservatives, and a large uncommitted middle) into texts that could pass by nearly unanimous vote in the same session, avoiding the schism that a forced binary choice would likely have produced.
% TRANSFER_FUNCTION: The arrangement transfers interpretive certainty away from the lay faithful and rank-and-file clergy and concentrates it in whichever hierarchical or academic authority currently controls the dominant hermeneutic; it also transfers the cost of unresolved theological disagreement from the 1962-1965 conciliar assembly onto subsequent generations who must live inside texts that were never fully resolved at the point of authorship.
% ABSENT_VOICES: The dissenting minority within the Council itself (the roughly 10-12% who voted non-placet or placet iuxta modum on contested passages) had their objections formally noted but structurally overridden by the supermajority threshold; their theological concerns were not resolved, merely outvoted, and their heirs (traditionalist communities) remain outside full participation in the ongoing interpretive contest despite the discomfort their vote pattern is supposed to signal.
% DISAPPEARANCE_RATIONALE: If the composite, ambiguity-preserving character of the texts were somehow dissolved into a single settled reading overnight, the entire post-conciliar ecosystem of competing seminaries, religious orders, episcopal alignments, and lay movements organized around rival hermeneutics would lose their present justification for existing as rivals — some would be vindicated, others delegitimized, and the decades-long practice of managing ambiguity through selective enforcement would become unnecessary, forcing an open reckoning the compromise formulations were originally built to defer.
% FOUNDING_PROBLEM: The Council needed to produce texts that a deeply divided body of bishops — representing incompatible ecclesiologies (societas perfecta vs. communio, juridical vs. pastoral, static vs. developmental views of tradition) — could nearly all vote to approve, without either faction being handed an outright doctrinal defeat that might trigger schism.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of the Council (e.g., work drawing on the Acta Synodalia and private conciliar diaries) attest, from outside any faction that currently benefits from a particular hermeneutic, that specific passages were redrafted multiple times specifically to secure votes from opposing blocs by removing language either side found unacceptable — this is documented in the drafting record itself, not merely alleged by partisans of the composite reading.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) and suppression (0.52) are both moderate-to-substantial but not maximal, reflecting that the coordination function (avoiding 1960s schism) was real and largely successful, while the ongoing cost is the deferred and distributed theological incompatibility that never received textual resolution. Accessibility collapse is low (0.35) precisely because the composite reading holds that alternatives to any single hermeneutic remain textually live — the ambiguity that enables extraction is the same ambiguity that keeps the interpretive contest genuinely open rather than settled. Resistance is high (0.71) because both traditionalist and progressive communities continue to actively contest whichever hermeneutic is currently dominant, generation after generation, rather than acquiescing.
 *
 * DIRECTIONALITY LOGIC:
 *   Curial hermeneutical authorities and favorably-positioned episcopal conferences sit near the beneficiary end: they do not need the ambiguity resolved and often benefit from being the ones who get to declare, provisionally, what a given passage 'really' means. Lay faithful, traditionalist communities, progressive reform communities, and local clergy sit nearer the target end: each bears the cost of an authority structure that can indefinitely defer resolving the incompatibility their formation, worship, or theological commitments depend on. Local clergy and episcopal conferences both carry secondary beneficiary roles because interpretive latitude, while costly, also grants them room to maneuver pastorally that a fully settled text would foreclose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (assembling a viable supermajority across incompatible factions without triggering schism) is scored as still live rather than dead, because the underlying theological incompatibility the compromise formulations were built to paper over has not been resolved by any subsequent authoritative act — it has only been administered through successive, contested pontificates. This blocks the naive mandatrophy diagnosis that would treat persistent implementation divergence as pure institutional decay; on the composite reading, divergence is not decay of a once-clear mandate but the designed consequence of a mandate that was never singular. The self-perpetuating aspect worth flagging is that hermeneutical control functions as a durable form of authority independent of doctrinal resolution — which is itself the structural feature this reading claims the sibling readings miss.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    single_vs_composite_authorial_intent,
    'Did the Council fathers, in the aggregate, hold a single (if internally complex) coherent ecclesiological vision that the ambiguous formulations imperfectly express, or did the drafting process encode genuinely incompatible visions that were never reconciled, only textually juxtaposed to secure votes?',
    'Systematic comparison of the successive schema drafts (via the Acta Synodalia and conciliar diaries) against the final promulgated texts, tracking which specific clauses were added, removed, or softened in response to which faction''s objections, and whether the resulting text is best modeled as a single evolving consensus or as a juxtaposition of previously irreconcilable positions.',
    'If the drafting record shows genuine reconciliation rather than juxtaposition, this composite reading is substantially weakened in favor of the continuity_reading; if it shows durable juxtaposition of positions that were never actually reconciled at the level of content, the composite reading is strengthened and the extractiveness attributable to deferred incompatibility should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_vs_composite_authorial_intent, empirical, 'Whether the conciliar drafting record supports composite juxtaposition versus genuine reconciliation of factions.').

omega_variable(
    kernel_disagreement_location,
    'Given three sibling readings of the same kernel (continuity, rupture, composite-overdetermination), where exactly does the disagreement between readings live: in what the texts assert, in what authority they carry, or in who gets to say which reading is authoritative?',
    'Explicit mapping of each reading''s claims against the same set of contested passages (e.g., Lumen Gentium 8 on the relationship between the Church of Christ and the Catholic Church, Dignitatis Humanae on religious liberty versus prior Syllabus-era condemnations) to identify whether the readings diverge on textual meaning, on doctrinal weight, or on interpretive authority itself.',
    'If disagreement is located primarily in interpretive authority (who decides) rather than textual meaning, this favors the composite reading''s central claim that hermeneutical control is the real locus of power; if disagreement is located primarily in textual meaning with a recoverable single sense, this favors either the continuity or rupture reading depending on which sense is recovered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Whether the three sibling readings disagree about text, doctrine, or authority — routes the framing under-determination for this kernel.').

omega_variable(
    rejection_vote_significance,
    'Do the roughly 10-12% non-placet and placet iuxta modum votes on contested passages signal unresolved theological incompatibility embedded in the final texts, or do they represent ordinary conciliar dissent that the supermajority mechanism was designed to legitimately override?',
    'Comparative analysis of rejection-vote patterns across contested versus uncontested conciliar documents, cross-referenced with the subsequent institutional trajectory of the dissenting minority (formal separations, disciplined internal dissent, eventual reconciliation) to assess whether the dissent tracked genuine doctrinal incompatibility or ordinary minority disagreement.',
    'If rejection votes track genuine doctrinal incompatibility that persists institutionally (as with the SSPX trajectory), this corroborates the composite reading''s diagnostic use of the vote margins; if the dissent largely dissolved or reconciled over time, this weakens the composite reading''s central evidentiary claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rejection_vote_significance, empirical, 'Whether persistent minority rejection votes are diagnostic of embedded incompatibility or ordinary dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 60, 0.44).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.51).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial label 'Vatican II's meaning' per the ε-invariance principle: continuity_reading assesses low extraction under an organic-development framing, rupture_reading assesses extraction concentrated in curial suppression of a genuine break, and this composite_overdetermination_reading assesses extraction as distributed across an unresolved, actively administered interpretive contest. Each carries its own epsilon and stakeholder set; they are linked, not merged, because they assign different extraction values to what the same texts are doing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
