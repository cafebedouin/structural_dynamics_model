% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Interpretive Authority — Composite Overdetermination Reading
 *   domain: theology/ecclesiology/religious authority
 *
 * SUMMARY:
 *   This story authors the composite-overdetermination reading of the Vatican
 *   II authority kernel: the claim that the council is not one interpretable
 *   event but an overdetermined bundle of distinct doctrinal shifts carrying
 *   incompatible theological rationales, so that the resulting ambiguity
 *   cannot be resolved into either a continuity narrative or a rupture
 *   narrative because the incompatibility is not an interpretive failure but
 *   the actual product of factional textual compromise during drafting. Two
 *   sibling readings exist as separate constraints and are NOT described
 *   here: continuity_reading (organic development within an unchanging
 *   deposit of faith) and rupture_reading (substantive doctrinal break). This
 *   story's ε is authored for the standing arrangement under contest — the
 *   Church's practice of governing through unresolved ambiguity — assessed by
 *   the composite reading's own lights, not for any resolved alternative.
 *
 * KEY AGENTS:
 *   - curial_interpretive_office: institutional agenda_setter and beneficiary, retains adjudicative leverage precisely because ambiguity is never closed
 *   - conciliar_hermeneutics_scholars: moderate-power beneficiary, academic field constituted by the unresolved tensions
 *   - parish_clergy_seeking_doctrinal_clarity: powerless, trapped payer, must preach under contradictory authoritative rationales
 *   - traditionalist_communities and progressive_reform_communities: organized, constrained payers, each denied the clean resolution (rupture or full development) that would validate their position
 *   - diocesan_bishops: institutional agenda_setters who also pay, blamed locally for a structural ambiguity they did not create
 *   - conciliar_periti_and_drafting_commissions: excluded non-agent historical entity whose negotiated compromises are the actual source of the incompatibility
 *   - academic_church_historians: analytical observers corroborating the structural (not accidental) origin of the ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.61).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II Interpretive Authority — Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, '1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d').
narrative_ontology:cs_kernel_codification('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', fixed_text).
narrative_ontology:cs_authority_grounding('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', extraction).
narrative_ontology:cs_interpretation_layer_present('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d').
narrative_ontology:cs_reading_relation('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', vatican_ii_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', vatican_ii_authority__rupture_reading, influences).
narrative_ontology:cs_axiom('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', foundational, conciliar_corpus_is_irreducibly_plural).
narrative_ontology:cs_axiom_status(conciliar_corpus_is_irreducibly_plural, holdable).
narrative_ontology:cs_axiom_grounding('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', conciliar_corpus_is_irreducibly_plural, empirically_contingent).
narrative_ontology:cs_axiom('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', foundational, factional_compromise_produces_genuine_contradiction_not_apparent_tension).
narrative_ontology:cs_axiom_status(factional_compromise_produces_genuine_contradiction_not_apparent_tension, holdable).
narrative_ontology:cs_axiom_grounding('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', factional_compromise_produces_genuine_contradiction_not_apparent_tension, empirically_contingent).
narrative_ontology:cs_axiom('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', secondary, univocal_magisterial_interpretation_claim_is_unsustainable).
narrative_ontology:cs_axiom_status(univocal_magisterial_interpretation_claim_is_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', univocal_magisterial_interpretation_claim_is_unsustainable, conventional).
narrative_ontology:cs_reference_frame('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', conciliar_textual_corpus_as_unitary_magisterial_act).
narrative_ontology:cs_drift_state('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', post_synodal_process_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1f4a3e8d-8bda-4877-8f33-d5d15ba7f20d', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, conciliar_hermeneutics_scholars).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, curial_interpretive_office).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, parish_clergy_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, progressive_reform_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, diocesan_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues authoritative clarifications, catechisms, and disciplinary rulings that adjudicate which conciliar document passages control in a given dispute, without ever resolving whether the council as a whole was continuous or ruptural. Retains permanent interpretive leverage precisely because the ambiguity is never closed — every fresh controversy routes back through the same office for a fresh ad hoc ruling, which preserves and even grows its authority.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, curial_interpretive_office, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, curial_interpretive_office, beneficiary).

% Build academic careers, journal literatures, and conference circuits on demonstrating the internal tensions between, for example, Lumen Gentium's collegiality language and the retained primacy formulas, or Dignitatis Humanae's religious liberty claims against prior Syllabus-era condemnations. The unresolved composite is their disciplinary subject matter; a forced resolution into pure continuity or pure rupture would collapse the interpretive puzzles that sustain the field.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, conciliar_hermeneutics_scholars, beneficiary,
    moderate, civilizational, mobile, global).

% Must preach, catechize, and administer sacraments under documents whose governing rationale shifts depending on which passage, which conciliar commission's drafting history, and which subsequent magisterial gloss is invoked. Cannot appeal to a single coherent hermeneutic; every homily on liturgy, ecumenism, or religious liberty risks contradicting some other authoritative reading. Exit means leaving ordained ministry or capitulating to whichever local bishop's preferred hermeneutic currently prevails.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, parish_clergy_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% Read the council as containing genuine ruptures with prior teaching and organize canonically fragile communities (indult groups, SSPX-adjacent formations, sedevacantist splinters) around that reading. The composite-overdetermination reading denies them the clean rupture narrative that would justify full separation, while the institution simultaneously denies them the clean continuity narrative that would let them fully submit without doctrinal reservation — they are held in a permanently unresolved canonical status that costs them standing, sacramental access, and institutional legitimacy.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    organized, generational, constrained, national).

% Read the council as licensing ongoing doctrinal development (women's ordination advocacy, collegial governance reform, expanded ecumenism) and press for the 'spirit of the council' to be realized institutionally. The composite reading denies them a settled textual mandate they can cite as final authority, since counter-passages and curial glosses are always available to check further development — they absorb decades of deferred reform and periodic disciplinary reversal.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, progressive_reform_communities, payer,
    organized, generational, constrained, national).

% Must apply the conciliar documents locally and are individually blamed by both traditionalist and progressive factions for choosing 'the wrong reading,' while lacking authority to settle the underlying textual contradiction themselves. They administer the ambiguity without owning it, absorbing local conflict that originates in the composite structure itself.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, diocesan_bishops, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, diocesan_bishops, payer).

% The historical drafting factions (the conservative Coetus Internationalis Patrum minority and the progressive majority alliance) whose literal compromise language produced the incompatible rationales are not present to arbitrate; their negotiated ambiguities are treated by all present factions as if they were unitary authorial intent, though the drafting record shows deliberate, unresolved factional trade-offs.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, conciliar_periti_and_drafting_commissions, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_authority__composite_overdetermination_reading, conciliar_periti_and_drafting_commissions).

% Study the conciliar acta, drafting history, and floor debates to document where and why incompatible rationales entered specific texts (e.g., the deliberate juxtaposition of collegiality and primacy language as a compromise between rival theological schools at the council). Their evidence corroborates that the ambiguity is structural rather than accidental, without being party to any faction's institutional stakes.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, academic_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, curial_interpretive_office).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The composite-overdetermination reading coordinates competing conciliar factions' surviving textual legacies into one canonical corpus without forcing either faction's defeat, allowing the institution to retain both progressive and conservative constituencies simultaneously under a single nominal authority.
% TRANSFER_FUNCTION: Moves interpretive authority and disciplinary leverage away from settled doctrinal clarity and toward whichever office currently controls the live adjudication of ambiguous passages — chiefly the curial interpretive apparatus — at the cost of clergy and lay communities who must operate without a stable hermeneutic.
% ABSENT_VOICES: The original conciliar periti and factional drafters who authored the specific compromise language are dead or unconsulted; their drafting-history evidence of deliberate, incompatible trade-offs is treated by current disputants as settled authorial unity rather than as the negotiated artifact the acta show it to be.
% DISAPPEARANCE_RATIONALE: If the composite-overdetermination reading were formally adopted as the Church's own self-description (rather than remaining a scholarly diagnosis contested by both continuity and rupture partisans), the curial interpretive office would lose its standing basis for ad hoc case-by-case rulings, traditionalist and progressive communities would gain grounds to press for an honest reckoning or formal resolution of specific contradictions, and the decades-long deferral of doctrinal settlement on collegiality, religious liberty, and liturgical authority would be forced into open renegotiation.
% FOUNDING_PROBLEM: The council was convened to address the Church's relationship to modernity, other Christian communities, and non-Christian religions through documents drafted under time pressure by rival theological schools (nouvelle théologie progressives and neo-scholastic conservatives) who reached textual compromises rather than theological synthesis on several central questions.
% FOUNDING_PROBLEM_CORROBORATION: Independent Church historians working from the conciliar acta and periti diaries (e.g., documented floor-debate records on Lumen Gentium's collegiality-primacy tension and Dignitatis Humanae's drafting fights) corroborate that specific passages resulted from unresolved factional compromise rather than achieved synthesis; this corroboration comes from historical-critical scholarship outside both the curial apparatus and the hermeneutics-scholar guild that benefits from the ambiguity's persistence.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) is authored moderate-high: the standing arrangement extracts real costs from clergy and lay communities forced to operate without stable doctrine, and channels interpretive authority toward the curial office and academic guild that benefit from the puzzle remaining open, though the extraction is less concentrated than a pure snare because a genuine coordination function (holding a fractured institution together across rival factions) is also served. Suppression (0.58) reflects the institution's active discouragement of any faction (traditionalist or progressive) attempting to force a clean resolution — canonical irregularity, disciplinary action, and marginalization are used against both poles. Theater ratio (0.47) is elevated: much conciliar-anniversary commemoration, synodal process, and hermeneutics conference activity performs the appearance of ongoing doctrinal engagement while the underlying textual contradictions remain structurally untouched. Accessibility collapse (0.4) is moderate-low because, unlike a natural law, real alternative resolutions (formal synthesis, formal repudiation of one faction's language, honest acknowledgment of compromise) remain conceptually available even if institutionally suppressed. Resistance (0.72) is high: both traditionalist and progressive communities actively contest the arrangement, each wanting a different resolution, which is itself evidence the ambiguity is felt as costly rather than accepted as settled.
 *
 * DIRECTIONALITY LOGIC:
 *   The curial interpretive office and conciliar hermeneutics scholars are structural beneficiaries: the office's authority and the scholars' disciplinary field both depend on the ambiguity persisting, so both get low d (beneficiary end). Parish clergy, traditionalist communities, and progressive reform communities are targets: each bears real costs (pastoral incoherence, canonical marginalization, deferred reform) from an ambiguity they did not create and cannot resolve, so each gets high d. Diocesan bishops sit in between — they administer the arrangement (agenda_setter role) but also absorb local blame and conflict they cannot structurally fix (payer secondary_role), producing a genuinely mixed directionality the engine should register as intermediate.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite-overdetermination reading resists mandatrophy mislabeling in both directions. Treating the arrangement as pure coordination (rope) would ignore that specific parties (curial office, hermeneutics guild) accrue durable interpretive leverage from the ambiguity's persistence — that is extraction riding on the coordination function of holding a factionally divided institution together. Treating it as pure extraction (snare) would ignore that the arrangement does perform a real, non-fake coordination service: it prevents outright schism by refusing to force either faction's total defeat. The tangled_rope claim is the honest middle: genuine coordination (avoiding institutional rupture) coexists with genuine asymmetric extraction (interpretive authority and academic capital accruing to specific seats while pastoral clarity is denied to others), and the arrangement requires active enforcement (disciplinary responses to both traditionalist separatism and progressive reform pressure) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_vs_synthesis_drafting_intent,
    'Did the conciliar drafting commissions intend the ambiguous passages (e.g., collegiality alongside retained primacy, religious liberty alongside prior condemnations) as genuine theological synthesis they believed coherent, or as deliberate compromise language known at the time to paper over unresolved disagreement?',
    'Systematic historical-critical review of the conciliar acta, periti diaries, and floor-debate transcripts to establish whether drafters explicitly acknowledged the incompatibility during drafting or believed they had achieved genuine synthesis.',
    'If drafters believed they achieved synthesis, the composite-overdetermination reading''s claim of manufactured incompatibility weakens toward an interpretive-difficulty reading closer to continuity; if drafters explicitly acknowledged irreconcilable compromise, the structural-contradiction claim strengthens, and the curial office''s use of the ambiguity for ongoing adjudicative leverage reads more clearly as extraction rather than incidental interpretive difficulty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_vs_synthesis_drafting_intent, empirical, 'Whether the incompatible rationales were known compromises or believed syntheses at drafting time.').

omega_variable(
    reading_as_kernel_component_or_meta_claim,
    'Is this composite-overdetermination reading itself a first-order reading of the Vatican II kernel on the same level as continuity and rupture, or is it better understood as a meta-level claim ABOUT the inadequacy of the continuity/rupture framework, which would place it structurally above rather than beside its siblings?',
    'Conceptual analysis of whether the composite reading makes any first-order doctrinal claims of its own (about what specific council teachings mean) versus purely diagnostic claims about the other two readings'' inadequacy.',
    'If purely meta-level, this reading may not compete for the same institutional adjudicative authority as continuity/rupture and its beneficiary structure (scholars, curial adjudicators) would need re-examination; if first-order, the tangled_rope classification and stakeholder structure as authored stand.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_as_kernel_component_or_meta_claim, conceptual, 'Whether this reading is a peer reading or a meta-level diagnosis of the kernel dispute.').

omega_variable(
    curial_office_capture_degree,
    'Does the curial interpretive office actively prefer and cultivate the ambiguity for its own authority (active capture), or does it merely inherit and administer an ambiguity it did not choose and would resolve if it could (passive administration)?',
    'Internal curial documents, statements by officials on preferred resolution paths, and comparison of disciplinary actions across traditionalist and progressive challenges to see whether enforcement patterns favor ambiguity-preservation over genuine resolution attempts.',
    'Active capture supports the tangled_rope/beneficiary framing as authored; passive administration would push the office''s seat toward observer/agenda_setter without meaningful beneficiary status, weakening the extraction component of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curial_office_capture_degree, empirical, 'Whether curial benefit from the ambiguity is actively cultivated or passively inherited.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 60, 0.47).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, rupture_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the vatican_ii_authority kernel. continuity_reading authors the council as organic development within an unchanging deposit of faith (lower authored ε, closer to rope/mountain-adjacent from its own lights). rupture_reading authors the council as containing genuine doctrinal breaks (high authored ε from a traditionalist vantage, closer to snare). This composite_overdetermination_reading authors ε (0.61) for the standing arrangement of institutionally-managed ambiguity itself, distinct from either sibling's referent, and classifies as tangled_rope because it identifies both a genuine coordination function (preventing schism) and genuine asymmetric extraction (interpretive-authority capture by curial and scholarly seats) that the other two readings, by asserting a single coherent narrative, do not surface.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
