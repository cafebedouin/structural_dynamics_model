% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority â Rupture Traditionalist Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the rupture_traditionalist_reading of
 *   the vatican_ii_doctrinal_authority kernel. Under this reading, the Second
 *   Vatican Council produced documents that are theologically ambiguous and,
 *   in places, erroneous. The post-conciliar hierarchy enforces these texts
 *   as binding magisterial acts, which enables progressive reformers to
 *   implement heterodox liturgical, doctrinal, and pastoral changes while
 *   suppressing traditional Catholic practice. The constraint extracts
 *   pre-conciliar tradition, doctrinal clarity, and missionary zeal from its
 *   victims, concentrating interpretive power in the progressive
 *   institutional apparatus. The reading treats the Council not as a solution
 *   to a coordination problem but as a snare whose cover storyâpastoral
 *   updatingâconceals structural rupture with the Church's prior teaching
 *   and worship.
 *
 * KEY AGENTS:
 *   - post_conciliar_hierarchy: Agenda-setter (institutional/constrained) â enforces conciliar implementation and suppresses traditional practice
 *   - progressive_reformers: Primary beneficiary (organized/mobile) â uses textual ambiguities to advance heterodox innovation
 *   - traditional_catholics: Primary target (moderate/identity_locked) â bears loss of liturgy, catechesis, and ecclesial identity
 *   - missionary_orders: Secondary target (organized/identity_locked) â loses pre-conciliar apostolic mandate and method
 *   - pre_conciliar_theologians: Excluded voice (moderate/trapped) â silenced by the hermeneutic priority of the Council
 *   - traditionalist_scholars: Analytical observer (moderate/analytical) â documents rupture without institutional power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.82).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.8).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, snare).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority â Rupture Traditionalist Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'aba0bf2c-15f7-4139-9879-66a856ca2bc9').
narrative_ontology:cs_kernel_codification('aba0bf2c-15f7-4139-9879-66a856ca2bc9', fixed_text).
narrative_ontology:cs_authority_grounding('aba0bf2c-15f7-4139-9879-66a856ca2bc9', lineage).
narrative_ontology:cs_interpretation_layer_present('aba0bf2c-15f7-4139-9879-66a856ca2bc9').
narrative_ontology:cs_reading_relation('aba0bf2c-15f7-4139-9879-66a856ca2bc9', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('aba0bf2c-15f7-4139-9879-66a856ca2bc9', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('aba0bf2c-15f7-4139-9879-66a856ca2bc9', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('aba0bf2c-15f7-4139-9879-66a856ca2bc9', foundational, conciliar_texts_contain_error).
narrative_ontology:cs_axiom_status(conciliar_texts_contain_error, holdable).
narrative_ontology:cs_axiom_grounding('aba0bf2c-15f7-4139-9879-66a856ca2bc9', conciliar_texts_contain_error, theological).
narrative_ontology:cs_axiom('aba0bf2c-15f7-4139-9879-66a856ca2bc9', foundational, liturgical_tradition_irreformable).
narrative_ontology:cs_axiom_status(liturgical_tradition_irreformable, holdable).
narrative_ontology:cs_axiom_grounding('aba0bf2c-15f7-4139-9879-66a856ca2bc9', liturgical_tradition_irreformable, theological).
narrative_ontology:cs_reference_frame('aba0bf2c-15f7-4139-9879-66a856ca2bc9', pre_conciliar_magisterial_continuity).
narrative_ontology:cs_drift_state('aba0bf2c-15f7-4139-9879-66a856ca2bc9', post_conciliar_implementation_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('aba0bf2c-15f7-4139-9879-66a856ca2bc9', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reformers).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholics).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the conciliar reform program through curial offices, diocesan bishops, and liturgical commissions. Presents the Council as an act of the Church's magisterium and suppresses public traditionalist practice by restricting the pre-conciliar Mass and disciplining clergy who dissent from the new liturgical and doctrinal orientation. Its institutional legitimacy is fused to the Council's authority, making reversal prohibitively costly.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% Theologians, liturgists, and pastoral bureaucrats who use the ambiguities in conciliar texts (especially on religious liberty, ecumenism, and collegiality) to advance heterodox implementation. They collect institutional positions, funding, and magisterial cover while traditional opposition is marginalized. Their personal careers are not fused to the pre-conciliar Church; they can migrate to secular academia or other denominations.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reformers, beneficiary,
    organized, biographical, mobile, global).

% Lay faithful, priests, and religious communities attached to the pre-conciliar liturgy, catechesis, and moral theology. They bear the loss of the traditional Mass, doctrinal clarity, and ecclesial identity. Institutional channels for their practice are closed or severely restricted. Their exit options are identity-locked because leaving the traditional Roman Rite and pre-conciliar moral framework means abandoning the form of Catholicism they hold to be the faith itself.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholics, payer,
    moderate, generational, identity_locked, global).

% Established missionary congregations whose apostolic methods and ecclesiological self-understanding were formed before the Council. They suffer the suppression of explicit proselytism, the replacement of conversion-oriented mission with interreligious dialogue, and the loss of distinct religious habits and community life. Their vocational identity is locked to the pre-conciliar missionary mandate; adaptation to the post-conciliar model is experienced as spiritual death.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders, payer,
    organized, generational, identity_locked, global).

% Theologians and magisterial writers of the pre-conciliar period whose work is excluded from seminary formation, academic hiring, and magisterial citation. Their theological framework is treated as superseded. They are literally or figuratively absent from the conversation; if present, they would contest the legitimacy of the conciliar innovations but are structurally silenced by the hermeneutic priority given to Vatican II.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_theologians, excluded,
    moderate, civilizational, trapped, global).

% Academic and independent scholars who analyze conciliar texts against the prior magisterium, documenting ambiguities, ruptures, and historical anomalies. They provide the intellectual architecture for the rupture reading but do not hold institutional power. Their classification does not depend on the constraint's persistence.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditionalist_scholars, observer,
    moderate, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None acknowledged by this reading. The Council was publicly convened to address Catholic engagement with modernity and to promote pastoral and liturgical renewal; this reading treats that stated purpose as the public justification for a structure whose actual operation is the suppression of pre-conciliar tradition and the extraction of doctrinal clarity.
% TRANSFER_FUNCTION: Moves doctrinal and liturgical authority from pre-conciliar tradition and its bearers to post-conciliar progressive implementers and the institutional hierarchy that enforces them. The cost of textual ambiguity and heterodox implementation is borne by traditional Catholics, missionary orders, and the integrity of the pre-conciliar magisterial corpus.
% ABSENT_VOICES: Pre-conciliar theologians and the Church's own prior magisterial tradition are structurally excluded from the interpretive conversation; their objections are ruled out by the hermeneutic privilege granted to the conciliar texts and their progressive interpreters. Traditional lay communities are present but treated as a problem to be managed rather than a voice to be heard.
% DISAPPEARANCE_RATIONALE: If the authority of Vatican II as a binding rupture-constraint vanished, the progressive implementation would lose its institutional cover. The traditional Roman liturgy, pre-conciliar catechesis, and missionary mandate would be restored; seminary formation, parish practice, and magisterial teaching would reorganize around pre-conciliar norms. The Church would undergo a massive institutional and theological rearrangement.
% FOUNDING_PROBLEM: The Church's perceived inability to engage the modern world and contemporary culture without wholesale doctrinal, liturgical, and pastoral adaptation.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist historians, statisticians, and theologians outside the progressive beneficiary set attest that the post-conciliar Church is in worse condition than the pre-conciliar Church (seminary collapse, Mass attendance decline, loss of missionary vigor, parish closures), corroborating that the founding problem either did not exist or was fatally mishandled and that the arrangement now persists as a zombie structure.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint systematically dismantles pre-conciliar liturgical, doctrinal, and missionary forms and transfers their authority to progressive implementers. Suppression is high (0.80) because the persistence of the post-conciliar arrangement depends on actively restricting the traditional Mass, disciplining dissenting clergy, and marginalizing traditional theological voices in seminaries and academia. Theater ratio is elevated (0.65) because the hierarchy performs continuity (the 'hermeneutic of continuity') while structurally enforcing rupture; a growing share of enforcement activity is performative maintenance of a legitimacy narrative rather than substantive coordination. Accessibility collapse is high (0.75) because, once the conciliar framework is accepted as authoritative, institutional alternatives (the pre-conciliar liturgical and theological regime) collapse almost completely within the Church's official structures. Resistance is moderate (0.60) because traditionalist movements (SSPX, FSSP, lay associations) mount persistent but institutionally suppressed opposition.
 *
 * PERSPECTIVAL GAP:
 *   The post-conciliar hierarchy and progressive reformers experience the constraint as necessary institutional renewal and legitimate magisterial authority; from their seats, the coordination story is real and the extraction is merely the cost of updating the Church. The traditional Catholic and missionary order seats experience the same structure as the active destruction of their religious identity. The engine computes this divergence from the structural data: agenda-setter/beneficiary seats with mobile or constrained exit map to low directionality, while identity-locked payer seats map to high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-conciliar hierarchy and progressive reformers are structural beneficiaries of the conciliar ambiguity: they collect institutional legitimacy, careers, and the power to remake doctrine and liturgy. Their directionality sits near the beneficiary end (low d), which damps effective extraction for them. Traditional Catholics and missionary orders are structural targets: they bear the loss of liturgical form, doctrinal certainty, and apostolic method, and their exit is identity-locked because their religious self-concept is fused with the pre-conciliar tradition. Their directionality sits near the full-target end (high d), amplifying effective extraction. The excluded pre-conciliar theologians are trapped rather than locked, but their exclusion means they do not register as active targets; their silencing is a suppression input rather than a directionality seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâCatholic engagement with modernityâhas, on this reading, not only failed but produced a worse pastoral crisis than the one it purported to solve. The R5 genealogy (founding_problem_status: dead, disappearance_verdict: world_rearranges) flags the constraint as a zombie structure that persists by inertia and active enforcement rather than by solving a live problem. This prevents mislabeling the arrangement as a rope (genuine coordination) or a scaffold (transitional support with a sunset): there is no sunset, the beneficiaries are not transient, and the victims are not net beneficiaries of a necessary reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conciliar_ambiguity_intentionality,
    'Were the ambiguities in Vatican II documents the result of deliberate theological compromise, magisterial error, or strategic openness to development?',
    'Historical reconstruction of conciliar commission debates, textual criticism of successive drafts, and analysis of the theological formation of the periti who drafted the ambiguous passages.',
    'If the ambiguities were errors or compromises, this supports the high extractiveness of the rupture reading (error imposed as binding). If they were strategic openness, the constraint may be better classified as tangled_rope or scaffold rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_ambiguity_intentionality, conceptual, 'Whether conciliar ambiguity was error, compromise, or strategy.').

omega_variable(
    implementation_heresy_necessity,
    'Is the post-conciliar heterodox implementation a structurally necessary consequence of the conciliar texts, or an abuse separable from the Council''s intent?',
    'Comparative analysis of implementation trajectories across dioceses, religious orders, and national churches; correlation between conciliar textual ambiguity and local heterodox outcomes.',
    'If necessary, the constraint is a snare whose texts inherently produce extraction. If separable, the extraction may be attributed to hijacking rather than textual structure, lowering base_extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_heresy_necessity, empirical, 'Whether heterodox implementation necessarily follows from the texts.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the rupture_traditionalist_reading logically foreclose the continuity_reading within a single ecclesiological framework, or do they coexist as incompatible hermeneutics held by different factions?',
    'Logical analysis of whether ''unchanging tradition'' and ''organic development of implicit teaching'' can cohabit as axioms when applied to the same conciliar acts; examination of whether any theologian simultaneously holds both premises in good faith.',
    'If foreclosed, the engine should treat these readings as mutually exclusive constraints competing for institutional occupancy. If coexisting, they are rival framings whose competition is itself the institutional dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether rupture and continuity readings are mutually exclusive or coexisting framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2dauth_rupt_trad_tr_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(v2dauth_rupt_trad_tr_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(v2dauth_rupt_trad_tr_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(v2dauth_rupt_trad_tr_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(v2dauth_rupt_trad_tr_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement(v2dauth_rupt_trad_tr_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 50, 0.63).
narrative_ontology:measurement(v2dauth_rupt_trad_tr_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(v2dauth_rupt_trad_be_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(v2dauth_rupt_trad_be_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(v2dauth_rupt_trad_be_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(v2dauth_rupt_trad_be_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(v2dauth_rupt_trad_be_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(v2dauth_rupt_trad_be_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 50, 0.8).
narrative_ontology:measurement(v2dauth_rupt_trad_be_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(v2dauth_rupt_trad_su_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(v2dauth_rupt_trad_su_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(v2dauth_rupt_trad_su_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(v2dauth_rupt_trad_su_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(v2dauth_rupt_trad_su_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(v2dauth_rupt_trad_su_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(v2dauth_rupt_trad_su_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 60, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
