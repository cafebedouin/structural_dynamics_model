% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II Rupture: Doctrinal Break with Tradition
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   The rupture reading of Vatican II holds that the Council's documents
 *   (especially Dignitatis Humanae, Unitatis Redintegratio, Nostra Aetate,
 *   Gaudium et Spes) contain propositions that contradict the prior defined
 *   magisterium (Quanta Cura, Syllabus of Errors, Mortalium Animos, Trent,
 *   Vatican I). The constraint is the post-conciliar regime that enforces
 *   these documents as binding magisterium while suppressing the traditional
 *   framework that would judge them. Beneficiaries are the modernist faction
 *   that captured the conciliar drafting and post-conciliar implementation;
 *   victims are traditional Catholics whose doctrinal stability and
 *   liturgical identity were extracted. The SSPX position instantiates this
 *   reading: the Council is not a legitimate exercise of the magisterium but
 *   a revolutionary event that the Church must resist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.82).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, snare).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II Rupture: Doctrinal Break with Tradition").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '57a7b210-b027-433e-8468-7fcd2f99b0a3').
narrative_ontology:cs_kernel_codification('57a7b210-b027-433e-8468-7fcd2f99b0a3', formalized).
narrative_ontology:cs_authority_grounding('57a7b210-b027-433e-8468-7fcd2f99b0a3', lineage).
narrative_ontology:cs_interpretation_layer_present('57a7b210-b027-433e-8468-7fcd2f99b0a3').
narrative_ontology:cs_reading_relation('57a7b210-b027-433e-8468-7fcd2f99b0a3', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('57a7b210-b027-433e-8468-7fcd2f99b0a3', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('57a7b210-b027-433e-8468-7fcd2f99b0a3', foundational, vatican_ii_contains_doctrinal_errors).
narrative_ontology:cs_axiom_status(vatican_ii_contains_doctrinal_errors, holdable).
narrative_ontology:cs_axiom_grounding('57a7b210-b027-433e-8468-7fcd2f99b0a3', vatican_ii_contains_doctrinal_errors, theological).
narrative_ontology:cs_axiom('57a7b210-b027-433e-8468-7fcd2f99b0a3', foundational, post_conciliar_church_in_crisis).
narrative_ontology:cs_axiom_status(post_conciliar_church_in_crisis, holdable).
narrative_ontology:cs_axiom_grounding('57a7b210-b027-433e-8468-7fcd2f99b0a3', post_conciliar_church_in_crisis, theological).
narrative_ontology:cs_axiom('57a7b210-b027-433e-8468-7fcd2f99b0a3', secondary, traditional_mass_never_abrogated).
narrative_ontology:cs_axiom_status(traditional_mass_never_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('57a7b210-b027-433e-8468-7fcd2f99b0a3', traditional_mass_never_abrogated, conventional).
narrative_ontology:cs_reference_frame('57a7b210-b027-433e-8468-7fcd2f99b0a3', pre_conciliar_doctrinal_unity).
narrative_ontology:cs_drift_state('57a7b210-b027-433e-8468-7fcd2f99b0a3', post_conciliar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('57a7b210-b027-433e-8468-7fcd2f99b0a3', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, reformist_hierarchy).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholics).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, sspx_faithful).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_stability).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, doctrinal_continuity_requires_rupture_rejection).
narrative_ontology:constraint_vindicates(vatican_ii_authority__rupture_reading, council_documents_contain_errors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls post-conciliar interpretation, liturgical reform, and institutional appointments. Gains authority to redefine doctrine, liturgy, and ecclesial practice under the banner of 'aggiornamento.' Can move between academic, curial, and episcopal positions; exit is upward mobility within the new framework.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_faction, beneficiary,
    institutional, generational, arbitrage, universal).

% Bishops and curial officials who implement and enforce the conciliar reforms. Benefit from expanded pastoral discretion and reduced doctrinal constraint. Exit would mean breaking with the very structure they administer; constrained by office and institutional identity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, reformist_hierarchy, beneficiary,
    institutional, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, reformist_hierarchy, agenda_setter).

% Academic theologians whose interpretive frameworks became normative after the Council. Gain professional recognition, publishing access, and influence over formation. Can exit to secular academia but lose ecclesial authority; mobile within the theological guild.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, progressive_theologians, beneficiary,
    organized, biographical, mobile, universal).

% Laity and clergy attached to pre-conciliar doctrine, liturgy, and ecclesiology. Bear the cost of doctrinal destabilization, liturgical suppression, and marginalization. Exit requires abandoning their self-understanding as Catholics; identity is fused with the traditional form, making exit psychologically and spiritually prohibitive.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholics, payer,
    organized, generational, identity_locked, universal).

% Society of St. Pius X adherents who reject the Council's authority. Bear canonical irregularity, social stigma, and exclusion from ordinary sacramental ministry. Their identity is constituted through resistance to the rupture; exit would dissolve the very grounds of their ecclesial existence.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_faithful, payer,
    organized, generational, identity_locked, global).

% The abstract good of a coherent, unchanging deposit of faith. Not an agent but the structural victim: the constraint's operation erodes the possibility of appealing to a stable doctrinal referent. No exit; it is the ground that collapses.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, doctrinal_stability, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_authority__rupture_reading, doctrinal_stability).

% The teaching authority that promulgated and enforces the conciliar documents. Sets the interpretive boundaries, disciplines dissent, and controls sacramental discipline. Constrained by the logic of the Council they authorized; cannot repudiate it without undermining their own legitimacy.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, magisterium_hierarchy, agenda_setter,
    institutional, generational, constrained, universal).

% Clergy who maintain pre-conciliar doctrine and liturgy within or at the margins of canonical structures. Would object to the rupture interpretation but are systematically excluded from governance, synods, and formation. Exit means either submission or canonical penalty.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditionalist_bishops_priests, excluded,
    moderate, biographical, constrained, global).

% Non-Catholic theologians, historians, and religious scholars who analyze the Council's reception. Neither collect nor pay; they map the structural transformation from outside the commitment.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, ecumenical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Officially: aggiornamento — updating the Church's presentation of the faith to engage the modern world. Rupture reading: this coordination story is cover; the actual function is legitimating a doctrinal break while maintaining institutional continuity.
% TRANSFER_FUNCTION: Moves interpretive authority, liturgical control, and doctrinal definition from the pre-conciliar framework (Trent, Vatican I, traditional liturgy) to the post-conciliar reformist establishment. Transfers the power to define 'Catholic identity' from the deposit of faith to the living magisterium's contemporary discernment.
% ABSENT_VOICES: Pre-conciliar theologians (Ottaviani, Lefebvre, Davies) who warned of doctrinal rupture; laity attached to the traditional liturgy who were never consulted; persecuted traditionalist clergy (e.g., Campos, Camposanto) canonically suppressed; the 'silent majority' of 1960s Catholics who experienced the transition as loss without consent.
% DISAPPEARANCE_RATIONALE: If the rupture interpretation and its enforcement vanished overnight, the pre-conciliar doctrinal and liturgical framework would be restored as normative. The Novus Ordo would lose its mandatory status; the 1962 Missal would return as the ordinary form; doctrinal ambiguities in conciliar texts would be resolved by reference to prior magisterium. The institutional Church would face a legitimacy crisis but the traditional structure would reassert itself.
% FOUNDING_PROBLEM: The Church faced a crisis of relevance in the modern world: declining practice, intellectual marginalization, and the need to proclaim the Gospel in a language contemporary humanity could hear.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist scholars (Davies, Madiran, Williamson) and SSPX attest the 'crisis of relevance' was manufactured by modernist theologians who had already captured seminaries and journals; the Council was the vehicle, not the response. Progressive historians (O'Malley, Alberigo) and reformist bishops attest the crisis was real and the Council was the necessary response. The corroboration is split along the beneficiary/victim line — no neutral third party adjudicates.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint transfers the very ground of doctrinal authority from the immutable deposit to the living magisterium's discretionary interpretation. Suppression is high (0.78) because the traditional liturgy was actively suppressed (1970-2007), traditionalist clergy were canonically penalized, and catechetical formation was restructured around conciliar categories. Theater ratio is moderate (0.55): the coordination rituals (synods, papal audiences, 'hermeneutic of continuity' discourses) are real but increasingly performative — they maintain the appearance of unity while the doctrinal rupture widens. Accessibility collapse is near-total (0.88): the pre-conciliar framework is institutionally unavailable in almost all dioceses; resistance remains high (0.72) because the traditionalist movement persists and grows despite suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (modernist_faction, reformist_hierarchy), the constraint appears as legitimate development — the Church adapting to modernity. From the payer seats (traditional_catholics, sspx_faithful), it appears as a snare: the coordination story (aggiornamento) is cover for a doctrinal revolution that extracts their stability. The engine computes this divergence from the structural data; the claimed_type 'snare' reflects the rupture reading's own structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernist faction and reformist hierarchy are structural beneficiaries (d ~ 0.1): they control the interpretation, the appointments, the liturgy, the narrative. Traditional Catholics and SSPX faithful are structural targets (d ~ 0.9): they pay the costs (doctrinal confusion, liturgical loss, canonical marginalization) with identity-locked exit. Doctrinal_stability is a non-agent victim (trapped). Magisterium_hierarchy is agenda_setter (d ~ 0.4): they administer the constraint but are constrained by it — they cannot repudiate the Council without losing their legitimacy. Progressive theologians are mobile beneficiaries (d ~ 0.2). Traditionalist clergy are excluded (d ~ 0.7) — they bear costs without voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (crisis of relevance) is dead from the rupture perspective: the post-conciliar Church has not evangelized the modern world but accommodated it; the 'signs of the times' proved to be the spirit of the age. The arrangement persists because the beneficiaries control the institutions that would judge it. Mandatrophy is unresolved — the constraint's mandate (renewal) has inverted into its opposite (rupture), but the institutional machinery prevents correction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_error_vs_misinterpretation,
    'Do the conciliar documents themselves contain formal doctrinal errors, or are the apparent contradictions products of misinterpretation and faulty reception?',
    'Systematic theological comparison of conciliar texts with prior defined magisterium (Quanta Cura, Vatican I, Trent) by a panel of theologians acceptable to both rupture and continuity parties — currently institutionally impossible.',
    'If errors are in the texts, the rupture reading''s snare classification is structurally vindicated; if errors are only in reception, the constraint may be a tangled_rope (genuine coordination with extractive implementation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_error_vs_misinterpretation, conceptual, 'Whether the rupture is textual or receptual.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditional doctrine and liturgy primarily structural (canonical penalties, institutional exclusion) or internalized (theological formation that makes the traditional framework unintelligible)?',
    'Post-exit trajectory study: track traditionalist communities that regularized (Ecclesia Dei, FSSP, ICRSS) — if suppression persists internally after canonical regularization, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure; the constraint reproduces itself in the subjectivity of its targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the post-conciliar Church.').

omega_variable(
    kernel_reading_framing,
    'Is the Vatican II authority kernel best framed as a single interpretive dispute (continuity vs. rupture) or as an overdetermined composite (composite_overdetermination_reading)?',
    'Analyze whether the conciliar texts admit a single coherent reading or contain structurally incompatible theological rationales that no hermeneutic can unify.',
    'If composite framing is correct, the rupture/continuity binary is itself a false constraint; the real constraint is the overdetermination that prevents resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Commitment-system framing ambiguity: binary dispute vs. overdetermined composite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__rupture_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__rupture_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_authority__rupture_reading, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(vati_tr_t1980, vatican_ii_authority__rupture_reading, theater_ratio, 1980, 0.45).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_authority__rupture_reading, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_authority__rupture_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_authority__rupture_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_authority__rupture_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__rupture_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__rupture_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_authority__rupture_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(vati_be_t1980, vatican_ii_authority__rupture_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_authority__rupture_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_authority__rupture_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_authority__rupture_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_authority__rupture_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__rupture_reading, suppression_requirement, 1962, 0.2).
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__rupture_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_authority__rupture_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(vati_su_t1980, vatican_ii_authority__rupture_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_authority__rupture_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_authority__rupture_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_authority__rupture_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_authority__rupture_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__rupture_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, traditional_liturgy_suppression).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, ecumenism_doctrinal_shift).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, religious_liberty_redefinition).

% DUAL FORMULATION NOTE:
% Part of the vatican_ii_authority constraint family. Rupture reading claims high extraction (snare) where continuity claims near-zero (mountain/rope). The composite reading claims structural ambiguity prevents either classification. All three share the kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__rupture_reading, institutional, 0.1).
constraint_indexing:directionality_override(vatican_ii_authority__rupture_reading, organized, 0.85).
constraint_indexing:directionality_override(vatican_ii_authority__rupture_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
