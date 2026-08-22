% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios Subordinationist Interpretation: Son Derives Being from Father
 *   domain: ecclesiastical_history/theology/philosophy_of_religion
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE) declared homoousios ('of one substance')
 *   as the binding term for the relationship between Father and Son,
 *   explicitly rejecting Arianism's subordinationist christology. Yet
 *   homoousios' precise meaning was contested throughout the fourth century.
 *   This constraint story instantiates the subordinationist reading: an
 *   interpretation holding that homoousios is compatible with the Son's
 *   derivation from and ontological subordination to the Father—the Son
 *   shares full divine status and power but not equality of being or
 *   eternity. This reading preserved communities maintaining non-Nicene
 *   traditions (Arians, Semi-Arians, some Eastern strands) by reinterpreting
 *   the winning conciliar term through their exegetical frameworks. The
 *   subordinationist reading was eventually institutionally foreclosed by
 *   subsequent councils (Constantinople 381, further refinements at 431 and
 *   451) that reread homoousios as securing strict metaphysical equality and
 *   co-eternity. This constraint models the subordinationist interpretation
 *   as a live hermeneutical option during the 325–450 window, showing how
 *   conciliar authority had to intensify suppression of the reading as the
 *   metaphysical-equality interpretation solidified.
 *
 * KEY AGENTS:
 *   - Subordinationist theological communities: Arian remnants, Semi-Arian theologians, eastern non-Nicene traditions claiming homoousios compatibility with derivation-language.
 *   - Scriptural literalist exegetes: scholars reading biblical subordination motifs (Proverbs 8:22, John 5:26, 1 Corinthians 11:3) as non-negotiable and seeking a christology that preserves them.
 *   - Nicene orthodoxy enforcer: institutional councils, episcopal succession, conciliar tradition enforcing the metaphysical-equality reading and marginalizing subordinationist alternatives.
 *   - Eastern homoiousios traditionalists: communities whose distinct mediating position was dissolved by the subordinationist reading's absorption into a reinterpreted homoousios.
 *   - Historical analysts: modern scholars examining whether the subordinationist reading was a coherent hermeneutical option or a covered-up attempt to preserve Arianism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.68).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.72).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios Subordinationist Interpretation: Son Derives Being from Father").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "ecclesiastical_history/theology/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, '0e38e5c1-499a-4e75-b053-29d64a86fc96').
narrative_ontology:cs_kernel_codification('0e38e5c1-499a-4e75-b053-29d64a86fc96', fixed_text).
narrative_ontology:cs_authority_grounding('0e38e5c1-499a-4e75-b053-29d64a86fc96', extraction).
narrative_ontology:cs_interpretation_layer_present('0e38e5c1-499a-4e75-b053-29d64a86fc96').
narrative_ontology:cs_reading_relation('0e38e5c1-499a-4e75-b053-29d64a86fc96', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('0e38e5c1-499a-4e75-b053-29d64a86fc96', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('0e38e5c1-499a-4e75-b053-29d64a86fc96', foundational, derivation_compatible_with_divinity).
narrative_ontology:cs_axiom_status(derivation_compatible_with_divinity, holdable).
narrative_ontology:cs_axiom_grounding('0e38e5c1-499a-4e75-b053-29d64a86fc96', derivation_compatible_with_divinity, empirically_contingent).
narrative_ontology:cs_axiom('0e38e5c1-499a-4e75-b053-29d64a86fc96', foundational, scriptural_subordination_language_binding).
narrative_ontology:cs_axiom_status(scriptural_subordination_language_binding, overridden).
narrative_ontology:cs_axiom_grounding('0e38e5c1-499a-4e75-b053-29d64a86fc96', scriptural_subordination_language_binding, deontological).
narrative_ontology:cs_reference_frame('0e38e5c1-499a-4e75-b053-29d64a86fc96', scriptural_subordination_preserved).
narrative_ontology:cs_drift_state('0e38e5c1-499a-4e75-b053-29d64a86fc96', post_constantinople_381, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0e38e5c1-499a-4e75-b053-29d64a86fc96', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_theological_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, scriptural_literalist_exegetes).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, nicene_orthodoxy_enforcer_sees_flexibility_loss).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, eastern_homoiousios_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, anti_nicene_apologists).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, anti_nicene_apologists).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, biblical_subordination_motifs).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, creator_creature_ontological_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities maintaining theological continuity with Arian and Semi-Arian traditions, or with non-Nicene Eastern christologies. The subordinationist reading of homoousios allows them to claim loyalty to conciliar terminology while preserving scriptural language of the Son's derivation from the Father. Their identity is fused with this exegetical tradition; exiting means abandoning theological inheritance.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_theological_communities, beneficiary,
    moderate, generational, identity_locked, regional).

% Theologians and scholars whose exegetical method reads biblical subordination language (Proverbs 8:22, John 5:26, 1 Corinthians 11:3) as binding theological claims. The subordinationist reading vindicates their interpretive approach by showing homoousios compatible with their exegetical results. They remain constrained by institutional pressure to conform to metaphysical-equality orthodoxy but find hermeneutical support in the subordinationist reading.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, scriptural_literalist_exegetes, beneficiary,
    organized, biographical, constrained, continental).

% The institutional system of conciliar authority, episcopal succession, and ecclesiastical hierarchy that enforces homoousios' interpretation as securing metaphysical equality and co-eternity. Sets binding christological doctrine, adjudicates heresy, controls ecclesiastical advancement and sacramental legitimacy. The subordinationist reading is experienced as ambiguity threatening doctrinal uniformity. This agent can reinterpret doctrine (has arbitrage-level flexibility) but faces institutional cost in doing so.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, nicene_orthodoxy_enforcer, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Eastern Christian communities and theologians whose tradition held homoiousios (similarity) as a mediating position between Arianism and Nicene equality. The subordinationist reinterpretation of homoousios collapses the distinct space their position occupied—homoousios is no longer opposed to subordinationist readings but encompasses them. They pay the cost of losing a distinct theological alternative.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, eastern_homoiousios_traditionalists, payer,
    powerful, generational, mobile, regional).

% Roman Catholic, Orthodox, and mainstream Protestant institutional churches whose identity and authority rest on conciliar tradition and the metaphysical-equality reading of homoousios. They are excluded from the subordinationist reading's theological development by institutional boundary-maintenance; they would object to it as heresy revival but cannot participate in its elaboration—their participation would be repudiation.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, conciliar_tradition_inheritors, excluded,
    institutional, civilizational, trapped, global).

% Theologians arguing against the metaphysical-equality reading on scriptural grounds. The subordinationist reading vindicates their hermeneutical method and gives them a way to claim partial Nicene compliance while maintaining their exegetical commitments. They pay the cost of exclusion from institutional orthodoxy but gain exegetical coherence.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, anti_nicene_apologists, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, anti_nicene_apologists, payer).

% Imperial power (particularly Constantine and his successors) that convened and enforced councils as instruments of political and theological unity. Used conciliar authority to suppress subordinationist alternatives and impose uniform orthodoxy across the empire. Can reshape the institutional apparatus but faces political cost in maintaining religious uniformity.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, imperial_conciliar_authority, agenda_setter,
    powerful, generational, arbitrage, global).

% Modern scholars examining the homoousios contest from outside the theological tradition—church historians, philosophers of language, comparative religionists. They observe whether subordinationist readings represent coherent exegetical alternatives or rhetorical appropriations of a term designed to exclude them.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, historical_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, nicene_orthodoxy_enforcer).
narrative_ontology:fixing_cost_class(homoousios_nicene__subordinationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures a unified christological terminology (homoousios) that can accommodate scriptural motifs of subordination while maintaining the Son's full divine status—solving the interpretive problem of reconciling biblical language of derivation with conciliar affirmations of divinity.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from conciliar tradition to scriptural exegesis by permitting subordinationist readings of homoousios; transfers theological legitimacy to communities maintaining non-Nicene christological traditions; dissolves the mediating position (homoiousios) that eastern traditionalists secured as a distinct option.
% ABSENT_VOICES: Arian and Semi-Arian communities, long institutionally excluded, would assert the subordinationist reading as vindication of their scriptural hermeneutics. Eastern homoiousios traditionalists would protest the absorption of their distinct position. Institutionalized anti-nicene communities in Late Antiquity (Nestorians, some Monophysite strands) would affirm subordinationist christology but would be heard only through historical reconstruction, not in contemporary conciliar spaces.
% DISAPPEARANCE_RATIONALE: Institutionally dominant reading (metaphysical equality) would persist through conciliar tradition without subordinationist readings; subordinationist communities would reorganize around scriptural literalism and non-Nicene traditionalism. The theological landscape would not rearrange fundamentally because subordinationist readings are already excluded from institutional christology, but their disappearance would close a hermeneutical option that some theological traditions claim as continuity with their inheritance.
% FOUNDING_PROBLEM: Biblical texts assert the Son's dependence on the Father (Proverbs 8:22 'the Lord created me at the beginning of his way'; John 5:26 'the Father has life in himself and has granted the Son also to have life in himself'); conciliar tradition affirms the Son's full divinity and ontological equality. How can homoousios be true without erasing the scriptural language of derivation and dependence?
% FOUNDING_PROBLEM_CORROBORATION: Subordinationist exegetes attest the founding problem is live and that their reading solves it by showing homoousios compatible with derivation. Metaphysical-equality readers and conciliar historians attest the founding problem is a hermeneutical artifact of reading derivation language literally rather than functionally or metaphorically; conciliar consensus since Constantinople 381 confirms the problem is superseded. The disagreement is not resolved by outside authority—it is constitutive of the kernel contest itself.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, contested).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 (pre-Nicaea, when multiple christologies coexist) to 0.68 (post-Constantinople, when subordinationist readings are excluded from institutional legitimacy). This trajectory reflects the progressive tightening of what counts as orthodox—the subordinationist reading is not rejected on exegetical grounds but on conciliar authority grounds. Suppression rises sharply from 0.35 to 0.72 over the same span, modeling the institutional machinery (council decrees, episcopal enforcement, sacramental exclusion, heresy charges) required to suppress subordinationist alternatives. Theater ratio rises from 0.18 to 0.41, suggesting that increasing share of the suppression effort is devoted to policing the reading's interpretive space rather than addressing the underlying exegetical problem. By 450, the reading is so thoroughly institutionally excluded that further suppression yields no additional extractiveness—the constraint has reached equilibrium through complete institutional embedding of the metaphysical-equality reading. Accessibility collapse is moderate (0.64) because the subordinationist reading remains exegetically coherent; alternatives do not collapse simply because the council declared them heretical—believers must actively maintain the reading against institutional pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional conciliar seat, the subordinationist reading is an ambiguity to be eliminated—it threatens the binding force of Nicene orthodoxy by permitting its reinterpretation. From the subordinationist theological community seat, the same reading is a vindication of scriptural integrity—they experience conciliar enforcement as suppression of legitimate exegesis. The beneficiary seat (subordinationist communities) experiences the constraint as extractive because institutional closure forces them to choose between theological identity and ecclesiastical legitimacy. The conciliar enforcer seat experiences the constraint as necessary coordination—establishing a unified christology against fragmentary alternatives. The engine should compute different types from these seats because the structural asymmetry is real: from subordinationist perspective, this is snare (institutional suppression of a coherent theological tradition); from conciliar perspective, this is rope or tangled_rope (coordination of orthodoxy with minor asymmetric cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist communities and scriptural literalists are beneficiaries of the reading because it preserves their theological tradition and exegetical method within a Nicene-compatible framework—their d is low (0.2–0.3). The conciliar orthodoxy enforcer is the structural beneficiary collecting conciliar authority and institutional control; institutional power means low extraction cost for them—their d is negative (−0.1 to 0.0, full beneficiary). Eastern homoiousios traditionalists are victims because the reading dissolves their mediating position—their d is high (0.7–0.8). Nicene orthodoxy itself becomes a victim set (excluded alternative space for theological flexibility)—this is counterintuitive but follows from the constraint's structure: conciliar enforcement of uniform orthodoxy extracts flexibility from the theological space, and Nicene orthodoxy becomes the institutional lock that prevents subordinationist readings from being taken seriously as options. The directionality override here is subtle: Eastern traditionalists have moderate power (theologians, bishops, regional influence) but are trapped by the absorption of their distinct position—their exit_options are constrained, not mobile, because they have no institutional shelter for homoiousios once homoousios is reinterpreted to serve subordinationist purposes.
 *
 * MANDATROPHY ANALYSIS:
 *   The subordinationist reading instantiates a classic mandatrophy signal: the founding problem (reconciling biblical derivation language with conciliar divinity affirmations) is LIVE for subordinationist communities and exegetes but the institutional apparatus treats it as DEAD (superseded by conciliar authority). The founding-problem-status is officially contested: conciliar authorities say the founding problem is solved by metaphysical equality (the problem is hermeneutical error, not theological residue); subordinationists say it is live (the problem is real and their reading solves it). This mismatch—contested founding-problem status paired with world_rearranges disappearance verdict—is the mandatrophy signature. If the subordinationist reading disappeared, subordinationist and scriptural-literalist communities would reorganize around explicit anti-Nicene positions (full Arianism, Nestorianism, etc.), which would force conciliar councils to invest even more in active suppression. The constraint persists not because it solves the founding problem but because conciliar institutional power prevents alternatives from being heard as solving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exegetical_vs_conciliar_authority_location,
    'Does homoousios'' meaning derive from scriptural exegesis or from conciliar authority? Can subordinationist exegetes claim homoousios compatibility, or does conciliar determination fix meaning independently of exegetical derivation?',
    'Examine fourth-century theological debates to determine whether subordinationist authors were genuine exegetes offering alternative readings or rhetorical appropriators trying to conform their position to a council-imposed term post hoc.',
    'If exegetical, the subordinationist reading claims independence from conciliar determination and remains a live hermeneutical option; if appropriation, the reading is incoherent and homoousios'' meaning is settled by institutional fiat. This affects whether the constraint models coordination or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exegetical_vs_conciliar_authority_location, empirical, 'Authority location: does scriptural exegesis or conciliar determination fix homoousios'' meaning?').

omega_variable(
    derivation_compatible_with_equality,
    'Is ontological subordination—the Son deriving being from the Father—logically compatible with full equality of divine status and power? Or does derivation necessarily entail inequality?',
    'Metaphysical analysis of the concepts ''derivation'', ''equality'', ''being'', and ''substance'' to determine whether they can coexist in a coherent theology. Examine whether fourth-century subordinationists articulated a coherent metaphysical claim or whether they conflated functional and ontological subordination.',
    'If compatible, subordinationist readings are philosophically defensible and institutionally suppressed for reasons of authority, not reason; if incompatible, subordinationism is conceptually incoherent and conciliar rejection is justified on philosophical grounds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivation_compatible_with_equality, conceptual, 'Is derivation logically compatible with equality, or do they entail contradictory metaphysics?').

omega_variable(
    conciliar_foreclosure_vs_hermeneutical_openness,
    'Did the councils intend to foreclose subordinationist readings of homoousios, or did they leave the term open to exegetical development, with later institutional pressure imposing foreclosure retroactively?',
    'Comparative analysis of council documents, canons, and attendee testimony to determine the conciliar intent regarding homoousios'' interpretive scope. Compare early vs. late conciliar language on subordination.',
    'If councils intended foreclosure from the start, subordinationist readings are a misappropriation of a term intended to rule them out; if the term was left open, later councils performed hermeneutical imperialism, narrowing meaning post hoc. This affects whether the constraint is extraction of theological flexibility or coordination around a disputed term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_foreclosure_vs_hermeneutical_openness, empirical, 'Did Nicaea intend to foreclose subordinationism, or did later councils impose that foreclosure?').

omega_variable(
    functional_vs_ontological_subordination_collapse,
    'Did subordinationist readers coherently maintain a distinction between functional (economic) and ontological (immanent) subordination—roles the Son plays vs. the Son''s being itself? Or did the reading collapse into ontological subordinationism?',
    'Textual analysis of subordinationist theology to track whether they argued the Son''s functional dependence in time (as God-made-flesh, as revealer) separately from ontological dependence in being. Determine if post-Nicene subordinationists abandoned functional distinction and endorsed pure ontological claims.',
    'If coherent functional distinction, the reading addresses a real theological problem (how the Son''s historical dependence on the Father coheres with full divinity); if collapse, the reading is incoherent and institutional suppression is justified. The constraint''s classification depends on whether the suppressed position is coherent or confused.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_ontological_subordination_collapse, empirical, 'Did subordinationists maintain functional/ontological distinction or collapse into pure ontological subordination?').

omega_variable(
    kernel_reading_vs_exegetical_cover_story,
    'Is this constraint a genuine reading of the homoousios kernel with its own exegetical coherence, or is subordinationism using ''subordinationist reading of homoousios'' as a cover story to preserve pre-Nicene christology under new institutional constraints?',
    'Determine whether subordinationist exegetes before Nicaea held the same exegetical commitments as post-Nicene subordinationists. If the reading is a new construct designed to preserve Arianism after the council, it is a rhetorical appropriation, not a genuine kernel reading.',
    'If genuine reading, the kernel permits multiple coherent instantiations and the conciliar foreclosure is enforced meaning-narrowing; if appropriation, subordinationism is a failed attempt to salvage a condemned position, and institutional suppression is justified as preventing rhetorical manipulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_vs_exegetical_cover_story, empirical, 'Is this a genuine kernel reading or a cover story for pre-Nicene Arianism?').

omega_variable(
    institutional_extraction_vs_legitimate_boundary_maintenance,
    'Does the institutional suppression of subordinationist readings represent extractive conciliar power consolidation, or legitimate boundary maintenance of a genuine doctrinal consensus?',
    'Determine whether conciliar consensus on metaphysical equality was genuinely held by bishops and theologians across regions, or whether it was imperial pressure and elite coercion masquerading as consensus. Examine dissent, minority reports, and post-conciliar resistance.',
    'If genuine consensus, suppression is coordination cost; if coerced, suppression is extraction. This determines the type classification from the conciliar institutional seat: if consensus-based, the constraint is rope or tangled_rope; if coerced, it is snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_vs_legitimate_boundary_maintenance, empirical, 'Is conciliar suppression legitimate boundary-maintenance or extractive power consolidation?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 300, 450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t300, homoousios_nicene__subordinationist_reading, theater_ratio, 300, 0.18).
narrative_ontology:measurement_basis(homo_tr_t300, projected).
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__subordinationist_reading, theater_ratio, 325, 0.24).
narrative_ontology:measurement_basis(homo_tr_t325, observed).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__subordinationist_reading, theater_ratio, 350, 0.31).
narrative_ontology:measurement_basis(homo_tr_t350, observed).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__subordinationist_reading, theater_ratio, 381, 0.38).
narrative_ontology:measurement_basis(homo_tr_t381, observed).
narrative_ontology:measurement(homo_tr_t415, homoousios_nicene__subordinationist_reading, theater_ratio, 415, 0.41).
narrative_ontology:measurement_basis(homo_tr_t415, observed).
narrative_ontology:measurement(homo_tr_t450, homoousios_nicene__subordinationist_reading, theater_ratio, 450, 0.41).
narrative_ontology:measurement_basis(homo_tr_t450, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t300, homoousios_nicene__subordinationist_reading, base_extractiveness, 300, 0.42).
narrative_ontology:measurement_basis(homo_be_t300, projected).
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__subordinationist_reading, base_extractiveness, 325, 0.48).
narrative_ontology:measurement_basis(homo_be_t325, observed).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__subordinationist_reading, base_extractiveness, 350, 0.56).
narrative_ontology:measurement_basis(homo_be_t350, observed).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__subordinationist_reading, base_extractiveness, 381, 0.64).
narrative_ontology:measurement_basis(homo_be_t381, observed).
narrative_ontology:measurement(homo_be_t415, homoousios_nicene__subordinationist_reading, base_extractiveness, 415, 0.68).
narrative_ontology:measurement_basis(homo_be_t415, observed).
narrative_ontology:measurement(homo_be_t450, homoousios_nicene__subordinationist_reading, base_extractiveness, 450, 0.68).
narrative_ontology:measurement_basis(homo_be_t450, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t300, homoousios_nicene__subordinationist_reading, suppression_requirement, 300, 0.35).
narrative_ontology:measurement_basis(homo_su_t300, projected).
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__subordinationist_reading, suppression_requirement, 325, 0.52).
narrative_ontology:measurement_basis(homo_su_t325, observed).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__subordinationist_reading, suppression_requirement, 350, 0.62).
narrative_ontology:measurement_basis(homo_su_t350, observed).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__subordinationist_reading, suppression_requirement, 381, 0.71).
narrative_ontology:measurement_basis(homo_su_t381, observed).
narrative_ontology:measurement(homo_su_t415, homoousios_nicene__subordinationist_reading, suppression_requirement, 415, 0.73).
narrative_ontology:measurement_basis(homo_su_t415, observed).
narrative_ontology:measurement(homo_su_t450, homoousios_nicene__subordinationist_reading, suppression_requirement, 450, 0.72).
narrative_ontology:measurement_basis(homo_su_t450, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__subordinationist_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% The homoousios_nicene kernel decomposes into three structurally distinct constraint stories, each representing a live reading of the same term. The subordinationist_reading interprets homoousios as compatible with derivation; the metaphysical_equality_reading interprets it as securing strict equality; the honorific_similarity_reading interprets it as permitting mediating positions (homoiousios-like). Each reading has different ε (extractiveness), different beneficiary/victim structures, and different classifications. The readings FORECLOSE or COEXIST based on their core exegetical premises, not their policy desirability. Network links track the hermeneutical dependencies: subordinationist and metaphysical-equality readings FORECLOSE each other; both INFLUENCE the honorific-similarity reading by defining its interpretive space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
