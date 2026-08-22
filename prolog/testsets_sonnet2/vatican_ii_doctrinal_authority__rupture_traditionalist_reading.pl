% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II as Doctrinal Rupture (Traditionalist Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   In the traditionalist rupture reading, the Second Vatican Council's
 *   documents were produced under severe internal compromise between
 *   reformist and conservative episcopal factions, yielding formulations on
 *   collegiality, religious liberty, ecumenism, and liturgy deliberately
 *   ambiguous enough to secure conciliar passage. This reading holds that the
 *   ambiguity was not benign: it functioned (and continues to function) as a
 *   permissive structure inside which heterodox implementation could be
 *   authorized under color of conciliar legitimacy, producing liturgical
 *   discontinuity, catechetical confusion, and institutional capture by
 *   actors whose theological projects the pre-conciliar magisterium would
 *   have excluded. The claimed type (tangled_rope) reflects that a genuine
 *   coordination problem existed — a council needed to reach consensus across
 *   a fractured episcopate — but the resulting structure now also serves as
 *   an ongoing mechanism transferring authority and resources away from those
 *   formed under the prior settlement.
 *
 * KEY AGENTS:
 *   - post_conciliar_episcopal_bureaucracy: institutional beneficiary and agenda-setter administering implementation
 *   - progressive_theological_faculties: organized beneficiary whose institutional position depends on the rupture reading being at least partly licensed by the texts
 *   - traditional_latin_mass_communities: powerless, trapped payer bearing restricted liturgical access
 *   - missionary_religious_orders: powerless payer whose founding charism was structurally undermined
 *   - sspx_and_allied_traditionalist_institutes: excluded party whose objection can only be voiced by breaking communion
 *   - ecclesiastical_historians: analytical observer tracing the drafting record independent of institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.71).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II as Doctrinal Rupture (Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '5e833822-486a-41be-a9c6-ab09af8e666f').
narrative_ontology:cs_kernel_codification('5e833822-486a-41be-a9c6-ab09af8e666f', fixed_text).
narrative_ontology:cs_authority_grounding('5e833822-486a-41be-a9c6-ab09af8e666f', extraction).
narrative_ontology:cs_interpretation_layer_present('5e833822-486a-41be-a9c6-ab09af8e666f').
narrative_ontology:cs_reading_relation('5e833822-486a-41be-a9c6-ab09af8e666f', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('5e833822-486a-41be-a9c6-ab09af8e666f', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e833822-486a-41be-a9c6-ab09af8e666f', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('5e833822-486a-41be-a9c6-ab09af8e666f', foundational, conciliar_ambiguity_constitutes_doctrinal_rupture).
narrative_ontology:cs_axiom_status(conciliar_ambiguity_constitutes_doctrinal_rupture, holdable).
narrative_ontology:cs_axiom_grounding('5e833822-486a-41be-a9c6-ab09af8e666f', conciliar_ambiguity_constitutes_doctrinal_rupture, conventional).
narrative_ontology:cs_axiom('5e833822-486a-41be-a9c6-ab09af8e666f', foundational, compromise_language_as_defect_not_development).
narrative_ontology:cs_axiom_status(compromise_language_as_defect_not_development, holdable).
narrative_ontology:cs_axiom_grounding('5e833822-486a-41be-a9c6-ab09af8e666f', compromise_language_as_defect_not_development, empirically_contingent).
narrative_ontology:cs_axiom('5e833822-486a-41be-a9c6-ab09af8e666f', secondary, pre_conciliar_settlement_as_normative_baseline).
narrative_ontology:cs_axiom_status(pre_conciliar_settlement_as_normative_baseline, holdable).
narrative_ontology:cs_axiom_grounding('5e833822-486a-41be-a9c6-ab09af8e666f', pre_conciliar_settlement_as_normative_baseline, deontological).
narrative_ontology:cs_reference_frame('5e833822-486a-41be-a9c6-ab09af8e666f', pre_conciliar_magisterial_settlement).
narrative_ontology:cs_drift_state('5e833822-486a-41be-a9c6-ab09af8e666f', contemporary_post_conciliar_church, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('5e833822-486a-41be-a9c6-ab09af8e666f', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopal_bureaucracy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theological_faculties).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_dialogue_institutes).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_religious_orders).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, catechetically_formed_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_clergy_displaced_from_office).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, hermeneutic_of_rupture_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, compromise_text_produces_doctrinal_ambiguity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers diocesan structures, seminaries, and liturgical implementation built on the post-conciliar settlement. Controls appointments, formation curricula, and disciplinary processes; can invoke the Council's authority to marginalize traditionalist objection while treating its own implementation choices as simply 'the Council' rather than one contestable reading of ambiguous texts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopal_bureaucracy, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_episcopal_bureaucracy, agenda_setter).

% Occupy chairs and publishing platforms built on reading the conciliar documents as licensing doctrinal development beyond prior magisterial formulations. Career advancement, academic prestige, and institutional funding flow from treating the ambiguous texts as warrant for reformist theology; face little structural cost for readings traditionalists regard as heterodox.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theological_faculties, beneficiary,
    organized, generational, mobile, global).

% Staffed and funded on the premise that the Council's ecumenical and interreligious documents (Unitatis Redintegratio, Nostra Aetate) represent a genuine doctrinal shift toward pluralism, not merely pastoral tone. Their institutional existence depends on the rupture reading of these texts being at least partly correct.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_dialogue_institutes, beneficiary,
    organized, generational, mobile, global).

% Lost licit, unimpeded access to the pre-conciliar liturgy for decades and remain subject to episcopal discretion for what limited access exists. Bear restriction, parish closures, and institutional suspicion for maintaining the liturgical and doctrinal forms traditionalists hold Vatican II's ambiguity displaced; cannot exit to another jurisdiction within the Church without submitting to the reading they contest.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_communities, payer,
    powerless, biographical, trapped, global).

% Orders built around explicit conversion and missionary mandate saw vocations collapse and institutional support withdrawn as post-conciliar theology reframed missionary urgency in light of ecumenical and interreligious openness. Many orders folded or were repurposed; individual religious had no exit that preserved their founding charism intact.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_religious_orders, payer,
    powerless, generational, trapped, global).

% Raised on pre-conciliar catechesis, experienced doctrinal content and liturgical form change within a generation without vote or consultation. Some left the faith entirely; others remained under catechesis they experienced as discontinuous with what they were taught was unchangeable. Exit means either leaving the Church or accepting a reading of continuity they find incredible.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, catechetically_formed_laity, payer,
    powerless, biographical, constrained, global).

% Clergy formed and ordained under pre-conciliar norms who resisted implementation were removed from teaching posts, denied faculties, or marginalized within diocesan structures. Their theological formation became a liability rather than a credential under the new institutional consensus.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_clergy_displaced_from_office, payer,
    moderate, biographical, trapped, national).

% Formed explicitly to preserve pre-conciliar doctrine, liturgy, and formation against what they read as rupture; exist in canonically irregular or contested status precisely because they refuse to accept the post-conciliar magisterium's own account of continuity. Their objection is structurally excluded from ordinary channels of ecclesial deliberation — they can be heard only by breaking communion or accepting terms that concede the point in dispute.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, sspx_and_allied_traditionalist_institutes, excluded,
    moderate, generational, identity_locked, global).

% Study the drafting history, conciliar debates, and subsequent implementation record. Can trace which ambiguities were deliberate compromises between opposing conciliar factions and which developed unintended readings in implementation, without being institutionally bound to any single reading's verdict.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A general council convened to address pastoral engagement with the modern world required compromise language acceptable to a broad episcopal majority spanning reformist and conservative factions; ambiguous formulations were, in this reading, the price of achieving conciliar consensus at all.
% TRANSFER_FUNCTION: Moves doctrinal and liturgical authority away from settled pre-conciliar formulations and clergy/laity formed under them, toward episcopal and academic actors positioned to interpret the ambiguous texts, and toward institutions whose function depends on the rupture reading being true.
% ABSENT_VOICES: Pre-conciliar missionary orders, traditional liturgical communities, and clergy displaced from teaching office were not consulted on implementation and have no standing forum within ordinary Church structures to contest the continuity narrative without appearing to reject the Council's authority itself.
% DISAPPEARANCE_RATIONALE: If the ambiguity-enabled implementation were withdrawn and the documents authoritatively read in strict continuity with prior magisterial teaching, the post-conciliar liturgical reform, ecumenical institutional apparatus, and much of the last sixty years of catechetical and theological formation would require substantial revision or dissolution; traditionalist communities would lose their rationale for canonically irregular status.
% FOUNDING_PROBLEM: The Council was convened to address how the Church should engage a modernizing, pluralizing world after centuries of a largely defensive posture against modernity — aggiornamento.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theological faculties and post-conciliar bureaucratic structures attest the engagement-with-modernity problem remains live and the Council's answer sound. Traditionalist orders and displaced clergy — outside the beneficiary set — attest the compromise language itself, not the underlying pastoral problem, produced the doctrinal instability; independent ecclesiastical historians corroborate that specific textual ambiguities (e.g., collegiality in Lumen Gentium, religious liberty in Dignitatis Humanae) were documented compromises at the time of drafting, prior to any implementation controversy.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.71 at 2025) and rising because, on this reading, the ambiguity is not merely interpreted differently over time but has been progressively exploited — each decade's implementation choices cite the prior decade's practice as settled precedent, compounding the departure from pre-conciliar doctrine and liturgy. Suppression is authored as elevated but non-monotonic (peaking mid-1970s to late-1980s during the most aggressive liturgical implementation, easing somewhat as traditionalist institutes achieved partial canonical accommodation in the 1980s-2000s, then rising again toward 2025 amid renewed restriction of the pre-conciliar liturgy). Theater ratio rises steadily as post-conciliar institutions increasingly perform continuity-with-tradition rhetoric while administering discontinuous practice — a widening gap between stated justification and operative content, which this reading treats as diagnostic of exactly the rupture it alleges.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (episcopal bureaucracy, theological faculties, ecumenical institutes) hold institutional or organized power with mobile-to-arbitrage exit — they can shift interpretive emphasis, relocate within academic or ecclesial structures, and are rarely made to bear the cost of contested readings. Victims are overwhelmingly powerless with trapped or constrained exit: traditional liturgical communities and missionary orders cannot leave the Church without abandoning the very tradition they seek to preserve, which forecloses ordinary market-style exit. This asymmetry — power and mobility concentrated among those the ambiguity favors, powerlessness and entrapment concentrated among those it costs — is the structural core of the tangled_rope claim: a real coordination function (achieving conciliar consensus) riding alongside asymmetric extraction sustained by active enforcement (disciplinary and liturgical policy).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (aggiornamento — engaging a modernizing world) is authored as contested rather than flatly dead, which prevents this reading from being mislabeled pure extraction: the pastoral problem the Council addressed was real, and the compromise language was a genuine, if reading-costly, coordination mechanism for reaching consensus across a divided episcopate. What keeps this from collapsing into simple continuity, however, is that the ambiguity's operative use has outlived any plausible transitional function — six decades on, the same ambiguous formulations are still cited to authorize departures the traditionalist reading regards as doctrinally impermissible, with no textual correction or sunset. That persistence-without-resolution is what the tangled_rope classification, rather than scaffold, is meant to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_vs_deliberate_ambiguity,
    'Were the contested formulations (collegiality, religious liberty, ecumenism) genuine unresolved theological compromises necessary to pass the documents, or were they drafted deliberately vague by a reformist minority to license future doctrinal development beyond what a plain-text vote would have authorized?',
    'Comparative analysis of conciliar drafting history — successive schema drafts, floor debate records, and peritus correspondence — to establish whether ambiguity was a floor-negotiated concession or an intentional drafting strategy.',
    'If deliberate strategic ambiguity, the traditionalist reading''s rupture characterization is strengthened structurally rather than merely interpretively; if genuine unresolved compromise, the constraint looks more like an unintended consequence of a real coordination problem than an extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_vs_deliberate_ambiguity, empirical, 'Whether textual ambiguity in the conciliar documents was strategic or an unavoidable byproduct of achieving consensus.').

omega_variable(
    rupture_kernel_framing_dependency,
    'Is ''Vatican II'' correctly treated as a single kernel with competing readings, or does the traditionalist rupture reading actually depend on treating specific documents (Dignitatis Humanae, Sacrosanctum Concilium, Nostra Aetate) as a bundle when their individual continuity/rupture profiles may differ sharply document by document?',
    'Document-by-document doctrinal comparison against pre-conciliar magisterial statements, assessing each constitution and decree independently rather than as a unified ''Council'' output.',
    'If the documents have substantially different continuity profiles, the composite_overdetermination_reading may be the more structurally accurate kernel decomposition, and this rupture_traditionalist story would itself need further splitting by document rather than treating Vatican II as one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_kernel_framing_dependency, conceptual, 'Whether treating Vatican II as one kernel obscures document-level heterogeneity that a finer decomposition would reveal.').

omega_variable(
    counterfactual_implementation_authority,
    'Would a hypothetical strict, traditionalist-preferred implementation of the same ambiguous texts have avoided the outcomes (liturgical discontinuity, vocation collapse, catechetical confusion) this reading attributes to the documents themselves, or were those outcomes driven primarily by the post-conciliar bureaucracy''s implementation choices rather than the texts?',
    'Comparative case study of the small number of dioceses/orders that implemented conciliar reforms conservatively versus the majority that implemented expansively, controlling for regional cultural factors.',
    'If conservative-implementation cases show substantially better preservation of pre-conciliar practice with no loss of conciliar legitimacy, the extraction is better attributed to implementation choices (an agenda-setter behavior) than to the documents'' ambiguity itself, which would somewhat reduce the ε attributable to the textual constraint proper.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_implementation_authority, empirical, 'Whether attributed harms trace to the ambiguous texts themselves or to discretionary implementation choices layered on top of them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1988, 0.38).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2013, 0.46).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1988, 0.6).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2013, 0.68).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1988, 0.58).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2013, 0.45).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_latin_mass_access_restriction).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_catechetical_formation_standard).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the vatican_ii_doctrinal_authority kernel. continuity_reading denies rupture occurred at all (logically foreclosed by this reading's foundational axiom). rupture_progressive_reading affirms the same rupture as liberation rather than damage — both readings share the rupture premise but assign opposite normative valence, hence coexists_with rather than forecloses. composite_overdetermination_reading questions whether 'Vatican II' is even a single kernel rather than several independently-driven reforms bundled together; this reading's document-bundling assumption is influenced by, and vulnerable to, that decomposition. Each reading carries its own epsilon, beneficiary/victim structure, and classification — none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
