% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Spirit-of-the-Council Authorization Regime (Rupture-Progressive Reading of Vatican II)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story models the rupture-progressive reading of Vatican II as an
 *   operating arrangement: the claim that the Council constituted a necessary
 *   break with pre-conciliar rigidity, and that the 'spirit of the Council'
 *   legitimately authorizes ongoing reform beyond the documents' textual
 *   limits, with post-conciliar implementation treated as authentic
 *   realization of conciliar intent. The arrangement under description is the
 *   spirit-based authorization regime as it has actually operated since 1962
 *   — assessed by this reading's own lights, which regard the break as
 *   necessary and the costs as largely justified, while still recording that
 *   the costs were real and fell unevenly. KEY AGENTS (by structural
 *   relationship): papal_magisterium_and_curia: agenda setter
 *   (institutional/arbitrage) — administers the hermeneutic and holds the
 *   final word on what the spirit permits;
 *   postconciliar_theological_establishment: primary beneficiary
 *   (institutional/identity_locked) — careers and agenda access ride on the
 *   received reading; national_episcopal_conferences: beneficiary and
 *   regional administrator (institutional/constrained);
 *   ecumenical_dialogue_partners: incidental beneficiary outside the governed
 *   system (institutional/mobile); traditionalist_clergy: primary target
 *   (organized/constrained); preconciliar_liturgical_communities: target
 *   (moderate/trapped); contemplative_religious_orders: target
 *   (organized/constrained); dissenting_parish_laity: excluded voice
 *   (powerless/trapped); church_historians: analytical observer. The claim
 *   and the metrics are independent authored facts: the claimed type states
 *   what this reading believes is structurally true; the metrics state what
 *   is descriptively true of the regime's operation, including features this
 *   reading regrets.
 *
 * KEY AGENTS:
 *   - papal_magisterium_and_curia: agenda setter (institutional/arbitrage) — declares what the Council's spirit requires, appoints its carriers, disciplines departure
 *   - postconciliar_theological_establishment: primary beneficiary (institutional/identity_locked) — collects careers, chairs, and agenda access from the received reading
 *   - national_episcopal_conferences: beneficiary with secondary administrative role (institutional/constrained) — gained standing relative to Rome, implements the hermeneutic regionally
 *   - ecumenical_dialogue_partners: incidental beneficiary (institutional/mobile) — collects dialogue gains from outside the governed system
 *   - traditionalist_clergy: primary target (organized/constrained) — bears canonical subordination and marginal ministry access
 *   - preconciliar_liturgical_communities: target (moderate/trapped) — lost ordinary access to inherited worship; identity-bound to the old forms
 *   - contemplative_religious_orders: target (organized/constrained) — restructured under obedience, absorbed demographic collapse
 *   - dissenting_parish_laity: excluded voice (powerless/trapped) — objected without ever holding a seat
 *   - church_historians: analytical observer (analytical/analytical) — supplies draft histories and vote records all parties cite selectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.66).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Spirit-of-the-Council Authorization Regime (Rupture-Progressive Reading of Vatican II)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, '65f113fe-9184-4e3e-84ea-2c60c4a26114').
narrative_ontology:cs_kernel_codification('65f113fe-9184-4e3e-84ea-2c60c4a26114', fixed_text).
narrative_ontology:cs_authority_grounding('65f113fe-9184-4e3e-84ea-2c60c4a26114', lineage).
narrative_ontology:cs_interpretation_layer_present('65f113fe-9184-4e3e-84ea-2c60c4a26114').
narrative_ontology:cs_reading_relation('65f113fe-9184-4e3e-84ea-2c60c4a26114', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('65f113fe-9184-4e3e-84ea-2c60c4a26114', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_axiom('65f113fe-9184-4e3e-84ea-2c60c4a26114', foundational, spirit_authorizes_beyond_textual_limits).
narrative_ontology:cs_axiom_status(spirit_authorizes_beyond_textual_limits, holdable).
narrative_ontology:cs_axiom_grounding('65f113fe-9184-4e3e-84ea-2c60c4a26114', spirit_authorizes_beyond_textual_limits, theological).
narrative_ontology:cs_axiom('65f113fe-9184-4e3e-84ea-2c60c4a26114', secondary, preconciliar_formulations_legitimately_superseded).
narrative_ontology:cs_axiom_status(preconciliar_formulations_legitimately_superseded, holdable).
narrative_ontology:cs_axiom_grounding('65f113fe-9184-4e3e-84ea-2c60c4a26114', preconciliar_formulations_legitimately_superseded, instrumental).
narrative_ontology:cs_reference_frame('65f113fe-9184-4e3e-84ea-2c60c4a26114', council_as_constitutive_new_beginning).
narrative_ontology:cs_drift_state('65f113fe-9184-4e3e-84ea-2c60c4a26114', contemporary_traditionis_custodes_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('65f113fe-9184-4e3e-84ea-2c60c4a26114', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, postconciliar_theological_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, national_episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_dialogue_partners).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, preconciliar_liturgical_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, contemplative_religious_orders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to declare what the Council's spirit requires. Appoints bishops and curial officials who carry the received reading into dioceses, regulates contested questions by decree, and disciplines offices, orders, and teachers whose practice departs from the approved post-conciliar direction. Because the office stands above the texts it interprets, it can also reverse course by decree when the preferred reading shifts, as later liturgical restrictions demonstrated. Collects the regime's core product: the last word on what the Council means.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, papal_magisterium_and_curia, agenda_setter,
    institutional, generational, arbitrage, global).

% Professors, liturgists, periti-turned-advisors, journal editors, and seminary faculty whose careers, publications, and consultancies were built on interpreting and implementing the Council after 1965. Staff synods, draft documents, and train the next generation of clergy in the received reading. Leaving the arrangement would mean repudiating a lifetime of work and forfeiting institutional position, so their professional selves are fused with the reading's authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, postconciliar_theological_establishment, beneficiary,
    institutional, generational, identity_locked, global).

% Collegial bodies that gained standing relative to Rome through the conciliar settlement. Translate the Council into local liturgical translation policy, catechetical norms, and disciplinary practice; administer the hermeneutic regionally while drawing authority from it. Their acquired autonomy is bound up with the post-conciliar settlement remaining in force, so they defend it while also bearing enforcement burdens where local resistance runs high.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, national_episcopal_conferences, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, national_episcopal_conferences, agenda_setter).

% Other churches and ecclesial communities whose bilateral dialogues, joint declarations, and mutual recognitions depend on the Catholic partner maintaining the conciliar openness. They are not governed by the hermeneutic: they collect its benefits from outside, and can deepen or freeze relations without bearing any of its internal costs.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_dialogue_partners, beneficiary,
    institutional, generational, mobile, global).

% Priests and seminarians formed in, or drawn to, pre-conciliar formation, doctrine, and liturgy. After the transition they serve at the margins — dependent on provisional arrangements or irregular status — barred from ordinary parish ministry in many dioceses, and subject to canonical discipline when their attachment becomes open opposition. Full exit means schism; staying means accepting subordinate status indefinitely.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    organized, biographical, constrained, global).

% Lay communities attached to the older liturgical forms, devotional life, and catechesis. The transition removed their familiar worship from ordinary parishes within a few years; they now depend on scattered provisions, travel long distances, or affiliate with irregular groups. Their spiritual identity is bound to the inherited forms, so abandoning them is not a neutral option, and the provisions they rely on can be narrowed by decree.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, preconciliar_liturgical_communities, payer,
    moderate, biographical, trapped, global).

% Religious congregations whose constitutions, habits, and daily schedules were restructured during implementation. Many lost most of their members in the following decades. Superiors who resisted restructuring faced visitations and replacement; those who complied watched vocations decline regardless. The remaining options are managed decline or costly re-foundation under newer provisions.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, contemplative_religious_orders, payer,
    organized, generational, constrained, global).

% Ordinary parishioners who experienced the liturgical and catechetical transition as loss but held no seat at any decision point — not at the Council, not in the conferences, not on the liturgical committees. They adapted in silence, drifted from practice, or joined the margins. Their objection registers in the record only as attendance statistics and grumble folklore.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, dissenting_parish_laity, excluded,
    powerless, biographical, trapped, global).

% Academic historians and sociologists of religion who study the conciliar event, the draft histories, the recorded votes, and the implementation record from outside confessional advocacy. They produce the vote counts, draft comparisons, and attrition data that every party cites selectively, and they bear no costs from the arrangement either way.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, papal_magisterium_and_curia).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective problem: how a global twentieth-century church engages modernity — vernacular participatory liturgy, relations with other Christians, religious liberty in pluralist states, dispersed governance — without relitigating each question locally. The spirit-hermeneutic gives bishops, theologians, and pastors a shared direction of travel and a common vocabulary for implementing change, so the community does not fragment into four thousand local settlements.
% TRANSFER_FUNCTION: Moves interpretive authority — and with it institutional power, liturgical practice, and doctrinal certainty — from the fixed pre-conciliar textual corpus and its custodians to the living magisterium and the post-conciliar theological class; moves security, familiarity, and ordinary ministry access away from traditionalist clergy and laity toward the reform project and its carriers.
% ABSENT_VOICES: Traditionalist laity and clergy who experienced the transition as loss were nearly absent from the decision surface: only a small minority of council fathers resisted the final texts, no lay seat existed, and the provision machinery was designed after the fact. Radical restorationist voices sit wholly outside the conversation. Most fundamentally, the pre-conciliar magisterium itself cannot object except through texts the hermeneutic subordinates — the injured party in this dispute is largely dead and speaks only in documents the arrangement reinterprets.
% DISAPPEARANCE_RATIONALE: If the spirit-hermeneutic lost its authority overnight, the post-conciliar settlement would lose its authorization engine: liturgical law, ecumenical commitments, episcopal collegiality, and the standing of thousands of appointees all rest on the received reading of the Council. Either the continuity reading or the traditionalist reading would have to replace it, rearranging appointments, curial structures, liturgical regulation, and the status of pre-conciliar communities. The world does not stay the same because a large apparatus is organized around this reading.
% FOUNDING_PROBLEM: The arrangement was built to solve the pre-conciliar impasse: an antimodernist siege posture that had failed to stem attrition, a liturgy many found opaque, the Syllabus's condemnation of religious liberty colliding with thriving pluralist states, and centralized governance smothering local churches. The progressive reading holds that these problems were real, urgent, and unsolvable within the inherited framework.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: secular historians of modern Catholicism document the antimodernist siege, the Syllabus's failed encounter with liberal states, and mid-century attrition; non-Catholic ecumenical partners independently attest that the pre-conciliar posture was a barrier to relations; sociological studies of the period corroborate the disengagement the reform answered. No corroborating source outside the beneficiary set attests that the founding problem is resolved — historians broadly agree the engagement with modernity remains unsettled, which supports the live status.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.66) because the authorization principle is uncapped by the texts: whatever the letter settles, the spirit can reopen, and the ratchet runs forward only — restoration is treated as betrayal while extension is treated as fidelity. Suppression (0.62) reflects real enforcement machinery — canonical discipline, appointment control, liturgical regulation, marginalization of irregular communities — coexisting with broad voluntary assent among the governed majority; suppression is authored as a raw structural property and is not scaled by power or scope. Theater ratio (0.38) captures the growing share of commemorative, celebratory, and consensus-performance activity (anniversaries, jubilee rhetoric, unity language) relative to substantive renewal work, which was front-loaded in 1965-1975. Accessibility collapse is moderate (0.48): alternatives persist — irregular communities, indult provisions, adjacent jurisdictions — so understanding the regime does not eliminate exit, but every exit carries canonical or social cost. Resistance (0.61) is high and durable: traditionalist movements, liturgical agitation, jurisdictional defiance, and scholarly revisionism have contested the regime for six decades. The measurement series run on one shared time grid (eight points, all three metrics at every point) and trace a CYCLICAL pattern rather than monotonic drift: aggressive implementation (late 1960s-1970s), consolidation and fatigue (1980s-1990s), a retrieval-minded interlude in the early 2000s that relaxed enforcement, then renewed hardening against traditionalist attachment after 2013. Roughly one full cycle is visible. Part of the oscillation functions as intermittent reinforcement: détente phases give restoration-oriented seats credible hope of normalization, and subsequent restriction resets expectations — the cycle itself stabilizes compliance among the payers. Base properties are measured at the 2025 endpoint, on the rising-enforcement phase of the cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the arrangement is the church faithfully receiving its own council: the spirit-principle is how a living institution reads a living event, and enforcement is pastoral care for unity. From the payer seats the same structure operates as an unaccountable authorization that overrides settled teaching and inherited worship without consent or compensation, enforced by people who do not bear its costs. The beneficiary seats split further: the theological establishment is identity-locked into defending the reading (its exit would repudiate its life's work), while ecumenical partners enjoy its fruits without being governed by it at all. The engine computes per-seat classifications from the structural data; the divergence between those computations is the measurement this story exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real receipts: the theological establishment collects careers, consultancies, and drafting access; episcopal conferences collect regional authority ceded by the center; ecumenical partners collect dialogue and recognition gains while remaining outside the regime's jurisdiction. Victim declarations map to borne costs: traditionalist clergy bear canonical subordination and blocked ordinary ministry; liturgical communities bear the loss of inherited worship from ordinary parishes; restructured orders bear demographic collapse under obedience. Exit asymmetry drives the derived directionalities: the establishment's identity lock pushes it toward defense of the arrangement despite nominal beneficiary status, while the trapped and constrained exits of the payer seats amplify their effective exposure — a trapped target sits nearer the full-target end than a mobile one. The magisterium seat is nominally the administrator and collects the regime's core product (final interpretive authority), but it also spends authority maintaining the hermeneutic against six decades of resistance; its net position remains beneficiary-side. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the correct qualitative ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Authoring this as a hybrid coordination-plus-extraction structure prevents two symmetrical mislabels. The progressive seat experiences the arrangement as pure coordination — necessary renewal solving a real collective problem — and would resist any pure-extraction verdict; the traditionalist seat experiences it as pure extraction and denies any coordination function. The structural data records both truths: a genuine coordination function (a shared framework for engaging modernity that spared the community from relitigating every question) AND asymmetric costs borne by seats that never consented, held in place by active enforcement. The founding-problem interview shows status=live with a world_rearranges disappearance verdict — the mandate has not outlived its function by this reading's lights, so no zombie flag fires; the necessity counterfactual omega keeps the 'was the mandate ever necessary' question open rather than letting the reading's own conviction settle it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading of the vatican_ii_doctrinal_authority kernel. Would the continuity_reading or rupture_traditionalist_reading instantiate a different constraint over the same historical arrangements, with different beneficiary/victim structure and different epsilon?',
    'Compile the sibling stories and compare computed classifications over identical structural facts. The disagreement locates in the authorization question: whether post-conciliar change is explication contained in the letter (continuity), legitimate development exceeding the letter (this reading), or error enabled by ambiguous texts (traditionalist).',
    'Under the continuity reading the same arrangements compute with far lower epsilon (organic development, thin victim set); under the traditionalist reading they compute as pure extraction with an expanded victim set (the whole pre-conciliar inheritance). This story''s tangled_rope verdict is reading-indexed, not topic-absolute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest: which reading of Vatican II authority governs the same historical arrangements.').

omega_variable(
    spirit_principle_determinacy,
    'Is the ''spirit of the Council'' a determinate hermeneutical principle recoverable from the texts, draft histories, and recorded votes, or an unbounded authorization device that yields whatever the interpreting faction requires?',
    'Compare draft histories and recorded conciliar votes against post-conciliar implementation: where implementation contradicts explicit votes or deleted draft material, the spirit-principle operated beyond the letter. Systematic patterns across domains (liturgy, religious liberty, collegiality) would establish determinacy or its absence.',
    'If the principle is unbounded, the arrangement''s effective reach exceeds any textual cap and epsilon rises toward the pure-extraction boundary; if determinate, part of the measured cost is the price of a legitimate living-teacher function rather than open-ended authorization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_principle_determinacy, empirical, 'Determinacy of the spirit-over-letter authorization principle.').

omega_variable(
    necessity_of_break_counterfactual,
    'Was the break with pre-conciliar formulations necessary for the church''s engagement with modernity, as this reading asserts, or could continuity-compatible renewal have achieved comparable adaptation at lower cost?',
    'Comparative institutional history: Eastern Catholic churches retained inherited forms while adapting to modern conditions; the Anglican ordinariate demonstrates continuity-compatible renewal; counterfactual modeling of reform-without-rupture trajectories using pre-conciliar reform proposals that were already in circulation.',
    'If the break was unnecessary, the costs borne by traditionalist seats were gratuitous rather than the price of institutional survival, shifting the verdict toward pure extraction; if necessary, the costs are defensible and the hybrid coordination-plus-cost reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_break_counterfactual, conceptual, 'Counterfactual necessity of the conciliar rupture.').

omega_variable(
    enforcement_symmetry_question,
    'Does the regime''s enforcement run only against restoration (a one-way ratchet that forbids return while permitting further change), or also against heterodox excess in the progressive direction, as the post-conciliar treatment of liberation theology suggests?',
    'Code canonical interventions, disciplinary measures, and appointment patterns from 1965 to the present by target direction: restorationist versus progressive-heterodox. Measure the asymmetry ratio across the interval.',
    'Substantially bidirectional enforcement recasts the arrangement as discipline-bearing coordination rather than directional extraction, lowering effective exposure for mid-power seats and complicating the victim set; strongly asymmetric enforcement confirms the one-way ratchet and raises effective extraction for restoration-oriented seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_symmetry_question, empirical, 'Directionality of post-conciliar enforcement machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_rp_tr_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1962, 0.12).
narrative_ontology:measurement(vatican_ii_rp_tr_t1968, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(vatican_ii_rp_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.31).
narrative_ontology:measurement(vatican_ii_rp_tr_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1985, 0.34).
narrative_ontology:measurement(vatican_ii_rp_tr_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1995, 0.36).
narrative_ontology:measurement(vatican_ii_rp_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(vatican_ii_rp_tr_t2013, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2013, 0.34).
narrative_ontology:measurement(vatican_ii_rp_tr_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(vatican_ii_rp_be_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1962, 0.4).
narrative_ontology:measurement(vatican_ii_rp_be_t1968, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(vatican_ii_rp_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.7).
narrative_ontology:measurement(vatican_ii_rp_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.66).
narrative_ontology:measurement(vatican_ii_rp_be_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(vatican_ii_rp_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement(vatican_ii_rp_be_t2013, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2013, 0.63).
narrative_ontology:measurement(vatican_ii_rp_be_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2025, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_rp_su_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1962, 0.25).
narrative_ontology:measurement(vatican_ii_rp_su_t1968, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1968, 0.45).
narrative_ontology:measurement(vatican_ii_rp_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vatican_ii_rp_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.56).
narrative_ontology:measurement(vatican_ii_rp_su_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(vatican_ii_rp_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.49).
narrative_ontology:measurement(vatican_ii_rp_su_t2013, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement(vatican_ii_rp_su_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Vatican II's doctrinal authority' covers four structurally distinct claims and is decomposed into four stories sharing the kernel vatican_ii_doctrinal_authority. Each member has its own epsilon, beneficiary/victim structure, and classification; they are linked here rather than merged because measuring the arrangement through different readings yields different stable epsilon values — the signature of distinct constraints, not one observable-dependent one. The continuity reading functions as the upstream member (the official hermeneutic of the recent past shaped the institutional environment in which both rupture readings operate), exerting structural pressure on this reading without resolving the dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
