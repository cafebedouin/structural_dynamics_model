% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Hermeneutic of Continuity: the Conciliar Interpretation Regime
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   The constraint under classification is the operative interpretive regime
 *   by which the Church's teaching offices read the Second Vatican Council:
 *   all sixteen documents valid, post-conciliar reforms legitimate when
 *   faithful to the texts, ambiguities resolvable through traditional
 *   hermeneutics. It solves a real coordination problem (a global institution
 *   absorbing sweeping change while claiming doctrinal permanence) and it
 *   imposes real, asymmetric costs (liturgical dispossession, canonical
 *   penalty, and career sanction falling on those who read the council
 *   otherwise, plus compliance burdens on implementers). The manifest's
 *   expected delta described the reading's self-presentation ('victim: none;
 *   reforms are cost-free development'); the authored structural data record
 *   who actually bears costs under this arrangement, and that divergence
 *   between self-description and cost incidence is part of what this story
 *   measures. KEY AGENTS (by structural relationship): -
 *   curial_doctrinal_administrators: agenda setter (institutional /
 *   identity_locked), adjudicates conciliar meaning and certifies continuity;
 *   the certification role is their standing -
 *   progressive_reformers_claiming_continuity: primary beneficiary (organized
 *   / constrained), legitimation for the implemented reform program -
 *   traditionalist_catholics: primary target (organized / identity_locked),
 *   bears canonical and liturgical costs - censured_conciliar_theologians:
 *   secondary target (moderate / constrained), career sanction for
 *   unauthorized readings from both flanks - mainstream_ordained_ministry:
 *   beneficiary-compliance hybrid (organized / constrained) -
 *   lay_faithful_majority: diffuse beneficiary-payer (powerless /
 *   constrained) - ecumenical_partners: excluded voice (organized / mobile) -
 *   church_historians: analytical observer (analytical / analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.36).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.48).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Hermeneutic of Continuity: the Conciliar Interpretation Regime").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '55c4084a-b427-4392-90c8-1c2f9c13c1d2').
narrative_ontology:cs_kernel_codification('55c4084a-b427-4392-90c8-1c2f9c13c1d2', fixed_text).
narrative_ontology:cs_authority_grounding('55c4084a-b427-4392-90c8-1c2f9c13c1d2', lineage).
narrative_ontology:cs_interpretation_layer_present('55c4084a-b427-4392-90c8-1c2f9c13c1d2').
narrative_ontology:cs_reading_relation('55c4084a-b427-4392-90c8-1c2f9c13c1d2', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('55c4084a-b427-4392-90c8-1c2f9c13c1d2', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('55c4084a-b427-4392-90c8-1c2f9c13c1d2', foundational, unchanging_deposit_guarantees_continuity).
narrative_ontology:cs_axiom_status(unchanging_deposit_guarantees_continuity, holdable).
narrative_ontology:cs_axiom_grounding('55c4084a-b427-4392-90c8-1c2f9c13c1d2', unchanging_deposit_guarantees_continuity, theological).
narrative_ontology:cs_axiom('55c4084a-b427-4392-90c8-1c2f9c13c1d2', secondary, traditional_hermeneutics_resolves_ambiguities).
narrative_ontology:cs_axiom_status(traditional_hermeneutics_resolves_ambiguities, holdable).
narrative_ontology:cs_axiom_grounding('55c4084a-b427-4392-90c8-1c2f9c13c1d2', traditional_hermeneutics_resolves_ambiguities, instrumental).
narrative_ontology:cs_reference_frame('55c4084a-b427-4392-90c8-1c2f9c13c1d2', organic_development_of_unchanging_deposit).
narrative_ontology:cs_drift_state('55c4084a-b427-4392-90c8-1c2f9c13c1d2', contemporary_hermeneutic_dispute_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('55c4084a-b427-4392-90c8-1c2f9c13c1d2', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, curial_doctrinal_administrators).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, mainstream_ordained_ministry).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_catholics).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, censured_conciliar_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, lay_faithful_majority).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, mainstream_ordained_ministry).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, lay_faithful_majority).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, organic_development_of_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, hermeneutic_of_reform_in_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff the dicasteries that decide what the sixteen conciliar documents mean, authorize translations and catechetical formulations, and process cases of teachers, orders, and communities judged to deviate. Their offices exist to certify that post-conciliar practice expresses the same faith the pre-conciliar magisterium professed, and that certification role is the source of their standing. Leaving the apparatus would mean abandoning ordained vocation and institutional identity at once.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, curial_doctrinal_administrators, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, curial_doctrinal_administrators, beneficiary).

% Liturgists, catechetical experts, religious superiors, and bishops who designed and carried out the post-conciliar changes. The continuity reading is what makes their work legitimate reform rather than novelty; without it their lifework stands exposed as breakage. When their implementations outrun the texts they have drawn scrutiny and sanction from the same offices that empower them, so the frame that lifts them also bounds them.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity, payer).

% Communities centered on the pre-conciliar liturgy and unpersuaded by the council's novelties, spanning irregular institutes and societies in open canonical conflict. They have lost access to their liturgical inheritance by restriction, endured excommunication and irregular status, and hear their own theological reading officially disqualified. Departure into Protestantism or sedevacantism abandons what they regard as the true Church; remaining means living under a frame that names their conviction as disobedience.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_catholics, payer,
    organized, generational, identity_locked, global).

% Academic theologians whose published readings of the council, from both the traditionalist and the progressive directions, drew notifications, precepts, silencing, removal from chairs, or expulsion from their orders. Their careers ride on credentials the censoring offices control; moving to secular institutions preserves income but ends the ecclesial audience their scholarship exists to address.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, censured_conciliar_theologians, payer,
    moderate, biographical, constrained, continental).

% Diocesan priests and bishops administering the reformed liturgy and catechesis. They receive a stable ministerial role, a functioning sacramental economy, and a legitimated teaching mandate; they pay in compliance labor, implementing each successive change, passing discipline down to those below them, and absorbing the parochial fallout of every new restriction wave. Ordination binds exit: leaving forfeits vocation, community, and livelihood together.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, mainstream_ordained_ministry, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, mainstream_ordained_ministry, payer).

% Catholic laity at large. They receive uninterrupted sacramental life and a single authoritative teaching voice, and they absorb the cumulative costs of disruption, changed worship, thinning catechesis, and each round of disciplinary scandal, with no seat in deciding what the documents mean. Quiet exit through lapse is common but costs family and cultural belonging.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, lay_faithful_majority, beneficiary,
    powerless, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, lay_faithful_majority, payer).

% Orthodox and Protestant interlocutors assessing the continuity claim from outside. They take part in the dialogues the council opened but hold no seat in deciding what the documents mean; several would testify that the continuity assertion papers over departures their own traditions experienced as real. Their objections enter the conversation only as consultative guests.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, ecumenical_partners, excluded,
    organized, generational, mobile, global).

% University-based scholars of the council who document its drafting history, the schemas that were discarded, and the recorded reversals of prior positions. They watch the whole structure without standing inside it; their findings circulate in academic channels and reach official catechetical materials only as filtered through the adjudicating offices.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, church_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, curial_doctrinal_administrators).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains doctrinal coherence and sacramental unity across a global institution absorbing sweeping liturgical and disciplinary change: the sixteen conciliar documents are heterogeneous in genre, draft history, and tone, and the continuity reading gives them one authoritative meaning, allowing implementation to proceed without every revision becoming a schism event.
% TRANSFER_FUNCTION: Moves interpretive authority from the conciliar texts themselves to the offices that adjudicate their meaning, and distributes legitimation to compliant implementers while the costs of adjustment, liturgical dispossession, canonical penalty, career sanction, fall on the dissenting minorities whose readings the frame disqualifies.
% ABSENT_VOICES: Rupture-reading adherents inside the Church are rarely seated when conciliar meaning is settled; Orthodox and Protestant partners assess continuity differently but attend only as consultative guests; and the historians whose documentation shows the discarded schemas and reversals speak in channels the adjudicating offices filter. All three would object to specific settlements and are structurally outside the room.
% DISAPPEARANCE_RATIONALE: If the continuity frame vanished overnight, sixty years of implemented reform would lose its legitimacy warrant at a stroke: the liturgical settlement, the ecumenical openings, and the collegial restructuring would each become open questions again, schismatic partitions already in embryo would formalize, and the teaching office would lose the authority chain that lets the same magisterium speak before and after the council.
% FOUNDING_PROBLEM: How an institution committed to teaching without error absorbs a council that reversed disciplinary and theological positions, liturgical language, religious liberty, ecumenical engagement, collegiality, without either ignoring the council or conceding that it broke with its own past.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting set by university historians of the council, whose drafting-history studies document the discarded schemas and reversals the frame must absorb, and by Orthodox and Protestant ecumenical assessments; the persistence of the rupture-reading constituency itself is negative attestation that the reception problem was never closed. The adjudicating offices' own accounts are not treated as corroboration, and no fully disinterested internal attestation exists.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.36 at interval end) because the frame's benefit flow, sacramental unity, legitimated ministry, coherent teaching, reaches the broad body while concentrated costs fall on dissenting minorities and compliance labor falls on implementers; the value is authored from the continuity reading's own lights, which concede the costs while judging them proportionate governance. Suppression is a raw structural property and is not scaled by power or scope: the canonical machinery (notifications, precepts, excommunications, liturgical restriction) is real and active but operates through juridical channels rather than raw force, hence 0.48 rather than higher. Accessibility collapse is moderate (0.42): rupture and overdetermined readings persist in enclaves, academia, and separated churches; the frame closes official channels, not the alternatives themselves. Resistance is high (0.62): a durable traditionalist movement, recurring theological dissent from both flanks, and episcopal-level contestation. Theater is low-moderate (0.28): the interpretive work is largely functional, with a rising ceremonial component of anniversary affirmations and 'no rupture' invocations that outpace the residual adjudicative load. The temporal series runs on one shared grid (t = 0, 10, 20, 30, 40, 50, 60 years from the council's close) with all three metrics authored at every point. The trajectories oscillate rather than drift monotonically, driven by alternating enforcement and detente administrations (suspensions in the 1970s, the 1988 excommunications, regularization and the 2007 liturgical liberalization, renewed restriction around 2021); part of this cycle is external (administrative style), but the cycle also functions as intermittent reinforcement, communities time compliance and dissent to the enforcement wave, so the oscillation is partly mechanism, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is structural, not rhetorical. From the agenda-setter seat the frame is self-evident: the office's authority across the council depends on the continuity claim, so admitting rupture would dissolve the office's own recent past, an identity fusion that makes the frame appear from that seat as mere fidelity. From the payer seats the same frame operates as enforced closure: a reading they hold in good faith is officially disqualified, with penalty attached. From the implementer seats it is both wage and burden: legitimation received, compliance paid. From the historian's analytical seat, documented discontinuities are simply data the frame must absorb interpretively. The engine derives these per-seat differences from the authored power, exit, and role data; the authored claimed_type does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The curial administrators are dual-positioned (agenda_setter + beneficiary) and derive the lowest directionality: they administer the frame, collect its authority concentration, and cannot leave it without abandoning vocation and identity together. Progressive reformers collect legitimation and sit near the beneficiary end, with a slight pull toward target from the sanction exposure recorded in their secondary role. Mainstream ministers and the lay majority sit nearer symmetric: broad benefit, real compliance and disruption costs, no adjudicative seat. Traditionalist Catholics sit near the full-target end, amplified by identity lock: their Catholic identity is constituted by belonging to the Church the frame governs, so exit is not available as relief and the extraction lands at full weight. Censured theologians are high-directionality targets with constrained rather than locked exit: they can flee to secular institutions at the price of their audience. Excluded and observer seats sit outside the extraction arithmetic proper.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, legitimating change inside an infallibility claim, is still live: every subsequent controversy (liturgical restrictions, doctrinal dubia, synodal conflicts) reactivates the frame's adjudicative function, so mandatrophy is not resolved and the arrangement is not coasting on inertia. The classification discipline matters in both directions here. Without the victim declarations, the reading's own self-description ('reforms are cost-free development') would pass the arrangement off as pure coordination, erasing the payer seats entirely; without the coordination-function declaration, the payer costs alone would read as pure predation, erasing the genuine unity problem the frame solves. Authoring both halves keeps the hybrid visible. The forward watch item is theater drift: if reception ever completes and the frame persists as ceremonial affirmation (theater_ratio climbing past roughly 0.5 while adjudicative load decays), the arrangement drifts toward inertial maintenance and warrants reclassification review; the reception_completion_status omega tracks exactly that question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_epsilon_index,
    'This story instantiates one reading (continuity_reading) of the contested kernel vatican_ii_authority. How do the sibling readings re-author the same referent, and where exactly does their disagreement sit?',
    'Author the sibling files (rupture_reading, composite_overdetermination_reading) over the same referent, the standing conciliar-interpretation arrangement, and compare the authored epsilon values. The disagreement is located in whether the sixteen documents admit a univocal reading that resolves into continuity with prior teaching.',
    'Rupture_reading would author substantially higher epsilon over the same referent (coercion toward alleged doctrinal error); composite_overdetermination_reading would author epsilon as exploitation of structural ambiguity. Per-reading classifications are the measurement; averaging across readings would destroy the signal the kernel decomposition exists to take.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_epsilon_index, conceptual, 'Committer structure: this file is one reading of a three-reading kernel; epsilon is a property of the reading, not the topic.').

omega_variable(
    organic_naturalness_ambiguity,
    'Is doctrinal development genuinely organism-like, a quasi-natural process the frame merely tracks, or is the ''organic development'' rhetoric a constructed interpretive regime whose naturalizing language conceals identifiable beneficiaries?',
    'Comparative development histories (Orthodox and Anglican patterns of doctrinal change) and counterfactual analysis: whether comparable change occurs in communions lacking a continuity-certifying apparatus, and who staffs and benefits from the certifying office here.',
    'Genuine emergence would push the arrangement toward rope-like or even naturalized classification; constructed-with-beneficiaries confirms the extraction half and hardens the tangled_rope reading against rope drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organic_naturalness_ambiguity, conceptual, 'Whether the frame''s organic-development self-presentation tracks a real emergent process or naturalizes an administered regime.').

omega_variable(
    cost_incidence_structural_vs_administrative,
    'Are the costs borne by traditionalist communities and censured theologians intrinsic to the continuity frame itself, or artifacts of particular administrations'' enforcement style, such that a maximally charitable continuity magisterium would impose near-zero costs?',
    'Compare enforcement intensity across successive administrations holding identical doctrine; if cost incidence tracks personnel and style rather than doctrine, the costs are episodic rather than structural.',
    'Structural incidence confirms durable payer seats and stabilizes the tangled_rope classification; episodic incidence suggests rope-drift with removable enforcement overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_incidence_structural_vs_administrative, empirical, 'Whether the payer seats are permanent features of the frame or byproducts of particular pontificates.').

omega_variable(
    reception_completion_status,
    'Has the council''s reception problem been substantively solved, leaving the frame ceremonial, or does each new controversy reactivate its adjudicative function?',
    'Track whether post-2020 disputes (liturgical restrictions, doctrinal dubia, synodal conflicts) invoke the continuity hermeneutic to do adjudicative work or merely ceremonial affirmation; count adjudicative outputs versus commemorative outputs per interval.',
    'Live adjudicative function sustains the current classification; completed reception with a persisted apparatus signals inertial drift and mandates reclassification review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reception_completion_status, empirical, 'Lifecycle probe: whether the frame is still solving its founding problem or performing its solution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__continuity_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__continuity_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__continuity_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__continuity_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__continuity_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__continuity_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__continuity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__continuity_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__continuity_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__continuity_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__continuity_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__continuity_reading, base_extractiveness, 50, 0.39).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__continuity_reading, base_extractiveness, 60, 0.36).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__continuity_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__continuity_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__continuity_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__continuity_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__continuity_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__continuity_reading, suppression_requirement, 50, 0.47).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__continuity_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the authority of Vatican II' covers three structurally distinct claims that cannot share one epsilon. This file instantiates the continuity reading (moderate epsilon; costs fall on dissenting minorities; coordination function intact). The rupture reading is a separate constraint with high epsilon over the same referent (coercion toward alleged doctrinal error, victims include all who receive the disputed teaching). The composite-overdetermination reading is a third constraint whose epsilon concerns exploitation of structural ambiguity itself. Each member links the others via network edges; the readings are competing constraints, not angles on one constraint, and cross-file comparison measures the contest rather than resolving it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
