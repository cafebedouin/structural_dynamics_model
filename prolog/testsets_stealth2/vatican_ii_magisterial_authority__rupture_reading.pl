% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority — Rupture Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The rupture reading holds that the conciliar texts encode an ecclesiology
 *   incompatible with prior magisterial teaching: Dignitatis Humanae against
 *   the nineteenth-century religious-liberty condemnations, the liturgical
 *   constitution against the entrenched 1962 settlement, collegiality against
 *   the maximal papal claims of the preceding century. This story
 *   instantiates that reading as a constraint on the standing post-conciliar
 *   arrangement — the official interpretive and enforcement regime that
 *   claims continuity while, on this reading, administering a settlement the
 *   texts' own content contradicts. The epsilon referent is the standing
 *   arrangement (the continuity-claiming interpretive regime as it actually
 *   operates), assessed by this reading's lights: it genuinely coordinates
 *   the institution's reception of the council, and it simultaneously removes
 *   the texts' authorized content, disciplines those who name the rupture,
 *   and accrues the gains to the magisterial office's self-understanding.
 *   Claim and metrics are authored independently: the reading claims
 *   tangled_rope because it observes real coordination plus asymmetric
 *   extraction; the metrics describe what this reading observes of the
 *   arrangement's operation, not what the claim needs. This is one member of
 *   a three-reading constraint family over the same kernel; the siblings
 *   (continuity_reading, composite_overdetermination_reading) are separate
 *   files with their own epsilon values, linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - roman_magisterium: primary beneficiary and agenda-setter (institutional / identity_locked) — promulgates the official interpretation, runs the discipline, and collects the arrangement's central good: an unbroken authority narrative
 *   - doctrinal_dicasteries: enforcement beneficiary (institutional / constrained) — curial offices whose remit and staffing depend on the ongoing need to adjudicate conciliar interpretation
 *   - progressive_theologians: primary target (moderate / constrained) — reads the texts as authorizing a new ecclesiology and bears investigation and career risk for saying so
 *   - traditionalist_catholics: co-target (organized / identity_locked) — attached to the pre-conciliar corpus the settlement displaced; bears liturgical restriction and canonical irregularity
 *   - diocesan_clergy: implementation layer, dual-positioned (moderate / constrained) — receives a workable unified settlement and absorbs the friction between text, interpretation, and parish
 *   - catholic_laity: beneficiary with diffuse costs (moderate / identity_locked) — receives the council's fruits and carries the incoherence of an unresolved hermeneutic
 *   - ecumenical_partners: external beneficiary (institutional / mobile) — collects the council's openings while sitting outside the enforcement perimeter that maintains them
 *   - lapsed_catholic_intellectuals: excluded voice (moderate / mobile) — left the Church over precisely this question and holds no seat in the conversation that adjudicates it
 *   - ecclesial_historians: analytical observer (analytical / analytical) — holds the documentary record any compatibility claim must answer to, owing allegiance to no seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority — Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, 'c566dab7-de3b-4fb7-a95a-9755ed7893b1').
narrative_ontology:cs_kernel_codification('c566dab7-de3b-4fb7-a95a-9755ed7893b1', fixed_text).
narrative_ontology:cs_authority_grounding('c566dab7-de3b-4fb7-a95a-9755ed7893b1', lineage).
narrative_ontology:cs_interpretation_layer_present('c566dab7-de3b-4fb7-a95a-9755ed7893b1').
narrative_ontology:cs_reading_relation('c566dab7-de3b-4fb7-a95a-9755ed7893b1', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('c566dab7-de3b-4fb7-a95a-9755ed7893b1', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('c566dab7-de3b-4fb7-a95a-9755ed7893b1', foundational, conciliar_texts_supersede_incompatible_prior_teaching).
narrative_ontology:cs_axiom_status(conciliar_texts_supersede_incompatible_prior_teaching, holdable).
narrative_ontology:cs_axiom_grounding('c566dab7-de3b-4fb7-a95a-9755ed7893b1', conciliar_texts_supersede_incompatible_prior_teaching, conventional).
narrative_ontology:cs_axiom('c566dab7-de3b-4fb7-a95a-9755ed7893b1', foundational, doctrinal_reversal_admissible_as_progress).
narrative_ontology:cs_axiom_status(doctrinal_reversal_admissible_as_progress, holdable).
narrative_ontology:cs_axiom_grounding('c566dab7-de3b-4fb7-a95a-9755ed7893b1', doctrinal_reversal_admissible_as_progress, deontological).
narrative_ontology:cs_axiom('c566dab7-de3b-4fb7-a95a-9755ed7893b1', secondary, liturgical_experimentation_authorized_by_council).
narrative_ontology:cs_axiom_status(liturgical_experimentation_authorized_by_council, holdable).
narrative_ontology:cs_axiom_grounding('c566dab7-de3b-4fb7-a95a-9755ed7893b1', liturgical_experimentation_authorized_by_council, instrumental).
narrative_ontology:cs_reference_frame('c566dab7-de3b-4fb7-a95a-9755ed7893b1', conciliar_rupture_authorization).
narrative_ontology:cs_drift_state('c566dab7-de3b-4fb7-a95a-9755ed7893b1', official_continuity_hermeneutic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c566dab7-de3b-4fb7-a95a-9755ed7893b1', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, roman_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, doctrinal_dicasteries).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, catholic_laity).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, ecumenical_partners).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_catholics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, diocesan_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, diocesan_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, catholic_laity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, doctrinal_discontinuity_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, historical_consciousness_development_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The pope, curia, and councils that author official interpretations of the conciliar documents and disciplines deviation from them. The office's claim to have preserved doctrine intact across the council is bound up with the continuity interpretation it promulgates; abandoning that claim would put its own authority narrative at risk. It cannot step outside the arrangement without dissolving the office's own warrant, so it administers the settlement it depends on.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, roman_magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, roman_magisterium, beneficiary).

% The curial offices (historically the Holy Office and Congregation for the Doctrine of the Faith, now the Dicastery for the Doctrine of the Faith) that run doctrinal investigations, issue censures, and clear theological publications for imprimatur. Their mandate, procedures, and staffing depend on the continuing need to adjudicate conciliar interpretation; a settled interpretation would shrink their remit, so the unresolved question is their institutional livelihood.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, doctrinal_dicasteries, beneficiary,
    institutional, generational, constrained, global).

% Academic and seminary theologians who read the conciliar texts as authorizing a new ecclesiology and publish accordingly. They draw their charter and standing from the council's own authority, and they bear investigations, publication denials, and career risk when they draw the rupture's implications explicitly. Leaving the Catholic academic world means losing the subject matter, the community, and the institutional platform all at once.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians, beneficiary).

% Communities and faithful attached to the pre-conciliar liturgy and doctrinal corpus, organized in fraternities and institutes, some canonically irregular. The settlement restricts their forms of worship and marks their doctrinal position as irregular. Their identity is constituted by the tradition the settlement displaced; leaving means ceasing to be who they are, so they persist inside the structure they reject.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_catholics, payer,
    organized, generational, identity_locked, global).

% Priests who implement the liturgical and pastoral settlement parish by parish. They receive a workable, unified liturgical life and clear norms, and they absorb the daily friction between what the texts say, what the official interpretation permits, and what their people ask for. Exit means abandoning ordination, community, and livelihood together.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, diocesan_clergy, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, diocesan_clergy, beneficiary).

% The baptized who receive the council's fruits — vernacular worship, ecumenical openness, a less clerical self-understanding — and who also carry the cost of unresolved hermeneutical conflict: shifting norms, contested catechesis, and an official story many cannot render coherent. Their Catholic identity is not portable; exit is experienced as self-loss rather than relocation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, catholic_laity, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, catholic_laity, payer).

% Other churches and ecclesial communities that gained recognition, dialogue structures, and theological legitimacy from the council's decrees on ecumenism and religious liberty. They sit outside the enforcement perimeter: the arrangement's discipline does not reach them, while its openings materially serve their standing. They can deepen or freeze engagement at will.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ecumenical_partners, beneficiary,
    institutional, generational, mobile, global).

% Formerly Catholic scholars and writers who left the Church over precisely the question this settlement governs — some because the rupture they saw was never acknowledged, others because the continuity they were taught failed to hold under reading. They have standing to describe what the conflict cost but no seat in the magisterial conversation that adjudicates it.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, lapsed_catholic_intellectuals, excluded,
    moderate, biographical, mobile, global).

% Academic historians of doctrine, Catholic and secular, who reconstruct what the pre-conciliar magisterium taught and what the conciliar texts changed. They hold the documentary record that any party's compatibility claim must answer to, and they owe allegiance to none of the seats in the dispute.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ecclesial_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, roman_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpretation of the conciliar documents for a worldwide institution, so that doctrine, liturgy, and discipline receive the council coherently instead of fragmenting into national, factional, or private readings.
% TRANSFER_FUNCTION: Moves interpretive authority from the conciliar texts as written to the magisterial office as interpreter; moves theological legitimacy toward continuity-affirming positions and away from rupture-naming ones; moves disciplinary power over both wings to the Roman center.
% ABSENT_VOICES: Canonically irregular traditionalists speak only from outside communion; theologians under investigation publish at career risk; the laity have no formal hermeneutical voice; and the lapsed Catholic intellectuals who left over this exact question are outside the conversation entirely — the people with the sharpest testimony about the settlement's costs have the least standing to give it.
% DISAPPEARANCE_RATIONALE: If the official interpretive regime vanished overnight, the compatibility question would have to be adjudicated from zero: the hermeneutic wars would become a constitutional crisis, liturgical pluralism or outright fragmentation would follow within years, and every party's position — from the irregular fraternities to the theological faculties — would have to be renegotiated against the raw texts with no standing arbiter.
% FOUNDING_PROBLEM: How a billion-member institution receives a council whose texts (on this reading) contradict prior teaching without splitting: the continuity hermeneutic was built as the bridge that lets the institution affirm the council whole while denying that anything it previously taught was overturned.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested from outside the beneficiary set: the continuing existence and institutional persistence of canonically irregular traditionalist bodies demonstrates that reception without schism remains unsolved; ecumenical partners formally note unresolved ecclesiological questions in bilateral dialogue documents; ecclesial historians across confessional lines document the compatibility dispute as open. No serious party inside or outside the arrangement claims reception is complete.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.68: on this reading the arrangement takes the council's authority and uses it to neutralize the council's content — the texts authorize implementation the regime regulates into inertness, and the continuity claim itself is the instrument of removal. Suppression 0.71: enforcement is real and bidirectional — publication controls and investigations for theologians who draw the rupture's implications, canonical irregularity and liturgical restriction for those who reject the council outright — and the machinery visibly matured across the interval. Theater 0.52: a substantial share of official interpretive activity is the performance of continuity (hermeneutic catecheses, anniversary magisterium) that this reading holds the texts contradict; the remainder — governance, liturgical regulation, formation — is functional. Accessibility collapse 0.58: the alternatives persist (the texts remain readable; both wings' hermeneutics circulate) but each carries escalating institutional cost, and only the official hermeneutic has standing. Resistance 0.66: the hermeneutic wars are the arrangement's most visible feature — traditionalist institutionalization, theological dissent, episodic synodal re-litigation. Suppression is authored as the raw structural property it is; only extractiveness is scaled downstream by directionality and scope. The measurement series share one time grid (t=0 to t=60, decade steps, 1965-2025): base extractiveness and the suppression requirement rise together as enforcement capacity matured, and theater climbs as the continuity claim's textual support thinned, plateauing as the explicit hermeneutic program receded after 2013 while the performative continuity persisted.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently from the same structural data. From the magisterial office, the arrangement is what saved the institution from the council's centrifugal potential — a coordination it built and must maintain, whose costs are the price of unity. From the progressive theologian seat, the same arrangement is a machine for saying what the texts say and punishing those who repeat it. From the traditionalist seat, it is the suppression of what the Church was. The engine computes these per-seat divergences; this file authors only the rupture reading's assessment of the standing arrangement, and the sibling files author theirs over the identical referent.
 *
 * DIRECTIONALITY LOGIC:
 *   The roman_magisterium and doctrinal_dicasteries sit near the beneficiary end: they collect interpretive authority, disciplinary remit, and the institutional self-preservation the continuity claim purchases. progressive_theologians and traditionalist_catholics sit near the target end: both bear the enforcement, with opposite complaints — one for accepting too little of the rupture, one for accepting too much of the settlement. diocesan_clergy and catholic_laity sit near symmetric: they receive the settlement's workable unity and pay its incoherence, with identity-lock keeping their exit costs high without making them net payers. ecumenical_partners are beneficiaries with arbitrage-grade exit — they collect the openings from outside the enforcement perimeter that maintains them, the lowest-directionality seat in the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Calling the arrangement pure extraction would erase its genuine coordination function: reception of a contested council without schism is a real problem the regime partially solves, and the council's fruits — vernacular worship, ecumenical recognition, the religious-liberty settlement itself — are real goods the arrangement protects and delivers. Calling it pure coordination would erase the asymmetry: the gains accrue to the magisterial office's self-understanding while the costs land on the two seats that take the texts most seriously, in opposite directions. The founding problem (reception without fragmentation) remains live, so this is not a mandatrophy case; but the theater trajectory is the drift signal to watch. If the compatibility question were ever settled — by scholarship, or by a future council — while the continuity performance persisted out of institutional habit, the arrangement would decay toward administered performance: an office that could change the settlement at a fix-cost it does not itself bear, maintained for an audience that has stopped asking the question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates one reading (rupture) of the kernel vatican_ii_magisterial_authority; the continuity reading and the composite_overdetermination reading instantiate different constraints over the same texts — what structural element do the readings actually disagree about, and what would adopting a sibling change?',
    'Cross-reading comparison at the family level: the readings disagree on exactly one structural element — whether the conciliar texts are compatible with prior magisterial teaching. Resolution proceeds per-reading, never by merging: each reading''s epsilon is assessed against the same standing arrangement from its own lights, and the corpus compares the three files.',
    'Merging the readings into one constraint would average incommensurable assessments into a meaningless epsilon; keeping them separate makes the corpus measure the hermeneutic dispute itself. A sibling''s adoption would relocate the extraction entirely: the continuity reading finds negligible extraction, the composite reading finds the mechanism in the ambiguity rather than in the continuity claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one kernel, three readings; disagreement located in text-compatibility with prior magisterium.').

omega_variable(
    dh_compatibility_empirics,
    'Is the religious-liberty teaching of Dignitatis Humanae textually incompatible with the nineteenth-century magisterium (Mirari Vos, Quanta Cura, the Syllabus of Errors), as this reading holds, or reconcilable under categories of doctrinal development?',
    'Systematic documentary comparison by historians of doctrine across confessional lines. The texts are fully available and untranslated in the relevant sense; the dispute is over their relation, not their content, so the question is resolvable by scholarship rather than by authority.',
    'Demonstrated incompatibility confirms the official continuity claim as the arrangement''s load-bearing falsehood and sustains this reading''s high epsilon; demonstrated reconcilability dissolves the rupture reading''s foundation and collapses epsilon toward the continuity reading''s assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dh_compatibility_empirics, empirical, 'Whether the DH discontinuity claim is textually demonstrable or reconcilable.').

omega_variable(
    implementation_counterfactual,
    'Would the radical implementation this reading says the conciliar texts authorize have produced the promised renewal, or the fragmentation the arrangement''s restraint prevented?',
    'Comparative ecclesial history: track communities, jurisdictions, and movements that implemented conciliar implications radically against those that resisted or regulated them, over multiple decades, controlling for prior institutional health.',
    'If radical implementation historically produced fragmentation, the standing arrangement''s restraint is protective coordination and its measured suppression is overstated; if it produced renewal, the suppression operated as pure removal of the texts'' content from circulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_counterfactual, empirical, 'Counterfactual on whether full rupture implementation would have renewed or split the institution.').

omega_variable(
    enforcement_symmetry,
    'The regime disciplines both wings — progressive theologians who name the rupture and traditionalists who reject the council. Is the enforcement''s function symmetric institutional self-preservation, or does one wing''s suppression purchase the other''s accommodation?',
    'Compare enforcement incidence and severity across the two wings over the interval: which positions drew censures, investigations, or liturgical restriction, which drew quiet accommodation, and what each act of enforcement purchased for the center.',
    'Asymmetric enforcement would sharpen the extraction assessment — the arrangement trading one wing''s liberty for the other''s compliance — while symmetric enforcement would support a self-preservation reading in which both wings pay for the office''s stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_symmetry, empirical, 'Whether bidirectional enforcement is symmetric self-preservation or a cross-subsidy between wings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 50, 0.51).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 50, 0.69).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (vatican_ii_magisterial_authority), three readings, each a separate constraint with its own epsilon over the same standing arrangement — the post-conciliar interpretive and enforcement regime. The continuity reading authors low epsilon (organic development, negligible removal from the texts). This rupture reading authors high epsilon (the continuity claim as the load-bearing falsehood suppressing authorized implementation). The composite_overdetermination reading authors a different structure (ambiguity itself as the coordination/extraction medium, no single ecclesiology to be faithful or unfaithful to). The upstream/downstream pressure runs from the documentary record outward: demonstrated compatibility or incompatibility at the DH question reshapes the operating environment of both sibling readings without logically resolving the composite reading's account. The readings are linked, never merged: each file's epsilon is reading-indexed over the fixed referent, and the corpus measures the hermeneutic dispute by comparing the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
