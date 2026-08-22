% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath as Bounded Reciprocal Coordination (Vassal Reading)
 *   domain: medieval_political_economy/legal_history
 *
 * SUMMARY:
 *   This story instantiates the vassal_coordination_reading of the
 *   feudal_oath_reciprocity kernel: the claim that the oath, backed by
 *   written or customary charter terms and adjudicated by peer courts,
 *   establishes fixed and mutually enforceable obligations rather than
 *   open-ended lord extraction. Under this reading the charter functions as a
 *   genuine coordination device — it lets a lord and a vassal commit credibly
 *   to a long-horizon relationship of protection-for-service without either
 *   side needing to trust the other's unilateral word, because the terms are
 *   enumerated and a peer body can adjudicate breach on either side. This is
 *   emphatically NOT a claim about the whole feudal order (which includes an
 *   unfree peasantry with no charter and no reciprocal bound) — it is a claim
 *   about the specific lord-vassal tie the oath and charter jointly govern.
 *   Two sibling constraints read the same kernel differently:
 *   lord_extraction_reading holds that the charter's bounds were nominal and
 *   extraction tracked the vassal's service capacity rather than the text;
 *   ecclesiastical_mediation_reading holds that sacramental oath obligations,
 *   not the charter's secular text, did the actual limiting work. Each
 *   sibling is authored as its own constraint with its own epsilon; this
 *   story does not average across them.
 *
 * KEY AGENTS:
 *   - enfeoffed_vassals: primary reciprocal party — bears defined service, gains bounded protection
 *   - liege_lords: primary reciprocal party — gains defined service, bears bounded obligation not to exceed the charter
 *   - peer_vassal_court: horizontal enforcement mechanism — adjudicates breach on either side
 *   - unfree_peasantry: excluded from the charter relationship entirely; the reading's low-epsilon claim does not extend to them
 *   - royal_or_ducal_overlord: analytical/oversight seat above individual charters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.22).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.28).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath as Bounded Reciprocal Coordination (Vassal Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '33b345c6-4a5e-4c26-93c7-45a2fe9c40b9').
narrative_ontology:cs_kernel_codification('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', fixed_text).
narrative_ontology:cs_authority_grounding('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', practice).
narrative_ontology:cs_interpretation_layer_present('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9').
narrative_ontology:cs_reading_relation('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', foundational, charter_text_is_operative_bound).
narrative_ontology:cs_axiom_status(charter_text_is_operative_bound, holdable).
narrative_ontology:cs_axiom_grounding('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', charter_text_is_operative_bound, conventional).
narrative_ontology:cs_axiom('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', foundational, peer_adjudication_constitutes_real_enforcement).
narrative_ontology:cs_axiom_status(peer_adjudication_constitutes_real_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', peer_adjudication_constitutes_real_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', charter_bounded_mutual_obligation).
narrative_ontology:cs_drift_state('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', late_medieval_bureaucratic_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33b345c6-4a5e-4c26-93c7-45a2fe9c40b9', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, liege_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, regional_peace_and_defense_network).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, charter_bounded_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds land and local authority in exchange for a fixed, charter-specified set of obligations: a defined number of days of military service, defined counsel duties, defined aids on enumerated occasions (ransom, eldest son's knighting, eldest daughter's marriage). The charter text is the vassal's own defense against arbitrary escalation — it can be produced, cited, and litigated in the lord's own court or before peer vassals. Exit from a specific lord relationship is difficult, but the obligation itself is bounded and known in advance, which is what the vassal actually bargained for.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals, agenda_setter).

% Receives predictable military and counsel service from a network of vassals without having to renegotiate terms constantly or maintain a standing coercive apparatus to extract compliance. The charter binds the lord as much as the vassal: demanding service beyond its terms breaches the oath and licenses the vassal's peers to judge the breach. The lord's own strategic position depends on vassals trusting the bound is real.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, liege_lords, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, liege_lords, agenda_setter).

% Fellow vassals sitting in judgment when a lord is accused of exceeding the charter's bounds, or a vassal is accused of shirking. This peer-adjudication function is what gives the charter teeth without requiring either party to trust the other's unilateral word — it is horizontal enforcement among equals rather than vertical imposition.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, peer_vassal_court, agenda_setter,
    organized, generational, constrained, regional).

% Works the land beneath the oath relationship entirely and has no charter of their own, no seat in the peer court, and no reciprocal bound on what can be demanded of them. Their situation is governed by manorial custom, not the vassal-lord oath this reading describes; they are noted here because the coordination reading's low-extraction claim applies specifically to the lord-vassal relationship and does not extend downward.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, unfree_peasantry, excluded,
    powerless, biographical, trapped, local).

% Sits above the individual lord-vassal charters as the ultimate guarantor of the tenurial order, occasionally called on to arbitrate disputes the peer court cannot resolve, and benefits from a stable, self-enforcing web of bounded obligations that does not require constant royal intervention to hold.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, royal_or_ducal_overlord, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__vassal_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__vassal_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an otherwise open-ended and constantly renegotiable relationship of protection-for-service into a fixed, enumerated, mutually citable set of obligations — solving the problem of how a lord and a vassal each commit credibly to a long-term relationship without a third-party enforcer, by making the charter text and peer judgment the enforcement mechanism instead.
% TRANSFER_FUNCTION: Moves defined military service, counsel, and enumerated aids from vassal to lord, and moves land tenure, protection, and reciprocal counsel obligations from lord to vassal — both directions are bounded by the same instrument, and the peer court can compel either side to honor it.
% ABSENT_VOICES: The unfree peasantry beneath the tenurial relationship has no charter, no peer court, and no reciprocal bound at all; they would object that describing the feudal order as a 'low-extraction coordination mechanism' describes only its upper tier. Ecclesiastical authorities and the lord's own household officers who might read the charter's silences differently are also not seated here — see the sibling readings.
% DISAPPEARANCE_RATIONALE: If the charter-bound oath vanished overnight, the vassal coordination reading holds that both lord and vassal would need to renegotiate the entire relationship from scratch under conditions of mutual suspicion, likely producing either more coercive extraction (lord side) or defection and reduced service (vassal side) — the world of stable regional defense and succession would rearrange. A rival reading (lord_extraction) would say little changes because the lord's practical leverage over service was never really bounded by the text to begin with; that dispute is exactly what the kernel contest is about.
% FOUNDING_PROBLEM: Early medieval polities lacked centralized bureaucratic capacity to raise armies, administer justice, or manage land tenure directly; the oath-and-charter mechanism let a lord project military and administrative reach through a network of semi-autonomous vassals whose obligations were fixed in advance rather than subject to continuous coercive renegotiation.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians examining the transition from tenurial charters to money-fief and eventually to standing royal bureaucracies and professional armies attest that the coordination problem the oath solved — projecting authority without a bureaucratic state apparatus — was superseded by direct taxation and salaried administration well before the charter form itself disappeared from use; this corroboration comes from institutional historians outside the beneficiary set (neither lords nor vassals), though within the vassal_coordination_reading's own terms the charter's persistence past this point is read as institutional inertia rather than active extraction.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, contested).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because, under this reading, neither party's obligation is open-ended: the vassal's service is enumerated and the lord's demands beyond the charter are breach, not entitlement. Suppression is likewise low-moderate (0.28) — the mechanism that holds the arrangement together is horizontal peer adjudication and reputational cost of breach, not coercive enforcement machinery aimed at extracting compliance. Theater ratio stays low and roughly flat (0.10 to 0.15) across the interval: the charter is doing real coordination work throughout, not increasingly performative work, under this reading's own account. accessibility_collapse is moderate (0.35), reflecting that alternatives to the specific lord relationship were genuinely constrained (a vassal could not easily walk away from a particular liege) even though the terms of that relationship were bounded. resistance is low (0.2) because peer-court adjudication provided a channel for grievance short of open revolt, reducing the need for either side to resist the arrangement itself.
 *
 * PERSPECTIVAL GAP:
 *   The lord and vassal seats compute similarly under this reading precisely because the reading's claim is mutual bound — that is the structural point being tested against the sibling readings, where the same charter text produces a lord-favoring or church-favoring asymmetry instead. The peer_vassal_court seat is where this reading's distinctive claim actually lives: if that seat's adjudicative function is real, the mutual-bound story holds; if the sibling lord_extraction_reading is right that the court rarely ruled against lords in practice, the coordination story collapses toward extraction. That empirical question is the substance of the omega below.
 *
 * DIRECTIONALITY LOGIC:
 *   Both enfeoffed_vassals and liege_lords are authored as beneficiaries because the reading's core claim is mutual bound: each gets a credible, low-transaction-cost long-term commitment from the other. The peer_vassal_court is agenda_setter rather than beneficiary because its function is adjudication, not collection. unfree_peasantry sits outside the beneficiary/victim structure of this specific constraint entirely — they are excluded, not a victim of the oath-charter mechanism, because the reading is about the lord-vassal tie specifically. No victims are declared because the reading's structural claim is precisely that the relationship it describes has no structural victim — that is the expected delta this story is instantiating, not a tuning choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem this reading identifies — projecting military and administrative authority without a bureaucratic state — is corroborated as dead by institutional historians outside the beneficiary set, while the arrangement in some regions persisted well past that point as fixed tenurial custom. This reading does not claim the charter form was eternally functional; it claims that WHILE functional, it operated as low-extraction coordination rather than extraction. Whether its late-period persistence should be read as piton-like inertia is a question for a different, later-dated constraint story, not this one — ε here is authored for the period in which the coordination function was live, consistent with the kernel-reading referent rule (the standing arrangement under contest, by this reading's own lights).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peer_court_impartiality,
    'Did the peer vassal court actually rule against lords who exceeded charter terms with meaningful frequency, or was its adjudicative function largely nominal — with lords able to pack, intimidate, or bypass the court in practice?',
    'Survey of surviving court rolls and dispute records across multiple regions and periods for the ratio of vassal-favorable to lord-favorable rulings in charter-breach disputes, controlling for the relative power of the lord involved.',
    'If the court ruled for vassals with real frequency, the coordination reading is well-supported. If rulings systematically favored lords regardless of charter text, this reading collapses toward the lord_extraction_reading and epsilon should be revised upward substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_court_impartiality, empirical, 'Whether peer-court enforcement was real or nominal — the load-bearing empirical question for this reading.').

omega_variable(
    charter_specificity_variance,
    'How much did the fixity of obligations actually vary across charters and regions — were ''fixed and bounded'' terms the norm, or a minority pattern generalized by this reading from favorable examples?',
    'Comparative survey of surviving charter texts for specificity of enumerated service obligations versus open-ended or discretionary language (''and such other aid as the lord may reasonably require'').',
    'If open-ended language was common, this reading''s low-extraction claim applies to a narrower set of relationships than implied, and a more granular set of constraint stories (per charter type) may be warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charter_specificity_variance, empirical, 'Whether charter fixity was the norm or a favorable-case generalization.').

omega_variable(
    kernel_reading_selection_basis,
    'Among the three sibling readings of this kernel, what evidentiary or interpretive commitment leads an analyst to adopt the vassal_coordination_reading specifically, rather than treating charter text as secondary to either lord practical power or church doctrinal limitation?',
    'This is a conceptual/framing question rather than an empirical one directly — it is resolved by which body of evidence (court rolls vs. lord administrative records vs. ecclesiastical court records) an analyst treats as primary for reconstructing ''actual'' obligation limits.',
    'Different evidentiary priors produce different readings as primary; this omega documents that the reading selection itself is a framing choice, not a discovered fact, consistent with the kernel-reading discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Framing basis for selecting this reading over its siblings within the feudal_oath_reciprocity kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 1050, 1350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1050, 0.1).
narrative_ontology:measurement(feud_tr_t1110, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1110, 0.11).
narrative_ontology:measurement(feud_tr_t1170, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1170, 0.13).
narrative_ontology:measurement(feud_tr_t1230, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1230, 0.14).
narrative_ontology:measurement(feud_tr_t1290, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1290, 0.15).
narrative_ontology:measurement(feud_tr_t1350, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1350, 0.15).

% Extraction over time
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1050, 0.18).
narrative_ontology:measurement(feud_be_t1110, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1110, 0.2).
narrative_ontology:measurement(feud_be_t1170, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1170, 0.21).
narrative_ontology:measurement(feud_be_t1230, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1230, 0.23).
narrative_ontology:measurement(feud_be_t1290, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1290, 0.24).
narrative_ontology:measurement(feud_be_t1350, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1350, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(feudal_oath_reciprocity__vassal_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the feudal_oath_reciprocity kernel: this vassal_coordination_reading (low epsilon rope — mutual bound, peer enforcement, no structural victim), lord_extraction_reading (expected higher epsilon tangled_rope or snare — extraction bounded only by vassal capacity, vassal as structural victim), and ecclesiastical_mediation_reading (expected moderate epsilon tangled_rope — church as limiting third party with its own extraction via tithes/spiritual sanction). Each is authored independently per the epsilon-invariance principle; none averages over the others. This file's network edges point to both siblings to preserve the kernel-family linkage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
