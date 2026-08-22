% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)
 *   domain: medieval/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   The feudal oath in medieval Christendom was contested at its theological
 *   foundations. The ecclesiastical-mediation reading frames the oath as a
 *   sacramental act binding both lord and vassal under Christian charity
 *   doctrine. The church interprets charity limits to justify constraining
 *   lords' extraction and protecting vassals from egregious breach. This
 *   reading coexists with two sibling readings: the lord-extraction reading
 *   (oath authorizes maximal extraction bounded only by vassal capacity) and
 *   the vassal-coordination reading (oath establishes fixed, charter-bounded
 *   reciprocal obligations enforceable by secular text). This constraint
 *   story models ONLY the ecclesiastical-mediation reading, with ε = 0.54
 *   (moderate extraction, substantial coordination benefit, active
 *   enforcement). The reading's core claim is that theological interpretation
 *   of charity doctrine provides a genuine limit on extraction—not a total
 *   constraint, but a structural brake on lords' maximal demands. The engine
 *   will compute different seats' classifications from this structural data;
 *   seat divergence is expected and diagnostically valuable.
 *
 * KEY AGENTS:
 *   - Ecclesiastical hierarchy (bishops, abbots, papal authority): mediators claiming theological grounding, enforcers via excommunication threat, beneficiaries of spiritual authority transfer.
 *   - Demanding lords: powerful actors wishing to maximize extraction, constrained by ecclesiastical interpretation, payers of the mediation cost.
 *   - Vassal collective: bound by oath, moderately protected by ecclesiastical charity doctrine, beneficiary of limited extraction, payer of obligation-keeping cost.
 *   - Village commons: powerless, trapped, bear extraction burdens reduced (if ecclesiastical enforcement works) or full (if lords break theological limits).
 *   - Cathedral clergy: organized beneficiaries, collectors of spiritual authority and social standing through mediation role.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.54).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.38).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Reciprocity (Ecclesiastical Mediation Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '3b9bec44-dc63-445b-84aa-b3635c6b8b29').
narrative_ontology:cs_kernel_codification('3b9bec44-dc63-445b-84aa-b3635c6b8b29', fixed_text).
narrative_ontology:cs_authority_grounding('3b9bec44-dc63-445b-84aa-b3635c6b8b29', lineage).
narrative_ontology:cs_interpretation_layer_present('3b9bec44-dc63-445b-84aa-b3635c6b8b29').
narrative_ontology:cs_reading_relation('3b9bec44-dc63-445b-84aa-b3635c6b8b29', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b9bec44-dc63-445b-84aa-b3635c6b8b29', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('3b9bec44-dc63-445b-84aa-b3635c6b8b29', foundational, oath_sacramental_binding).
narrative_ontology:cs_axiom_status(oath_sacramental_binding, holdable).
narrative_ontology:cs_axiom_grounding('3b9bec44-dc63-445b-84aa-b3635c6b8b29', oath_sacramental_binding, deontological).
narrative_ontology:cs_axiom('3b9bec44-dc63-445b-84aa-b3635c6b8b29', foundational, charity_limits_extraction).
narrative_ontology:cs_axiom_status(charity_limits_extraction, holdable).
narrative_ontology:cs_axiom_grounding('3b9bec44-dc63-445b-84aa-b3635c6b8b29', charity_limits_extraction, deontological).
narrative_ontology:cs_reference_frame('3b9bec44-dc63-445b-84aa-b3635c6b8b29', ecclesiastical_charity_reciprocity_framework).
narrative_ontology:cs_drift_state('3b9bec44-dc63-445b-84aa-b3635c6b8b29', high_medieval_institutional_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3b9bec44-dc63-445b-84aa-b3635c6b8b29', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_collective).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, demanding_lords).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, cathedral_clergy).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_collective).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, village_commons).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, christian_charity_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_oath_binding_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the feudal oath as binding under Christian charity doctrine and sacramental obligation. Bishops and abbots enforce oath interpretation through ecclesiastical authority: threats of excommunication, denial of sacraments, and interdict. They position themselves as authoritative mediators between lord and vassal, claiming theological grounding for limits on extraction. Gain interpretive authority over what the oath legitimately demands.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).

% Wish to extract maximum value from vassal obligations: labor, military service, payments, hospitality. Constrained by ecclesiastical interpretation that frames unlimited extraction as violating charity and sacramental oath. Face excommunication for egregious breach of oath spirit. Their extraction ceiling is policed by the church's theological authority, not by contractual text alone.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, demanding_lords, payer,
    powerful, biographical, constrained, regional).

% Bound by oath to provide military service, labor, and rents to their lord. Gain from ecclesiastical interpretation that frames oath as mutual and bounded by charity: extractive lords face theological sanction, ecclesiastical protection extends to oath-keepers who honor reciprocal obligation. They remain obligated to the oath but within limits the church defends. They also bear the burden of oath-keeping itself and ecclesiastical enforcement mechanisms.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_collective, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_collective, payer).

% Subject to the lord's demands for labor and harvest-tithes, mediated by the ecclesiastical frame. If lords are constrained by ecclesiastical interpretation to moderate extraction, commons experience lower pressure; if lords break theological limits and face no sanction, commons bear the full weight. Their exit options (flight, rebellion) are nearly impossible; they depend entirely on ecclesiastical enforcement of the charity doctrine.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, village_commons, payer,
    powerless, biographical, trapped, local).

% From Rome, issues doctrine on oath-binding authority, charity obligations, and the church's role as mediator of secular power. Can reinforce or relax ecclesiastical enforcement through papal pronouncement. Analytical seat: the authority grounding this reading's legitimacy.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, papal_authority, observer,
    institutional, generational, analytical, continental).

% Gain spiritual authority and social standing by mediating oath disputes and enforcing ecclesiastical interpretations. Receive donations from penitent lords and protection fees from vassals seeking ecclesiastical backing. Benefit from the role as arbiter without directly extracting feudal obligations themselves—they collect through spiritual services and almsgiving conventions.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, cathedral_clergy, beneficiary,
    organized, biographical, constrained, regional).

% Rejected ecclesiastical interpretation of charity and oath-binding; propose alternative readings that delegitimize both the feudal extraction AND the church's mediation role. Excluded from official oath ceremonies and ecclesiastical enforcement apparatus. Their competing reading is suppressed through inquisitorial mechanisms.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, heretical_movements, excluded,
    moderate, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__ecclesiastical_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of oath collapse under unlimited lord extraction: establishes shared interpretive authority (the church) to enforce that oaths are mutual obligations bounded by Christian charity, preventing lords from extracting without limit and vassals from unilaterally breaking oath-bonds. Enables stable long-term vassal-lord relationships where both parties trust the church as mediator and interpreter.
% TRANSFER_FUNCTION: Moves ecclesiastical interpretive authority INTO feudal relations. Lords transfer some degree of extraction discretion to the church's theological reading of charity limits. Vassals transfer obligation-keeping to a framework that claims to protect them from egregious breach. The church transfers spiritual authority into temporal enforcement: oaths are sacramental acts, breaking charity is sin, excommunication is the penalty.
% ABSENT_VOICES: Vassals and commons who would challenge the church's interpretive monopoly are excluded from official oath-interpretation; dissident clergy proposing non-ecclesiastical mediation are silenced. Heretical movements proposing that charity forbids feudalism entirely are suppressed. No secular guild or merchant alternative interpretation is admitted to official oath ceremonies.
% DISAPPEARANCE_RATIONALE: If ecclesiastical mediation of feudal oath vanished, lords would lack theological constraint on extraction; extraction would rise sharply or vassals would seek alternative arrangements (secular guarantors, armed coalitions, flight). The stable reciprocity that the church's interpretation enables would collapse; vassal obligations would either become pure coercion or be renegotiated under entirely different frames.
% FOUNDING_PROBLEM: Early feudal arrangements were unstable: lords extracted without limit, vassals broke oaths under extreme pressure, relationships dissolved into violence. The church proposed that the oath is a sacramental act binding both parties under Christian charity doctrine—a theological framework to police extraction and stabilize mutual obligation.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical authorities attest the founding problem is still live, citing oath-breakers and extractive lords. Vassal rebellions and charter movements (Magna Carta's descendants) attest that lords continue to breach charity limits and that non-ecclesiastical constraints are sought. Historians outside the benefiting parties note that ecclesiastical interpretation successfully reduced the frequency of oath collapse during the 12th–13th centuries, but the church's mediation also entrenched feudal extraction by legitimizing it theologically—the problem was partly solved, partly institutionalized.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.54, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.54 reflects that ecclesiastical mediation IS a genuine coordination mechanism (charity doctrine + sacramental oath authority provide real constraints on extraction), but it ALSO concentrates interpretive power in the church (which benefits and has incentive to legitimize feudal arrangements by moderating them). The constraint requires active enforcement (bishops must threaten excommunication and deliver on threats); suppression is moderate (0.38) because the constraint works partly through internalized norm-acceptance and partly through external sanction threat. Theater rises over time (0.12→0.29) as ecclesiastical enforcement becomes increasingly ritualized and divorced from actual extraction constraints—the measurement series shows the constraint's extraction effect plateauing while theater performance increases, which is the classic piton degradation pattern. Suppression requirement actually DECLINES slightly (0.42→0.38) over the interval, suggesting that internalized charity norms stabilize the constraint without requiring escalating enforcement pressure. This reading's claim is that ecclesiastical mediation provides real reciprocity protection; the rising theater metric tests whether that protection is genuine or increasingly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical seat, the constraint is genuine coordination—the church has invested authority in stabilizing mutuality and preventing oath collapse. From the demanding-lords' seat, it is an external constraint on profit maximization, experienced as extraction of interpretive authority that reduces what they can legitimately demand. From the vassal seat, it is protection-with-obligation: genuine benefit (extraction is bounded) but also coercive (they must fulfill oath anyway). The engine computes these divergent experiences from the declared power, exit, and beneficiary/victim structure; the ecclesiastical-mediation reading does not adjudicate which seat's perception is 'right.' The commentary establishes the structural gap; the seat calculations prove it.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical hierarchy: d ≈ 0.2 (beneficiary, powerful, arbitrage exit—they can adjust interpretations, leverage into other jurisdictions). Demanding lords: d ≈ 0.65 (constrained extraction, powerful but constrained exit—they cannot simply leave the feudal system, constrained by ecclesiastical authority). Vassal collective: d ≈ 0.5 (symmetric—genuine coordination benefit in bounded extraction, but coercive obligation-keeping). Village commons: d ≈ 0.75 (trapped, powerless, bear full consequences if ecclesiastical constraint fails). The directionality spread models the structural asymmetry: the church's interpretive power is extracted from lords' discretion but granted to ecclesiastical authority; vassal and commons positions bracket the middle, with commons bearing the most vulnerability.
 *
 * MANDATROPHY ANALYSIS:
 *   The rising theater_ratio (0.12→0.29) coupled with plateauing extractiveness (0.38→0.54) and declining suppression_requirement (0.42→0.38) suggests potential mandatrophy: the founding problem (oath collapse under unlimited extraction) may be solved—ecclesiastical interpretation has stabilized some baseline of reciprocity—but the constraint persists partly through theater. The measuring point is the excommunication-credibility omega: if bishops continue excommunicating extractive lords but the lords extract anyway without secular consequences, the constraint becomes piton-like. The constraint does NOT show mandatrophy YET (extraction plateaus, not reverses), but the rising theater is the warning signal. If theater climbs above 0.4 and extraction continues rising in subsequent intervals, mandatrophy would be declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charity_doctrine_interpretation_variance,
    'What counts as fulfilling ''Christian charity'' in oath obligation? Is it a strict doctrinal limit (fixed by papal pronouncement) or a flexible, context-dependent standard interpreted locally by bishops?',
    'Historical analysis of ecclesiastical court records and synodal rulings: do bishops enforce consistent charity thresholds across regions, or does interpretation drift based on local power dynamics (powerful lords get looser readings, weak lords get strict readings)?',
    'If charity is strict and consistent, ecclesiastical mediation genuinely constrains extraction; if it drifts toward local power, the reading collapses into extraction under theological cover (shifts classification toward snare). This is the reading''s core fragility point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charity_doctrine_interpretation_variance, empirical, 'Whether ecclesiastical charity doctrine is a fixed limit or a variable one warped by local power.').

omega_variable(
    excommunication_credibility_decay,
    'Over time, does the threat of excommunication remain credible as a constraint on powerful lords, or does it become purely theatrical—lords are excommunicated but extract anyway, and the sanction loses force?',
    'Measurement of excommunication frequency, duration, and actual lord compliance over the interval; analysis of whether lords who ignore excommunication face secular consequences (loss of vassals, military coalitions, reputation damage) or whether the threat becomes isolated to the spiritual realm.',
    'Rising theater ratio and falling suppression effectiveness would indicate the constraint is degrading into piton territory: ecclesiastical authority persists as theater while actual extraction rises unchecked. This is the mechanism for mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(excommunication_credibility_decay, empirical, 'Whether excommunication sanction remains effective or becomes performative.').

omega_variable(
    sibling_reading_contest_irreducibility,
    'Can the same feudal oath text and practice be read coherently through all three readings (ecclesiastical mediation, lord extraction maximization, vassal coordination charter), or does adopting one reading logically exclude the others?',
    'Engagement with each reading''s grounding axiom: do the axioms contradict, coexist, or influence? If they coexist as live positions held by different parties, the kernel is genuinely contested; if one axiom foreclosed the others, the kernel would collapse toward a single reading.',
    'This omega documents the kernel''s reading-irreducibility itself. It routes to the cs_structure.reading_relations and cs_structure.axioms fields, which model the contest structurally rather than dissolving it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contest_irreducibility, conceptual, 'Whether the feudal oath kernel is genuinely multi-readable or reducible to one reading.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.38) structural—lords'' extraction is constrained by external ecclesiastical authority and sanction threat—or partially internalized—vassals and lords have internalized charity as a legitimate norm such that extraction remains below the theological limit partly through internalized shame rather than external threat?',
    'Post-reformation analysis: if protestantism rejects ecclesiastical mediation, do extraction rates surge immediately (external suppression was real) or hold relatively steady (internalized norms persist)? If rates hold, suppression was partially internalized; if they surge, it was mostly external.',
    'If substantially internalized, the constraint''s effectiveness outlasts ecclesiastical authority; if mostly external, removing the church collapses the constraint quickly. This affects long-term mandatrophy prospects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or partially internalized norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t75, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 75, 0.16).
narrative_ontology:measurement_basis(feud_tr_t75, observed).
narrative_ontology:measurement(feud_tr_t150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 150, 0.21).
narrative_ontology:measurement_basis(feud_tr_t150, observed).
narrative_ontology:measurement(feud_tr_t225, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 225, 0.26).
narrative_ontology:measurement_basis(feud_tr_t225, observed).
narrative_ontology:measurement(feud_tr_t300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 300, 0.29).
narrative_ontology:measurement_basis(feud_tr_t300, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t75, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement_basis(feud_be_t75, observed).
narrative_ontology:measurement(feud_be_t150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 150, 0.52).
narrative_ontology:measurement_basis(feud_be_t150, observed).
narrative_ontology:measurement(feud_be_t225, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 225, 0.54).
narrative_ontology:measurement_basis(feud_be_t225, observed).
narrative_ontology:measurement(feud_be_t300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 300, 0.54).
narrative_ontology:measurement_basis(feud_be_t300, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t75, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 75, 0.4).
narrative_ontology:measurement_basis(feud_su_t75, observed).
narrative_ontology:measurement(feud_su_t150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 150, 0.39).
narrative_ontology:measurement_basis(feud_su_t150, observed).
narrative_ontology:measurement(feud_su_t225, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 225, 0.38).
narrative_ontology:measurement_basis(feud_su_t225, observed).
narrative_ontology:measurement(feud_su_t300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 300, 0.38).
narrative_ontology:measurement_basis(feud_su_t300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% Feudal oath reciprocity is a contested kernel with three structurally distinct readings. This story models the ecclesiastical-mediation reading (ε=0.54, tangled_rope, church gains interpretive authority). The lord_extraction_reading models the same oath as authorizing maximal extraction (higher ε, snare-flavored). The vassal_coordination_reading models the oath as bounded by charter text (lower ε, rope-flavored). All three share the same nucleus (the feudal oath commitment) but differ in who interprets it legitimately and what limits are binding. The three stories form a constraint family; each affects the others' credibility and institutional power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, powerless, 0.77).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
