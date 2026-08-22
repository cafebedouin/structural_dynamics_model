% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Allocation: Functional Accommodation Reading
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   The U.S. Constitution vests Congress with the power to declare war and
 *   the President with commander-in-chief authority—a bifurcated allocation
 *   that has generated sustained constitutional conflict. The functional
 *   accommodation reading holds that the allocation is properly
 *   context-dependent: imminent threats (invasion, sudden attack) permit
 *   executive unilateral action because deliberation would be suicidal;
 *   prolonged campaigns require congressional authorization because
 *   democratic legitimacy through legislative process is constitutionally
 *   required for sustained military commitments. This reading treats the
 *   constraint as a negotiated power-sharing arrangement, not a hierarchical
 *   rule. The extractiveness is moderate (0.58) because both branches benefit
 *   (executive gains speed in emergencies, Congress retains authority over
 *   sustained action) but the gray zone between imminent and merely
 *   foreseeable threat permits executive expansion. Suppression is high
 *   (0.72) because the boundary depends on executive interpretation, and
 *   Congress's ability to enforce the boundary is structurally asymmetric.
 *   Theater is moderate-high (0.48) because the functional accommodation
 *   language masks a de facto executive dominance of the gray
 *   zone—enforcement rhetoric about boundaries is high, but actual
 *   enforcement is weak.
 *
 * KEY AGENTS:
 *   - executive_branch: Sets and interprets the imminent-threat boundary; controls military apparatus and intelligence; administers the rule by determining what is imminent and what requires authorization
 *   - congress: Retains formal authorization power for sustained campaigns; enforcement of the boundary depends on Congress maintaining will to assert authority; institutional capacity to enforce is variable
 *   - domestic_opposition_movements: Bear costs of unilateral executive action in gray zones where imminence is contested; identity-locked to nation-state and cannot exit; depend on Congress to enforce the boundary
 *   - international_partners: Benefit from predictable U.S. commitment to constitutional process; gain signaling value of deliberation in sustained operations; access to the U.S. commitment is constrained by relational factors
 *   - affected_foreign_populations: Excluded from the constitutional arrangement; bear costs of military action initiated unilaterally; have no voice in the boundary-setting process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.58).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.72).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Allocation: Functional Accommodation Reading").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '5915ddde-1b91-43ff-b4bd-c7f6283ce962').
narrative_ontology:cs_kernel_codification('5915ddde-1b91-43ff-b4bd-c7f6283ce962', fixed_text).
narrative_ontology:cs_authority_grounding('5915ddde-1b91-43ff-b4bd-c7f6283ce962', lineage).
narrative_ontology:cs_interpretation_layer_present('5915ddde-1b91-43ff-b4bd-c7f6283ce962').
narrative_ontology:cs_reading_relation('5915ddde-1b91-43ff-b4bd-c7f6283ce962', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5915ddde-1b91-43ff-b4bd-c7f6283ce962', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('5915ddde-1b91-43ff-b4bd-c7f6283ce962', foundational, context_dependent_authority_allocation).
narrative_ontology:cs_axiom_status(context_dependent_authority_allocation, holdable).
narrative_ontology:cs_axiom_grounding('5915ddde-1b91-43ff-b4bd-c7f6283ce962', context_dependent_authority_allocation, instrumental).
narrative_ontology:cs_axiom('5915ddde-1b91-43ff-b4bd-c7f6283ce962', foundational, emergency_speed_legitimacy_balance).
narrative_ontology:cs_axiom_status(emergency_speed_legitimacy_balance, holdable).
narrative_ontology:cs_axiom_grounding('5915ddde-1b91-43ff-b4bd-c7f6283ce962', emergency_speed_legitimacy_balance, deontological).
narrative_ontology:cs_reference_frame('5915ddde-1b91-43ff-b4bd-c7f6283ce962', negotiated_executive_congressional_balance).
narrative_ontology:cs_drift_state('5915ddde-1b91-43ff-b4bd-c7f6283ce962', contemporary_terrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5915ddde-1b91-43ff-b4bd-c7f6283ce962', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, excluded_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, international_partners).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, domestic_opposition_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the power to initiate military action in imminent-threat contexts without prior authorization; gains operational flexibility in response to sudden crises. Must seek congressional authorization for campaigns beyond the narrow imminent-defense window, but the boundary is contestable and enforcement of that boundary depends on congressional willingness to enforce. The executive administers the rule by interpreting what counts as imminent and what requires authorization.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, executive_branch, beneficiary).

% Retains the power of the purse and the formal power to authorize military force. In the functional accommodation reading, Congress gains the benefit of rapid executive response in genuine emergencies (without needing emergency session or overnight debate) while preserving its authority over sustained campaigns. The constraint's persistence depends on Congress maintaining institutional will to enforce the boundary; that will has periodically atrophied, allowing executive expansion into the gray area.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, congress, agenda_setter).

% Benefit from predictable U.S. commitment to constitutional process and from the mutual-deterrence value of slow, deliberative war authorization (it signals that military response is not impulsive). The functional accommodation reading permits swift response to immediate threats while preserving the signal of deliberation for sustained operations. Their exit is constrained by the relational nature of alliance.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, international_partners, beneficiary,
    powerful, biographical, constrained, global).

% Bear the costs of military operations initiated unilaterally in imminent-threat contexts, without the deliberation and public-debate period that a full authorization process would afford. Their exit is identity-locked to the nation-state; they cannot leave without abandoning citizenship. They depend on Congress to enforce the functional boundary, but Congress has variable institutional capacity to do so.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, domestic_opposition_movements, payer,
    organized, biographical, identity_locked, national).

% Are not parties to the U.S. constitutional arrangement and have no voice in the debate over where the imminent-threat boundary sits. They experience the consequences of military action taken under either the executive's or Congress's framing of what is authorized. Their only recourse is international legal and diplomatic pressure, which is structurally weak against a major power.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, affected_foreign_populations, excluded,
    powerless, immediate, trapped, global).

% Have repeatedly declared war powers questions non-justiciable (political questions doctrine), treating the executive-congressional boundary as outside judicial review. They sit outside the constraint's enforcement structure and have not enforced it, despite being nominally co-equal. Their analytical position allows them to observe but not adjudicate.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits rapid military response to imminent threats (where deliberation would mean vulnerability) while preserving congressional authorization for sustained campaigns (where democratic legitimacy through legislative process matters most). Solves the coordination problem of balancing security responsiveness with constitutional process.
% TRANSFER_FUNCTION: Transfers power to initiate force from exclusive congressional prerogative to a context-dependent allocation: the executive gains unilateral power in imminent-threat contexts; Congress retains it for sustained operations. The domestic opposition movement and affected foreign populations bear the costs of unilateral executive action in the gray zones where threat imminence is contested.
% ABSENT_VOICES: Affected foreign populations are structurally excluded. Domestic opposition movements are nominally included (they are U.S. citizens) but have no direct seat at the executive-congressional negotiation table. The constraint's persistence depends on Congress maintaining will to enforce it; when Congress is itself fractured or distracted, the executive's interpretation expands into the gray area unchecked.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared and were replaced with pure executive war-making power, the U.S. constitutional separation of powers would collapse; if replaced with pure congressional primacy, rapid emergency response capacity would require restructuring (emergency sessions, standing authorizations, or constitutional amendment). Either alternative world requires major institutional reorganization.
% FOUNDING_PROBLEM: The Constitution allocates war power ambiguously between branches: Congress declares war; the President is commander-in-chief. Early practice generated conflict. The functional accommodation emerged from recognition that imminent threats (invasion, sudden attack) require executive response speed, while prolonged wars require political legitimacy only Congress can confer.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch attests the founding problem is still live, citing cyber threats and terrorism that move at speeds Congress cannot match. Scholars of constitutional law (outside both branches) and some congressional voices attest the founding problem persists but the functional accommodation has been eroded by executive overreach into the gray area. Congressional testimony and historical analysis from neutral analysts support the contested reading.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-to-substantial) because the functional accommodation reading is itself a partial accommodation of executive advantage. The executive branch defines what counts as imminent without binding review; the gray zone between imminent and merely foreseeable is where most contested military actions live. Suppression at 0.72 (high) reflects structural asymmetry: the executive controls the military apparatus and intelligence that informs threat assessment; Congress's recourse is to withhold funding or pass resolutions after the fact. Theater at 0.48 (moderate-high) reflects that much of the enforcement activity consists of rhetorical assertion of boundaries rather than functional enforcement—Congress passes resolutions, holds hearings, and debates authorization long after operations are underway. The measurement series shows a peak in extractiveness at t=15 (point of maximum executive expansion into gray areas) and suppression at t=15, followed by modest compression as congressional re-assertion increases (perhaps following an election that shifted control or a particularly egregious unilateral action that unified opposition). The constraint is claimed as tangled_rope because it genuinely coordinates emergency response (both branches benefit) while asymmetrically extracting unilateral authority for the executive in the contestable gray zone. The interpretation-layer measurement peak at t=15 reflects maximum executive interpretation dominance; the subsequent compression reflects congressional re-assertion of interpretive authority through the mechanism that actually matters (the power of the purse).
 *
 * PERSPECTIVAL GAP:
 *   From the executive's institutional seat, the functional accommodation is a pragmatic allocation that permits necessary speed while preserving congressional oversight for sustained operations—a genuine coordination. From the domestic opposition seat, the same structure is asymmetrically extraction: the boundaries are interpreted by the agent being bounded, and enforcement is optional. From the congressional seat, the arrangement is theoretically balanced but operationally weighted toward the executive (through interpretation advantage and suppression of congressional action). The engine computes these divergences from the structural data: the executive's institutional power, the opposition's identity-lock, Congress's variable enforcement will. The measured types should diverge by seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch sits at high directionality (d near 0.7–0.8): it gains unilateral power in imminent-threat contexts, controls the interpretation of imminence, and benefits from the functional accommodation because it permits speed. Congress sits at lower directionality (d near 0.3–0.4): it retains formal authority but enforcement depends on its own will, which is variable. Domestic opposition movements sit at high directionality (d near 0.8): they bear costs of unilateral action without the deliberation period. The asymmetry is structural: the executive's institutional position (control of military and intelligence) gives it advantage in interpreting boundaries, while Congress's institutional position (need for quorum, floor time, political alignment) makes enforcement costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for rapid emergency response balanced against democratic legitimacy) is contested, not dead. The functional accommodation reading defends the current arrangement as a solution to that problem. However, if Congress's enforcement will atrophies and the executive's interpretation of imminent threat expands unchecked, the founding coordination problem (emergency speed + democratic legitimacy) becomes zombie—the arrangement persists under the name of functional accommodation but actually instantiates pure executive dominance (inherent reading). Conversely, if Congress reasserts enforcement and narrows the imminence boundary, the founding problem compresses (emergency response is secured through pre-authorized pathways, democratic legitimacy is secured through actual authorization). The measurement series shows this dynamic: the peak at t=15 represents maximum executive interpretation dominance (mandatrophy risk—the founding coordination problem is shadowed by executive extraction). The subsequent compression suggests congressional re-assertion and restoration of the functional balance. The constraint is vulnerable to mandatrophy if the executive's interpretation gains becomes permanent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminent_threat_boundary_contestability,
    'What constitutes an imminent threat sufficiently proximate to permit unilateral executive action without prior authorization? Where is the boundary between imminent and merely foreseeable?',
    'Case-by-case congressional assertion (passing resolutions challenging executive interpretations) or Supreme Court injunction. Historical pattern analysis: which executive actions sparked immediate congressional challenge vs. which went uncontested? Accumulation of precedent may narrow the ambiguity, or political conditions may widen it.',
    'If the boundary shifts toward the executive''s expansive reading, the constraint becomes increasingly extractive (executive gains more unilateral power) and approaches the inherent_executive_reading. If Congress successfully reasserts narrow imminence standards, extractiveness compresses toward the functional accommodation framing. The current measurement assumes moderate contestation with moderate executive advantage in boundary definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminent_threat_boundary_contestability, conceptual, 'The structural ambiguity of what counts as imminent threat.').

omega_variable(
    congressional_enforcement_will_variability,
    'Does Congress maintain consistent institutional will to enforce the functional boundary, or does that will fluctuate with partisan alignment, threat perception, and legislative capacity?',
    'Longitudinal study of congressional responses to unilateral military actions: vote counts on authorization resolutions, floor debate intensity, committee investigations. Periods of alignment between branches show lower enforcement; periods of opposition show higher enforcement.',
    'High congressional enforcement will compresses extractiveness (Congress enforces the boundary, executive compliance improves). Low enforcement will expands extractiveness (executive interprets broadly unchecked, approaches inherent reading). The measurements show a modest peak at t=15 (point of maximum executive expansion into gray area) and compression toward interval end (partial congressional re-assertion). If enforcement will drops below current levels, this constraint could tip toward snare; if congressional will strengthens, it could approach rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_enforcement_will_variability, empirical, 'Congress''s actual institutional capacity and will to enforce the authorization boundary.').

omega_variable(
    suppression_mechanism_source,
    'Is the measured suppression structural (the executive''s control of military machinery and intelligence makes congressional oversight materially difficult) or internalized (Congress has come to accept executive dominance as normal and doesn''t assert its authority even when it could)?',
    'Comparative analysis: when Congress does assert authority (successful challenge, sustained investigation, denied appropriations), does executive compliance follow? If compliance is swift, suppression is more structural-legal; if compliance is slow/contested, suppression has internalized-norm components. Also: post-administration congressional investigations into prior actions show whether Congress''s own sense of legitimacy was suppressed by the time or whether it recovered it in hindsight.',
    'If suppression is largely structural, the functional accommodation reading is defensible — it allocates power rationally given institutional constraints. If suppression is largely internalized (Congress has absorbed the logic of executive dominance and stopped asserting its own authority), the constraint is partly snare-like (extraction masked by internalized acceptance). High structural component supports tangled_rope classification; high internalized component raises snare risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Whether suppression is structural (institutional asymmetry) or internalized (normalized acceptance).').

omega_variable(
    kernel_reading_contest_ambiguity,
    'Is this reading (functional accommodation) a genuinely stable third position, or is it a rhetorical cover for the underlying executive dominance (inherent reading) that Congress has internalized as normal?',
    'Critical textual analysis: do executive officials invoke functional accommodation language when defending unilateral actions, or do they invoke inherent authority? Do congressional voices defend functional accommodation as a positive constitutional principle, or do they invoke it only defensively (accepting it because congressional assertion costs more than non-assertion)? A stable reading should generate positive endorsement from both branches; a cover story generates endorsement only from beneficiaries.',
    'If functional accommodation is a stable equilibrium, the constraint is a defensible tangled_rope with genuine coordination (rapid response to genuine threats) and asymmetry (executive gain in the gray zone). If it is a rhetorical cover for executive dominance, the constraint is closer to snare — the appearance of a negotiated boundary masks actual executive control. This omega documents the reading-level contestation between the functional accommodation reading and the inherent_executive_reading (which would classify the same arrangement as snare from the inherent perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'Whether functional accommodation is a stable constitutional reading or a rhetorical cover for executive dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(war__tr_t5, war_powers_allocation__functional_accommodation_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__functional_accommodation_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(war__tr_t15, war_powers_allocation__functional_accommodation_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__functional_accommodation_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement(war__tr_t25, war_powers_allocation__functional_accommodation_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__functional_accommodation_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(war__be_t5, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(war__be_t15, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(war__be_t25, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(war__su_t5, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(war__su_t15, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(war__su_t25, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__functional_accommodation_reading, 0.18).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__inherent_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the war_powers_allocation kernel. The congressional_primacy_reading and inherent_executive_reading are sibling readings instantiated as separate constraints. All three share the same underlying constitutional text (Congress declares war; President is commander-in-chief) but interpret it differently. The functional_accommodation_reading coexists with both siblings in contemporary political discourse. See kernel_context in commentary for the structural relationships between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
