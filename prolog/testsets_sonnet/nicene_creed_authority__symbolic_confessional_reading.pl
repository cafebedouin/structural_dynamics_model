% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__symbolic_confessional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__symbolic_confessional_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: nicene_creed_authority__symbolic_confessional_reading
 *   human_readable: Nicene Creed as Historically Contingent Symbolic Witness
 *   domain: systematic_theology/ecclesiology/history_of_doctrine
 *
 * SUMMARY:
 *   This story instantiates the symbolic-confessional reading of the Nicene
 *   Creed kernel: the Creed is a historically contingent theological witness,
 *   produced by a fourth-century ecumenical council responding to specific
 *   christological controversies, whose ongoing authority derives not from
 *   binding metaphysical precision enforced by a hierarchy but from continued
 *   communal discernment and the personal faith of believers who find it a
 *   faithful (if time-bound) articulation of shared conviction. Under this
 *   reading, congregations and individual believers retain interpretive
 *   latitude — the Creed functions as a shared touchstone for worship and
 *   ecumenical identity rather than a court-enforceable metaphysical ruling.
 *   This is structurally distinct from the strict_orthodox_reading (which
 *   treats deviation as heresy warranting sanction) and the
 *   liturgical_habituation_reading (which brackets cognitive assent entirely
 *   in favor of performative identity-marking). All three are readings of the
 *   same kernel — the persisting text and conciliar act of 325/381 CE — but
 *   they instantiate different authority structures with different ε
 *   profiles, different beneficiaries, and different victim sets. This story
 *   authors only the symbolic-confessional reading; the siblings are separate
 *   constraints linked via cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - local_congregations: primary beneficiary (organized/mobile) — exercise interpretive latitude, retain the Creed as a devotional and ecumenical resource without binding metaphysical liability
 *   - lay_theologians: beneficiary (moderate/mobile) — free to engage the Creed's historical context, propose reinterpretations, participate in constructive theology without institutional sanction
 *   - centralized_magisterial_authorities: structural victim of this reading (institutional/constrained) — lose the enforcement leverage that binding subscription would provide; their historical claim to adjudicate orthodoxy is relativized
 *   - ecumenical_dialogue_partners: beneficiary (organized/mobile) — the Creed's status as shared historical witness (rather than exclusive metaphysical claim of one tradition) enables cross-denominational and interfaith engagement
 *   - systematic_theologians: observer (analytical/analytical) — study how creedal authority structures shift across historical periods and denominational traditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__symbolic_confessional_reading, 0.18).
domain_priors:suppression_score(nicene_creed_authority__symbolic_confessional_reading, 0.12).
domain_priors:theater_ratio(nicene_creed_authority__symbolic_confessional_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__symbolic_confessional_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__symbolic_confessional_reading, "Nicene Creed as Historically Contingent Symbolic Witness").
narrative_ontology:topic_domain(nicene_creed_authority__symbolic_confessional_reading, "systematic_theology/ecclesiology/history_of_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__symbolic_confessional_reading, '8bb6e276-ed42-474b-9ad2-314d05489376').
narrative_ontology:cs_kernel_codification('8bb6e276-ed42-474b-9ad2-314d05489376', fixed_text).
narrative_ontology:cs_authority_grounding('8bb6e276-ed42-474b-9ad2-314d05489376', distributed).
narrative_ontology:cs_reading_relation('8bb6e276-ed42-474b-9ad2-314d05489376', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('8bb6e276-ed42-474b-9ad2-314d05489376', nicene_creed_authority__liturgical_habituation_reading, influences).
narrative_ontology:cs_axiom('8bb6e276-ed42-474b-9ad2-314d05489376', foundational, creedal_authority_is_historically_contingent).
narrative_ontology:cs_axiom_status(creedal_authority_is_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('8bb6e276-ed42-474b-9ad2-314d05489376', creedal_authority_is_historically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('8bb6e276-ed42-474b-9ad2-314d05489376', foundational, communal_discernment_and_personal_faith_ground_authority_not_hierarchy).
narrative_ontology:cs_axiom_status(communal_discernment_and_personal_faith_ground_authority_not_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('8bb6e276-ed42-474b-9ad2-314d05489376', communal_discernment_and_personal_faith_ground_authority_not_hierarchy, conventional).
narrative_ontology:cs_reference_frame('8bb6e276-ed42-474b-9ad2-314d05489376', conciliar_metaphysical_settlement).
narrative_ontology:cs_drift_state('8bb6e276-ed42-474b-9ad2-314d05489376', post_historical_critical_scholarship_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('8bb6e276-ed42-474b-9ad2-314d05489376', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, local_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, lay_theologians).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, ecumenical_dialogue_partners).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_participants).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, centralized_magisterial_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite the Creed in worship and treat it as a shared historical witness to the faith of the early church, while retaining latitude to interpret its metaphysical claims non-literally or as time-bound articulations. They are not threatened with sanction for holding nuanced or minority positions on the Trinity or Incarnation, and can engage other traditions without treating the Creed as an exclusive litmus test.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, local_congregations, beneficiary,
    organized, generational, mobile, regional).

% Write, teach, and publish constructive theology that engages the Creed's history and context rather than treating its propositions as settled metaphysics. Under this reading they face no institutional censure for exploring revisionist Christologies or historicist readings of Nicaea; their professional and vocational standing does not depend on subscription to the Creed's precise ontological claims.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, lay_theologians, beneficiary,
    moderate, biographical, mobile, national).

% Historically claimed the authority to adjudicate orthodoxy and sanction deviation from the Creed's metaphysical content. Under the symbolic-confessional reading, this adjudicating function is relativized: doctrinal minorities and reinterpretations proceed without requiring their sanction. They bear the cost of this reading in the currency of institutional authority and enforcement relevance, not money — their teaching office loses the leverage that binding subscription standards would otherwise supply, and they cannot easily 'exit' the situation since the reading's spread is not something they control.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, centralized_magisterial_authorities, payer,
    institutional, civilizational, constrained, global).

% Denominational bodies and interfaith councils that use the Creed's historical, shared-witness status as common ground for dialogue across traditions that would otherwise be foreclosed by exclusive metaphysical subscription claims. The symbolic reading is precisely what enables their work — a strict-orthodox reading held by all parties would make such dialogue structurally impossible.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, ecumenical_dialogue_partners, beneficiary,
    organized, generational, mobile, global).

% Hold that the Creed states binding, non-negotiable metaphysical truths and that treating it as merely symbolic witness constitutes a departure from apostolic faith. They are not consulted as authoritative within congregations that adopt the symbolic-confessional reading, and would object that this reading dissolves the very thing the councils fought to secure — a determinate, sanctionable orthodoxy.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, strict_orthodox_adherents, excluded,
    organized, civilizational, constrained, global).

% Study the historical development of creedal authority across periods and traditions, documenting how different communities have variously treated the Creed as binding ontology, liturgical boundary marker, or historically contingent witness, without themselves adjudicating which reading is correct.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, systematic_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared historical touchstone that lets diverse congregations, denominations, and theological traditions recognize continuity with the fourth-century church's Trinitarian and Christological discernment without requiring uniform metaphysical assent — enabling worship, ecumenical dialogue, and interfaith engagement that a binding-subscription reading would foreclose.
% TRANSFER_FUNCTION: Moves interpretive authority away from centralized magisterial bodies (who would otherwise adjudicate orthodoxy and threaten sanction) toward local congregations and individual believers, who gain latitude at the structural expense of the teaching office's enforcement leverage.
% ABSENT_VOICES: Strict orthodox adherents and centralized magisterial authorities would object that this reading dissolves the determinate content the councils intended to secure, reducing a metaphysical ruling to sentiment; they are present in the broader ecclesial conversation but are not the authoritative voice within congregations and denominations that have adopted the symbolic-confessional reading.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the Creed reverted universally to binding subscription, ecumenical and interfaith dialogue infrastructure built on shared-witness framing would need to be rebuilt on other grounds, and congregations currently holding non-literal interpretations would face renewed pressure toward conformity or exit. Whether this counts as the world 'rearranging' is genuinely contested between the beneficiary seats (who would say a great deal changes) and observers who note that many congregations already function this way informally regardless of official doctrine, so formal disappearance of the reading might change little in practice.
% FOUNDING_PROBLEM: The Creed itself was built to resolve a specific fourth-century crisis: competing claims about the nature of Christ and the Trinity threatened to fracture the early church, and a council was convened to articulate a common confession. The symbolic-confessional reading of the Creed's authority was built later, to solve a different, second-order problem: how communities holding the Creed as inherited tradition could remain in fellowship despite doctrinal diversity, historical-critical awareness of the council's contingent political and philosophical context, and the practical impossibility of enforcing uniform metaphysical assent across modern pluralistic societies.
% FOUNDING_PROBLEM_CORROBORATION: Historians of doctrine and comparative religion scholars outside any single confessional tradition attest that pluralistic, multi-denominational societies genuinely require some non-coercive account of shared symbols to sustain interfaith and ecumenical cooperation; sociologists of religion studying congregational life corroborate that formal subscription requirements are widely unenforceable in practice regardless of official doctrine. Centralized magisterial authorities dispute that this is the Creed's true purpose, but their dissent is exactly the disagreement the reading exists to manage, not independent corroboration against it.
narrative_ontology:disappearance_verdict(nicene_creed_authority__symbolic_confessional_reading, contested).
narrative_ontology:founding_problem_status(nicene_creed_authority__symbolic_confessional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__symbolic_confessional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__symbolic_confessional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__symbolic_confessional_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__symbolic_confessional_reading_tests).
:- end_tests(nicene_creed_authority__symbolic_confessional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end, declining from 0.30 at the reading's point of articulation) because this reading structurally divests the Creed of coercive enforcement machinery — no ecclesial court, no excommunication apparatus, no career-ending heresy trial rides on this reading's authority claim. Suppression is correspondingly low and falling (0.30 → 0.12): the reading explicitly denies that deviation from precise metaphysical assent warrants sanction, so the structural suppression apparatus that would enforce compliance is absent by the reading's own premise. Theater ratio rises modestly (0.10 → 0.22) reflecting that liturgical recitation persists in many communities holding this reading even as its binding force is denied — a mild but real gap between continued performative use and diminished propositional authority. Accessibility collapse is low (0.25): alternative theological framings remain genuinely available and are not foreclosed by this reading. Resistance is moderate (0.35): centralized authorities and strict-orthodox adherents actively contest this reading's legitimacy, which is exactly what one would expect from a reading that reallocates authority away from them.
 *
 * PERSPECTIVAL GAP:
 *   From the congregational/lay seat, this reading looks like liberation — genuine coordination around shared historical witness with no coercive residue. From the magisterial-authority seat, the same reading looks like erosion of a legitimate teaching function, an abdication that permits doctrinal drift the council fathers intended to foreclose. The engine should compute these divergently: low χ for the beneficiary seats, and for the magisterial seat a directionality closer to the target end despite that seat's high nominal power — because what is extracted from them is not resource but authority itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are local congregations, lay theologians, and ecumenical/interfaith partners — d sits low for these seats because the reading removes coercive costs from their position and grants them interpretive latitude they would not have under strict subscription. The victim seat is centralized magisterial authority — not because they are coerced by this reading, but because the reading's diffusion structurally displaces the leverage that binding subscription would have given them; their capacity to adjudicate orthodoxy and threaten sanction is the thing this reading structurally erodes. This is an inverted topology relative to the strict_orthodox sibling, where centralized authority is the primary institutional beneficiary and dissenting believers are the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in a specific way: because it explicitly denies that the founding conciliar problem (settling one metaphysically precise, sanctionable orthodoxy) remains its own operative purpose, it cannot be accused of persisting past its function while still claiming that function's authority. It reframes the founding problem itself as historically bounded — the fourth-century Trinitarian and Christological crisis was real and specific, and the Creed's continuing value is witness to how that community discerned truth, not an ongoing sanctioning mandate. Where it must be watched is the omega on stability: does this framing represent honest historical humility, or is it a way-station toward the Creed becoming religiously inert (theater_ratio creep already visible in the measurement series)?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_reading_stability_under_institutional_pressure,
    'Can a symbolic-confessional reading of the Creed persist as a stable ecclesial practice, or does it structurally drift toward either re-absorption by magisterial authority (strict orthodox reading) or toward liturgical performance without propositional content (liturgical habituation reading)?',
    'Longitudinal study of denominations that formally adopt symbolic/non-literal creedal subscription (e.g. some Reformed, Anglican, and liberal Protestant bodies) — track whether creedal recitation persists, is dropped, or reverts to binding subscription standards over multiple generations.',
    'If the reading proves unstable and reliably collapses into one of the sibling readings, this constraint''s low-extraction profile may be a transitional snapshot rather than a durable equilibrium; if stable, it supports the reading as a genuine independent kernel-instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_reading_stability_under_institutional_pressure, empirical, 'Whether symbolic-confessional creedal authority is a stable attractor or a way-station between the other two readings.').

omega_variable(
    community_discernment_vs_individual_dissent,
    'When ''community discernment'' is invoked as the ground of authority, whose community, and does this merely relocate coercive pressure from a centralized hierarchy to a local congregational majority rather than eliminating it?',
    'Case studies of congregations exercising creedal discernment where a minority theological view is present — examine whether the minority experiences genuine latitude or informal social sanction functionally equivalent to excommunication.',
    'If local discernment reproduces coercive dynamics at smaller scale, the inverted beneficiary/victim topology (congregations as beneficiaries, central authority as victim) is incomplete — congregational minorities could re-emerge as victims within the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_discernment_vs_individual_dissent, conceptual, 'Whether decentralizing creedal authority to congregations eliminates coercion or merely relocates it.').

omega_variable(
    historicity_claim_vs_devotional_function,
    'Does treating the Creed as ''historically contingent witness'' remain religiously load-bearing (a confession genuinely shaping worship and identity) or does the historicity framing hollow the Creed into a cultural-historical artifact with no operative theological authority at all?',
    'Compare liturgical retention rates and confessional self-description across congregations holding the symbolic reading versus congregations that have formally dropped creedal recitation altogether.',
    'If symbolic framing reliably precedes abandonment, the reading may be a way-station toward disappearance of the constraint rather than a stable alternative authority structure — relevant to the founding_problem_status assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historicity_claim_vs_devotional_function, conceptual, 'Whether the symbolic reading retains genuine authority-function or drifts toward creedal irrelevance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__symbolic_confessional_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nice_tr_t30, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(nice_tr_t60, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement(nice_tr_t90, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 90, 0.19).
narrative_ontology:measurement(nice_tr_t120, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 120, 0.21).
narrative_ontology:measurement(nice_tr_t150, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 150, 0.22).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nice_be_t30, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement(nice_be_t60, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement(nice_be_t90, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 90, 0.19).
narrative_ontology:measurement(nice_be_t120, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 120, 0.18).
narrative_ontology:measurement(nice_be_t150, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 150, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(nice_su_t30, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 30, 0.24).
narrative_ontology:measurement(nice_su_t60, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 60, 0.19).
narrative_ontology:measurement(nice_su_t90, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 90, 0.15).
narrative_ontology:measurement(nice_su_t120, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 120, 0.13).
narrative_ontology:measurement(nice_su_t150, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 150, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__symbolic_confessional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__symbolic_confessional_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the nicene_creed_authority kernel. The three readings share a kernel (the conciliar text and act of 325/381 CE) but instantiate structurally distinct constraints with different ε values, different beneficiary/victim topologies, and different enforcement profiles. strict_orthodox_reading has high suppression and centralized-authority-as-beneficiary; liturgical_habituation_reading has moderate suppression concentrated on performative/boundary compliance rather than propositional assent; symbolic_confessional_reading (this story) has the lowest extraction and an inverted topology where local congregations benefit and centralized authority is structurally displaced. Do not average these into one ε — per DP-001 ε-invariance, each is a separate constraint linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
