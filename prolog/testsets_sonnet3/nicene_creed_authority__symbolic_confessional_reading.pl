% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__symbolic_confessional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: nicene_creed_authority__symbolic_confessional_reading
 *   human_readable: Nicene Creed as Historically Contingent Witness (Symbolic-Confessional Reading)
 *   domain: religious/theological
 *
 * SUMMARY:
 *   This story instantiates the symbolic-confessional reading of the Nicene
 *   Creed kernel: the creed is understood as a historically contingent act of
 *   communal witness, formed at Nicaea (325) and Constantinople (381) under
 *   specific political and philosophical conditions, whose ongoing authority
 *   derives from continued community discernment and personal faith rather
 *   than from an externally-guaranteed fixed ontology. Under this reading,
 *   extraction is low and the authority topology inverts relative to the
 *   strict-orthodox sibling reading: local congregations, ecumenical
 *   partners, and individual believers are net beneficiaries of the
 *   interpretive latitude this reading opens, while centralized magisterial
 *   authorities lose the disciplinary leverage a binding-ontology reading
 *   would give them. This is one reading among three of a single contested
 *   kernel (nicene_creed_authority); the strict_orthodox_reading and
 *   liturgical_habituation_reading are separate constraint stories with their
 *   own ε values and stakeholder structures, linked here via
 *   network.affects_constraints, not folded into this one.
 *
 * KEY AGENTS:
 *   - local_congregations: primary beneficiary (moderate/mobile) — gain interpretive latitude
 *   - progressive_theologians: beneficiary and agenda-setter (organized/mobile) — supply the historical-critical framing
 *   - centralized_magisterial_authorities: primary payer (institutional/constrained) — lose disciplinary leverage over doctrinal deviation
 *   - individual_believers: beneficiary and secondary payer (powerless/mobile) — gain latitude, lose external certainty
 *   - historians_of_doctrine: analytical observer — supplies the contingency evidence this reading rests on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__symbolic_confessional_reading, 0.18).
domain_priors:suppression_score(nicene_creed_authority__symbolic_confessional_reading, 0.15).
domain_priors:theater_ratio(nicene_creed_authority__symbolic_confessional_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__symbolic_confessional_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__symbolic_confessional_reading, "Nicene Creed as Historically Contingent Witness (Symbolic-Confessional Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__symbolic_confessional_reading, "religious/theological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__symbolic_confessional_reading, 'de457f55-d87e-4564-951a-9e830eb50aba').
narrative_ontology:cs_kernel_codification('de457f55-d87e-4564-951a-9e830eb50aba', fixed_text).
narrative_ontology:cs_authority_grounding('de457f55-d87e-4564-951a-9e830eb50aba', practice).
narrative_ontology:cs_interpretation_layer_present('de457f55-d87e-4564-951a-9e830eb50aba').
narrative_ontology:cs_reading_relation('de457f55-d87e-4564-951a-9e830eb50aba', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('de457f55-d87e-4564-951a-9e830eb50aba', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('de457f55-d87e-4564-951a-9e830eb50aba', foundational, authority_located_in_communal_discernment).
narrative_ontology:cs_axiom_status(authority_located_in_communal_discernment, holdable).
narrative_ontology:cs_axiom_grounding('de457f55-d87e-4564-951a-9e830eb50aba', authority_located_in_communal_discernment, conventional).
narrative_ontology:cs_axiom('de457f55-d87e-4564-951a-9e830eb50aba', foundational, creedal_content_is_historically_situated_witness).
narrative_ontology:cs_axiom_status(creedal_content_is_historically_situated_witness, holdable).
narrative_ontology:cs_axiom_grounding('de457f55-d87e-4564-951a-9e830eb50aba', creedal_content_is_historically_situated_witness, empirically_contingent).
narrative_ontology:cs_reference_frame('de457f55-d87e-4564-951a-9e830eb50aba', conciliar_witness_as_communal_testimony).
narrative_ontology:cs_drift_state('de457f55-d87e-4564-951a-9e830eb50aba', post_historical_critical_scholarship_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('de457f55-d87e-4564-951a-9e830eb50aba', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, local_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, ecumenical_dialogue_partners).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, interfaith_practitioners).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, centralized_magisterial_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, individual_believers).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, individual_believers).
narrative_ontology:constraint_vindicates(nicene_creed_authority__symbolic_confessional_reading, communal_discernment_as_locus_of_authority).
narrative_ontology:constraint_vindicates(nicene_creed_authority__symbolic_confessional_reading, historically_situated_confession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite the creed as an act of shared memory and communal identity, not as a metaphysical test administered by an external body. They interpret its clauses through their own discernment process, adapt emphasis to context, and treat disagreement over particular clauses as a matter for ongoing conversation rather than discipline. Exit from any single interpretive tradition is available through denominational mobility or local reinterpretation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, local_congregations, beneficiary,
    moderate, generational, mobile, regional).

% Write and teach a historical-critical account of the creed's formation at Nicaea and Constantinople, framing it as a product of fourth-century political and philosophical context rather than timeless revelation. They benefit from the interpretive space this reading opens for doctrinal development and are not answerable to a centralized tribunal for departing from strict ontological readings.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, progressive_theologians, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__symbolic_confessional_reading, progressive_theologians, agenda_setter).

% Use the creed as a shared symbolic touchstone across denominational lines precisely because its authority is read as witness rather than binding metaphysical dictate; this lowers the stakes of doctrinal difference and enables cooperative worship and dialogue that a strict-orthodox reading would foreclose.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, ecumenical_dialogue_partners, beneficiary,
    moderate, generational, mobile, global).

% Engage with the creed's language as historically and culturally embedded testimony that can be held alongside, or in respectful tension with, other traditions' truth claims, without the creed functioning as an exclusionary boundary that forecloses interfaith conversation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, interfaith_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Rely on the creed functioning as a fixed, binding ontological standard to adjudicate heresy and maintain doctrinal uniformity across a global institution. Under this reading, their disciplinary and teaching authority is structurally weakened: they cannot compel assent to a single metaphysical construal when the creed's authority is relocated to community discernment and personal faith. Their exit from this reading is constrained by the fact that abandoning the claim of binding authority undercuts their own institutional warrant.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, centralized_magisterial_authorities, payer,
    institutional, civilizational, constrained, global).

% Confess the creed as an act of personal faith formed in community rather than as submission to an external metaphysical verdict. They gain interpretive latitude and reduced risk of disciplinary sanction for doubt or reinterpretation, but also lose the stabilizing certainty that a fixed, externally guaranteed doctrinal anchor can provide in moments of crisis or grief.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, individual_believers, beneficiary,
    powerless, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__symbolic_confessional_reading, individual_believers, payer).

% Study the councils of Nicaea (325) and Constantinople (381) as contingent political-theological events shaped by imperial politics, factional dispute, and specific philosophical vocabularies available at the time. Their scholarship is a primary evidentiary basis for the historically-contingent-witness reading, though they do not themselves adjudicate which reading a worshipping community should adopt.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__symbolic_confessional_reading, diffuse).
narrative_ontology:fixing_cost_class(nicene_creed_authority__symbolic_confessional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared confessional vocabulary that lets historically and culturally diverse Christian communities recognize one another as participants in a common tradition, without requiring uniform metaphysical agreement on every clause's precise ontological content.
% TRANSFER_FUNCTION: Moves interpretive authority away from centralized magisterial bodies and toward local congregations and individual conscience; what is transferred is not money but the power to authoritatively fix doctrinal meaning and to discipline deviation.
% ABSENT_VOICES: Strict-orthodox clergy and councils who hold that the creed's authority is precisely its fixed, binding ontological content would object that this reading dissolves the creed's disciplinary function into sentiment; they are represented in the sibling reading (strict_orthodox_reading), not as excluded stakeholders here, since their position is a rival kernel-reading rather than a silenced voice within this one.
% DISAPPEARANCE_RATIONALE: If the symbolic-confessional reading disappeared and every community reverted to treating the creed as an externally-adjudicated binding ontology, ecumenical cooperation would narrow sharply, interfaith dialogue premised on shared symbolic witness would lose its warrant, and local congregational discernment would be displaced by centralized doctrinal policing — a substantial rearrangement of contemporary ecumenical and interfaith practice.
% FOUNDING_PROBLEM: The historical-critical study of the fourth-century councils, combined with modern pluralism and ecumenical necessity, created pressure to explain how the creed can remain meaningful and unifying without functioning as a coercive metaphysical test in a religiously plural, historically self-aware age.
% FOUNDING_PROBLEM_CORROBORATION: Attested by historians of doctrine (an outside, non-beneficiary scholarly community) whose historical-critical account of Nicaea and Constantinople as contingent, politically-shaped events is the evidentiary basis this reading draws on; also corroborated by ecumenical bodies documenting reduced doctrinal conflict where the creed is read as shared witness rather than binding tribunal standard.
narrative_ontology:disappearance_verdict(nicene_creed_authority__symbolic_confessional_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__symbolic_confessional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__symbolic_confessional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.18-0.28 across the interval) because this reading's own structure removes the coercive mechanism that would extract compliance from dissenting believers or local communities — there is no tribunal collecting doctrinal conformity as a rent. Suppression is correspondingly low (0.15): the reading explicitly rejects sanction as the mechanism of creedal authority. Theater ratio is moderate (0.28-0.35) because recitation persists as communal liturgical practice even where the underlying claim to binding metaphysical assent has been relocated to discernment — some performative recitation continues independent of the doctrinal function, but it is not rising over time in this reading (a mild downward drift as the confessional framing further settles into ecumenical and academic practice over the interval).
 *
 * PERSPECTIVAL GAP:
 *   From the local-congregation and individual-believer seats, this arrangement reads as low-cost, low-coercion communal coordination — closer to a rope than any form of extraction. From the centralized-magisterial-authority seat, the same arrangement reads as an erosion of the disciplinary function their institutional authority depends on; they experience this reading itself as a cost, since it displaces their capacity to adjudicate heresy. The engine should compute a genuine seat divergence here: beneficiary seats see coordination, the payer seat sees an authority loss it did not choose.
 *
 * DIRECTIONALITY LOGIC:
 *   Local congregations, progressive theologians, ecumenical partners, interfaith practitioners, and individual believers are declared beneficiaries because the symbolic-confessional reading removes external compulsion and returns interpretive authority to their own discernment — this pushes their derived directionality toward the low-d beneficiary end. Centralized magisterial authorities are declared the sole victim/payer group because their institutional warrant depends on the creed functioning as an externally fixed, binding standard; this reading's own logic strips that warrant, pushing their derived directionality toward the high-d target end despite their nominal institutional power. This is the inverted-topology structural delta the reading is defined by: power and directionality run in opposite directions for the magisterial seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling creedal continuity with historical self-awareness and religious pluralism) is authored as live, not dead — historical-critical scholarship and ongoing ecumenical practice continue to generate the pressure this reading answers. This blocks a mandatrophy misreading in either direction: the reading is not a dead mandate being defended by inertia (it answers an active problem), and it is not being falsely credited with solving a problem that vanished long ago.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the Nicene Creed''s authority genuinely locatable in community discernment and personal faith, or does the symbolic-confessional reading itself represent a modern reinterpretation that the historical councils would not have recognized as legitimate?',
    'Comparative historical analysis of fourth-century conciliar self-understanding (did the bishops at Nicaea and Constantinople understand themselves as issuing a binding ontological ruling or as articulating a communal witness statement?) against the reception history of the creed across subsequent centuries and traditions.',
    'If the historical councils clearly intended a binding ontological ruling, this reading is better understood as a modern reconstruction responding to contemporary pluralism rather than a recovery of original intent — which would not change this story''s authored ε (which is about the reading''s own operation now) but would sharpen the omega for the sibling strict_orthodox_reading''s claim to historical priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the symbolic-confessional reading recovers or reconstructs the creed''s original authority claim.').

omega_variable(
    authority_relocation_stability,
    'Can communal discernment and personal faith sustain doctrinal coherence over civilizational timescales without any centralized adjudicating mechanism, or does the inverted authority topology this reading proposes tend to drift toward fragmentation that eventually re-invites centralized control?',
    'Longitudinal study of denominational traditions that have historically operated with congregational or discernment-based authority structures (e.g., certain Reformed, Anabaptist, or Quaker communities) to observe whether doctrinal coherence persists or fragments over multi-generational timescales.',
    'If fragmentation reliably follows, this reading''s low-extraction, low-suppression profile may be a transitional state rather than a stable equilibrium, with implications for how the reading should be weighted against the liturgical_habituation_reading as a more durable alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_relocation_stability, empirical, 'Whether discernment-based creedal authority is a stable long-run equilibrium or a transitional state.').

omega_variable(
    magisterial_victim_framing,
    'Is it accurate to describe centralized magisterial authorities as ''victims'' of this reading, or do they retain sufficient independent institutional power (financial, legal, cultural) that the loss of creedal disciplinary leverage is better described as a minor cost than a genuine extraction?',
    'Assess whether magisterial institutions'' actual disciplinary and financial power depends materially on creedal-authority claims specifically, versus other independent sources of institutional authority (property, canon law, historical prestige).',
    'If magisterial power is largely independent of creedal-authority claims, the payer/victim framing for this seat should be softened toward a more symmetric directionality; if creedal authority is load-bearing for magisterial power, the current high-d framing is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_victim_framing, empirical, 'Whether centralized authorities'' loss under this reading is a genuine extraction or a minor incidental cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__symbolic_confessional_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nice_tr_t10, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(nice_tr_t20, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(nice_tr_t30, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(nice_tr_t40, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(nice_tr_t50, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(nice_tr_t60, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nice_be_t10, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(nice_be_t20, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(nice_be_t30, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(nice_be_t40, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(nice_be_t50, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(nice_be_t60, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 60, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nicene_creed_authority__symbolic_confessional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__symbolic_confessional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__symbolic_confessional_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the nicene_creed_authority kernel. strict_orthodox_reading authors high extraction and a conventional authority topology (centralized bodies as beneficiaries, dissenting believers as victims); liturgical_habituation_reading authors a performative/behavioral authority claim independent of metaphysical assent; this story (symbolic_confessional_reading) authors low extraction and an inverted topology (local congregations as beneficiaries, centralized authorities as payers). Each carries its own ε and stakeholder structure per the ε-invariance principle; they are linked via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
