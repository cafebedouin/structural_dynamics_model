% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Feudal Oath as Bounded Reciprocal Coordination (Vassal-Coordination Reading)
 *   domain: medieval_political_economy/legal_history
 *
 * SUMMARY:
 *   This story instantiates the vassal-coordination reading of the feudal
 *   oath kernel: the charter fixes reciprocal obligations between lord and
 *   vassal in writing, and a peer court of fellow vassals gives those fixed
 *   terms real bite against either party's overreach. Under this reading the
 *   oath is a coordination mechanism — it solves a genuine mutual-commitment
 *   problem (unreliable service on one side, arbitrary demand on the other)
 *   with mutual enforceability and no structural victim WITHIN the
 *   lord-vassal relationship itself. This is deliberately a different
 *   constraint from the lord-extraction reading (which treats the same oath
 *   as authorizing maximal extraction bounded only by vassal capacity) and
 *   the ecclesiastical-mediation reading (which treats Christian charity
 *   obligations as the actual limiting mechanism). All three share the kernel
 *   — the feudal oath as an institution — but each reading assigns a
 *   different epsilon, a different enforcement mechanism, and a different
 *   beneficiary/victim structure, per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - enfeoffed_vassals: primary party, both beneficiary and bound payer of service
 *   - liege_lords: primary party, both beneficiary and bound payer of protection/tenure
 *   - peer_court_of_vassals: enforcement mechanism, observer/adjudicator seat
 *   - unfree_peasantry: excluded from the oath entirely, structurally below the coordination this reading describes
 *   - regional_peace_and_order: non-agent beneficiary condition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.28).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.22).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath as Bounded Reciprocal Coordination (Vassal-Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy/legal_history").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, 'd0e2c2ef-8b07-4329-8f66-f49e58665a85').
narrative_ontology:cs_kernel_codification('d0e2c2ef-8b07-4329-8f66-f49e58665a85', fixed_text).
narrative_ontology:cs_authority_grounding('d0e2c2ef-8b07-4329-8f66-f49e58665a85', lineage).
narrative_ontology:cs_interpretation_layer_present('d0e2c2ef-8b07-4329-8f66-f49e58665a85').
narrative_ontology:cs_reading_relation('d0e2c2ef-8b07-4329-8f66-f49e58665a85', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0e2c2ef-8b07-4329-8f66-f49e58665a85', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('d0e2c2ef-8b07-4329-8f66-f49e58665a85', foundational, charter_terms_bind_both_parties_symmetrically).
narrative_ontology:cs_axiom_status(charter_terms_bind_both_parties_symmetrically, holdable).
narrative_ontology:cs_axiom_grounding('d0e2c2ef-8b07-4329-8f66-f49e58665a85', charter_terms_bind_both_parties_symmetrically, conventional).
narrative_ontology:cs_axiom('d0e2c2ef-8b07-4329-8f66-f49e58665a85', foundational, peer_court_judgment_is_the_operative_enforcement_mechanism).
narrative_ontology:cs_axiom_status(peer_court_judgment_is_the_operative_enforcement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('d0e2c2ef-8b07-4329-8f66-f49e58665a85', peer_court_judgment_is_the_operative_enforcement_mechanism, conventional).
narrative_ontology:cs_reference_frame('d0e2c2ef-8b07-4329-8f66-f49e58665a85', bilateral_charter_bound_tenure).
narrative_ontology:cs_drift_state('d0e2c2ef-8b07-4329-8f66-f49e58665a85', late_medieval_commutation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d0e2c2ef-8b07-4329-8f66-f49e58665a85', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, liege_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, regional_peace_and_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds land and title in exchange for defined military service (a fixed number of knights or days of service per year), council attendance, and specified aids (ransom, eldest son's knighting, eldest daughter's marriage). The charter fixes these obligations in writing so the lord cannot demand more than the instrument specifies. In exchange the vassal gets security of tenure, protection, and a court of peers to adjudicate disputes over whether the lord has overreached. Exit from a specific lord is difficult (land and lineage are tied to the tenure) but the bound obligations themselves are the source of protection against arbitrary demands, not merely a cost.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals, payer).

% Grants land and protection in exchange for reliable, predictable military and administrative service without needing to maintain a standing army or bureaucracy. The lord is bound by the same charter: demanding service beyond its fixed terms is a breach that releases the vassal from the oath and can trigger coalition resistance from the vassal's peers. The lord's exit from a specific vassal relationship is also constrained by the same instrument — escheat and forfeiture require due process under the charter, not unilateral seizure.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, liege_lords, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, liege_lords, beneficiary).

% A stable, non-agent condition that emerges when the reciprocal obligation is honored on both sides: reduced armed dispute over undefined obligations, predictable mobilization for defense, and a peer-adjudication mechanism instead of private war. Represents the coordination surplus the oath structure is claimed to produce for the region as a whole.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, regional_peace_and_order, beneficiary,
    analytical, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(feudal_oath_reciprocity__vassal_coordination_reading, regional_peace_and_order).

% Fellow vassals of the same lord who sit in judgment when a vassal alleges the lord has exceeded the charter's bounds, or when a lord alleges a vassal has failed a specified duty. Their judgment is what gives the charter's fixed terms real enforceability against either party rather than leaving the lord as sole judge of the vassal's compliance.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, peer_court_of_vassals, observer,
    organized, biographical, analytical, regional).

% Works the land underlying the fief but is not a party to the oath at all — the reciprocal charter obligations run between lord and vassal, not between either of them and the unfree tenantry who actually produce the surplus that funds the arrangement. Their labor obligations are governed by manorial custom, a separate and much less bounded constraint, and they have no seat in the peer court that enforces the oath's limits.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, unfree_peasantry, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__vassal_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__vassal_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an otherwise unbounded and constantly renegotiated relationship of military protection for land-tenure into a fixed, written, mutually legible schedule of obligations — solving the problem of a lord needing predictable mobilization without a standing army, and a vassal needing security against arbitrary escalating demands.
% TRANSFER_FUNCTION: Moves defined military service, court attendance, and specified feudal aids from vassal to lord; moves land tenure, protection, and adjudicated limits on demand from lord to vassal. Within THIS reading the transfer is bilateral and bounded by the charter text on both sides, not a one-way extraction.
% ABSENT_VOICES: The unfree peasantry who work the underlying land are not parties to the oath and have no standing in the peer court that enforces its bounds — their own obligations are governed by manorial custom, a separate and far less bounded arrangement. They would object that the 'reciprocity' this reading describes is a compact among the propertied and armed, financed by labor that is not party to it.
% DISAPPEARANCE_RATIONALE: If the charter-bound reciprocal schedule vanished, lords would revert to ad hoc, unilateral demands for service and aid, vassals would lose the peer-court mechanism for contesting overreach, and the predictable mobilization that substitutes for a standing army would break down — private feuding and renegotiation-by-force would likely rise, which is the condition the charter form is credited with suppressing.
% FOUNDING_PROBLEM: Early medieval land-for-service arrangements were verbal, personal, and unbounded, leaving vassals exposed to escalating demands and lords exposed to unreliable, unenforceable promises of service; the written charter fixed both sides' obligations so either could be held to account by a peer tribunal rather than by force alone.
% FOUNDING_PROBLEM_CORROBORATION: Charter texts and peer-court judgment rolls (where they survive) corroborate that vassals successfully invoked fixed terms against lords who demanded extra service, which is evidence from outside either party's self-interested framing. However, the ecclesiastical courts and later royal justices — genuinely external observers — increasingly treat the same charters as instruments that entrenched lordly power over the peasantry beneath the fief, suggesting the founding problem (mutual predictability among the propertied) was real but coexisted from the start with an unaddressed extraction problem below it.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.28) because within this reading's own lights the obligations run both ways and are fixed by the charter text rather than open-ended — a lord who demands beyond the specified service is in breach, not merely ungenerous. Suppression is authored low-moderate (0.22) because the primary enforcement mechanism is peer adjudication and reputational sanction (a lord who over-demands loses vassal cooperation and standing) rather than unilateral coercion. Theater ratio is low (0.15) because the charter's terms are substantively enforced through the peer court, not merely performed. accessibility_collapse is moderate (0.35), not near a mountain's ceiling, because alternative arrangements (allodial tenure, direct royal service, monetary commutation) existed and were occasionally used, and resistance is moderate (0.3) reflecting periodic vassal revolt when lords tested the bounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Both lord and vassal are declared beneficiaries because, under this specific reading, the charter's fixed terms genuinely constrain each party's demands on the other — this is what makes it a coordination mechanism rather than a one-directional extraction. Neither seat carries the 'victim' declaration in this story; the excluded unfree peasantry are not victims of THIS constraint (the lord-vassal oath) because they are not party to it at all — they are excluded voices, structurally outside the reciprocal relationship the oath governs, which is why base_properties.victims is empty here even though the broader manorial economy plainly has victims (that belongs to a different constraint about manorial labor obligations, not this one).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (predictable, bounded mutual obligation instead of unbounded verbal promise) is treated as contested rather than flatly live or dead: charter enforcement evidence (peer-court rolls) corroborates the coordination function persisted in practice for centuries, which is why founding_problem_status is 'contested' rather than 'dead' — this reading does not claim the arrangement degraded into pure extraction, which is exactly the claim the lord_extraction_reading sibling makes about the SAME oath text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_text_vs_lived_practice_gap,
    'Did the written charter''s fixed terms actually bind lords in practice, or did lords retain de facto capacity to escalate demands that the charter and peer court could not effectively check?',
    'Comparative study of surviving charter texts against surviving peer-court judgment rolls and chronicled disputes: a high rate of successful vassal challenges to lordly overreach supports the coordination reading; a low rate or systematic vassal non-recourse supports the lord-extraction sibling reading.',
    'If lords systematically evaded the charter''s bounds with impunity, this reading''s low epsilon is wrong and the constraint''s true operation is closer to the lord_extraction_reading sibling — the same textual artifact would then be evidence for a different constraint, not a different measurement of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_text_vs_lived_practice_gap, empirical, 'Whether charter enforceability was real or nominal in practice.').

omega_variable(
    peasant_exclusion_as_boundary_or_denial,
    'Is excluding the unfree peasantry from the oath''s reciprocity a legitimate scope boundary (the oath genuinely only governs the lord-vassal relationship) or a denial that hides the peasantry''s role as the actual source of the surplus being reciprocally allocated?',
    'Trace whether manorial extraction from the peasantry increased in years when lord-vassal service demands rose, which would indicate the ''bounded'' lord-vassal reciprocity was subsidized by unbounded pressure passed downward rather than absorbed by the lord.',
    'If lordly compliance with the charter''s vassal-facing limits was financed by increased extraction from the peasantry, the low epsilon of THIS reading is locally accurate but structurally incomplete — a fuller accounting would require linking this constraint to a manorial-labor constraint via network.affects_constraints rather than folding the peasantry into this story''s victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_exclusion_as_boundary_or_denial, conceptual, 'Whether the peasantry''s exclusion from this reading is a legitimate scope decision or a hidden cost transfer.').

omega_variable(
    reading_selection_disagreement,
    'Which of the three kernel readings (vassal-coordination, lord-extraction, ecclesiastical-mediation) best describes the actual dominant mechanism of enforcement across the feudal period, and does the answer vary by region and century?',
    'Regional and period-specific comparative legal history: Norman England''s strong royal courts, French regional variation in seigneurial power, and the differing strength of ecclesiastical courts across dioceses would each shift which reading''s mechanism was dominant.',
    'This story assumes the peer-court/charter mechanism was operative and non-trivial; if regional evidence shows peer courts were rarely convened or routinely overridden, the vassal_coordination_reading''s applicability narrows to specific times and places rather than the institution as a whole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_disagreement, conceptual, 'Regional and temporal scope of applicability for this reading versus its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t60, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(feud_tr_t120, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 120, 0.12).
narrative_ontology:measurement(feud_tr_t180, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 180, 0.13).
narrative_ontology:measurement(feud_tr_t240, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 240, 0.14).
narrative_ontology:measurement(feud_tr_t300, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 300, 0.15).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(feud_be_t60, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(feud_be_t120, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 120, 0.26).
narrative_ontology:measurement(feud_be_t180, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 180, 0.27).
narrative_ontology:measurement(feud_be_t240, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 240, 0.28).
narrative_ontology:measurement(feud_be_t300, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 300, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(feud_su_t60, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(feud_su_t120, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 120, 0.21).
narrative_ontology:measurement(feud_su_t180, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 180, 0.21).
narrative_ontology:measurement(feud_su_t240, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 240, 0.22).
narrative_ontology:measurement(feud_su_t300, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 300, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the feudal_oath_reciprocity kernel, each authored as a separate constraint story per the epsilon-invariance principle: vassal_coordination_reading (this file, low epsilon, rope), lord_extraction_reading (high epsilon, snare/tangled_rope, vassal as structural victim), and ecclesiastical_mediation_reading (moderate epsilon, tangled_rope, Church as mediating authority with its own extraction interest). The three share the same underlying institutional kernel — the feudal oath — but assign different mechanisms, different beneficiary/victim structures, and different epsilon values, reflecting genuinely contested historiographical readings rather than measurement noise on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
