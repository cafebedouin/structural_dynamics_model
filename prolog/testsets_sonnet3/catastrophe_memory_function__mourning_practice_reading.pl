% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Tisha B'Av as Mourning-Practice / Boundary-Norm Preservation (D1/D4 reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This story authors the mourning_practice_reading of the
 *   catastrophe_memory_function kernel around Tisha B'Av: the observance's
 *   payload is D1/D4 — mourning-practice preservation and boundary-norm
 *   maintenance — and nothing more. On this reading, the fast's function is
 *   to keep loss legible across generations and to mark communal membership
 *   through shared ritual obligation; it does NOT, on this reading, transmit
 *   adaptive survival competence (that is a distinct claim, authored
 *   separately as the survival_competence_reading, and a third story,
 *   hybrid_transformation_reading, claims the two payloads co-occur). This
 *   story's ε (0.28) is authored specifically for the
 *   mourning/boundary-maintenance arrangement as this reading sees it —
 *   low-moderate extraction, since the primary cost borne is
 *   psychological/social discomfort and reduced individual autonomy, not
 *   material transfer, and most participants regard the arrangement as a
 *   benefit rather than an imposition.
 *
 * KEY AGENTS:
 *   - observant_community: primary participant/beneficiary (organized/constrained) — bears ritual cost, receives identity-continuity benefit
 *   - rabbinic_authorities: agenda_setter (institutional/identity_locked) — sets and interprets observance content
 *   - boundary_dissenting_members: payer (moderate/constrained) — bears social cost of the boundary function without endorsing it
 *   - assimilation_inclined_descendants: payer (powerless/mobile) — bears relational cost of the D1/D4 payload as a brake on integration
 *   - ritual_theorists: analytical observer — evaluates whether the ritual's payload is pure boundary-maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Tisha B'Av as Mourning-Practice / Boundary-Norm Preservation (D1/D4 reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '6e279a5c-c02b-4a79-905a-9c77361c1fb5').
narrative_ontology:cs_kernel_codification('6e279a5c-c02b-4a79-905a-9c77361c1fb5', fixed_text).
narrative_ontology:cs_authority_grounding('6e279a5c-c02b-4a79-905a-9c77361c1fb5', lineage).
narrative_ontology:cs_interpretation_layer_present('6e279a5c-c02b-4a79-905a-9c77361c1fb5').
narrative_ontology:cs_reading_relation('6e279a5c-c02b-4a79-905a-9c77361c1fb5', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e279a5c-c02b-4a79-905a-9c77361c1fb5', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('6e279a5c-c02b-4a79-905a-9c77361c1fb5', foundational, ritual_payload_is_commemorative_not_adaptive).
narrative_ontology:cs_axiom_status(ritual_payload_is_commemorative_not_adaptive, holdable).
narrative_ontology:cs_axiom_grounding('6e279a5c-c02b-4a79-905a-9c77361c1fb5', ritual_payload_is_commemorative_not_adaptive, conventional).
narrative_ontology:cs_axiom('6e279a5c-c02b-4a79-905a-9c77361c1fb5', secondary, boundary_maintenance_is_intrinsic_to_mourning_obligation).
narrative_ontology:cs_axiom_status(boundary_maintenance_is_intrinsic_to_mourning_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6e279a5c-c02b-4a79-905a-9c77361c1fb5', boundary_maintenance_is_intrinsic_to_mourning_obligation, conventional).
narrative_ontology:cs_reference_frame('6e279a5c-c02b-4a79-905a-9c77361c1fb5', rabbinic_post_temple_mourning_consolidation).
narrative_ontology:cs_drift_state('6e279a5c-c02b-4a79-905a-9c77361c1fb5', contemporary_diaspora, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6e279a5c-c02b-4a79-905a-9c77361c1fb5', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, observant_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, communal_identity_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, boundary_dissenting_members).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, assimilation_inclined_descendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Observes the fast, the liturgical readings of Lamentations, and the graduated mourning period leading up to the day. The practice binds the community to a shared loss-narrative (the Temples' destruction, expulsions, pogroms layered onto the same date) and to each other through synchronized ritual obligation. Members who keep the observance gain a reinforced sense of who belongs and what belonging requires; the cost is the discomfort of the fast and the emotional weight of annual re-immersion in catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, observant_community, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, observant_community, agenda_setter).

% Set and interpret the halakhic requirements of the fast (its restrictions, its liturgy, its exceptions), and have historically layered additional catastrophes onto the date, consolidating a diffuse set of communal traumas into one observance. Their institutional legitimacy depends in part on being the recognized interpreters of what the mourning day requires; exit from this role would mean abandoning the interpretive authority itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Not an actor but the collective good the ritual is oriented toward: a durable sense of shared peoplehood transmitted across generations and geographic dispersion, sustained by the fact that dispersed communities mourn the same day, the same texts, the same losses, at the same time.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, communal_identity_continuity, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_function__mourning_practice_reading, communal_identity_continuity).

% Individuals within observant communities who find the mourning obligation psychologically costly, theologically strained (mourning a Temple they do not wish restored, or a catastrophe narrative they find totalizing), or socially coercive. Exiting the observance risks visible communal censure and loss of standing; remaining means annual participation in a framework they experience as imposed rather than chosen.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, boundary_dissenting_members, payer,
    moderate, biographical, constrained, regional).

% Younger or more assimilated members who experience the boundary-maintenance function as a brake on integration into surrounding society. For them, the ritual's D1/D4 payload (this is what marks you as separate, this is what you owe the dead) reads as a cost imposed by inherited obligation rather than a benefit; they retain mobile exit (secularization, disaffiliation) but bear reputational or relational cost within family and community for taking it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, assimilation_inclined_descendants, payer,
    powerless, biographical, mobile, national).

% Scholars of collective memory and ritual studying whether Tisha B'Av functions as pure commemorative/boundary maintenance or carries additional transmitted content. This reading is their claim: the observance's payload is mourning-practice and identity-boundary maintenance, not adaptive survival instruction.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritual_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes a dispersed population's expression of collective grief and reinforces who is inside the mourning community and who is not, producing a durable, transmissible marker of shared peoplehood across geography and generations.
% TRANSFER_FUNCTION: Moves individual comfort, dietary/behavioral autonomy, and a day of ordinary functioning from each observant member to the collective good of demonstrated, synchronized group memory and boundary legibility; no material resources change hands.
% ABSENT_VOICES: Boundary-dissenting members and assimilation-inclined descendants are present in the community but structurally underweighted in setting the observance's terms — the content and obligations of the fast are set by rabbinic authority, not negotiated with those who bear the discomfort or the boundary cost most acutely.
% DISAPPEARANCE_RATIONALE: If the observance vanished overnight, the annual mechanism that re-synchronizes dispersed communities around a shared catastrophe-narrative and marks communal boundary would be gone; identity transmission would have to route through some other mechanism (which, per the sibling hybrid reading, may already be happening) or would attenuate. Communities structured around the fast (synagogue calendars, communal fast-breaking, liturgical cycles) would need to reorganize.
% FOUNDING_PROBLEM: A dispersed, repeatedly catastrophized population needed a mechanism to keep the memory of loss (Temple destruction, later layered catastrophes) from dissolving into forgetting, and needed a way to mark who remains bound to that memory as the population scattered and assimilated into diverse host societies.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists of religion and comparative-ritual scholars outside the observant tradition (studying diaspora identity maintenance generally) attest that dispersed populations without a periodic collective-mourning mechanism show measurably faster assimilation and boundary erosion; this corroborates that the founding problem — boundary and memory attrition under dispersion — remains structurally live, independent of the tradition's own theological framing.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-moderate (0.28) because the resource moved is not wealth or labor extracted upward to an elite but autonomy and comfort surrendered to a collective memory function most participants value. Suppression (0.42) reflects real but moderate social pressure against non-observance (censure, standing loss) rather than coercive enforcement — there is no formal enforcement mechanism (requires_active_enforcement is false), consistent with a rope rather than tangled_rope classification. Theater ratio (0.30) captures that some of the mourning performance (public displays of grief, communal fast-breaking rituals) has become somewhat routinized/performative relative to its original felt urgency, without dominating the function. Accessibility_collapse (0.50) is moderate: exit from full observance is possible (many maintain partial or symbolic observance, or disaffiliate) but carries real relational cost, so alternatives are constrained rather than foreclosed.
 *
 * DIRECTIONALITY LOGIC:
 *   Observant community members and rabbinic authorities sit near the beneficiary end: they receive the identity-continuity good the ritual produces and, in the rabbis' case, hold the interpretive authority that constitutes their institutional role. Boundary-dissenting members and assimilation-inclined descendants sit nearer the target end: they bear the ritual's psychological and social costs without proportionate benefit, and their exit options differ structurally — dissenters remain embedded (constrained exit, social cost of leaving is high) while assimilation-inclined descendants have more genuine mobility (mobile exit) but pay a relational price for using it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (dispersion-driven memory and boundary attrition) is authored as still live, corroborated by comparative-ritual scholarship outside the tradition — this blocks a premature 'purely inertial piton' read of the observance. Because the mourning function continues to do real work (as attested externally, not merely self-asserted by the observant community), classifying it as rope rather than piton or snare is structurally supported: the coordination good is genuine and the extraction is comparatively low, even though real, unevenly distributed costs exist for dissenting and assimilating members.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    d1d4_exclusivity_vs_hybrid_payload,
    'Does Tisha B''Av''s ritual structure carry only mourning-practice/boundary content (D1/D4), or does it also encode transmissible survival-competence content (D5) as the hybrid_transformation_reading claims?',
    'Close textual and practice analysis of the liturgy (Lamentations, kinnot) and associated communal practices for content that functions as adaptive/institutional-transformation instruction beyond loss-commemoration and boundary marking — e.g., does the post-Temple narrative structure encode decentralized-continuity strategies that this reading''s D1/D4-only framing would miss?',
    'If D5 content is present and functionally load-bearing, this story''s exclusivity claim is wrong and the hybrid_transformation_reading is the more accurate single account, changing the beneficiary/extraction profile (adaptive competence has different, likely lower, extraction than pure identity-boundary maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(d1d4_exclusivity_vs_hybrid_payload, conceptual, 'Whether the D1/D4-only framing is exhaustive or whether it strips out a real D5 component that belongs to a different, hybrid reading.').

omega_variable(
    boundary_function_natural_vs_constructed,
    'Is the boundary-maintenance function of the ritual an emergent byproduct of shared mourning practice, or is it actively constructed and maintained by rabbinic authority for institutional-continuity reasons?',
    'Historical analysis of how and when additional catastrophes were layered onto the Tisha B''Av date, and whether this layering was driven by organic communal memory-formation or by deliberate rabbinic consolidation decisions.',
    'If the boundary function is substantially rabbinically constructed rather than organically emergent, the extraction/beneficiary structure shifts toward the institutional agenda_setter seat more heavily than the low ε authored here suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_function_natural_vs_constructed, conceptual, 'Whether boundary-maintenance is emergent from shared grief or actively engineered by religious authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__mourning_practice_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the catastrophe_memory_function kernel applied to Tisha B'Av. mourning_practice_reading (this story) claims exclusive D1/D4 payload with ε=0.28. survival_competence_reading claims exclusive D5 payload (adaptive/institutional-transformation transmission) with its own independently authored ε. hybrid_transformation_reading claims both payloads co-occur (D1/D4 + D5) with a distinct ε reflecting the combined structure. Each reading is ε-invariant on its own terms; they are not averaged or reconciled into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
