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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Catastrophe Memory Function: Mourning Practice and Boundary Maintenance Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This reading instantiates the 'mourning-practice' interpretation of the
 *   catastrophe-memory kernel, exemplified by Tisha B'Av observance in Jewish
 *   tradition. The ritual is understood PRIMARILY as a structured occasion
 *   for collective grief-work and boundary-maintenance: it affirms group
 *   identity through synchronized mourning, legitimizes the community's role
 *   as custodian of shared loss, and reintegrates bereaved kinship groups
 *   into communal standing. The reading does NOT foreground
 *   survival-competence transmission or adaptive institutional learning —
 *   those are the differential content of the hybrid and survival-competence
 *   siblings. This reading's ε is moderate (0.42) because the ritual
 *   coordinates a genuine collective problem (preserving identity across
 *   dispersal) with low coercive overhead, but extraction is present: the
 *   cost of participation is distributed (emotional labor, time), the benefit
 *   of boundary-maintenance accrues disproportionately to organized
 *   community-keepers whose legitimacy depends on custodianship, and the
 *   ritual's performance structures who counts as 'in' and who is excluded.
 *   Theater is low (0.15) — the ritual's performative function is its primary
 *   purpose, not a cover for something else — and the constraint operates
 *   with modest suppression (0.28) because exit is theoretically available
 *   but identity-fused: you can stop participating, but the cost is
 *   experienced as self-excommunication rather than external coercion.
 *
 * KEY AGENTS:
 *   - Community identity maintainers (organized, generational horizon, identity-locked) — custodians of ritual and tradition, beneficiaries of role legitimacy
 *   - Bereaved kinship groups (moderate power, biographical horizon, identity-locked) — direct loss-bearers, receive grief-recognition and reintegration
 *   - Ritual participants (moderate power, biographical horizon, constrained exit) — bear time/emotional cost, receive identity affirmation
 *   - Non-participants (moderate power, biographical horizon, constrained exit) — structurally absent voices, experience drift or rejection
 *   - External scholarly observer (analytical, civilizational horizon) — understands but does not perform the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Catastrophe Memory Function: Mourning Practice and Boundary Maintenance Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'a103819a-ee62-4151-aa46-fded3437fbd3').
narrative_ontology:cs_kernel_codification('a103819a-ee62-4151-aa46-fded3437fbd3', distributed).
narrative_ontology:cs_authority_grounding('a103819a-ee62-4151-aa46-fded3437fbd3', lineage).
narrative_ontology:cs_interpretation_layer_present('a103819a-ee62-4151-aa46-fded3437fbd3').
narrative_ontology:cs_reading_relation('a103819a-ee62-4151-aa46-fded3437fbd3', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a103819a-ee62-4151-aa46-fded3437fbd3', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('a103819a-ee62-4151-aa46-fded3437fbd3', foundational, mourning_practice_is_primary_function).
narrative_ontology:cs_axiom_status(mourning_practice_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('a103819a-ee62-4151-aa46-fded3437fbd3', mourning_practice_is_primary_function, deontological).
narrative_ontology:cs_axiom('a103819a-ee62-4151-aa46-fded3437fbd3', foundational, boundary_maintenance_constitutes_survival).
narrative_ontology:cs_axiom_status(boundary_maintenance_constitutes_survival, holdable).
narrative_ontology:cs_axiom_grounding('a103819a-ee62-4151-aa46-fded3437fbd3', boundary_maintenance_constitutes_survival, deontological).
narrative_ontology:cs_reference_frame('a103819a-ee62-4151-aa46-fded3437fbd3', synchronized_collective_grief_as_identity_anchor).
narrative_ontology:cs_drift_state('a103819a-ee62-4151-aa46-fded3437fbd3', contemporary_secular_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a103819a-ee62-4151-aa46-fded3437fbd3', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, community_identity_maintainers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, bereaved_kinship_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, ritual_participants).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, historical_dispersed_diaspora).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, ritual_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, commemorative_obligation_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, boundary_maintenance_through_memory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized bodies (rabbinic courts, community councils, kinship leadership networks) who determine when and how the commemorative ritual occurs, teach its meanings, and enforce its observance norms. They collect legitimacy and authority from their role as custodians of collective memory and group identity. Their power derives from control over the ritual's execution and interpretation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, community_identity_maintainers, agenda_setter,
    organized, generational, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, community_identity_maintainers, beneficiary).

% Extended families and kinship networks who have experienced direct loss (death of ancestors, destruction of homes, displacement from territories). The ritual provides a sanctioned channel for grief expression and a temporal container for mourning that reintegrates them into community standing. Participation affirms that their loss is collectively recognized.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, bereaved_kinship_groups, beneficiary,
    moderate, biographical, identity_locked, local).

% Community members (both bereaved and non-bereaved) who attend and perform the commemorative observance. They invest time, emotional labor, and cognitive attention in synchronized performance. They receive the benefit of identity affirmation, belonging to a community that persists through memory, and shared witness to collective loss. Exit is available but costly at the identity level.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritual_participants, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, ritual_participants, beneficiary).

% Individuals nominally part of the community who do not participate (secular members, those geographically distant, those who reject the ritual's theological frame, families that have assimilated). They are absent from the conversation the ritual enacts; their non-participation is read as boundary violation. They have no designated voice in how group memory is constructed or what the ritual means.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, non_participating_members, excluded,
    moderate, biographical, constrained, local).

% Scattered communities across geographies and centuries who participate in synchronized ritual observance without shared institutional structure. The ritual's persistence across dispersal is the primary mechanism through which they experience themselves as a single, continuous people. They cannot exit without dissolving their connection to collective identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, historical_dispersed_diaspora, beneficiary,
    powerless, civilizational, trapped, global).

% Historians, anthropologists, and religious studies scholars who document and analyze the ritual's function in maintaining community identity across dispersal and generational time. They take no role in execution or community adjudication; their position is external understanding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, scholarly_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, community_identity_maintainers).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a synchronized, recurring (typically annual) collective occasion in which the community gathers to mark shared catastrophic loss, reaffirms bonds of kinship and group membership, and renews the symbolic boundaries that distinguish 'us' (the remembering community) from 'them' (those outside the covenant of memory). Solves the coordination problem: how does a dispersed community maintain shared identity across generations when the original catastrophic event is no longer in living memory and material continuity (territory, institutions, family structures) has been severed?
% TRANSFER_FUNCTION: Moves emotional labor, time, and cognitive attention from individuals and families into collective, synchronized performance. Bereaved kinship groups invest grief-work; the community receives continuity of identity and boundary-maintenance. Organized community-keepers receive legitimacy and authority from their role as custodians. The ritual transfers acknowledgment-of-loss from private to collective domain.
% ABSENT_VOICES: Those who have drifted from community observance (secular members, diaspora who cannot attend, those who reject the ritual's theological framing) are structurally absent. Their potential counter-claim — that ritual is non-functional, anachronistic, that memory could be preserved through other means (institutional history, secular memorialization, scientific study of the catastrophe) — is not part of the ritual's internal logic. Younger generations raised outside the community and those who experience the ritual as re-traumatization rather than healing have no designated seat.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared, the community would immediately face reorganization: kinship groups would grieve privately; the organized community's role as meaning-keeper would erode; generational transmission of 'who we are' would fragment into family stories, institutional archives, and individual memory. The community might reorganize around alternative identity markers (language, law, theology, territory-if-recovered), but the specific mechanism for synchronized boundary-maintenance across dispersal would require active replacement. Diaspora communities especially would lose their primary mechanism for experiencing themselves as one continuous people.
% FOUNDING_PROBLEM: In the aftermath of catastrophic loss (destruction, displacement, genocide, systemic violence), survivors and their descendants face the problem of preserving group identity, kinship bonds, and community continuity across separation, generational time, and the gradual fading of direct memory. How does a community that has been fragmented by loss stay 'us' rather than dissolving into separate families, assimilating into surrounding populations, or developing entirely separate post-loss identities?
% FOUNDING_PROBLEM_CORROBORATION: Historians of diaspora (Salo Baron, David Biale, Jonathan Sarna) and contemporary genocide studies scholars document that historical survival of group identity across dispersal and time has depended on synchronized commemorative practice — without it, communities fragmented and assimilated. Contemporary participants attest that discontinuation of the ritual would erode community bonds. Secular members and younger generations outside the community attest that without the ritual structure, they experience themselves as drifting from group identity. Non-academic historians and genealogists tracking diaspora communities document ritual observance as the primary mechanism through which they experience continuity.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-low because the constraint solves a real coordination problem (group identity preservation across dispersal) with minimal external coercion — participation appears voluntary and internally motivated. However, the measurement series is flat at 0.42 across the 50-year interval because the extraction structure does not accumulate: the ritual's cost structure remains stable; the organized community's extraction of legitimacy does not compound; and the identity-lock that binds participants operates consistently across generations. Theater is deliberately low (0.15 at all time points) because, under this reading, the ritual IS the primary function — there is no gap between what it performs and what it is supposed to do. The ritual's meaning is its mourning, not a proxy for something else. Suppression is low (0.28) because exit is available (you can stop attending) but costly at the identity level — the constraint operates through internalized boundary-maintenance, not external enforcement. The measurement values remain flat because this reading does not posit historical drift in the ritual's core function: each generation inherits the same boundary-maintenance problem and the same ritual structure.
 *
 * PERSPECTIVAL GAP:
 *   The keeper seat experiences the ritual as coordination it administers and that voluntary participants sustain. The bereaved-participant seat experiences it as a structured obligation that reintegrates them but whose cost is internalized. The keeper computed as rope; the bereaved participant computed as tangled-rope (coordination + extraction). This divergence is exactly what the framework is designed to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Community-identity-maintainers are beneficiaries with organized/institutional power and arbitrage-level exit — they collect legitimacy from custodianship and can reframe the ritual if participation flags. Directionality near beneficiary end (d ~ 0.20–0.35). Bereaved-kinship-groups are also beneficiaries but with moderate power and identity-locked exit — they receive grief-recognition and reintegration, but the cost of non-participation is identity-dissolution. Directionality symmetric-to-moderate (d ~ 0.45–0.55). Ritual-participants are mixed: they are listed as beneficiary + payer (dual role), moderate power, constrained exit. They receive identity affirmation but bear time and emotional cost. Directionality near symmetric (d ~ 0.50–0.60). Non-participants are excluded and structurally absent, so no directionality is computed — they have no authored role. The systematic difference between keeper and participant exit (arbitrage vs. identity-locked) drives the per-seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem (preserving group identity across dispersal and generational time) remains live. The ritual persists because it still solves that problem. There is no mandatrophy: the constraint's original justification has not outlived its function. The ritual is not a zombie — it is actively maintained because the coordination problem it solves has not been solved any other way. An alternative reading (survival-competence) might argue that the founding problem has shifted from 'preserve identity' to 'transmit adaptive capacity,' and under THAT reading, the ritual's function is partially obsolete. But under THIS reading (mourning-practice), the founding problem is as live as it was 2,000 years ago, and the ritual's persistence is functionally justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mourning_vs_survival_function_boundary,
    'Is the primary function of the ritual mourning-practice and identity-maintenance (D1/D4), or does it equally serve as transmission mechanism for adaptive institutional knowledge (D5)?',
    'Ethnographic observation of participant self-understanding: what do practitioners say they are doing and why? Content analysis of ritual text and teaching: what is emphasized — grief-work and boundary-norms, or adaptive strategies? Comparison with post-catastrophe communities that abandon the ritual — do they lose identity-cohesion only, or do they also lose institutional resilience?',
    'If mourning-practice is primary, the constraint classifies as rope (coordinate grief + boundary-maintenance). If survival-competence transmission is equally primary, classification shifts toward tangled-rope or hybrid. If survival-competence is primary and mourning is secondary cover, classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_vs_survival_function_boundary, conceptual, 'Whether the ritual''s primary function is D1/D4 mourning/boundary-maintenance or D5 survival-competence transmission.').

omega_variable(
    identity_lock_mechanism,
    'Is the measured identity-lock (constrained exit for bereaved groups, identity-locked exit for community keepers) structural (economic or social barriers to leaving) or internalized (the participant''s self-concept is fused with group membership such that exit feels like identity-dissolution)?',
    'Post-exit trajectory analysis: if bereaved individuals who stop participating in the ritual report persistent identity-alienation long after exit, identity-lock is partially internalized. If they report relief and no persistent identity-cost, identity-lock was primarily structural. Comparative case: communities that deliberately de-emphasize the ritual — do participants report identity-drift or not?',
    'If identity-lock is primarily internalized, suppression is effectively higher than authored (0.28) — the constraint carries its enforcement with individuals across geographic and temporal boundaries. If identity-lock is structural, suppression is accurately measured. This affects directionality computation and potentially shifts seat classification from rope to tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock is structural or internalized (memory-carrying capacity of the constraint across exit).').

omega_variable(
    reading_foreclosure_contest,
    'Does this reading''s core premise (ritual IS boundary-maintenance, not primarily survival-competence transmission) logically foreclose the survival-competence reading''s core premise?',
    'Logical analysis: can a single party coherently hold both claims? A party could say ''the ritual''s primary function is mourning/boundary-norms, but it also incidentally transmits some adaptive knowledge.'' That party coexists-with the hybrid reading. A party could not say ''the ritual is primarily survival-competence'' AND ''the ritual is primarily mourning-practice'' in the same breath — but they can hold these commitments serially or in different contexts. Foreclosure is rare and requires that premise A directly contradicts premise B such that no framework could hold both.',
    'If reading_relations shows forecloses, the two readings are live only across different parties or frameworks. If coexists_with, the readings remain live even within a single party''s account (they are emphasizing different aspects of a multifunctional constraint). If influences, this reading creates downstream pressure on the sibling reading''s legitimacy without logically eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_contest, conceptual, 'Logical structure of the mourning-practice vs. survival-competence contrast.').

omega_variable(
    non_participant_structural_voice,
    'Are non-participants (secular members, those alienated by the ritual''s theological frame, diaspora unable to attend) a genuine absence of voice, or are they represented through alternative channels (secular memory practices, institutional history, counter-narratives)?',
    'Ethnographic documentation: do non-participants have organizing alternatives? Do they contest the ritual''s legitimacy in community discourse, or are they simply silent? Institutional analysis: do secular memory organizations, institutional archives, or diaspora networks offer competing meaning-making about the same catastrophe?',
    'If non-participants are truly voiceless, the ritual''s boundary-maintenance function carries closure/suppression elements not captured in the 0.28 suppression measurement. If they have alternative channels, the ritual is one among several competing commemorative framings. This affects the accessibility_collapse measurement (0.72) — are alternatives collapsed for participants, or are they simply less salient?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_participant_structural_voice, empirical, 'Whether non-participants are genuinely absent or have unacknowledged alternative voice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(cata_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(cata_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(cata_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__mourning_practice_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe-memory kernel. The kernel is the persisting commitment: commemorative ritual preserves something essential after catastrophic loss. This reading emphasizes the ritual's primary function as mourning-practice and boundary-maintenance (D1/D4). The survival-competence reading emphasizes adaptive institutional knowledge transmission (D5). The hybrid-transformation reading emphasizes both D1/D4 and D5. All three share the same kernel (commemorative ritual post-catastrophe) but instantiate different constraint structures via different reading emphasis. Network links reflect epistemic upstreams: this reading influences both siblings because boundary-maintenance is foundational to any community continuity claim; the survival-competence reading influences this one because if adaptive transmission is primary, mourning-practice becomes secondary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
