% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Mourning-Ritual as Persecution-Survival Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint isolates one reading of the catastrophe_memory_kernel:
 *   mourning-practice ritual functions as a transmission mechanism for
 *   operational persecution-survival competence — mutual-aid activation
 *   patterns, early-warning recognition, rapid mobilization logistics —
 *   rehearsed across generations through liturgical cycles so that the
 *   community does not have to relearn crisis response from scratch after
 *   long intervals of relative safety. As the founding threat recedes into
 *   historical memory but the ritual apparatus persists and even intensifies
 *   its boundary-policing function over time, the same practices that
 *   preserve competence also generate assimilation-pressure costs borne by
 *   members who would otherwise integrate more fully with the host society.
 *   This is a distinct constraint from sibling readings of the same kernel:
 *   the symbol_continuity_reading concerns identity persistence independent
 *   of any operational function, the trauma_encoding_reading concerns the
 *   ritual as a warning-signal system keyed to trauma memory rather than
 *   trained competence, and the boundary_maintenance_reading treats the
 *   boundary-policing function as the primary purpose rather than as a
 *   side-cost of competence transmission. Each has its own epsilon and its
 *   own stakeholder structure; they should not be averaged together.
 *
 * KEY AGENTS:
 *   - diaspora_community_under_threat: primary beneficiary (organized/constrained) — draws real crisis-preparedness value from the ritual cycle
 *   - communal_leadership_bodies: agenda-setter (institutional/identity_locked) — administers and enforces the ritual calendar, cannot exit without dissolving their institutional role
 *   - assimilation_inclined_members: payer (moderate/constrained) — bears social-standing costs disproportionate to their actual persecution-risk profile
 *   - exogamous_families: payer (powerless/trapped) — used as boundary-maintenance object lesson, denied full standing
 *   - surrounding_host_society: excluded (powerful/analytical) — the anticipated threat source, has no voice in the community's self-narration
 *   - religious_studies_observer: analytical seat — distinguishes the competence-transmission function from adjacent kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Mourning-Ritual as Persecution-Survival Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, 'c6543e94-db8a-49ea-9d49-cc836022dc14').
narrative_ontology:cs_kernel_codification('c6543e94-db8a-49ea-9d49-cc836022dc14', distributed).
narrative_ontology:cs_authority_grounding('c6543e94-db8a-49ea-9d49-cc836022dc14', practice).
narrative_ontology:cs_interpretation_layer_present('c6543e94-db8a-49ea-9d49-cc836022dc14').
narrative_ontology:cs_reading_relation('c6543e94-db8a-49ea-9d49-cc836022dc14', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6543e94-db8a-49ea-9d49-cc836022dc14', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_reading_relation('c6543e94-db8a-49ea-9d49-cc836022dc14', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('c6543e94-db8a-49ea-9d49-cc836022dc14', foundational, ritual_as_trained_operational_skill).
narrative_ontology:cs_axiom_status(ritual_as_trained_operational_skill, holdable).
narrative_ontology:cs_axiom_grounding('c6543e94-db8a-49ea-9d49-cc836022dc14', ritual_as_trained_operational_skill, instrumental).
narrative_ontology:cs_axiom('c6543e94-db8a-49ea-9d49-cc836022dc14', secondary, boundary_cost_is_side_effect_not_purpose).
narrative_ontology:cs_axiom_status(boundary_cost_is_side_effect_not_purpose, holdable).
narrative_ontology:cs_axiom_grounding('c6543e94-db8a-49ea-9d49-cc836022dc14', boundary_cost_is_side_effect_not_purpose, conventional).
narrative_ontology:cs_reference_frame('c6543e94-db8a-49ea-9d49-cc836022dc14', rehearsed_mutual_aid_infrastructure).
narrative_ontology:cs_drift_state('c6543e94-db8a-49ea-9d49-cc836022dc14', contemporary_reduced_acute_threat_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6543e94-db8a-49ea-9d49-cc836022dc14', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, diaspora_community_under_threat).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, communal_leadership_bodies).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_inclined_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, exogamous_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in structured mourning cycles (fast days, liturgical recitations of past persecutions, ritualized recounting of expulsion and pogrom narratives) that function as rehearsed contingency drills: what to do when trust networks collapse, how to move assets quickly, whom to appeal to, how to recognize early warning signs of escalating hostility. The community draws real operational readiness from this, at the cost of maintaining a demanding ritual calendar and internal cohesion machinery.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, diaspora_community_under_threat, beneficiary,
    organized, generational, constrained, regional).

% Designs, schedules, and enforces the ritual calendar; adjudicates who participates fully and who is treated as marginal. Their institutional authority and social standing are constituted through administering this competence-transmission function, which makes their own exit from the arrangement functionally impossible without dissolving their role.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, communal_leadership_bodies, agenda_setter,
    institutional, generational, identity_locked, regional).

% Wish to reduce ritual observance, intermarry, or integrate more fully into the surrounding society. They bear the disciplinary cost of the boundary-maintenance apparatus riding alongside the survival-competence training — social pressure, reduced standing, sometimes exclusion from communal support networks — even though their own persecution-risk profile may not differ from full participants.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilation_inclined_members, payer,
    moderate, biographical, constrained, local).

% Households formed across the community boundary are treated as a transmission failure risk by leadership: children are sometimes denied full ritual standing, communal resources are withheld, and reintegration paths are narrow. Their situation is used within the community as an object lesson reinforcing why the mourning-cycle discipline matters, converting their exclusion into pedagogical material for others.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, exogamous_families, payer,
    powerless, biographical, trapped, local).

% Is the object of the community's threat-modeling and, at times, the actual source of the persecution the rituals train against. Has no voice in how the community narrates or ritualizes that history, and the community's survival-training function is partly built around anticipating this society's future hostility.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, surrounding_host_society, excluded,
    powerful, biographical, analytical, regional).

% Studies the mourning-ritual complex comparatively across communities and eras, distinguishing the operational-competence function from adjacent functions (identity continuity, trauma signaling, boundary policing) that ride on the same practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, religious_studies_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual calendar rehearses persecution-response patterns — flight logistics, mutual-aid activation, recognition of escalating hostility signals, rapid trust-network mobilization — preserving operational competence that would otherwise decay between crises separated by generations.
% TRANSFER_FUNCTION: Moves social standing, resources, and belonging-security from members who deviate from full observance (through intermarriage or reduced participation) toward the communal apparatus that administers and narrates the mourning cycle; it also moves genuine risk-preparedness from individual improvisation to collectively rehearsed, transmissible competence.
% ABSENT_VOICES: The surrounding host society, whose historical and potential future conduct the rituals are calibrated against, has no say in how it is characterized or remembered; exogamous families and their children, treated as cautionary examples, are structurally present but denied a voice in adjudicating the boundary rules used against them.
% DISAPPEARANCE_RATIONALE: Leadership and many long-tenured members would say the world rearranges catastrophically — competence atrophies, the community becomes naive to warning signs it once caught early. Assimilation-inclined members and exogamous families would say the world is substantially unchanged for them personally, since the apparatus currently costs them more than it protects them; some would say it actively improves for them. The verdict genuinely depends on which seat is asked.
% FOUNDING_PROBLEM: Repeated historical episodes of sudden, severe persecution (expulsion, pogrom, forced conversion) in which communities that lacked rehearsed response patterns suffered disproportionately worse outcomes than communities with existing mutual-aid and rapid-mobilization infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Historians of persecution episodes and comparative sociologists of diaspora communities corroborate that communities with rehearsed mutual-aid infrastructure had measurably better crisis outcomes in several documented historical cases — this is attested outside the beneficiary group. Whether the CURRENT threat environment still requires this level of rehearsed readiness is disputed even among community members themselves, with no external corroborating consensus either way.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) reflecting a real, non-trivial coordination function (rehearsed crisis competence) coexisting with growing extraction from members who bear boundary-maintenance costs without proportional threat exposure. Suppression is moderate (0.38) — social and institutional pressure rather than physical coercion, but real enough to constrain exit for exogamous families and identity-lock leadership. Theater ratio rises across the interval (0.12 to 0.30) as the founding persecution threat recedes in immediacy relative to the ritual apparatus's continued intensity, consistent with a drift toward performative maintenance of boundary functions once decoupled from acute crisis-response need. All three metrics share one time grid across six points.
 *
 * PERSPECTIVAL GAP:
 *   From the community's collective seat, the arrangement reads as vital, hard-won operational wisdom — a rope of genuine coordination. From the exogamous-family and assimilation-inclined seats, the same structure computes as extractive: they pay boundary-maintenance costs calibrated to a threat model they may not share, administered by leadership whose own standing depends on the apparatus persisting. This divergence is exactly what the tangled_rope classification is built to hold — both readings are structurally correct from their respective seats.
 *
 * DIRECTIONALITY LOGIC:
 *   communal_leadership_bodies and diaspora_community_under_threat sit toward the beneficiary end: they receive transmitted competence and, for leadership, institutional standing, at relatively low personal marginal cost. assimilation_inclined_members and exogamous_families sit toward the target end: they bear the disciplinary and exclusionary costs of the same apparatus with constrained or trapped exit, and their situations are used pedagogically to reinforce compliance in others, which is itself a further extraction. surrounding_host_society is excluded rather than positioned on the beneficiary/victim axis at all — it is the anticipated external threat, not a participant in the internal transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than dead, which matters: unlike a pure mandatrophy case, this constraint's coordination function has not cleanly outlived its use — historical corroboration from outside the community establishes that rehearsed infrastructure genuinely improved crisis outcomes in the past, and no external consensus establishes the current threat environment as fully resolved. This blocks a simple 'ritual is now pure legacy extraction' verdict; the tangled_rope reading — real coordination value plus a genuinely rising extraction/boundary-cost component — is the more defensible structural claim than either a clean rope or a clean snare reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_decay_vs_active_transmission,
    'Is the operational competence genuinely being transmitted and refreshed by current ritual practice, or has the practical skill component atrophied while the ritual form persists as pure symbolic performance?',
    'Compare crisis-response outcomes in communities with intact ritual observance against communities with lapsed observance during a documented contemporary threat event; also assess whether ritual content still includes actionable procedural knowledge (contact networks, asset-mobility practices) versus purely liturgical/symbolic recitation.',
    'If competence has substantially atrophied, this reading collapses toward the symbol_continuity_reading or a piton classification; if competence transmission remains functionally active, the tangled_rope classification with moderate extraction is the accurate structural reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_vs_active_transmission, empirical, 'Whether trained operational competence is still actively transmitted or has decayed into symbolic form.').

omega_variable(
    threat_environment_currency,
    'Does the persecution risk the ritual trains against still exist at a level that justifies the current intensity of boundary-maintenance enforcement, or has the risk substantially receded such that the enforcement now outruns its justifying threat?',
    'Independent (non-community) historical and sociological assessment of contemporary persecution risk faced by the specific diaspora population, compared against the intensity of boundary-maintenance sanctions currently imposed on deviating members.',
    'A substantially receded threat environment combined with intensifying enforcement supports reclassification toward snare (extraction with a decayed coordination justification); a persistent threat environment supports the tangled_rope reading as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threat_environment_currency, empirical, 'Whether current enforcement intensity is proportionate to current persecution risk.').

omega_variable(
    reading_boundary_with_boundary_maintenance,
    'Is the boundary-maintenance cost genuinely a side-effect of competence transmission (this reading''s claim), or is competence transmission actually a cover story for a boundary-maintenance function that is the real underlying purpose (the sibling boundary_maintenance_reading''s claim)?',
    'Examine whether ritual content and enforcement intensity scale with actual external threat indicators (supporting this reading) or with internal assimilation/intermarriage rates independent of external threat (supporting the sibling reading).',
    'If enforcement tracks internal assimilation rates rather than external threat levels, the primary function is better modeled by the boundary_maintenance_reading and this story''s beneficiary framing (community resilience) would need to be substantially revised downward in emphasis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_with_boundary_maintenance, conceptual, 'Which of two sibling kernel readings better describes the dominant driver of enforcement intensity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 80, 0.36).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the catastrophe_memory_kernel, each authored as a separate ε-invariant constraint per the decomposition principle. survival_competence_reading (this story) claims moderate, rising extractiveness (0.42) tied to a genuine but eroding operational-coordination function. symbol_continuity_reading is expected to show lower extractiveness tied to identity-preservation rather than trained skill. trauma_encoding_reading is expected to center on intergenerational affect-transmission rather than procedural competence. boundary_maintenance_reading is expected to show the highest extractiveness/suppression, treating group-boundary policing as the primary function rather than a side-cost. All four link to each other via affects_constraints since they share the same underlying ritual practices and beneficiary/administrator seats even though their claimed functions and metrics differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
