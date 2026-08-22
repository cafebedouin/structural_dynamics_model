% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Legitimacy Climb from Institutional Fringes
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the endogenous climb reading of how new state
 *   commitments gain legitimacy: fringe actors (intellectuals, marginalized
 *   communities, reform advocates) develop superior institutional forms or
 *   normative commitments at the margins; these demonstrate practical
 *   superiority through pilot implementations, crisis response, or moral
 *   authority; gradual adoption spreads upward through institutional layers
 *   until apex institutions are compelled to adopt or lose legitimacy. The
 *   apex resists initially (protecting existing commitments) but eventually
 *   yields because the demonstrated superiority creates a legitimacy crisis
 *   for non-adoption. This reading asserts the coordination function is
 *   genuine: fringe innovation solves real governance problems. The
 *   extraction is asymmetric: incumbent elites and traditional authorities
 *   lose status, decision-rights, and resource control during the transition.
 *   Active enforcement is required because apex institutions actively
 *   suppress fringe alternatives until the legitimacy cost of suppression
 *   exceeds adoption cost.
 *
 * KEY AGENTS:
 *   - fringe_intellectuals: Primary beneficiaries (moderate/constrained) — develop and advocate new commitments from institutional margins
 *   - marginalized_communities: Primary beneficiaries (powerless/constrained) — their lived experience generates the superior commitments
 *   - reform_advocates: Primary beneficiaries (organized/mobile) — bridge fringe innovation to institutional adoption
 *   - incumbent_elites: Primary victims (institutional/trapped) — lose authority and resource control when commitments shift
 *   - traditional_authorities: Primary victims (institutional/identity_locked) — legitimacy grounded in the displaced commitments
 *   - apex_institutions: Agenda setters/victims (institutional/constrained) — initially resist, eventually adopt under legitimacy pressure
 *   - historical_sociologists: Observers (analytical/analytical) — analyze the climb mechanism across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.22).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.35).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Legitimacy Climb from Institutional Fringes").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '7c27b94c-9d06-4113-b092-42d664eaf685').
narrative_ontology:cs_kernel_codification('7c27b94c-9d06-4113-b092-42d664eaf685', distributed).
narrative_ontology:cs_authority_grounding('7c27b94c-9d06-4113-b092-42d664eaf685', practice).
narrative_ontology:cs_interpretation_layer_present('7c27b94c-9d06-4113-b092-42d664eaf685').
narrative_ontology:cs_reading_relation('7c27b94c-9d06-4113-b092-42d664eaf685', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c27b94c-9d06-4113-b092-42d664eaf685', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('7c27b94c-9d06-4113-b092-42d664eaf685', foundational, legitimacy_requires_demonstrated_superiority).
narrative_ontology:cs_axiom_status(legitimacy_requires_demonstrated_superiority, holdable).
narrative_ontology:cs_axiom_grounding('7c27b94c-9d06-4113-b092-42d664eaf685', legitimacy_requires_demonstrated_superiority, empirically_contingent).
narrative_ontology:cs_axiom('7c27b94c-9d06-4113-b092-42d664eaf685', foundational, fringe_innovation_precedes_apex_adoption).
narrative_ontology:cs_axiom_status(fringe_innovation_precedes_apex_adoption, holdable).
narrative_ontology:cs_axiom_grounding('7c27b94c-9d06-4113-b092-42d664eaf685', fringe_innovation_precedes_apex_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('7c27b94c-9d06-4113-b092-42d664eaf685', fringe_innovation_legitimacy_gradient).
narrative_ontology:cs_drift_state('7c27b94c-9d06-4113-b092-42d664eaf685', contemporary_digital_acceleration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c27b94c-9d06-4113-b092-42d664eaf685', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_intellectuals).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, reform_advocates).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_organizers).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, traditional_authorities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutions).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, demonstrated_superiority_principle).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, bottom_up_legitimacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop new normative commitments and institutional designs from academic or civil society margins. Their innovations gain traction when they solve governance problems apex institutions cannot. They gain legitimacy, citations, and eventually advisory roles when their commitments climb. Exit means abandoning the climb attempt for other research, but their professional identity is tied to institutional innovation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_intellectuals, beneficiary,
    moderate, biographical, constrained, national).

% Generate superior commitments from lived experience of governance failure (e.g., mutual aid networks, restorative justice, participatory budgeting). When these climb, communities gain recognition and resource flows. Exit is constrained — they cannot leave the polity, but can disengage from specific climb campaigns. Their identity is often fused with the commitments they generate.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, marginalized_communities, beneficiary,
    powerless, generational, constrained, national).

% Bridge fringe innovations to institutional adoption: translate, pilot, build coalitions, pressure apex. They gain organizational funding, staff positions, and policy influence when climbs succeed. Exit is mobile — they can shift to other reform campaigns or sectors. Their role is dual: they benefit from successful climbs but also set the agenda for which innovations get climbed.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, reform_advocates, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, reform_advocates, agenda_setter).

% Hold authority and resource control grounded in existing commitments. When fringe commitments climb, they lose decision-rights, status, and patronage networks. Exit is trapped — their identity and power are constituted by the incumbent order; they cannot 'exit' to a rival elite without surrendering their position. Resistance is fierce until legitimacy cost of resistance exceeds adoption cost.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, incumbent_elites, payer,
    institutional, biographical, trapped, national).

% Religious, customary, or aristocratic authorities whose legitimacy derives from the commitments being displaced. They lose moral authority and institutional standing. Exit is identity_locked — their self-concept and social role are fused with the traditional commitments; adopting new ones dissolves their authority. They resist symbolically and materially.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, traditional_authorities, payer,
    institutional, generational, identity_locked, national).

% State agencies, supreme courts, legislatures that control adoption thresholds. Initially resist fringe commitments (protecting incumbent order, procedural gatekeeping). Eventually adopt when non-adoption creates legitimacy crisis (protests, international pressure, governance failure). They lose some autonomy but gain stabilized legitimacy. Exit is constrained — they cannot abdicate their institutional role, but can modulate adoption timing and terms.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutions, payer).

% Analyze the climb mechanism across historical cases. They neither collect nor pay; they map the structural dynamics. Their exit is analytical — they can change theoretical frameworks. They provide the corroboration for founding problem claims.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how state commitments update when apex institutions are captured by incumbents and cannot self-correct. Fringe actors generate and test superior commitments at low cost; demonstrated superiority creates a legitimacy gradient that compels apex adoption without requiring apex benevolence.
% TRANSFER_FUNCTION: Moves decision-rights, resource control, and moral authority from incumbent elites and traditional authorities to fringe intellectuals, marginalized communities, and reform advocates. The transfer is mediated by demonstrated superiority — not automatic, but compelled by legitimacy crisis at apex.
% ABSENT_VOICES: Future generations who inherit the climbed commitments without participating in the climb; external powers who may exploit the transition instability; fringe actors whose innovations fail to climb and are erased from the record.
% DISAPPEARANCE_RATIONALE: If the endogenous climb mechanism vanished, state commitments could only change through exogenous imposition (coups, conquest, elite pacts) or hybrid cascades. The specific pathway of fringe-generated, superiority-driven, gradual apex adoption would disappear — changing which commitments get installed, which actors gain authority, and the legitimacy grammar of the state.
% FOUNDING_PROBLEM: Apex institutions captured by incumbents lose capacity to solve novel governance problems (famine, epidemic, technological disruption, legitimacy crisis). The climb mechanism was built (emerged) to bypass captured apex by generating solutions at the fringe where experimentation is cheaper and failure less catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (Tilly, Mann, Scott) document recurrent apex capture and fringe innovation across state formation episodes. Development economists (Duflo, Banerjee) show pilot-to-scale patterns matching climb dynamics. Political scientists (Fox, Gerschewski) document legitimacy gradients compelling authoritarian adaptation. Corroboration comes from outside the benefiting fringe actors — from analysts of state capacity and institutional change.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).
:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.22) reflects that the climb mechanism transfers authority and resources from apex to fringe, but the transfer is mediated by demonstrated superiority — not pure extraction. Suppression (0.35) captures apex resistance to fringe alternatives before demonstrated superiority becomes undeniable. Theater ratio (0.18) is low: the coordination function (solving governance problems via fringe innovation) is real, not performative. Accessibility collapse (0.4) is moderate: alternatives (apex-led reform, status quo) remain viable but lose legitimacy. Resistance (0.55) is substantial: apex institutions actively resist fringe-driven change until forced. The claimed_type tangled_rope captures the hybrid: genuine coordination (fringe solves real problems) + asymmetric extraction (apex loses power/resources). Requires active enforcement because apex institutions must be compelled to adopt against initial resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the fringe seat: the constraint is a rope — they coordinate to solve problems the apex cannot, and their demonstrated superiority legitimately compels adoption. From the apex seat: the constraint is a snare — they are forced to surrender authority to actors they previously excluded, and the 'demonstrated superiority' criterion is manipulable by fringe advocates. From the observer seat: the constraint is a tangled rope — both coordination and extraction are structurally real; the climb mechanism solves a genuine collective-action problem (how to update state commitments when apex is captured) but extracts from incumbents in the process.
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe actors (intellectuals, marginalized communities, reform advocates) are beneficiaries: they gain institutional recognition, decision-rights, and resource access when their commitments climb. Their exit is constrained — they cannot easily leave the institutional field, but they can abandon specific climb attempts. Incumbent elites and traditional authorities are victims: they lose legitimacy, authority, and resource control. Their exit is trapped/identity_locked — their identity and power are constituted by the commitments being displaced. Apex institutions are dual-positioned: they set the agenda (adoption thresholds) but are victims of the climb's legitimacy pressure. Their exit is constrained — they can delay but not permanently block without losing systemic legitimacy. The directionality derivation from beneficiary/victim + exit correctly captures this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The climb mechanism prevents mandatrophy by tethering legitimacy to demonstrated superiority rather than ancestral authority. When the founding problem (governance failure at apex) is live, the climb is coordination. When the founding problem is dead (apex functions adequately) but the climb mechanism persists as a ritual of 'innovation theater,' it becomes extractive — fringe actors extract status/resources without solving real problems. The endogenous_climb_reading claims the founding problem is live; the exogenous_imposition_reading claims it is dead and the climb is cover for elite rotation. This reading's mandatrophy resolution: coordination function remains live because state capacity challenges (climate, inequality, technology) persistently generate problems apex cannot solve alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine endogenous climb mechanism, or does it mask exogenous imposition dynamics?',
    'Compare adoption curves of commitments claimed as endogenous climbs against archival evidence of covert apex sponsorship or elite co-optation timing.',
    'If exogenous sponsorship is found in cases coded as endogenous, the extraction profile shifts toward apex beneficiaries and the reading''s structural classification changes from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the endogenous_climb_reading of the state_commitment_installation_mechanism kernel. Sibling readings: exogenous_imposition_reading, hybrid_cascade_reading. The core disagreement is whether legitimacy flows bottom-up (this reading) or top-down (exogenous) or requires both (hybrid).').

omega_variable(
    fringe_beneficiary_verification,
    'Do fringe actors genuinely benefit from the climb mechanism, or is their advocacy a performative legitimating ritual for apex-driven installation?',
    'Track resource flows and decision-rights post-adoption: do fringe advocates gain institutional positions, budget authority, or policy-setting power, or are they ceremonially acknowledged while apex actors capture the gains?',
    'If fringe actors are performative only, the beneficiaries list should shift toward apex institutions and the extraction profile re-evaluated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_beneficiary_verification, empirical, 'Verifies the beneficiary structure declared for this reading.').

omega_variable(
    gradual_adoption_vs_punctuated_equilibrium,
    'Are adoption curves genuinely gradual (endogenous climb signature) or do they show punctuated equilibrium with sudden apex-driven phase shifts?',
    'Quantitative adoption-timeline analysis across multiple commitment-installation episodes; look for power-law vs. exponential inflection patterns.',
    'Punctuated patterns would support hybrid_cascade_reading or exogenous_imposition_reading over this reading''s claimed gradual climb.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gradual_adoption_vs_punctuated_equilibrium, empirical, 'Tests the gradual adoption curve structural delta for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(stat_tr_t75, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement(stat_tr_t100, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 50, 0.2).
narrative_ontology:measurement(stat_be_t75, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 75, 0.22).
narrative_ontology:measurement(stat_be_t100, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 100, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 50, 0.33).
narrative_ontology:measurement(stat_su_t75, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 75, 0.35).
narrative_ontology:measurement(stat_su_t100, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__endogenous_climb_reading, 0.08).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_formation_legitimacy_cascade).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, institutional_innovation_diffusion).

% DUAL FORMULATION NOTE:
% This constraint (endogenous_climb_reading) and its siblings (exogenous_imposition_reading, hybrid_cascade_reading) form a constraint family decomposing the state_commitment_installation_mechanism kernel. The endogenous reading claims fringe-driven coordination with apex extraction (tangled_rope). The exogenous reading claims apex-driven coordination with fringe extraction (different beneficiary/victim structure). The hybrid reading claims bidirectional coordination with mutual extraction (dual tangled_rope). Each has distinct ε, stakeholders, and temporal dynamics. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__endogenous_climb_reading, institutional, 0.65).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__endogenous_climb_reading, powerless, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
