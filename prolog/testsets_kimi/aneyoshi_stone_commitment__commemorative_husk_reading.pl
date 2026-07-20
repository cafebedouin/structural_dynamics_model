% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone â Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone warned inhabitants not to build below its
 *   elevation. In the commemorative husk reading, the stone has decayed from
 *   an operational land-use rule into a heritage artifact maintained by
 *   ritual and bureaucratic inertia. Land-use decisions in 2011 were made
 *   independently of the inscription; village survival is attributed to luck
 *   and modern geography rather than ancestral compliance. The stone persists
 *   as a museum piece with high symbolic theater and diffuse, low-grade
 *   extraction of maintenance resources.
 *
 * KEY AGENTS:
 *   - municipal_heritage_board: Agenda-setter (institutional/constrained) â administers the stone site and curates heritage narrative without concentrated profit
 *   - local_residents: Payer (moderate/constrained) â bear diffuse costs of maintenance and ritual observance without receiving protective land-use coordination
 *   - disaster_anthropologists: Observer (analytical/analytical) â study the stone as an instance of commitment-system decay
 *   - rationalist_urban_planners: Excluded (organized/mobile) â advocate modern disaster infrastructure but are outside heritage discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.76).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.25).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.88).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Stone â Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, 'db6109f5-a3ee-40f1-a6f7-b67dc818e037').
narrative_ontology:cs_kernel_codification('db6109f5-a3ee-40f1-a6f7-b67dc818e037', fixed_text).
narrative_ontology:cs_authority_grounding('db6109f5-a3ee-40f1-a6f7-b67dc818e037', lineage).
narrative_ontology:cs_interpretation_layer_present('db6109f5-a3ee-40f1-a6f7-b67dc818e037').
narrative_ontology:cs_reading_relation('db6109f5-a3ee-40f1-a6f7-b67dc818e037', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('db6109f5-a3ee-40f1-a6f7-b67dc818e037', foundational, heritage_commemoration_separable_from_compliance).
narrative_ontology:cs_axiom_status(heritage_commemoration_separable_from_compliance, holdable).
narrative_ontology:cs_axiom_grounding('db6109f5-a3ee-40f1-a6f7-b67dc818e037', heritage_commemoration_separable_from_compliance, conventional).
narrative_ontology:cs_reference_frame('db6109f5-a3ee-40f1-a6f7-b67dc818e037', commemorative_heritage_marker).
narrative_ontology:cs_drift_state('db6109f5-a3ee-40f1-a6f7-b67dc818e037', post_2011_resilience_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('db6109f5-a3ee-40f1-a6f7-b67dc818e037', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, local_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the stone heritage site, maintains the grounds, and interprets the inscription for visitors. Could decommission or relocate the marker but faces no constituency demanding removal; inertia and low political reward for change keep the site active. Does not capture concentrated revenue from the stone.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_heritage_board, agenda_setter,
    institutional, generational, constrained, local).

% Bear diffuse costs of stone maintenance through municipal taxes and ritual observance. Their land-use and building decisions are governed by modern economic incentives and zoning codes, not by the inscription, yet they continue symbolic upkeep inherited from prior generations. Collective exit from heritage practice would carry social stigma.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_residents, payer,
    moderate, biographical, constrained, local).

% Study the stone as a case of commitment-system decay and traditional knowledge transmission. They document maintenance rituals and interview residents without intervening in heritage management or land-use policy.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_anthropologists, observer,
    analytical, biographical, analytical, global).

% Would advocate for engineered sea walls, managed retreat, and evidence-based coastal setbacks rather than symbolic markers. They are not consulted in heritage preservation decisions and their modern alternatives are treated as administratively unrelated to the stone's cultural function.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, rationalist_urban_planners, excluded,
    organized, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated tsunami-safe settlement boundaries by providing a durable, visible prohibition on construction below a marked elevation in a pre-bureaucratic coastal community. In its present state, it coordinates no active land-use decisions.
% TRANSFER_FUNCTION: Moves maintenance labor, municipal heritage funds, and ritual attention from local residents and public budgets to the symbolic upkeep of a non-operational stone marker and its surrounding narrative.
% ABSENT_VOICES: Rationalist urban planners and modern civil engineers who would argue for evidence-based disaster infrastructure over symbolic heritage observance are structurally excluded from the heritage-management conversation; their alternative frameworks are treated as belonging to a different policy domain.
% DISAPPEARANCE_RATIONALE: If the stone vanished overnight, municipal heritage budgets would reallocate, local ritual calendars would lose a focal point, and community identity narratives would shift, though actual land-use patterns and building locations would remain unchanged.
% FOUNDING_PROBLEM: Tsunami risk in pre-modern coastal Japan required a persistent, visible settlement boundary that could regulate building location across generations without centralized written administration or engineering capacity.
% FOUNDING_PROBLEM_CORROBORATION: Municipal disaster management agencies and civil engineers attest that contemporary tsunami mitigation relies on sea walls, evacuation routes, and early warning systems rather than stone markers. Heritage scholars corroborate that the stone's original land-use regulatory function has been entirely superseded by modern building codes.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.76 across the interval as the protective coordination function atrophies and maintenance costs become pure overhead. Theater_ratio rises from 0.08 to 0.88, modeling the transition from functional emergency marker to performative heritage object. Suppression is low (0.25) because no active enforcement is required to maintain a museum piece; inertia and social habit suffice. Accessibility_collapse is low (0.15) because modern zoning and engineering alternatives are clearly visible, and resistance is low (0.15) because the costs are too diffuse to mobilize opposition.
 *
 * PERSPECTIVAL GAP:
 *   The heritage board experiences the stone as an administrative obligation with negligible political benefit; local residents experience it as a taken-for-granted ritual cost. Neither seat experiences the constraint as a live protective rule. The anthropological observer seat sees the full decay curve, while the agenda-setter seat sees only budget-line inertia. The engine computes these divergences from structural position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared because the piton structure lacks a concentrated capturer of extraction. Local residents are declared victims (payers) with constrained exit, driving their directionality toward the target end. The municipal heritage board is agenda_setter but not beneficiary; without rent capture, its directionality derives from institutional position and constrained exit, sitting nearer the symmetric middle. The absence of a beneficiary atom means effective extraction is not inverted into subsidy for any seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â marking safe settlement boundaries without state capacity â was solved by modern building codes and engineered infrastructure long ago. The constraint persists not because it solves a live problem, but because the costs of formal decommissioning exceed the political benefit. This is canonical mandatrophy: a dead mandate maintained as theatrical heritage. The R5 genealogy (founding_problem_status=dead paired with disappearance_verdict=world_rearranges) flags the zombie structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the Aneyoshi stone currently function as an operational land-use constraint or as a commemorative heritage artifact?',
    'Archaeological and ethnographic investigation of pre-2011 building permits, oral histories, and settlement patterns to determine whether stone-adjacent land-use decisions were made with reference to the inscription.',
    'If operational force is demonstrated, the constraint classifies as tangled_rope or rope; if purely commemorative, it remains piton. The resolution restructures the entire constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Empirical resolution of the kernel contest between operational and commemorative readings').

omega_variable(
    diffuse_cost_visibility,
    'Are the costs of stone maintenance and ritual observance visible enough to local residents to trigger reform pressure, or are they buried in municipal budgets and normalized social habit?',
    'Municipal budget-line analysis separating stone-maintenance expenditure from general heritage funding, combined with resident surveys on perceived costs of ritual observance.',
    'If costs are invisible, the constraint persists as piton; if visible and resented, pressure for decommissioning may mount, potentially shifting classification or triggering dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_cost_visibility, empirical, 'Visibility of diffuse piton maintenance costs to payers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 13, 0.2).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 26, 0.35).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 39, 0.5).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 52, 0.65).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 65, 0.78).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 78, 0.88).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 13, 0.28).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 26, 0.42).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 39, 0.55).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 52, 0.65).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 65, 0.71).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 78, 0.76).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint and behavioral_competence_reading are two structurally distinct claims arising from the same kernel (the Aneyoshi stone inscription). The commemorative_husk_reading treats the stone as operationally inert with high theater and diffuse costs; the behavioral_competence_reading treats it as a live coordination mechanism with operational force. Their epsilon values diverge widely. They form a constraint family linked by reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
