% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Folk Syncretistic Divine Legitimacy Substrate
 *   domain: ancient_history/religious_studies/political_economy_of_belief
 *
 * SUMMARY:
 *   In ancient Egypt, divine legitimacy was not solely the province of the
 *   pharaoh or the Amun priesthood. At the village and household level,
 *   people practiced a pragmatic, syncretistic religion that incorporated
 *   multiple deities — local, national, and imported — according to immediate
 *   need. This folk substrate operated as a diffuse coordination mechanism:
 *   it organized agricultural labor, mediated social disputes, and provided a
 *   shared symbolic framework for life passages. The pharaoh and central
 *   priesthood were distant elites whose theological projects (Amun-Ra
 *   supremacy, Atenist monotheism) had to contend with this resilient,
 *   decentralized practice. The constraint is the requirement that legitimate
 *   divine authority must flow through, or at least accommodate, these local
 *   practices. Extraction is low to moderate: local specialists extract some
 *   resources, but the primary flow is coordination, not rent. Suppression is
 *   low because the practice is self-sustaining through identity and
 *   tradition, not coercion. The claimed type is rope: a genuine coordination
 *   function with minimal coercive overhead.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.3).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.2).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Folk Syncretistic Divine Legitimacy Substrate").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "ancient_history/religious_studies/political_economy_of_belief").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '298953be-e00e-466e-99c0-bca967817f05').
narrative_ontology:cs_kernel_codification('298953be-e00e-466e-99c0-bca967817f05', distributed).
narrative_ontology:cs_authority_grounding('298953be-e00e-466e-99c0-bca967817f05', practice).
narrative_ontology:cs_interpretation_layer_present('298953be-e00e-466e-99c0-bca967817f05').
narrative_ontology:cs_reading_relation('298953be-e00e-466e-99c0-bca967817f05', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('298953be-e00e-466e-99c0-bca967817f05', divine_legitimacy_substrate__atenist_monotheistic_reading, influences).
narrative_ontology:cs_axiom('298953be-e00e-466e-99c0-bca967817f05', foundational, divine_legitimacy_derives_from_local_practice).
narrative_ontology:cs_axiom_status(divine_legitimacy_derives_from_local_practice, holdable).
narrative_ontology:cs_axiom_grounding('298953be-e00e-466e-99c0-bca967817f05', divine_legitimacy_derives_from_local_practice, theological).
narrative_ontology:cs_axiom('298953be-e00e-466e-99c0-bca967817f05', secondary, deities_are_pragmatically_incorporated).
narrative_ontology:cs_axiom_status(deities_are_pragmatically_incorporated, holdable).
narrative_ontology:cs_axiom_grounding('298953be-e00e-466e-99c0-bca967817f05', deities_are_pragmatically_incorporated, instrumental).
narrative_ontology:cs_reference_frame('298953be-e00e-466e-99c0-bca967817f05', ancestral_village_ritual_tradition).
narrative_ontology:cs_drift_state('298953be-e00e-466e-99c0-bca967817f05', new_kingdom_peak, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('298953be-e00e-466e-99c0-bca967817f05', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_practitioners).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_ritual_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__folk_syncretistic_reading, central_priesthood).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_derives_from_local_practice).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, deities_are_pragmatically_incorporated).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform daily and seasonal rituals at household shrines and village festivals, invoking whichever deities address immediate needs (health, harvest, protection). Gain social cohesion, spiritual reassurance, and practical coordination of agricultural labor. Exit means abandoning communal identity and risking divine disfavor.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_practitioners, beneficiary,
    moderate, biographical, identity_locked, local).

% Lead communal rites, maintain local shrines, and adjudicate which deities are invoked for which occasions. Receive portions of offerings, fees for services, and status. Their authority rests on recognized ritual competence, not state appointment. Exit is difficult because role is embedded in village social structure.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_ritual_specialists, agenda_setter,
    organized, biographical, constrained, local).

% Must acknowledge and fund local cults to maintain legitimacy; cannot impose exclusive theology without provoking resistance. Bears cost of building/endowing local temples and tolerating heterodox practices. Can attempt radical reform (as Akhenaten did) but risks losing the substrate of legitimacy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh, payer,
    institutional, generational, arbitrage, national).

% Official theology centers on Amun-Ra; folk syncretism dilutes their doctrinal control and diverts resources to local shrines. They issue corrective decrees and send inspectors, but enforcement is sporadic and often ignored. Their exit is constrained by institutional identity and dependence on state patronage.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, central_priesthood, payer,
    institutional, generational, constrained, national).

% Modern scholar reconstructing the constraint from archaeological, textual, and anthropological evidence. Sees the full structural relationship between diffuse folk practice and centralized state religion without participating in either.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates household and village life around agricultural cycles, life passages, and communal identity through flexible, pragmatic invocation of multiple deities.
% TRANSFER_FUNCTION: Moves labor, resources, and status from households to local ritual specialists and communal feasts; moves legitimacy from local practice up to the pharaoh (who must acknowledge local gods to be legitimate).
% ABSENT_VOICES: The voices of those who would prefer a unified, centralized theology (e.g., reforming pharaohs, central priesthood) are excluded from the local practice; they are at court and temples.
% DISAPPEARANCE_RATIONALE: The folk practice is the substrate of divine legitimacy; without it, the state religion loses its connection to the populace, and village life loses its ritual coordination.
% FOUNDING_PROBLEM: The need for communities to secure divine favor for survival (harvest, health, protection) in an uncertain environment, without relying on distant state institutions.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic records of village religion in Egypt (e.g., from Deir el-Medina, modern anthropological analogs) and the persistence of folk practice despite state religious changes.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.3, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.3) reflects modest resource flows to local specialists and the cost to central elites of accommodating pluralism. Suppression (0.2) is low because the constraint persists through cultural reproduction, not enforcement. Theater ratio (0.15) is low: rituals are functional, not performative. Accessibility collapse (0.6) is moderate: within the cultural milieu, alternatives (atheism, exclusive monotheism) are largely unthinkable, but the practice itself is inclusive. Resistance (0.4) comes mainly from centralizing elites trying to standardize worship. The measurement series uses a shared time grid (0–30, roughly Early Dynastic to New Kingdom peak) with six points per metric.
 *
 * PERSPECTIVAL GAP:
 *   From the folk seat, the constraint is a benign coordination rope. From the pharaoh's seat, it is a tangling rope that limits his theological authority and extracts tribute to local cults. From the central priesthood's seat, it is a snare-like diffusion of their monopoly. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Household practitioners and village specialists are beneficiaries (d near 0.0) — they gain coordination and status. Pharaoh and central priesthood are payers (d near 0.7–0.8) — they bear costs of accommodation and lose doctrinal control, though pharaoh has arbitrage-grade exit (can attempt reform). The analytical observer sits at d=0.5. The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing divine favor for survival) remains live. The arrangement has not atrophied into a piton because it continues to perform its coordination function actively. No sunset clause exists because the practice is not transitional. Mandatrophy is not resolved; the constraint remains functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_structure_unclear,
    'Who are the primary beneficiaries of the folk syncretistic substrate — the household practitioners, the village specialists, or the community as a collective? Does the practice extract from households to benefit specialists?',
    'Comparative analysis of offering distributions, household vs. specialist wealth in archaeological record (e.g., Deir el-Medina), and ethnographic analogs of village religious economies.',
    'If specialists capture most benefits, the constraint shifts toward tangled_rope; if benefits are diffuse, it remains a rope. Affects classification of village_specialists seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_unclear, empirical, 'Uncertainty about whether the folk substrate is a pure coordination rope or has an extractive layer at the village level.').

omega_variable(
    naturalness_vs_constructed_ambiguity,
    'Is the folk syncretistic substrate a genuine emergent feature of human religious cognition (a mountain of anthropology) or a historically constructed constraint that could have been otherwise?',
    'Cross-cultural comparison of syncretistic folk religion in early state societies; cognitive science of religion studies on intuitive theology.',
    'If mountain, the constraint is inevitable and non-extractive by nature; if constructed, its current form may serve hidden interests. Would change claimed_type and FSM evaluation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_vs_constructed_ambiguity, conceptual, 'Whether the pragmatic pluralism is a cognitive universal or a contingent historical arrangement.').

omega_variable(
    reading_relations_folk_to_atenist,
    'Does the folk syncretistic reading foreclose, coexist with, or influence the atenist monotheistic reading?',
    'Historical analysis of Akhenaten''s repression of folk practice and its rapid restoration post-Amarna. Determines whether folk pluralism logically excludes exclusive monotheism or merely resists it.',
    'If forecloses, the two readings cannot coexist in one framework; if influences, folk practice creates structural pressure on atenist imposition without logical exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_folk_to_atenist, conceptual, 'Structural relationship from folk reading to atenist reading for cs_structure.reading_relations.').

omega_variable(
    reading_relations_folk_to_amun,
    'Does the folk syncretistic reading foreclose, coexist with, or influence the amun polytheistic reading?',
    'Analysis of whether Amun priesthood''s standardization efforts ever succeeded in replacing folk practice, or whether they operated at different levels (state vs. village).',
    'If coexists_with, both readings operate simultaneously at different scales; if influences, folk practice resists standardization pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_folk_to_amun, empirical, 'Structural relationship from folk reading to amun reading for cs_structure.reading_relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_tr_t6, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_tr_t12, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_tr_t18, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_tr_t24, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_tr_t30, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_be_t6, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_be_t12, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_be_t18, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 18, 0.3).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_be_t24, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 24, 0.3).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_be_t30, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 30, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_su_t6, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 6, 0.18).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_su_t12, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_su_t18, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 18, 0.2).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_su_t24, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 24, 0.2).
narrative_ontology:measurement(divine_legitimacy_substrate__folk_syncretistic_reading_su_t30, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__folk_syncretistic_reading, 0.08).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% This folk reading is one of three constraints in the divine_legitimacy_substrate kernel family. The amun reading centralizes legitimacy in priestly interpretation; the atenist reading centralizes it in pharaonic revelation. The folk reading distributes it in local practice. All three share the kernel but instantiate different constraints with different ε, beneficiaries, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
