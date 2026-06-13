% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Anthropological Record: Creationist Reading
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint represents the 'creationist reading' of the
 *   anthropological record, asserting divine creation events compatible with
 *   scriptural timelines or designed complexity. It actively suppresses
 *   materialist timelines and requires divine causation, leading to a loss of
 *   adjudicative monopoly for credentialed science within religious
 *   communities. The constraint is claimed as a Tangled Rope because it
 *   offers a coordination function (coherence for religious communities) but
 *   also involves active suppression of alternative scientific narratives and
 *   extraction of epistemic authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.6).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.7).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Anthropological Record: Creationist Reading").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, 'c7a17d64-ef0d-463a-b5bb-f01601a6df9e').
narrative_ontology:cs_kernel_codification('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', fixed_text).
narrative_ontology:cs_authority_grounding('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', lineage).
narrative_ontology:cs_interpretation_layer_present('c7a17d64-ef0d-463a-b5bb-f01601a6df9e').
narrative_ontology:cs_reading_relation('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', foundational, divine_causation_of_life).
narrative_ontology:cs_axiom_status(divine_causation_of_life, holdable).
narrative_ontology:cs_axiom_grounding('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', divine_causation_of_life, theological).
narrative_ontology:cs_axiom('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', foundational, scriptural_timeline_literalism).
narrative_ontology:cs_axiom_status(scriptural_timeline_literalism, holdable).
narrative_ontology:cs_axiom_grounding('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', scriptural_timeline_literalism, theological).
narrative_ontology:cs_reference_frame('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', scriptural_inerrancy_framework).
narrative_ontology:cs_drift_state('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', contemporary_scientific_consensus, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('c7a17d64-ef0d-463a-b5bb-f01601a6df9e', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, religious_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_scholars).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, mainstream_anthropologists).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, public_education_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a narrative that affirms their scriptural timeline and divine causation, reinforcing community identity and theological coherence. Exit from this reading would mean challenging foundational beliefs.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, religious_communities, beneficiary,
    organized, generational, identity_locked, local).

% Actively promote and defend the creationist interpretation of the anthropological record, publishing alternative research and critiquing mainstream science. Their careers and legitimacy are tied to the persistence of this reading.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_scholars, agenda_setter,
    moderate, biographical, constrained, national).

% Bear the cost of defending scientific consensus against challenges from this reading, often facing public skepticism or political pressure. Their professional authority is contested in spaces where this reading holds sway.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, mainstream_anthropologists, payer,
    institutional, generational, constrained, global).

% Are forced to navigate legal and political challenges regarding the teaching of human origins, often facing pressure to include or give equal weight to creationist perspectives, diluting scientific curricula.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, public_education_systems, payer,
    institutional, generational, constrained, national).

% Often caught in the public debate, they may be exposed to conflicting narratives without the tools to adjudicate scientific claims, leading to confusion or distrust in scientific institutions.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, secular_public, excluded,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared understanding of human origins within specific religious communities, providing a coherent narrative that integrates faith and perceived empirical evidence.
% TRANSFER_FUNCTION: Transfers epistemic authority regarding human origins from mainstream science to scriptural interpretation and creationist scholarship within its sphere of influence, reinforcing religious authority.
% ABSENT_VOICES: Indigenous epistemologies, which offer alternative non-materialist but also non-creationist accounts of human origins, are largely absent from the binary debate between creationism and naturalism, and would challenge the universalizing claims of both.
% DISAPPEARANCE_RATIONALE: If this reading vanished, religious communities would face a significant challenge to their foundational narratives, potentially leading to internal theological crises or a re-evaluation of scriptural interpretation. Public education systems would experience less pressure regarding science curricula.
% FOUNDING_PROBLEM: The perceived conflict between scientific findings on human origins (e.g., evolution, deep time) and literal interpretations of religious texts, creating a crisis of faith for some believers.
% FOUNDING_PROBLEM_CORROBORATION: Religious leaders and creationist organizations consistently attest to the live status of this problem, citing ongoing challenges to faith from secular science. Mainstream scientists and educators acknowledge the persistence of the conflict, though they frame it as a misunderstanding of scientific methodology rather than a genuine scientific problem.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6) is moderate, reflecting the cost to public education and the epistemic burden on mainstream science. Suppression (0.7) is high due to active efforts to challenge scientific consensus, promote alternative curricula, and discredit evolutionary theory. Theater ratio (0.4) indicates that while genuine scholarly work exists within creationism, a significant portion of its public-facing activity is performative, aimed at maintaining a public challenge to mainstream science rather than engaging in scientific peer review.
 *
 * PERSPECTIVAL GAP:
 *   For religious communities and creationist scholars, this reading provides coherence and meaning, acting as a coordination mechanism for their worldview. For mainstream anthropologists and public education systems, it functions as an extractive and suppressive force, diverting resources and challenging established scientific authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious communities and creationist scholars are beneficiaries, gaining affirmation and authority. Mainstream anthropologists and public education systems are victims, bearing the costs of defending scientific consensus and navigating curriculum disputes. The secular public is excluded, often caught in the crossfire without a clear path to resolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (resolving the perceived conflict between faith and science) remains 'live' for its beneficiaries, preventing it from becoming a Piton. However, the 'contested' status of the founding problem suggests a potential for Mandatrophy if the scientific consensus becomes overwhelmingly dominant or if religious communities find alternative ways to reconcile faith and science without suppressing scientific findings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_authority_locus,
    'Is the anthropological record primarily a domain for scientific inquiry or theological interpretation?',
    'A shift in societal consensus regarding the boundaries of scientific and religious authority, or a formal reconciliation within religious traditions that redefines the scope of scriptural interpretation.',
    'If resolved towards scientific inquiry, the creationist reading''s suppressive and extractive functions would be delegitimized. If resolved towards theological interpretation, mainstream science would lose its claim to universal adjudicative authority in this domain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_authority_locus, conceptual, 'Ambiguity over which epistemic framework holds primary authority for interpreting human origins.').

omega_variable(
    materialist_timeline_suppression,
    'To what extent does the creationist reading actively suppress or merely offer an alternative to the materialist timeline?',
    'Analysis of curriculum challenges, legal cases, and public statements by creationist organizations: active suppression involves direct attempts to remove or discredit materialist accounts, while merely offering an alternative would focus solely on promoting creationist views without attacking others.',
    'If active suppression is the dominant mode, the constraint''s ''suppression'' metric is accurate. If it''s primarily an alternative, the suppression metric might be overstated, and the constraint might lean more towards a ''Rope'' for its internal coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(materialist_timeline_suppression, empirical, 'Distinguishing active suppression from mere alternative offering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1960, anthropological_record__creationist_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(anth_tr_t1980, anthropological_record__creationist_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(anth_tr_t2000, anthropological_record__creationist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(anth_tr_t2024, anthropological_record__creationist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(anth_be_t1960, anthropological_record__creationist_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(anth_be_t1980, anthropological_record__creationist_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(anth_be_t2000, anthropological_record__creationist_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(anth_be_t2024, anthropological_record__creationist_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1960, anthropological_record__creationist_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(anth_su_t1980, anthropological_record__creationist_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(anth_su_t2000, anthropological_record__creationist_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(anth_su_t2024, anthropological_record__creationist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'anthropological_record' kernel. Its claims directly contest the 'naturalist_reading' and largely ignore the 'indigenous_epistemology_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
