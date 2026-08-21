% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39: Liberal Due Process Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'liberal due process' reading of Magna
 *   Carta's Clause 39, which interprets the clause as establishing universal
 *   individual rights against arbitrary state power, evolving into modern
 *   concepts of due process. This reading is expansive, applying to all
 *   citizens and significantly constraining executive and legislative
 *   discretion. It is one of several competing interpretations of the same
 *   historical text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.85).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.7).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39: Liberal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, 'ea2f4301-91cd-4099-9935-a6f2b4a5b13e').
narrative_ontology:cs_kernel_codification('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', fixed_text).
narrative_ontology:cs_authority_grounding('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', lineage).
narrative_ontology:cs_interpretation_layer_present('ea2f4301-91cd-4099-9935-a6f2b4a5b13e').
narrative_ontology:cs_reading_relation('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', foundational, universal_individual_rights).
narrative_ontology:cs_axiom_status(universal_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', universal_individual_rights, deontological).
narrative_ontology:cs_axiom('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', foundational, evolving_constitutional_meaning).
narrative_ontology:cs_axiom_status(evolving_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', evolving_constitutional_meaning, conventional).
narrative_ontology:cs_reference_frame('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', post_enlightenment_rights_framework).
narrative_ontology:cs_drift_state('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ea2f4301-91cd-4099-9935-a6f2b4a5b13e', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, citizens_and_residents).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, judiciary).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_branch).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, legislative_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from protection against arbitrary arrest, detention, or seizure of property without due process of law. They are the primary subjects of the rights established by this reading, but their ability to enforce these rights depends on judicial and legal systems.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, citizens_and_residents, beneficiary,
    organized, generational, constrained, national).

% Interprets and enforces Clause 39, expanding its scope over centuries to cover modern due process rights. Their institutional legitimacy is tied to upholding these fundamental protections, which grants them significant power over the executive and legislative branches.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, judiciary, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Bears the cost of limitations on its power to act unilaterally or arbitrarily. Must adhere to established legal procedures, obtain warrants, and justify actions in court, which can impede efficiency and political objectives.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_branch, payer,
    institutional, immediate, constrained, national).

% Face constraints on their ability to pass laws that infringe on individual rights without due process. Judicial review, grounded in this reading, can strike down legislation, forcing compromise or abandonment of popular but unconstitutional measures.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legislative_majorities, payer,
    institutional, biographical, constrained, national).

% Analyze the historical evolution and contemporary application of Clause 39, advocating for its expansive interpretation and defending its role in protecting fundamental rights. They shape public and legal discourse around the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legal_scholars_and_advocates, observer,
    analytical, generational, analytical, global).

% Argue that the liberal due process reading distorts the original intent and historical context of Magna Carta, expanding its scope far beyond what was envisioned in 1215. Their arguments are often marginalized in mainstream constitutional discourse but persist in academic and some judicial circles.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, originalist_legal_theorists, excluded,
    moderate, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common framework for legitimate state action, ensuring predictability and fairness in the application of law, thereby coordinating individual liberty with governmental authority.
% TRANSFER_FUNCTION: Transfers power from the executive and legislative branches to the individual, requiring the state to expend resources (time, legal process, justification) before infringing on liberty or property.
% ABSENT_VOICES: Originalist legal theorists and those who prioritize state efficiency over individual procedural protections are often marginalized in the discourse surrounding this reading, arguing that its expansive interpretation creates undue burdens on governance.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the state would gain immense arbitrary power, leading to widespread abuses, erosion of civil liberties, and a fundamental shift in the relationship between the government and its citizens. The entire legal and political system would have to reorganize.
% FOUNDING_PROBLEM: The problem of arbitrary royal power and the need to establish fundamental limits on the monarch's ability to act without legal justification, particularly concerning the lives, liberty, and property of subjects.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, constitutional scholars, and human rights organizations universally corroborate the historical problem of arbitrary power and its ongoing relevance in modern states, even if they dispute the precise scope of Clause 39's original intent.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading imposes significant costs on state power, requiring extensive procedural safeguards. Suppression (0.7) is also high, reflecting the active judicial enforcement needed to maintain these limits against governmental impulses. The theater ratio is low (0.1) because the judicial function of upholding due process is generally genuine, not performative. The historical measurements show a clear trend of increasing extractiveness and suppression as the reading's scope expanded over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and rights advocates, this reading is a vital Rope or even a Mountain, representing fundamental justice. From the perspective of the executive or legislative majorities, it can feel like a Snare, unduly restricting their ability to govern efficiently or implement popular policies. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary acts as the agenda-setter and primary beneficiary, gaining institutional power and legitimacy by enforcing these rights. Citizens and residents are also beneficiaries, receiving protection. The executive branch and legislative majorities are the payers, as their power is constrained. Originalist theorists are excluded, as their narrow interpretation is largely rejected by this reading's proponents.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_scope_ambiguity,
    'Is the expansive ''liberal due process'' interpretation a legitimate evolution of Clause 39''s principles, or a modern anachronism imposed on a medieval text?',
    'Further historical and legal scholarship on the ''spirit'' vs. ''letter'' of Magna Carta, and comparative analysis of constitutional evolution in other common law systems.',
    'If deemed anachronistic, the legitimacy of this reading as a foundational constraint would weaken, potentially reclassifying it as a Snare or Tangled Rope that extracts power from the state based on a misreading. If affirmed as legitimate evolution, its Mountain-like qualities (as a foundational principle) would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_scope_ambiguity, conceptual, 'Debate over the historical fidelity of the liberal due process reading.').

omega_variable(
    judicial_activism_vs_interpretation,
    'Does the judiciary''s expansive interpretation of Clause 39 constitute legitimate constitutional interpretation, or an overreach of judicial power (judicial activism)?',
    'Ongoing legal and political debate, public opinion shifts, and the outcomes of future constitutional challenges and appointments to the judiciary.',
    'If widely seen as activism, the judiciary''s legitimacy as an agenda-setter could erode, increasing resistance to its rulings and potentially shifting the constraint towards a more contested Tangled Rope or even a Snare from the perspective of the legislative and executive branches. If seen as legitimate, its Rope-like coordination function would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_interpretation, preference, 'The normative debate over the proper role of the judiciary in interpreting fundamental rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1215, 0.05).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1688, 0.08).
narrative_ontology:measurement(magn_tr_t1791, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(magn_tr_t1945, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1215, 0.2).
narrative_ontology:measurement(magn_be_t1688, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1688, 0.4).
narrative_ontology:measurement(magn_be_t1791, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1791, 0.6).
narrative_ontology:measurement(magn_be_t1945, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1945, 0.75).
narrative_ontology:measurement(magn_be_t2024, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1215, 0.3).
narrative_ontology:measurement(magn_su_t1688, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1688, 0.45).
narrative_ontology:measurement(magn_su_t1791, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1791, 0.55).
narrative_ontology:measurement(magn_su_t1945, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1945, 0.65).
narrative_ontology:measurement(magn_su_t2024, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, us_bill_of_rights_due_process_clause).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, european_convention_human_rights_article_5).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of Magna Carta Clause 39, each modeled as a separate constraint due to differing ε values and stakeholder structures. The other readings are 'feudal_prerogative_reading' and 'originalist_limitation_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
