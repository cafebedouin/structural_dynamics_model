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
    narrative_ontology:coordination_type/2,
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
 *   Carta's Clause 39, which interprets it as a foundational statement of
 *   universal individual rights against arbitrary state power. This reading
 *   expands the clause beyond its original feudal context to apply to all
 *   citizens and to constrain executive and legislative overreach. It is a
 *   contested interpretation, standing in contrast to more restrictive
 *   readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.65).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.4).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39: Liberal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, '04ccd19d-3327-4597-b469-f4301429a084').
narrative_ontology:cs_kernel_codification('04ccd19d-3327-4597-b469-f4301429a084', fixed_text).
narrative_ontology:cs_authority_grounding('04ccd19d-3327-4597-b469-f4301429a084', lineage).
narrative_ontology:cs_interpretation_layer_present('04ccd19d-3327-4597-b469-f4301429a084').
narrative_ontology:cs_reading_relation('04ccd19d-3327-4597-b469-f4301429a084', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('04ccd19d-3327-4597-b469-f4301429a084', magna_carta_clause_39__originalist_limitation_reading, forecloses).
narrative_ontology:cs_axiom('04ccd19d-3327-4597-b469-f4301429a084', foundational, universal_individual_rights).
narrative_ontology:cs_axiom_status(universal_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('04ccd19d-3327-4597-b469-f4301429a084', universal_individual_rights, deontological).
narrative_ontology:cs_axiom('04ccd19d-3327-4597-b469-f4301429a084', foundational, evolving_constitutional_meaning).
narrative_ontology:cs_axiom_status(evolving_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('04ccd19d-3327-4597-b469-f4301429a084', evolving_constitutional_meaning, conventional).
narrative_ontology:cs_reference_frame('04ccd19d-3327-4597-b469-f4301429a084', post_enlightenment_constitutionalism).
narrative_ontology:cs_drift_state('04ccd19d-3327-4597-b469-f4301429a084', contemporary_authoritarian_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('04ccd19d-3327-4597-b469-f4301429a084', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, citizens).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_power).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, legislative_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces Clause 39, expanding its scope to protect universal individual rights. Benefits from increased authority and legitimacy as a guardian of rights. Bears the cost of defending this interpretation against political pressure.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from protection against arbitrary state action, including due process rights and limits on executive power. Their ability to exit arbitrary rule is constrained by the state's monopoly on force, making the constraint vital.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, citizens, beneficiary,
    organized, biographical, constrained, national).

% Bears the cost of limitations on its discretionary power, requiring adherence to due process and legal procedures. Its ability to act unilaterally is curtailed, making it a primary target of this reading.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_power, payer,
    institutional, immediate, trapped, national).

% Bears the cost of limitations on its ability to pass laws that infringe on individual rights without due process. While powerful, it is constrained by judicial review based on this reading of Clause 39.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legislative_majorities, payer,
    institutional, biographical, constrained, national).

% Analyze and debate the historical and contemporary meaning of Clause 39, contributing to the intellectual framework that sustains or challenges the liberal due process reading. They do not directly benefit or pay, but their work influences the judiciary and public discourse.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__liberal_due_process_reading, judiciary).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__liberal_due_process_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common understanding of fundamental individual rights and procedural fairness, coordinating state action to prevent arbitrary rule and ensure justice.
% TRANSFER_FUNCTION: Transfers power from unchecked state authority to individual citizens and the judiciary, requiring the state to expend resources on due process and legal justification for its actions.
% ABSENT_VOICES: Those who advocate for unchecked executive power or legislative supremacy, arguing that the liberal due process reading unduly constrains effective governance, are often marginalized in legal discourse that prioritizes individual rights.
% DISAPPEARANCE_RATIONALE: If this reading of Clause 39 vanished, the legal landscape would fundamentally shift. Executive and legislative powers would face fewer constraints, potentially leading to arbitrary actions, erosion of individual liberties, and a significant rebalancing of power within the state. The judiciary's role would diminish, and citizens would lose a key defense against state overreach.
% FOUNDING_PROBLEM: The problem of arbitrary rule by the monarch and the need for a legal framework to protect individuals from unchecked state power.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and international bodies consistently attest that the problem of arbitrary state power remains live, even in modern democracies. Judicial decisions and public advocacy from outside the immediate beneficiaries (e.g., civil liberties groups) corroborate the ongoing relevance of this constraint.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high because this reading actively limits state power, forcing it to adhere to due process. Suppression (0.40) is moderate; while the state is constrained, it still possesses significant power to interpret and apply the law, and resistance to this expansive reading exists. Theater ratio (0.10) is low, as the constraint is genuinely invoked and enforced, not merely performative. Accessibility collapse (0.30) is low because alternatives to due process (e.g., arbitrary action) are always present for the state, but resistance (0.70) is high because citizens and the judiciary actively defend this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and citizens experience this as a vital protection and a coordination mechanism for a just society. Executive and legislative powers, however, experience it as a significant constraint on their authority, requiring active enforcement by the judiciary to hold.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and citizens are beneficiaries (d near 0.0) as they gain protection and a framework for justice. Executive power and legislative majorities are victims (d near 1.0) as their ability to act arbitrarily is curtailed. The constraint actively extracts from unchecked state power.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the constraint from becoming a Piton by actively reinterpreting and applying it to contemporary issues, thus keeping its mandate 'live' and preventing its function from atrophying into mere historical theater. The ongoing contestation ensures its relevance, even if it means it computes as a Tangled Rope rather than a pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_scope_ambiguity,
    'Is Clause 39 a foundational statement of universal rights (liberal due process reading) or a specific feudal grievance (feudal prerogative reading)?',
    'Analysis of historical legal interpretation evolution, judicial precedent, and contemporary constitutional application across jurisdictions.',
    'If the feudal prerogative reading prevails, the constraint''s scope and extractiveness against state power would be significantly reduced, potentially reclassifying it as a Rope or even a Piton for modern contexts. If the liberal due process reading is affirmed, it remains a Tangled Rope, actively extracting from arbitrary state power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_scope_ambiguity, conceptual, 'Ambiguity in the scope and intent of Magna Carta Clause 39.').

omega_variable(
    originalist_vs_living_constitution,
    'Does Clause 39''s meaning evolve with societal norms (liberal due process reading) or is it fixed to its 1215 context (originalist limitation reading)?',
    'Ongoing judicial and scholarly debate, and the practical application of the clause in novel legal challenges.',
    'If the originalist limitation reading were to dominate, the constraint''s effective extractiveness against modern state power would diminish, as many contemporary abuses fall outside the 1215 context. This would shift its classification towards a Piton or even a Mountain (if its ''naturalness'' as a historical artifact is emphasized).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_vs_living_constitution, conceptual, 'Contest between originalist and evolving interpretations of Clause 39.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(magn_tr_t50, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(magn_tr_t200, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement(magn_tr_t300, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 300, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(magn_be_t50, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 100, 0.5).
narrative_ontology:measurement(magn_be_t200, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(magn_be_t300, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 300, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(magn_su_t50, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 50, 0.25).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 100, 0.3).
narrative_ontology:measurement(magn_su_t200, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement(magn_su_t300, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 300, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of Magna Carta Clause 39, each with different structural properties and classifications. This 'liberal due process' reading is linked to the 'feudal prerogative' and 'originalist limitation' readings through shared kernel identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
