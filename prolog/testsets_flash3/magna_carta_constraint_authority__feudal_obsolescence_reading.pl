% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta's Feudal Obsolescence (Historical Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'feudal obsolescence' reading of Magna
 *   Carta's authority, arguing that it was a specific baronial compact with
 *   no binding force over modern sovereignty. This reading serves to maximize
 *   executive discretion and parliamentary power by dismissing historical
 *   constitutional limits. The constraint is classified as a Piton because
 *   its primary function (limiting royal power) has atrophied, but it
 *   persists as a rhetorical tool, maintained theatrically to justify modern
 *   power structures. The high theater ratio reflects the performative
 *   dismissal of its modern relevance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.75).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta's Feudal Obsolescence (Historical Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'ff5f50ee-8197-48b0-a2fb-07a1fb164456').
narrative_ontology:cs_kernel_codification('ff5f50ee-8197-48b0-a2fb-07a1fb164456', fixed_text).
narrative_ontology:cs_authority_grounding('ff5f50ee-8197-48b0-a2fb-07a1fb164456', extraction).
narrative_ontology:cs_interpretation_layer_present('ff5f50ee-8197-48b0-a2fb-07a1fb164456').
narrative_ontology:cs_reading_relation('ff5f50ee-8197-48b0-a2fb-07a1fb164456', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('ff5f50ee-8197-48b0-a2fb-07a1fb164456', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('ff5f50ee-8197-48b0-a2fb-07a1fb164456', foundational, charter_authority_is_time_bound).
narrative_ontology:cs_axiom_status(charter_authority_is_time_bound, holdable).
narrative_ontology:cs_axiom_grounding('ff5f50ee-8197-48b0-a2fb-07a1fb164456', charter_authority_is_time_bound, conventional).
narrative_ontology:cs_axiom('ff5f50ee-8197-48b0-a2fb-07a1fb164456', foundational, modern_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(modern_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ff5f50ee-8197-48b0-a2fb-07a1fb164456', modern_sovereignty_is_absolute, conventional).
narrative_ontology:cs_reference_frame('ff5f50ee-8197-48b0-a2fb-07a1fb164456', original_feudal_compact).
narrative_ontology:cs_drift_state('ff5f50ee-8197-48b0-a2fb-07a1fb164456', contemporary_legal_discourse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ff5f50ee-8197-48b0-a2fb-07a1fb164456', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_proponents).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, citizens_seeking_charter_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the argument that Magna Carta is historically obsolete, allowing greater executive discretion and fewer constitutional restraints. Actively promotes this reading to justify expansive powers.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch, agenda_setter,
    institutional, biographical, mobile, national).

% Benefits from the obsolescence reading by asserting that any enduring principles from Magna Carta are now solely within the purview of parliamentary statute, reinforcing legislative supremacy over ancient charters.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_advocates, beneficiary,
    institutional, generational, mobile, national).

% Bears the cost of this reading, as it undermines the idea of a foundational, enduring constitutional text that limits state power. Their arguments for inherited rights and popular sovereignty are weakened.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates, payer,
    organized, generational, constrained, national).

% Judges and legal scholars who argue for judicial review and constitutional limits on power find their arguments diminished by a reading that strips Magna Carta of modern legal force. Their ability to invoke historical precedent is curtailed.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_proponents, payer,
    institutional, generational, constrained, national).

% Individuals who might appeal to Magna Carta as a source of fundamental rights or due process find such claims dismissed as anachronistic, leaving them with fewer avenues for legal recourse against state power.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, citizens_seeking_charter_rights, payer,
    powerless, immediate, trapped, local).

% Analyze Magna Carta within its original 13th-century context, often supporting the 'feudal compact' interpretation without necessarily endorsing its modern political implications. Their work provides the empirical grounding for the obsolescence claim.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, historical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the understanding of historical legal documents, asserting that their authority is time-bound and specific to their original context, preventing anachronistic application to modern governance.
% TRANSFER_FUNCTION: Transfers interpretive authority over constitutional limits from ancient charters to contemporary political institutions (Parliament, Executive), effectively maximizing executive discretion and legislative power by removing historical constraints.
% ABSENT_VOICES: Advocates for a 'higher law' tradition, natural rights theorists, and those who believe in the enduring, evolving spirit of constitutional documents are marginalized. They would argue for the charter's continued relevance as a source of fundamental liberties.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the actual legal and political structures would not immediately change, as modern sovereignty already largely operates under the assumption of parliamentary or executive supremacy. However, the rhetorical and philosophical landscape for constitutional arguments would shift, empowering those who seek to invoke historical charters as binding constraints.
% FOUNDING_PROBLEM: The problem of anachronism: how to prevent ancient documents, created for specific historical contexts, from being misapplied to fundamentally different modern political systems, thereby distorting contemporary legal and political discourse.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholars and legal positivists outside the executive branch corroborate the problem of anachronism, arguing that applying 13th-century feudal law directly to modern states is conceptually unsound. However, popular constitutionalism advocates contest that the 'problem' is a pretext for power maximization.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_unchanged).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this reading removes historical checks on power, allowing modern institutions to operate with fewer constraints. Suppression is high because it actively suppresses alternative readings that would assert enduring constitutional limits. The theater ratio is very high (0.85) as the constraint's 'maintenance' involves performatively declaring its irrelevance, while its actual function is to clear the way for modern power. Resistance is high from those who advocate for a living constitution or popular constitutionalism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the executive and Parliament, this reading is a necessary clarification of historical context, enabling effective modern governance. From the perspective of popular constitutionalism and juridical restraint, it is a strategic dismissal of foundational principles to consolidate power.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and parliamentary sovereignty advocates are beneficiaries, as this reading expands their power. Popular constitutionalism advocates, juridical restraint proponents, and citizens seeking charter rights are victims, as their claims are undermined. Historical scholars act as observers, providing the academic basis for the obsolescence argument without necessarily endorsing its political use.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear case of mandatrophy. Its original mandate (to limit royal power in a feudal context) is obsolete. However, the constraint persists not as a benign historical artifact, but as a Piton, where its 'obsolescence' is actively maintained and leveraged to extract power from those who would invoke it as a living constitutional limit. The high theater ratio and rising extractiveness over time indicate this performative maintenance for extractive ends.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modern_relevance_of_feudal_grievances,
    'Are the underlying principles of Magna Carta (e.g., due process, rule of law) truly obsolete, or do they represent universal legal principles that transcend their feudal origins?',
    'Comparative legal analysis across jurisdictions that explicitly incorporate Magna Carta principles into modern constitutional law, and philosophical inquiry into the universality of its core tenets.',
    'If universal principles are identified, the ''feudal obsolescence'' reading''s extractiveness would be re-evaluated downward, as its dismissal of modern relevance would be seen as a cover for power maximization rather than a genuine historical insight. This would shift its classification closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modern_relevance_of_feudal_grievances, conceptual, 'Whether Magna Carta''s principles are time-bound or universal.').

omega_variable(
    rhetorical_use_vs_actual_legal_effect,
    'To what extent is the ''feudal obsolescence'' reading a genuine legal interpretation versus a rhetorical strategy employed by the executive and Parliament to justify expanded powers?',
    'Analysis of legislative and judicial decisions where Magna Carta is invoked: does the obsolescence argument genuinely guide legal reasoning, or is it primarily used to dismiss inconvenient challenges to authority?',
    'If primarily rhetorical, the ''theater_ratio'' would be even higher, and the ''suppression'' metric would be seen as more directly coercive, as the legal argument serves as a cover for power consolidation. This would reinforce its Piton classification, potentially pushing it towards Snare if the coordination function is entirely absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_use_vs_actual_legal_effect, empirical, 'Distinguishing genuine legal interpretation from rhetorical power plays.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine historical reading of the Magna Carta kernel, or a strategic re-framing to enable contemporary power maximization?',
    'Comparative analysis of historical scholarship on Magna Carta''s reception and interpretation across different eras, focusing on the motivations and contexts of those who advanced the ''obsolescence'' argument.',
    'If primarily strategic, the ''claimed_type'' would be re-evaluated from Piton to Snare, as the ''atrophied function'' would be revealed as a deliberate, extractive re-interpretation rather than a natural decay. The ''extractiveness'' and ''suppression'' metrics would be seen as more intentional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''feudal_obsolescence_reading'' of the ''magna_carta_constraint_authority'' kernel. Sibling readings include ''living_constitutionalism_reading'' and ''parliamentary_sovereignty_reading''. The disagreement is located in the enduring legal and political authority of the charter''s principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1900, 0.7).
narrative_ontology:measurement(magn_tr_t1930, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1930, 0.75).
narrative_ontology:measurement(magn_tr_t1960, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1960, 0.8).
narrative_ontology:measurement(magn_tr_t1990, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1990, 0.83).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2024, 0.85).

% Extraction over time
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(magn_be_t1930, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1930, 0.55).
narrative_ontology:measurement(magn_be_t1960, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(magn_be_t1990, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(magn_su_t1930, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1930, 0.65).
narrative_ontology:measurement(magn_su_t1960, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(magn_su_t1990, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, identity_coordination).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_constraint).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_discretion_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'magna_carta_constraint_authority' kernel. This 'feudal obsolescence' reading contrasts with the 'living constitutionalism' and 'parliamentary sovereignty' readings, each representing a distinct interpretation of Magna Carta's modern legal force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
