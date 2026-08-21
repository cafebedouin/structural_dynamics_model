% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Allocation: Functional Accommodation Reading
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint describes the 'functional accommodation' reading of U.S.
 *   war powers, where the executive branch has latitude for unilateral action
 *   in immediate threats, but prolonged campaigns are understood to require
 *   congressional authorization. This reading acknowledges the practical
 *   necessity of executive speed while attempting to preserve legislative
 *   oversight. It operates as a Tangled Rope because it genuinely coordinates
 *   rapid response but also enables asymmetric extraction of authority by the
 *   executive, sustained by active enforcement (e.g., executive branch legal
 *   interpretations, information control) and the suppression of categorical
 *   rules.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.65).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.7).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Allocation: Functional Accommodation Reading").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'ed3c275d-b262-4737-b9b8-f65d9b376752').
narrative_ontology:cs_kernel_codification('ed3c275d-b262-4737-b9b8-f65d9b376752', fixed_text).
narrative_ontology:cs_authority_grounding('ed3c275d-b262-4737-b9b8-f65d9b376752', lineage).
narrative_ontology:cs_interpretation_layer_present('ed3c275d-b262-4737-b9b8-f65d9b376752').
narrative_ontology:cs_reading_relation('ed3c275d-b262-4737-b9b8-f65d9b376752', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed3c275d-b262-4737-b9b8-f65d9b376752', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('ed3c275d-b262-4737-b9b8-f65d9b376752', foundational, executive_flexibility_in_crisis).
narrative_ontology:cs_axiom_status(executive_flexibility_in_crisis, holdable).
narrative_ontology:cs_axiom_grounding('ed3c275d-b262-4737-b9b8-f65d9b376752', executive_flexibility_in_crisis, instrumental).
narrative_ontology:cs_axiom('ed3c275d-b262-4737-b9b8-f65d9b376752', foundational, congressional_oversight_for_sustained_force).
narrative_ontology:cs_axiom_status(congressional_oversight_for_sustained_force, holdable).
narrative_ontology:cs_axiom_grounding('ed3c275d-b262-4737-b9b8-f65d9b376752', congressional_oversight_for_sustained_force, deontological).
narrative_ontology:cs_reference_frame('ed3c275d-b262-4737-b9b8-f65d9b376752', post_war_powers_resolution_balance).
narrative_ontology:cs_drift_state('ed3c275d-b262-4737-b9b8-f65d9b376752', contemporary_global_threat_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed3c275d-b262-4737-b9b8-f65d9b376752', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_branch).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, public_discourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, military_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts the need for flexibility in responding to threats, often acting unilaterally in initial phases of military action. Benefits from the ambiguity that allows for rapid deployment without immediate legislative hurdles. Bears the political cost of prolonged, unauthorized engagements.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, biographical, constrained, global).

% Holds the constitutional power to declare war and fund military operations, but often finds its authority bypassed or presented with faits accomplis by the executive. Bears the cost of ceding oversight and legitimacy in military actions, but is reluctant to challenge the executive directly in times of perceived crisis.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congressional_branch, payer,
    institutional, generational, constrained, national).

% Generally defers to the political branches on war powers, viewing it as a 'political question' outside its purview. Observes the ongoing contest but rarely intervenes, reinforcing the functional accommodation by its non-action.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, judicial_branch, observer,
    institutional, civilizational, analytical, national).

% Receives conflicting signals about the legitimacy and scope of military actions, leading to confusion and reduced accountability. Bears the cost of diminished democratic oversight and a less informed debate on matters of war and peace.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, public_discourse, payer,
    moderate, immediate, constrained, national).

% Execute orders from the executive branch, often in contexts where the legal basis for deployment is ambiguous or contested. Their professional identity and chain of command make challenging the authority difficult, regardless of the constitutional debate.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, military_personnel, payer,
    powerless, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows for flexible and timely response to national security threats by permitting executive action in urgent situations, while theoretically requiring legislative input for sustained engagements.
% TRANSFER_FUNCTION: Transfers decision-making authority and operational control over military force from the legislative branch to the executive branch in situations deemed urgent, and transfers the burden of legitimizing prolonged engagements back to Congress.
% ABSENT_VOICES: Constitutional scholars advocating for strict adherence to congressional war powers, and citizens' groups demanding greater democratic accountability for military interventions, are often marginalized in the immediate aftermath of executive action, only gaining traction as campaigns prolong.
% DISAPPEARANCE_RATIONALE: If this functional accommodation vanished, either the executive would be paralyzed in responding to immediate threats (requiring prior authorization for every action), or Congress would lose all claim to war powers (ceding total authority to the executive). The current system, however imperfect, allows both branches to operate within a contested but functional framework.
% FOUNDING_PROBLEM: The U.S. Constitution divided war powers between the executive (Commander-in-Chief) and legislative (declare war, raise armies) branches, creating an inherent tension that needed a practical mechanism for governance in a dynamic threat environment.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate the founding problem, noting the framers' intent to balance efficiency with deliberation. The ongoing debates and legislative actions (e.g., War Powers Resolution) attest to its continued live status, with corroboration from both executive and legislative archives, as well as independent academic analysis.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the executive consistently pushes the boundaries of 'imminent threat' to expand its unilateral authority, effectively extracting power from Congress. Suppression (0.70) is high due to the executive's control over information, its ability to present Congress with faits accomplis, and the political difficulty for Congress to challenge military action once underway. Theater ratio (0.40) reflects the performative aspects of 'consultation' with Congress that often occur after decisions are made, or the use of vague authorizations that serve more to provide political cover than genuine oversight. The cyclical nature of measurements reflects periods of executive overreach followed by congressional pushback or public scrutiny, but the underlying structural ambiguity persists.
 *
 * PERSPECTIVAL GAP:
 *   The executive branch perceives this as a necessary, efficient coordination mechanism for national security, while the legislative branch and public discourse increasingly view it as an extractive mechanism that erodes democratic checks and balances. The engine's classification will highlight this divergence, showing a claimed 'rope' (coordination) operating with 'tangled_rope' or 'snare' characteristics from the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch is the primary beneficiary, gaining flexibility and initiative (low d). The legislative branch and public discourse are the primary targets, losing oversight and accountability (high d). The judicial branch is an analytical observer, largely outside the direct flow of extraction. Military personnel are targets, bearing the direct costs of deployment under ambiguous authority, with their identity-locked exit options amplifying their target status.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the functional accommodation as pure coordination (a Rope) by highlighting the substantial extraction and suppression involved. It also avoids mislabeling it as pure extraction (a Snare) by acknowledging the genuine, albeit often abused, coordination function of rapid executive response. The 'tangled_rope' classification captures the hybrid nature, where a legitimate coordination problem is solved through a structure that also enables asymmetric power transfer. The founding problem is 'live' but its 'status' is 'contested' because the original balance intended by the framers is constantly being re-negotiated through practice, often to the executive's advantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_imminent_threat,
    'What constitutes an ''imminent threat'' justifying unilateral executive action, and who defines its boundaries?',
    'Establishment of clear, judicially reviewable criteria for ''imminence'' by legislative action, or a Supreme Court ruling defining the term''s constitutional scope.',
    'A narrower definition would reduce executive unilateralism and shift the constraint towards a more ''rope-like'' coordination, increasing congressional oversight. A broader definition would further entrench executive power, pushing it towards a ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_imminent_threat, conceptual, 'Ambiguity in defining ''imminent threat'' allows executive overreach.').

omega_variable(
    congressional_will_to_assert,
    'To what extent does congressional inaction or acquiescence reflect a genuine delegation of authority versus a political reluctance to challenge the executive?',
    'Empirical analysis of voting patterns, legislative debates, and public statements by members of Congress, particularly during periods of executive military action, coupled with expert testimony on political incentives.',
    'If inaction is primarily political reluctance, the ''suppression'' metric is higher than it appears, as Congress''s ''constrained'' exit is amplified by internal political costs. If it''s genuine delegation, the constraint is more ''rope-like'' than currently assessed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(congressional_will_to_assert, empirical, 'Ambiguity in congressional intent regarding executive war powers.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of congressional war powers structural (executive control of information, legal interpretations) or internalized (congressional deference, political risk aversion)?',
    'Post-executive-action suppression trajectory: if congressional deference persists even after executive action is widely criticized or deemed unsuccessful, reclassify as partially internalized. If legislative challenges increase with public dissent, it''s more structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — Congress carries the suppression with them after the immediate crisis. If structural, legislative remedies are more likely to be effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in war powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__functional_accommodation_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__functional_accommodation_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__functional_accommodation_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__functional_accommodation_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__functional_accommodation_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__functional_accommodation_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(war__tr_t70, war_powers_allocation__functional_accommodation_reading, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(war__be_t70, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 70, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 60, 0.67).
narrative_ontology:measurement(war__su_t70, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 70, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, inherent_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'war_powers_allocation' kernel. It represents a functional accommodation between executive and legislative branches, influencing and coexisting with other readings that emphasize either strict congressional primacy or inherent executive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
