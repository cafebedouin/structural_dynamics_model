% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story analyzes the doctrine of qualified immunity from a
 *   'constitutional fidelity' reading. This reading asserts that qualified
 *   immunity is a judicially fabricated doctrine lacking constitutional or
 *   statutory authorization, and is therefore illegitimate regardless of its
 *   purported policy outcomes. It views the doctrine as an expansion of
 *   judicial power and a barrier to accountability for constitutional
 *   violations. The metrics reflect the increasing extractiveness and
 *   suppression of this judicially created barrier over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.9).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.95).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, 'b7082121-065c-4aab-8d85-dcd5e83d3468').
narrative_ontology:cs_kernel_codification('b7082121-065c-4aab-8d85-dcd5e83d3468', implicit).
narrative_ontology:cs_authority_grounding('b7082121-065c-4aab-8d85-dcd5e83d3468', lineage).
narrative_ontology:cs_interpretation_layer_present('b7082121-065c-4aab-8d85-dcd5e83d3468').
narrative_ontology:cs_reading_relation('b7082121-065c-4aab-8d85-dcd5e83d3468', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7082121-065c-4aab-8d85-dcd5e83d3468', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('b7082121-065c-4aab-8d85-dcd5e83d3468', foundational, judicial_power_limited_to_constitution_and_statute).
narrative_ontology:cs_axiom_status(judicial_power_limited_to_constitution_and_statute, holdable).
narrative_ontology:cs_axiom_grounding('b7082121-065c-4aab-8d85-dcd5e83d3468', judicial_power_limited_to_constitution_and_statute, deontological).
narrative_ontology:cs_axiom('b7082121-065c-4aab-8d85-dcd5e83d3468', foundational, accountability_for_constitutional_violations_is_fundamental).
narrative_ontology:cs_axiom_status(accountability_for_constitutional_violations_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('b7082121-065c-4aab-8d85-dcd5e83d3468', accountability_for_constitutional_violations_is_fundamental, deontological).
narrative_ontology:cs_reference_frame('b7082121-065c-4aab-8d85-dcd5e83d3468', original_constitutional_design_accountability).
narrative_ontology:cs_drift_state('b7082121-065c-4aab-8d85-dcd5e83d3468', contemporary_judicial_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b7082121-065c-4aab-8d85-dcd5e83d3468', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_constitutional_violations).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The federal courts, particularly the Supreme Court, created and continue to refine the doctrine of qualified immunity without explicit constitutional or statutory basis. This reading views them as expanding their own power by fabricating legal principles.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Officers benefit from the doctrine by being shielded from liability in civil rights lawsuits unless their conduct violates 'clearly established statutory or constitutional rights of which a reasonable person would have known.' This reading sees them as beneficiaries of an illegitimate judicial overreach.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, beneficiary,
    organized, biographical, constrained, local).

% Individuals whose constitutional rights are violated by law enforcement often find their claims dismissed due to qualified immunity, even when a violation occurred. They bear the cost of the doctrine's existence by being denied legal recourse.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_constitutional_violations, payer,
    powerless, immediate, trapped, local).

% Organizations and lawyers dedicated to protecting civil rights face significant hurdles in litigating cases against law enforcement due to qualified immunity. They expend considerable resources challenging a doctrine they view as illegitimate and an impediment to justice.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_advocates, payer,
    organized, generational, constrained, national).

% The legislative branch has not explicitly authorized qualified immunity, yet its attempts to reform or abolish the doctrine face judicial resistance and political inertia. This reading views the judiciary as having usurped legislative authority.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, legislative_branch, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the doctrine does not solve a legitimate coordination problem but rather creates a framework for judicial power expansion and officer impunity, under the guise of protecting law enforcement.
% TRANSFER_FUNCTION: Transfers accountability for constitutional violations from individual officers and the state to the victims, effectively denying redress and shifting the burden of harm.
% ABSENT_VOICES: The framers of the Constitution and the authors of Section 1983 (the civil rights statute under which most qualified immunity cases are brought) would object to a judicially created immunity that undermines the principle of government accountability.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, the landscape of civil rights litigation would fundamentally change. More cases against officers would proceed, potentially leading to increased accountability, changes in police training and policy, and a rebalancing of power between citizens and the state. The federal judiciary would lose a significant area of discretionary power.
% FOUNDING_PROBLEM: The doctrine was ostensibly created to protect government officials from frivolous lawsuits and the chilling effect of potential liability, allowing them to perform their duties without undue fear of litigation.
% FOUNDING_PROBLEM_CORROBORATION: The federal judiciary and law enforcement agencies attest the problem is live, arguing that officers need protection to perform their duties effectively. Civil rights advocates and legal scholars, citing historical analysis and the lack of statutory basis, contend that the 'founding problem' is a pretext for judicial overreach and that the doctrine itself is the problem.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.9) because the doctrine effectively denies legal recourse to victims of constitutional violations, transferring the cost of harm and the burden of proof onto them. Suppression is extremely high (0.95) as the doctrine actively shields officers from liability, making it nearly impossible for victims to succeed in court. The theater ratio is low (0.1) because, from this reading, the doctrine's stated purpose (protecting officers from frivolous lawsuits) is largely a cover for its actual function of granting impunity, with little genuine coordination benefit. The increasing values over time reflect the judicial expansion of the doctrine, making it progressively harder to overcome.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the federal judiciary, the doctrine is a necessary judicial tool for managing litigation and ensuring effective governance. From the perspective of victims and civil rights advocates, it is an illegitimate barrier to justice. This story explicitly adopts the latter, constitutional fidelity, perspective, which sees the doctrine as a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary is the primary agenda-setter and beneficiary, having expanded its own power through the doctrine's creation and refinement. Law enforcement officers are direct beneficiaries, shielded from liability. Victims of constitutional violations and civil rights advocates are the primary payers, bearing the costs of denied justice and increased litigation burden. The legislative branch is excluded, as its authority to define immunities has been usurped by the judiciary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_source,
    'Does the federal judiciary possess inherent authority to create immunities not explicitly granted by the Constitution or statute?',
    'A definitive Supreme Court ruling explicitly disavowing or affirming such inherent power, or a constitutional amendment clarifying the scope of judicial authority.',
    'If such authority is denied, the doctrine is fundamentally illegitimate, reinforcing its classification as a snare. If affirmed, the doctrine gains a new, albeit controversial, source of legitimacy, potentially shifting its classification towards a tangled rope (if a coordination function is acknowledged).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_legitimacy_source, conceptual, 'Ambiguity regarding the source and scope of judicial power to create legal immunities.').

omega_variable(
    legislative_inaction_implication,
    'Does congressional inaction on qualified immunity imply tacit approval or merely political gridlock?',
    'Explicit legislative action (or clear, unambiguous inaction with stated intent) to either codify, reform, or abolish qualified immunity.',
    'If inaction implies tacit approval, it could lend a thin layer of ''conventional'' legitimacy to the doctrine, slightly reducing its perceived extractiveness from a purely legalistic standpoint. If it''s merely gridlock, the doctrine''s lack of legislative grounding remains a core illegitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_inaction_implication, empirical, 'Whether legislative silence on qualified immunity constitutes implicit authorization or merely a failure to act.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(qual_be_t1980, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(qual_be_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(qual_be_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(qual_su_t1980, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(qual_su_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1995, 0.8).
narrative_ontology:measurement(qual_su_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, police_accountability_mechanisms).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_litigation_access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
