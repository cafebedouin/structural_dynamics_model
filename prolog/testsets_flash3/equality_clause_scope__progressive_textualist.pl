% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope (Progressive Textualist Reading)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'progressive textualist' reading of a
 *   constitutional equality clause, where the principle of equality is
 *   acknowledged in the text, but its application scope expands primarily
 *   through the democratic amendment process, not through judicial
 *   reinterpretation. This reading seeks a moderate path, balancing the
 *   original limits of the text with the capacity for societal evolution,
 *   always requiring supermajority democratic consent for expansion. It is
 *   one reading of the 'equality_clause_scope' kernel, distinct from more
 *   restrictive or expansive interpretations.
 *
 * KEY AGENTS:
 *   - democratic_majority: Primary beneficiary (institutional/mobile) — defines scope via amendment
 *   - minority_groups_awaiting_recognition: Primary payer (organized/constrained) — bears delay of recognition
 *   - judicial_activism_advocates: Payer (powerful/constrained) — opposes limits on judicial power
 *   - amendment_process_advocates: Agenda setter (institutional/analytical) — champions democratic change
 *   - expansive_universalist_advocates: Excluded (powerful/identity_locked) — prefers immediate judicial application
 *   - restrictive_originalist_advocates: Excluded (powerful/identity_locked) — opposes any expansion beyond original intent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.35).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.45).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.35).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope (Progressive Textualist Reading)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, 'e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a').
narrative_ontology:cs_kernel_codification('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', fixed_text).
narrative_ontology:cs_authority_grounding('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', lineage).
narrative_ontology:cs_interpretation_layer_present('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a').
narrative_ontology:cs_reading_relation('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', foundational, amendment_as_primary_evolution_mechanism).
narrative_ontology:cs_axiom_status(amendment_as_primary_evolution_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', amendment_as_primary_evolution_mechanism, conventional).
narrative_ontology:cs_axiom('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', foundational, textual_limits_on_judicial_interpretation).
narrative_ontology:cs_axiom_status(textual_limits_on_judicial_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', textual_limits_on_judicial_interpretation, conventional).
narrative_ontology:cs_reference_frame('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', constitutional_amendment_supremacy).
narrative_ontology:cs_drift_state('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e7d6b2c2-71f2-4d5f-a6e1-d0db70b83a4a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, democratic_majority).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, amendment_process_advocates).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, minority_groups_awaiting_recognition).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, judicial_activism_advocates).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, popular_sovereignty).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, constitutional_amendment_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the principle that fundamental changes to equality's scope require broad democratic consensus, typically expressed through the amendment process, rather than judicial fiat. This preserves the majority's role in defining societal norms.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, democratic_majority, beneficiary,
    institutional, generational, mobile, national).

% Bears the cost of delayed recognition of their equality claims, as expansion of rights requires the slow and difficult process of constitutional amendment rather than potentially faster judicial interpretation. Their access to full equality is contingent on supermajority consent.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, minority_groups_awaiting_recognition, payer,
    organized, generational, constrained, national).

% Opposes this reading because it limits the judiciary's power to interpret and expand equality rights, forcing social change through the legislative and amendment processes, which they view as less efficient or responsive to rights violations.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judicial_activism_advocates, payer,
    powerful, biographical, constrained, national).

% Champions the constitutional amendment process as the legitimate and primary mechanism for evolving the scope of equality. They actively work to mobilize political will for amendments rather than relying on judicial rulings.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, amendment_process_advocates, agenda_setter,
    institutional, civilizational, analytical, national).

% Would argue that equality is a universal, self-evident truth that should not be constrained by historical text or democratic process, but immediately applied by courts. This reading excludes their preferred mechanism for rights expansion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, expansive_universalist_advocates, excluded,
    powerful, civilizational, identity_locked, universal).

% Would argue that equality's scope is strictly limited to the original intent of the framers, and any expansion, even by amendment, risks undermining the constitutional order. This reading's acceptance of amendment-driven expansion is anathema to them.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, restrictive_originalist_advocates, excluded,
    powerful, civilizational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the evolution of equality rights with the principle of popular sovereignty, ensuring that fundamental changes reflect broad democratic consensus rather than narrow judicial interpretation, thereby maintaining constitutional legitimacy.
% TRANSFER_FUNCTION: Transfers the authority for defining the scope of equality from the judiciary to the democratic amendment process, requiring a higher bar (supermajority consent) for rights expansion, which can delay recognition for minority groups.
% ABSENT_VOICES: Advocates of both expansive universalism and restrictive originalism are structurally excluded from this reading's core mechanism for change. Universalists would argue for immediate judicial application, while originalists would resist any expansion beyond historical intent, even by amendment.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the constitutional landscape would shift dramatically. Either judicial interpretation would become the primary driver of equality's scope (as universalists desire), or the scope would revert to a more restrictive, originalist interpretation, leading to significant legal and political upheaval.
% FOUNDING_PROBLEM: The problem of how to reconcile a foundational equality principle with a constitutional text that initially applied it narrowly, while preserving democratic legitimacy in its evolution.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political theorists from across the ideological spectrum (outside the direct beneficiaries of this reading) corroborate the ongoing tension between constitutional text, democratic will, and evolving social norms regarding equality. The debate over judicial review vs. democratic process is a live one.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).
:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because while it imposes a high bar for rights expansion, it does not deny the possibility of such expansion, only its mechanism. Suppression is moderate (0.45) as it actively suppresses alternative mechanisms for rights expansion (e.g., judicial activism) in favor of the amendment process. Theater ratio is low (0.1) because the commitment to democratic amendment is genuine, not merely performative. The metrics reflect a system that is functional in its own terms, but with clear costs for those whose rights are not yet recognized by supermajority consensus.
 *
 * PERSPECTIVAL GAP:
 *   The democratic majority and amendment advocates experience this as a legitimate and stable mechanism for societal evolution, ensuring broad buy-in. Minority groups and judicial activism advocates, however, experience it as a source of delay and an impediment to justice, forcing them to navigate a difficult political process for fundamental rights. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The democratic majority and amendment process advocates are beneficiaries, as this reading empowers their preferred mechanism for change. Minority groups and judicial activism advocates are payers, as they bear the costs of this mechanism's slowness and difficulty. Expansive universalist and restrictive originalist advocates are excluded, as their core tenets are incompatible with this reading's approach to constitutional evolution.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by explicitly linking the evolution of equality to a specific, legitimate constitutional mechanism (amendment). It avoids the pitfall of a 'dead' founding problem by acknowledging the ongoing need for equality's scope to adapt, but insists on a particular, democratically robust method for that adaptation. It is a 'rope' because it genuinely coordinates the evolution of rights with democratic principles, even if the costs are borne unevenly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_process_accessibility,
    'Is the constitutional amendment process genuinely accessible and responsive enough to address evolving equality claims, or does its difficulty effectively ''trap'' minority groups?',
    'Empirical analysis of amendment success rates for civil rights issues over time, compared to judicial rulings. Examination of political barriers and resource requirements for successful amendment campaigns.',
    'If the amendment process is found to be effectively inaccessible, the ''payer'' seats'' extractiveness would be higher, pushing the constraint closer to a ''snare'' for those groups, as the promised path to equality is illusory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_accessibility, empirical, 'Assesses the practical viability of the amendment process as a mechanism for rights expansion.').

omega_variable(
    legitimacy_vs_justice_tradeoff,
    'Does prioritizing democratic legitimacy through the amendment process inherently delay or deny justice for minority groups, creating an unavoidable tradeoff?',
    'Conceptual analysis comparing theories of constitutional legitimacy with theories of distributive and corrective justice. This is a philosophical question about the inherent tension between these values.',
    'If an unavoidable tradeoff is confirmed, the ''rope'' classification would be challenged by the inherent, non-reducible cost to justice, potentially reclassifying it as a ''tangled_rope'' due to the structural extraction embedded in the coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_vs_justice_tradeoff, conceptual, 'Examines the inherent tension between democratic legitimacy and the timely realization of justice.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''progressive textualist'' reading, or does it subtly align with ''restrictive originalist'' outcomes by emphasizing process over substantive rights?',
    'Comparative analysis of judicial decisions and legislative outcomes under this reading versus a purely originalist one. Examination of the rhetoric and policy positions of its proponents.',
    'If found to align with restrictive outcomes, the classification would shift towards a ''tangled_rope'' or ''snare'' for minority groups, as the ''progressive'' label would be a form of theater masking a more extractive function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies the true ideological alignment and practical effect of the ''progressive textualist'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__progressive_textualist, theater_ratio, 0, 0.08).
narrative_ontology:measurement(equa_tr_t10, equality_clause_scope__progressive_textualist, theater_ratio, 10, 0.09).
narrative_ontology:measurement(equa_tr_t20, equality_clause_scope__progressive_textualist, theater_ratio, 20, 0.1).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__progressive_textualist, theater_ratio, 30, 0.09).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__progressive_textualist, theater_ratio, 40, 0.11).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__progressive_textualist, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__progressive_textualist, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(equa_be_t10, equality_clause_scope__progressive_textualist, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(equa_be_t20, equality_clause_scope__progressive_textualist, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__progressive_textualist, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__progressive_textualist, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__progressive_textualist, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__progressive_textualist, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(equa_su_t10, equality_clause_scope__progressive_textualist, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(equa_su_t20, equality_clause_scope__progressive_textualist, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__progressive_textualist, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__progressive_textualist, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__progressive_textualist, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'equality_clause_scope' kernel. Each reading instantiates a different constraint with its own structural properties and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
