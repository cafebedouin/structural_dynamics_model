% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Conventions Protective Scope: Hybrid Proportionality Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid proportionality reading' of the
 *   Geneva Conventions' protective scope. It acknowledges the distinction
 *   between International Armed Conflicts (IAC) and Non-International Armed
 *   Conflicts (NIAC), applying AP I standards to the former and AP II/Common
 *   Article 3 to the latter. The application of these standards is further
 *   mediated by proportionality analysis, which often leads to a variable and
 *   context-dependent interpretation of protections. This reading allows for
 *   significant flexibility for powerful state actors, often at the expense
 *   of non-state armed groups and civilians in NIACs. The constraint is
 *   claimed as a 'rope' by its proponents, emphasizing its coordination
 *   function, but its operational metrics reveal it as a 'tangled_rope' due
 *   to asymmetric extraction and active enforcement.
 *
 * KEY AGENTS:
 *   - powerful_state_actors: Agenda setter (institutional/arbitrage)
 *   - military_commanders: Beneficiary (powerful/constrained)
 *   - non_state_armed_groups: Payer (powerless/trapped)
 *   - civilians_in_non_international_conflict: Payer (powerless/trapped)
 *   - international_humanitarian_law_scholars: Observer (analytical/analytical)
 *   - weaker_parties_in_conflict: Payer (powerless/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Conventions Protective Scope: Hybrid Proportionality Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'a1b86375-f175-4345-ac26-dafa9d3f348c').
narrative_ontology:cs_kernel_codification('a1b86375-f175-4345-ac26-dafa9d3f348c', fixed_text).
narrative_ontology:cs_authority_grounding('a1b86375-f175-4345-ac26-dafa9d3f348c', lineage).
narrative_ontology:cs_interpretation_layer_present('a1b86375-f175-4345-ac26-dafa9d3f348c').
narrative_ontology:cs_reading_relation('a1b86375-f175-4345-ac26-dafa9d3f348c', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1b86375-f175-4345-ac26-dafa9d3f348c', geneva_conventions_protective_scope__universal_rights_reading, influences).
narrative_ontology:cs_axiom('a1b86375-f175-4345-ac26-dafa9d3f348c', foundational, differential_protection_by_conflict_type).
narrative_ontology:cs_axiom_status(differential_protection_by_conflict_type, holdable).
narrative_ontology:cs_axiom_grounding('a1b86375-f175-4345-ac26-dafa9d3f348c', differential_protection_by_conflict_type, conventional).
narrative_ontology:cs_axiom('a1b86375-f175-4345-ac26-dafa9d3f348c', foundational, proportionality_as_interpretive_tool).
narrative_ontology:cs_axiom_status(proportionality_as_interpretive_tool, holdable).
narrative_ontology:cs_axiom_grounding('a1b86375-f175-4345-ac26-dafa9d3f348c', proportionality_as_interpretive_tool, conventional).
narrative_ontology:cs_reference_frame('a1b86375-f175-4345-ac26-dafa9d3f348c', post_geneva_protocols_era).
narrative_ontology:cs_drift_state('a1b86375-f175-4345-ac26-dafa9d3f348c', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a1b86375-f175-4345-ac26-dafa9d3f348c', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_state_actors).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflict).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_parties_in_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply the Geneva Conventions, often leveraging the distinction between IAC and NIAC to their strategic advantage. They benefit from the flexibility in applying different standards, particularly in non-international armed conflicts where their actions face less scrutiny.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_state_actors, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate under the legal framework, finding the proportionality calculus and conflict classification useful for justifying operations and minimizing legal liability, especially when targeting non-state actors. The ambiguity allows for broader interpretations of 'military necessity'.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders, beneficiary,
    powerful, biographical, constrained, national).

% Are often the primary targets in non-international armed conflicts, where AP II and Common Article 3 offer fewer protections than AP I. Their members are frequently denied combatant status, leading to higher vulnerability and less legal recourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups, payer,
    powerless, immediate, trapped, local).

% Experience varying levels of protection depending on the classification of the conflict and the proportionality assessments made by state actors. They bear the costs of reduced protections and the subjective application of legal standards.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflict, payer,
    powerless, immediate, trapped, local).

% Analyze the application of the Geneva Conventions, often highlighting the gaps and inconsistencies arising from the hybrid proportionality reading. They advocate for clearer, more consistent protections but have no direct enforcement power.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% Face significant disadvantages due to the differential application of protections. They lack the legal and military resources to challenge interpretations or ensure full compliance, making them vulnerable to the ambiguities of the hybrid reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_parties_in_conflict, payer,
    powerless, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for regulating armed conflict, aiming to limit suffering and protect non-combatants by coordinating the actions of belligerents according to established legal principles.
% TRANSFER_FUNCTION: Transfers legal flexibility and reduced accountability to powerful state actors and military commanders, while transferring increased vulnerability and reduced protections to non-state armed groups and civilians in non-international conflicts.
% ABSENT_VOICES: Victims of conflict, particularly those in non-international armed conflicts, and advocates for universal human rights would argue for a more consistent and expansive application of protections, minimizing the distinctions based on conflict classification. Their voices are often marginalized in state-centric legal interpretations.
% DISAPPEARANCE_RATIONALE: If the Geneva Conventions and their interpretive framework vanished, the conduct of armed conflict would likely become even more brutal and unregulated, leading to a significant increase in civilian casualties and violations of basic human dignity. States would lose a common (if imperfect) legal language for engagement, and the international legal order would be severely destabilized.
% FOUNDING_PROBLEM: The need to mitigate the brutality of warfare and establish minimum standards of humanity and protection for those not participating in hostilities, particularly in the aftermath of devastating international conflicts.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, humanitarian organizations (e.g., ICRC), and human rights advocates consistently attest to the ongoing relevance of the founding problem, citing contemporary conflicts and the persistent need for humanitarian protection. While interpretations vary, the core problem remains universally acknowledged.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is driven by the differential application of protections, which allows stronger parties to operate with fewer constraints in NIACs. Suppression (0.70) is high because the legal framework is actively enforced by powerful states, and alternative interpretations (e.g., universal rights) are suppressed through legal and political means. The theater ratio (0.20) indicates that while genuine humanitarian concerns are present, a portion of the legal discourse and enforcement serves to legitimize selective application rather than universal protection. Accessibility collapse (0.40) is moderate, as some alternatives (e.g., human rights law) exist but are often difficult to invoke effectively. Resistance (0.55) is significant, primarily from humanitarian organizations and weaker parties who challenge the restrictive interpretations.
 *
 * PERSPECTIVAL GAP:
 *   Powerful state actors and military commanders perceive this reading as a necessary and balanced framework for modern conflict, providing essential coordination while allowing for effective military operations. Conversely, non-state armed groups and civilians in NIACs experience it as a highly extractive and suppressive constraint, where their protections are diminished and subject to the discretion of stronger parties. The engine's classification as 'tangled_rope' reflects this fundamental divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful state actors and military commanders are beneficiaries, as the hybrid reading grants them interpretive flexibility and reduces their accountability in certain contexts. Non-state armed groups, civilians in NIACs, and weaker parties are victims, as they face reduced protections and increased vulnerability due to the scaling of standards and the subjective nature of proportionality assessments. International humanitarian law scholars act as observers, analyzing the effects without direct participation in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling this constraint as a 'rope' (pure coordination) by highlighting the significant and asymmetric extraction embedded within its coordination function. While it genuinely coordinates aspects of conflict, it simultaneously extracts protections from vulnerable groups, a function that has intensified over time as NIACs have become more prevalent. The 'contested' status of the founding problem further underscores the ongoing debate about whether the constraint's original mandate is being fulfilled or subverted by its current operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conflict_classification_ambiguity,
    'Is the distinction between IAC and NIAC sufficiently clear and consistently applied, or does it serve as a tool for powerful actors to selectively apply protections?',
    'Empirical analysis of conflict classifications by independent bodies (e.g., UN, ICRC) across multiple conflicts, comparing classifications with the actual conduct of hostilities and the protections afforded.',
    'If classification is consistently ambiguous or selectively applied, the effective extractiveness of the constraint is higher, as it allows powerful actors to downgrade protections. This would strengthen the ''snare'' elements of the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_ambiguity, empirical, 'Ambiguity in conflict classification as a source of differential protection.').

omega_variable(
    proportionality_calculus_objectivity,
    'To what extent is the proportionality calculus an objective legal standard, versus a subjective justification for military actions that cause civilian harm?',
    'Detailed case studies of proportionality assessments in specific incidents, comparing military justifications with independent assessments of civilian harm and military advantage. Legal scholarship on the ''reasonable commander'' standard.',
    'If proportionality is largely subjective, the constraint''s suppression of protections is higher, as it provides a legal ''cover'' for actions that might otherwise be deemed unlawful. This would push the classification closer to a ''snare'' for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calculus_objectivity, conceptual, 'Objectivity of proportionality assessments in IHL.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''hybrid proportionality reading'' of the Geneva Conventions, or is it a ''state-centric reading'' that merely incorporates proportionality as a rhetorical device?',
    'Analysis of state practice and legal arguments in international forums. If states consistently prioritize their own security interests over humanitarian protections, even when proportionality would suggest otherwise, it indicates a stronger ''state-centric'' influence.',
    'If it is primarily a ''state-centric reading'', the extractiveness and suppression would be higher, and the coordination function would be more theatrical, potentially shifting the classification towards a ''snare'' or a more extractive ''tangled_rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing the hybrid proportionality reading from a more purely state-centric interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1949, 0.4).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1977, 0.5).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1977, 0.6).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'geneva_conventions_protective_scope' kernel. Its ε value differs from sibling readings due to its specific interpretation of conflict classification and proportionality, which allows for greater flexibility for powerful actors and thus higher extraction from vulnerable groups compared to a universal rights reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
