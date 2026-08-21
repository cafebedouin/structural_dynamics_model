% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution: Hyper-Presidential Reading
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This constraint represents the 'hyper-presidential' reading of the French
 *   Fifth Republic Constitution, where the President is seen as the direct
 *   embodiment of the national will, minimally constrained by the
 *   legislature. This reading emphasizes presidential prerogative,
 *   particularly through mechanisms like Article 49.3 (allowing bills to pass
 *   without a vote unless a censure motion is filed) and Article 16
 *   (emergency powers). It is one of several competing interpretations of the
 *   Fifth Republic's constitutional framework.
 *
 * KEY AGENTS:
 *   - incumbent_president: Primary beneficiary and agenda-setter (institutional/constrained)
 *   - presidency_as_institution: Institutional beneficiary (institutional/identity_locked)
 *   - national_assembly: Primary victim (organized/constrained)
 *   - opposition_parties: Victim (moderate/constrained)
 *   - electorate: Payer/Beneficiary (organized/constrained)
 *   - constitutional_council: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.85).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.75).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution: Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, 'b1255564-657f-40fa-9c1e-27af2cde6bbb').
narrative_ontology:cs_kernel_codification('b1255564-657f-40fa-9c1e-27af2cde6bbb', fixed_text).
narrative_ontology:cs_authority_grounding('b1255564-657f-40fa-9c1e-27af2cde6bbb', lineage).
narrative_ontology:cs_interpretation_layer_present('b1255564-657f-40fa-9c1e-27af2cde6bbb').
narrative_ontology:cs_reading_relation('b1255564-657f-40fa-9c1e-27af2cde6bbb', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1255564-657f-40fa-9c1e-27af2cde6bbb', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('b1255564-657f-40fa-9c1e-27af2cde6bbb', foundational, president_embodies_national_will).
narrative_ontology:cs_axiom_status(president_embodies_national_will, holdable).
narrative_ontology:cs_axiom_grounding('b1255564-657f-40fa-9c1e-27af2cde6bbb', president_embodies_national_will, deontological).
narrative_ontology:cs_axiom('b1255564-657f-40fa-9c1e-27af2cde6bbb', foundational, executive_efficiency_over_legislative_friction).
narrative_ontology:cs_axiom_status(executive_efficiency_over_legislative_friction, holdable).
narrative_ontology:cs_axiom_grounding('b1255564-657f-40fa-9c1e-27af2cde6bbb', executive_efficiency_over_legislative_friction, instrumental).
narrative_ontology:cs_reference_frame('b1255564-657f-40fa-9c1e-27af2cde6bbb', de_gaulle_founding_vision).
narrative_ontology:cs_drift_state('b1255564-657f-40fa-9c1e-27af2cde6bbb', contemporary_political_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b1255564-657f-40fa-9c1e-27af2cde6bbb', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_parties).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, electorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The current holder of the presidential office, who benefits directly from the expansive powers granted by this reading, particularly the ability to bypass legislative approval for policy and dissolve the National Assembly. Their political agenda is advanced with minimal legislative friction.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, constrained, national).

% The enduring office of the President, which accrues power and prestige under this interpretation, solidifying its central role in the French political system. Its institutional identity is fused with the exercise of strong executive authority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, generational, identity_locked, national).

% The legislative body whose power is significantly curtailed by the president's ability to invoke Article 49.3 (forcing a bill without a vote) or Article 16 (emergency powers). Its role is reduced to oversight and occasional challenge, rather than primary legislative initiation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, immediate, constrained, national).

% Political parties not aligned with the president, who bear the cost of being systematically bypassed in the legislative process. Their ability to influence policy through parliamentary means is severely limited, forcing them into extra-parliamentary resistance or electoral challenges.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, payer,
    moderate, biographical, constrained, national).

% The voting public, who directly elect the president and may appreciate decisive leadership, but whose legislative representatives are often sidelined. They experience a strong, centralized executive but with reduced direct democratic accountability in policy-making.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, electorate, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, electorate, beneficiary).

% The body responsible for reviewing the constitutionality of laws and presidential actions. While it can check abuses, its scope is limited, and it often defers to the executive in matters of political judgment, particularly regarding Article 16.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides strong, decisive executive leadership, particularly in times of crisis or when facing a fragmented legislature, ensuring governmental stability and the rapid implementation of policy.
% TRANSFER_FUNCTION: Transfers significant legislative and policy-making authority from the National Assembly to the President, concentrating power in the executive branch.
% ABSENT_VOICES: Stronger parliamentary factions and regional assemblies, whose input is often overridden by presidential decree or legislative bypass mechanisms. They would advocate for a more balanced distribution of power and greater legislative scrutiny.
% DISAPPEARANCE_RATIONALE: If this hyper-presidential interpretation vanished, the French political system would immediately rebalance towards a more parliamentary model. The National Assembly would reclaim legislative initiative, the president's powers would be significantly curtailed, and the dynamics of government formation and policy-making would fundamentally shift, likely leading to more coalition governments and slower policy implementation.
% FOUNDING_PROBLEM: The instability and perceived ineffectiveness of the Fourth Republic's parliamentary system, characterized by frequent changes in government and an inability to address national crises effectively.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including many within the presidency and its supporting parties, argue that the threat of governmental instability and legislative gridlock remains live, justifying strong executive powers. Critics, including opposition parties and many constitutional scholars, contend that while the original problem was real, the current interpretation overcorrects, creating an imbalance of power that undermines democratic accountability.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant transfer of legislative power to the executive, allowing the president to implement policy with minimal parliamentary consent. Suppression (0.75) is high due to the constitutional mechanisms (49.3, 16) that actively bypass or override legislative opposition, effectively suppressing alternative policy paths. Theater ratio is low (0.20) because the mechanisms are genuinely functional in achieving presidential policy goals, even if they are seen as undemocratic by critics. Resistance (0.70) is high, manifested in frequent censure motions, public protests, and legal challenges, indicating active contestation of this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent president and the institution of the presidency experience this as a legitimate and effective coordination mechanism for national governance, ensuring stability and decisive action. Conversely, the National Assembly and opposition parties experience it as a highly extractive and suppressive constraint that undermines parliamentary democracy and their representative function. The electorate's experience is mixed, balancing decisive leadership against reduced legislative accountability.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent president and the presidency as an institution are clear beneficiaries, as the constraint directly empowers them. The National Assembly and opposition parties are victims, bearing the cost of their diminished legislative role. The electorate is a mixed seat, benefiting from perceived stability but paying through reduced democratic input. The Constitutional Council acts as an analytical observer, interpreting the constraint's boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by highlighting the active, functional (though extractive) nature of the constraint. It is not a Piton, as the mechanisms are actively used and yield concentrated benefits. It is not a pure Snare, as it does provide a coordination function (governmental stability, decisive action) that is genuinely valued by some, even if the extraction is asymmetric. The 'contested' status of the founding problem indicates ongoing debate about whether the original mandate (stability) still justifies the current level of executive power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_article_49_3,
    'Is the use of Article 49.3 a legitimate tool for governmental stability or an anti-democratic bypass of legislative process?',
    'Analysis of legislative outcomes and public opinion in jurisdictions with similar mechanisms, and comparative constitutional law studies on executive-legislative balance.',
    'If deemed anti-democratic, the suppression metric would be re-evaluated upwards, and the constraint''s classification would lean more strongly towards Snare. If seen as legitimate, the coordination function would be emphasized, potentially lowering perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_article_49_3, conceptual, 'Ambiguity regarding the democratic legitimacy of legislative bypass mechanisms.').

omega_variable(
    scope_of_presidential_mandate,
    'To what extent does the direct election of the President confer a mandate to override legislative opposition, embodying the ''national will''?',
    'Empirical studies on voter behavior and public attitudes towards presidential vs. parliamentary authority, and legal scholarship on the interpretation of ''national will'' in a dual-executive system.',
    'If the mandate is broadly accepted as overriding, the extractiveness from the legislature might be seen as a legitimate cost of coordination. If not, the extraction would be viewed as illegitimate power seizure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_presidential_mandate, empirical, 'Ambiguity regarding the scope of the presidential mandate derived from direct election.').

omega_variable(
    cohabitation_as_equilibrium_or_failure,
    'Is ''cohabitation'' (president and prime minister from opposing parties) a sign of constitutional flexibility and balance, or a failure of the hyper-presidential model?',
    'Comparative analysis of governmental stability and policy effectiveness during periods of cohabitation versus unified executive control, and expert legal/political analysis.',
    'If cohabitation is seen as a necessary equilibrium, it challenges the ''hyper-presidential'' reading''s claim to sole embodiment of national will. If seen as a failure, it reinforces the need for strong presidential powers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_as_equilibrium_or_failure, conceptual, 'Whether cohabitation represents a constitutional feature or a systemic anomaly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fift_tr_t10, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fift_tr_t20, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fift_tr_t30, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(fift_tr_t40, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(fift_tr_t50, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(fift_tr_t60, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(fift_be_t50, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(fift_be_t60, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 60, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(fift_su_t50, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(fift_su_t60, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 60, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Fifth Republic Constitution kernel. It is linked to 'parliamentary_constraint_reading' and 'cohabitation_equilibrium_reading' as sibling interpretations of the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
