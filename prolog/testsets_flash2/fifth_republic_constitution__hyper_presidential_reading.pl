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
 *   legislature. This interpretation emphasizes the President's ability to
 *   act decisively, often bypassing parliamentary procedures through
 *   constitutional mechanisms like Article 49.3 (passing legislation without
 *   a vote) or Article 16 (emergency powers). The constraint is claimed as a
 *   'rope' by its proponents, emphasizing its coordination function for
 *   governmental stability, but the metrics reflect a 'tangled_rope' or
 *   'snare' due to high extraction from the legislature and active
 *   suppression of parliamentary alternatives. This story is one reading of
 *   the 'fifth_republic_constitution' kernel, distinct from the
 *   'parliamentary_constraint_reading' and
 *   'cohabitation_equilibrium_reading'.
 *
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
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution: Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '08eed35a-608c-469b-807c-3b631f03cf57').
narrative_ontology:cs_kernel_codification('08eed35a-608c-469b-807c-3b631f03cf57', fixed_text).
narrative_ontology:cs_authority_grounding('08eed35a-608c-469b-807c-3b631f03cf57', lineage).
narrative_ontology:cs_interpretation_layer_present('08eed35a-608c-469b-807c-3b631f03cf57').
narrative_ontology:cs_reading_relation('08eed35a-608c-469b-807c-3b631f03cf57', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('08eed35a-608c-469b-807c-3b631f03cf57', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('08eed35a-608c-469b-807c-3b631f03cf57', foundational, president_embodies_national_will).
narrative_ontology:cs_axiom_status(president_embodies_national_will, holdable).
narrative_ontology:cs_axiom_grounding('08eed35a-608c-469b-807c-3b631f03cf57', president_embodies_national_will, deontological).
narrative_ontology:cs_axiom('08eed35a-608c-469b-807c-3b631f03cf57', foundational, executive_decisiveness_prioritizes_legislative_process).
narrative_ontology:cs_axiom_status(executive_decisiveness_prioritizes_legislative_process, holdable).
narrative_ontology:cs_axiom_grounding('08eed35a-608c-469b-807c-3b631f03cf57', executive_decisiveness_prioritizes_legislative_process, instrumental).
narrative_ontology:cs_reference_frame('08eed35a-608c-469b-807c-3b631f03cf57', de_gaulle_founding_vision).
narrative_ontology:cs_drift_state('08eed35a-608c-469b-807c-3b631f03cf57', contemporary_political_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('08eed35a-608c-469b-807c-3b631f03cf57', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, political_opposition).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, citizens_seeking_legislative_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The directly elected head of state, who, under this reading, embodies the national will and can bypass the National Assembly through constitutional mechanisms like Article 49.3 (passing legislation without a vote) or Article 16 (emergency powers). Benefits from concentrated power and reduced legislative checks.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, constrained, national).

% The institutional office of the President, which accrues power and prestige through this interpretation, becoming the dominant branch of government. Its identity is fused with the exercise of strong executive authority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, generational, identity_locked, national).

% The legislative body, whose power to debate, amend, and vote on legislation is significantly curtailed when the President invokes specific constitutional articles. Bears the cost of legislative marginalization and reduced policy influence.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, immediate, constrained, national).

% Political parties and movements not aligned with the President, who find their ability to challenge executive policy through parliamentary means severely limited. Their resistance is often channeled into street protests or electoral challenges rather than legislative action.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, political_opposition, payer,
    moderate, biographical, constrained, national).

% Citizens who expect their elected representatives to have a meaningful role in lawmaking and government oversight. They bear the cost of reduced democratic accountability and feel disempowered when executive actions bypass the legislature.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, citizens_seeking_legislative_accountability, payer,
    powerless, biographical, trapped, national).

% The body responsible for reviewing the constitutionality of laws and executive actions. While it can check abuses, its role is often limited to formal compliance rather than substantive policy review, especially regarding the President's use of special powers.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides strong, decisive executive leadership, particularly in times of crisis or when legislative consensus is difficult to achieve, ensuring governmental stability and policy implementation.
% TRANSFER_FUNCTION: Transfers legislative authority and policy initiative from the National Assembly to the President, concentrating decision-making power in the executive branch.
% ABSENT_VOICES: Stronger parliamentary factions and civil society groups advocating for a more balanced distribution of power would object to the marginalization of the legislature. Their voices are often heard in public discourse but lack direct institutional channels to alter the constraint's operation under this reading.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential interpretation vanished, the French political system would fundamentally rebalance. The National Assembly would reclaim its full legislative powers, requiring greater executive-legislative cooperation. This would likely lead to more coalition governments, slower policy implementation, and a shift towards a more parliamentary-centric system.
% FOUNDING_PROBLEM: The instability of the Fourth Republic, characterized by frequent changes in government and weak executive authority, led to a desire for a stronger, more stable executive.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including many within the presidency and its supporting parties, argue that the need for strong executive leadership and governmental stability remains paramount, especially in a complex global environment. Critics, including opposition parties and constitutional scholars, acknowledge the historical problem but argue that the current interpretation overcorrects, creating new democratic deficits.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) reflects the significant transfer of legislative power from the National Assembly to the President. Suppression (0.75) is high because the constitutional mechanisms used by the President actively curtail legislative debate and voting, effectively suppressing parliamentary resistance. The theater ratio (0.20) is relatively low, as the President's actions are genuinely functional in implementing policy, even if they are highly extractive. The increasing trend in extractiveness and suppression over the interval reflects a historical tendency for presidents to increasingly utilize these powers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the President, this reading provides necessary tools for effective governance and national leadership (a 'rope'). From the perspective of the National Assembly and opposition, it represents an overreach of executive power that undermines democratic principles (a 'snare' or 'tangled_rope'). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent president and the presidency as an institution are clear beneficiaries, gaining concentrated power and policy control. The National Assembly, political opposition, and citizens seeking legislative accountability are victims, bearing the costs of reduced influence and democratic checks. The constitutional council acts as an observer, reviewing legality but not fundamentally altering the power balance under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (governmental stability) is still live, but its implementation through hyper-presidentialism has led to significant extraction. The classification as a 'tangled_rope' prevents mislabeling it as pure coordination, acknowledging the genuine coordination function while exposing the asymmetric extraction and active enforcement required to maintain it. If the founding problem (instability) were 'dead', the constraint would lean more towards a 'snare' or 'piton'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_direct_mandate,
    'Does the President''s direct popular mandate genuinely supersede the legislative mandate of the National Assembly, or is this a conceptual framing to justify executive dominance?',
    'Comparative analysis of other semi-presidential systems and public opinion surveys on the perceived legitimacy of executive vs. legislative power in policy-making.',
    'If the direct mandate is widely accepted as superseding, the extraction from the legislature might be seen as a legitimate cost of effective governance. If it''s primarily a framing, the extraction is more clearly a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_direct_mandate, conceptual, 'Ambiguity regarding the source and hierarchy of democratic legitimacy.').

omega_variable(
    article_49_3_necessity,
    'Is the use of Article 49.3 (passing legislation without a vote) genuinely necessary for governmental stability and policy implementation, or has it become a routine tool for executive convenience?',
    'Empirical study of legislative gridlock and policy outcomes in periods with and without frequent Article 49.3 invocation, compared to other parliamentary systems.',
    'If genuinely necessary, the suppression of legislative debate is a cost of coordination. If used for convenience, it''s a clear mechanism of extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_49_3_necessity, empirical, 'Whether constitutional bypass mechanisms are used out of necessity or for executive advantage.').

omega_variable(
    kernel_reading_divergence,
    'How do the ''hyper_presidential_reading'', ''parliamentary_constraint_reading'', and ''cohabitation_equilibrium_reading'' structurally differ in their impact on executive-legislative power balance?',
    'Detailed comparative legal and political analysis of each reading''s interpretation of constitutional articles (e.g., 49.3, 16, 20) and their practical application in different political contexts.',
    'Each reading would yield a distinct constraint classification and metric profile, reflecting different levels of executive extraction and legislative suppression. This story represents only one such profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'The Fifth Republic Constitution is a kernel with multiple contested readings, each instantiating a distinct constraint.').


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
narrative_ontology:measurement(fift_tr_t50, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(fift_tr_t60, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(fift_be_t50, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 50, 0.84).
narrative_ontology:measurement(fift_be_t60, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 60, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(fift_su_t50, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 50, 0.74).
narrative_ontology:measurement(fift_su_t60, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 60, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'fifth_republic_constitution' kernel. The 'hyper_presidential_reading' emphasizes executive dominance, while the 'parliamentary_constraint_reading' and 'cohabitation_equilibrium_reading' posit different balances of power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
