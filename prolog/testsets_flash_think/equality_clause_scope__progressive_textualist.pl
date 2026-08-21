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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   acknowledged in the text, but its application scope is understood to
 *   expand primarily through the democratic amendment process, rather than
 *   through judicial reinterpretation. This reading emphasizes popular
 *   sovereignty and the supermajority consent required for fundamental
 *   constitutional change. It stands in contrast to both rigidly originalist
 *   and broadly universalist interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.45).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.6).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope (Progressive Textualist Reading)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '86a6f30a-4738-4fd8-b951-887b9cef200a').
narrative_ontology:cs_kernel_codification('86a6f30a-4738-4fd8-b951-887b9cef200a', fixed_text).
narrative_ontology:cs_authority_grounding('86a6f30a-4738-4fd8-b951-887b9cef200a', lineage).
narrative_ontology:cs_interpretation_layer_present('86a6f30a-4738-4fd8-b951-887b9cef200a').
narrative_ontology:cs_reading_relation('86a6f30a-4738-4fd8-b951-887b9cef200a', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('86a6f30a-4738-4fd8-b951-887b9cef200a', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_axiom('86a6f30a-4738-4fd8-b951-887b9cef200a', foundational, constitutional_text_as_living_document_through_amendment).
narrative_ontology:cs_axiom_status(constitutional_text_as_living_document_through_amendment, holdable).
narrative_ontology:cs_axiom_grounding('86a6f30a-4738-4fd8-b951-887b9cef200a', constitutional_text_as_living_document_through_amendment, conventional).
narrative_ontology:cs_axiom('86a6f30a-4738-4fd8-b951-887b9cef200a', foundational, democratic_supermajority_for_rights_expansion).
narrative_ontology:cs_axiom_status(democratic_supermajority_for_rights_expansion, holdable).
narrative_ontology:cs_axiom_grounding('86a6f30a-4738-4fd8-b951-887b9cef200a', democratic_supermajority_for_rights_expansion, conventional).
narrative_ontology:cs_reference_frame('86a6f30a-4738-4fd8-b951-887b9cef200a', amendment_driven_constitutional_evolution).
narrative_ontology:cs_drift_state('86a6f30a-4738-4fd8-b951-887b9cef200a', contemporary_judicial_activism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('86a6f30a-4738-4fd8-b951-887b9cef200a', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, citizens_within_expanded_scope).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, democratic_process_legitimacy).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, excluded_groups_seeking_inclusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the expanded protections and recognition of the equality principle, secured through legitimate democratic processes. Their rights are affirmed and defended by the constitutional framework.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, citizens_within_expanded_scope, beneficiary,
    organized, generational, constrained, national).

% Bear the significant costs and delays associated with mobilizing for constitutional amendments, including political organizing, public education, and legislative lobbying. They are structurally constrained from achieving equality through judicial fiat under this reading.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, excluded_groups_seeking_inclusion, payer,
    powerless, generational, trapped, national).

% Holds the primary power to propose constitutional amendments, acting as the gatekeeper for expanding the scope of equality. Their actions are subject to political will and supermajority requirements.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, legislature, agenda_setter,
    institutional, biographical, constrained, national).

% Interprets the existing constitutional text but is constrained by this reading from unilaterally expanding the scope of equality beyond what the text or amendments explicitly allow. Their role is to apply, not to create, new equality rights.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Ultimately ratifies constitutional amendments through state-level processes, providing the democratic consent necessary for expanding equality. Their collective will is the final arbiter of constitutional change.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, electorate, agenda_setter,
    organized, immediate, mobile, national).

% Analyze and debate the historical development and contemporary application of the equality clause, including the mechanisms for its expansion. They provide critical commentary on the fidelity of practice to this reading.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% The constitutional system's legitimacy is enhanced by ensuring that fundamental changes to rights, especially the expansion of equality, arise from broad democratic consensus rather than judicial decree, reinforcing popular sovereignty.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, democratic_process_legitimacy, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(equality_clause_scope__progressive_textualist, democratic_process_legitimacy).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__progressive_textualist, diffuse).
narrative_ontology:fixing_cost_class(equality_clause_scope__progressive_textualist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the process by which the scope of constitutional equality can legitimately expand, ensuring that such expansions are rooted in broad democratic consensus via the amendment process, rather than unilateral judicial action.
% TRANSFER_FUNCTION: Transfers the authority for significant expansions of equality from potentially activist judicial bodies to the supermajority democratic will, while transferring the burden of achieving such expansion to groups seeking inclusion.
% ABSENT_VOICES: Advocates for immediate, judicially-mandated universal equality are structurally marginalized by this reading, as their preferred mechanism for change is deemed illegitimate. They would argue for a more expansive, rights-based interpretation that does not require the slow, costly amendment process.
% DISAPPEARANCE_RATIONALE: If this constraint (the progressive textualist reading of amendment-driven expansion) vanished, the constitutional landscape would fundamentally shift. Either judicial interpretation would become the primary, unchecked mechanism for rights expansion (as in the expansive universalist reading), or the equality clause would remain rigidly fixed to its original meaning (as in the restrictive originalist reading), leading to different political and social outcomes.
% FOUNDING_PROBLEM: The founding problem was how to reconcile a foundational equality principle with a text that initially applied it narrowly, and how to allow for future expansion without undermining the democratic basis of constitutional authority.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political scientists widely attest that the tension between constitutional stability, democratic change, and evolving social norms regarding equality remains a live and central problem in constitutional theory and practice. The judiciary and legislature also acknowledge this ongoing tension in their respective roles.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.45) is moderate, reflecting the significant political and social costs borne by groups seeking to expand equality through the arduous amendment process. Suppression (0.60) is moderate because it actively suppresses alternative, faster routes to expansion (e.g., judicial activism) in favor of democratic mechanisms. Theater ratio is low (0.10) as the amendment process, while slow, is generally functional and not performative. Accessibility collapse (0.70) is high for non-amendment paths, but democratic avenues remain open. Resistance (0.35) comes from those who find the amendment process too slow or who advocate for judicial remedies.
 *
 * PERSPECTIVAL GAP:
 *   For 'excluded_groups_seeking_inclusion', the constraint can feel highly extractive due to the immense effort and time required for amendments. For 'legislature' and 'electorate', it represents a legitimate and proper exercise of democratic power. The 'judiciary' experiences it as a boundary on its interpretive authority.
 *
 * DIRECTIONALITY LOGIC:
 *   'Citizens_within_expanded_scope' and 'democratic_process_legitimacy' are beneficiaries, as the process ensures the stability and popular acceptance of their rights. 'Excluded_groups_seeking_inclusion' are payers, bearing the direct costs of advocating for change. The 'legislature' and 'electorate' are agenda-setters, controlling the mechanism of change. The 'judiciary' is also an agenda-setter, but with a constrained role under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the democratic amendment process as pure extraction. While the process is costly for those seeking inclusion, it serves a genuine coordination function by legitimizing constitutional change through broad consensus. It is not a Piton, as the function of democratic legitimation is live and actively defended. It is not a Snare, as the coordination function is genuine, even if the costs are high for some parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_vs_democratic_expansion,
    'Is the democratic amendment process the only legitimate mechanism for expanding constitutional equality, or does the judiciary have an inherent role in interpreting and expanding such rights without explicit amendment?',
    'Analysis of historical constitutional practice, comparative constitutional law, and evolving theories of judicial review and popular sovereignty. A shift in prevailing legal theory or a landmark judicial decision could alter the perceived legitimacy of each path.',
    'If judicial expansion is deemed legitimate, the ''suppression'' metric for this reading would decrease, and the ''accessibility_collapse'' for alternative paths would lessen, potentially shifting the classification towards a more flexible ''rope'' or even ''scaffold'' for judicial action. If democratic amendment is reaffirmed as the sole legitimate path, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_vs_democratic_expansion, conceptual, 'Ambiguity regarding the legitimate mechanism for constitutional equality expansion.').

omega_variable(
    cost_of_amendment_as_extraction,
    'Are the high costs and delays associated with the amendment process an inherent, legitimate feature of democratic coordination, or do they constitute an extractive burden on marginalized groups seeking inclusion?',
    'Empirical study of the resources required for successful amendment campaigns versus the benefits gained, and normative analysis of whether such burdens are proportional to the goal of broad consensus. Comparative analysis with other constitutional systems'' amendment processes.',
    'If the costs are deemed disproportionately high and primarily extractive, the ''extractiveness'' metric for this reading would increase, potentially pushing the classification towards a ''tangled_rope'' or even ''snare'' from the perspective of ''excluded_groups_seeking_inclusion''. If deemed a necessary cost of robust democratic coordination, the current ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_amendment_as_extraction, preference, 'Whether the costs of democratic amendment are legitimate coordination costs or extractive burdens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1950, equality_clause_scope__progressive_textualist, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(equa_tr_t1975, equality_clause_scope__progressive_textualist, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(equa_tr_t2000, equality_clause_scope__progressive_textualist, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(equa_tr_t2025, equality_clause_scope__progressive_textualist, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1950, equality_clause_scope__progressive_textualist, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(equa_be_t1975, equality_clause_scope__progressive_textualist, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement(equa_be_t2000, equality_clause_scope__progressive_textualist, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(equa_be_t2025, equality_clause_scope__progressive_textualist, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1950, equality_clause_scope__progressive_textualist, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(equa_su_t1975, equality_clause_scope__progressive_textualist, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(equa_su_t2000, equality_clause_scope__progressive_textualist, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(equa_su_t2025, equality_clause_scope__progressive_textualist, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
