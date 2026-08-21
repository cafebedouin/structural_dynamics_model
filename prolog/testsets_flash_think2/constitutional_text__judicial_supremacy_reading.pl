% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of a
 *   constitutional text, where courts hold final interpretive authority, and
 *   their invalidation of legislation is the conclusive determination of
 *   constitutional meaning. This reading positions courts as gatekeepers,
 *   making legislative override impossible and leading to high rigidity in
 *   constitutional interpretation. While it aims to protect rights-claimants
 *   against majoritarian overreach, it often comes at the cost of democratic
 *   responsiveness. The claimed type is Tangled Rope, reflecting both a
 *   genuine coordination function (rights protection, constitutional
 *   stability) and significant asymmetric extraction (from legislative will
 *   and democratic self-governance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.75).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '099ae2a3-3c58-49ee-89e3-d44b90aeca62').
narrative_ontology:cs_kernel_codification('099ae2a3-3c58-49ee-89e3-d44b90aeca62', fixed_text).
narrative_ontology:cs_authority_grounding('099ae2a3-3c58-49ee-89e3-d44b90aeca62', lineage).
narrative_ontology:cs_interpretation_layer_present('099ae2a3-3c58-49ee-89e3-d44b90aeca62').
narrative_ontology:cs_reading_relation('099ae2a3-3c58-49ee-89e3-d44b90aeca62', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('099ae2a3-3c58-49ee-89e3-d44b90aeca62', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('099ae2a3-3c58-49ee-89e3-d44b90aeca62', foundational, judicial_review_is_final).
narrative_ontology:cs_axiom_status(judicial_review_is_final, holdable).
narrative_ontology:cs_axiom_grounding('099ae2a3-3c58-49ee-89e3-d44b90aeca62', judicial_review_is_final, conventional).
narrative_ontology:cs_axiom('099ae2a3-3c58-49ee-89e3-d44b90aeca62', foundational, constitution_is_supreme_law).
narrative_ontology:cs_axiom_status(constitution_is_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('099ae2a3-3c58-49ee-89e3-d44b90aeca62', constitution_is_supreme_law, deontological).
narrative_ontology:cs_reference_frame('099ae2a3-3c58-49ee-89e3-d44b90aeca62', marbury_v_madison_doctrine).
narrative_ontology:cs_drift_state('099ae2a3-3c58-49ee-89e3-d44b90aeca62', contemporary_constitutional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('099ae2a3-3c58-49ee-89e3-d44b90aeca62', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional text, issues binding rulings, and invalidates legislation deemed unconstitutional. Benefits from enhanced institutional power and legitimacy as the final arbiter of fundamental law, shaping the constitutional order over generations.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, civilizational, analytical, universal).

% Passes laws that can be invalidated by the judiciary. Bears the cost of having its legislative will overridden and its policy choices constrained by judicial interpretation. Exit options are limited to constitutional amendment or political pressure, which are often difficult and slow.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Experiences its collective will, expressed through elected representatives, being constrained or overturned by judicial decisions. Bears the cost of reduced democratic responsiveness and self-governance. Exit options are limited to electoral change or the arduous constitutional amendment process.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_majority, payer,
    organized, biographical, constrained, national).

% Individuals or groups whose fundamental rights are protected by judicial review against majoritarian infringement. Benefit from a stable, authoritative defense of their liberties, often against popular sentiment or legislative action, providing a check on potential tyranny of the majority.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    moderate, biographical, mobile, national).

% Analyze, critique, and theorize about judicial supremacy, its historical development, and its implications for constitutional democracy. Their 'exit' is analytical distance, allowing them to evaluate the constraint's operation without direct participation.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% Must enforce judicial rulings, even those that invalidate its policy initiatives or administrative actions. Bears the cost of having its executive agenda constrained by judicial interpretation, requiring compliance with judicial mandates.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, judiciary).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative interpretation of constitutional meaning, protecting fundamental rights from majoritarian impulses and ensuring consistency in the application of supreme law across different branches and levels of government.
% TRANSFER_FUNCTION: Transfers final interpretive authority over constitutional meaning from elected legislative bodies to unelected judicial courts, and transfers the power to invalidate legislation from the people's representatives to the judiciary.
% ABSENT_VOICES: Advocates for legislative sovereignty (who would argue for parliamentary supremacy and legislative finality on constitutional meaning) and popular sovereignty (who would argue for direct popular input or ultimate popular interpretive authority) are structurally marginalized or excluded from the final interpretive process.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the constitutional order would be fundamentally altered. Legislative bodies would assert final interpretive authority, potentially leading to inconsistent constitutional applications, increased political conflict over fundamental law, and a rebalancing of power between branches, with significant implications for rights protection.
% FOUNDING_PROBLEM: The constraint was built to prevent tyranny of the majority, protect minority rights, ensure a stable and consistent application of fundamental law, and provide a check on legislative overreach, particularly in a system with a written constitution declared to be supreme law.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historical documents (e.g., Federalist Papers), and some rights advocacy groups corroborate the original intent to protect fundamental rights and constitutional stability. However, legislative bodies and popular movements often contest its current necessity or scope, arguing it stifles democratic responsiveness and entrenches judicial power.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.65) due to the significant transfer of power from elected legislative bodies to unelected courts, constraining the democratic process. Suppression is also high (0.75) because judicial rulings are binding and legislative alternatives to judicial invalidation are structurally difficult (e.g., requiring constitutional amendment). The theater ratio is low (0.15) as judicial review is a real, active, and consequential function, not merely performative. Accessibility collapse is very high (0.85) because legislative override of constitutional rulings is effectively impossible within the framework of judicial supremacy. Resistance is substantial (0.7) due to ongoing political and academic debates, and occasional legislative pushback against judicial decisions.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this arrangement is a necessary Rope, coordinating constitutional stability and rights protection. From the perspective of the legislature and democratic majority, it can feel like a Snare, extracting democratic responsiveness and legislative autonomy. The engine's computation of per-seat classifications will highlight this divergence, showing how the same structure is experienced differently based on one's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a clear beneficiary and agenda-setter, gaining institutional power and legitimacy as the final arbiter. Rights claimants also benefit from the protection against majoritarian infringement. The legislature and the democratic majority are the primary payers, bearing the cost of having their will constrained or overturned. The executive branch also acts as a payer, as it must enforce judicial rulings that may conflict with its agenda. Legal scholars act as observers, analyzing the system without direct participation in its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_monopoly_legitimacy,
    'Is the judiciary''s claim to final interpretive authority genuinely derived from the constitutional text and founding intent, or is it a self-asserted power that has become entrenched through practice and precedent?',
    'Historical-legal analysis of founding documents and debates, combined with comparative analysis of constitutional systems that do not grant courts final interpretive authority.',
    'If primarily self-asserted, the constraint''s legitimacy is weaker, and its extraction from democratic processes is less justified, pushing it closer to a Snare. If demonstrably derived from the text and intent, its coordination function for constitutional stability is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_legitimacy, conceptual, 'Ambiguity regarding the source of judicial interpretive authority.').

omega_variable(
    counter_majoritarian_dilemma_resolution,
    'Does judicial supremacy effectively protect minority rights and fundamental liberties, or does it primarily serve to entrench the policy preferences of unelected judges, creating a ''counter-majoritarian dilemma''?',
    'Empirical studies of judicial outcomes over time, analyzing whose rights are protected and whose interests are advanced, alongside analysis of judicial appointments and their ideological impact.',
    'If it primarily entrenches judicial preferences, the extraction from democratic responsiveness is less justified by a genuine coordination function, pushing the constraint closer to a Snare. If it consistently protects vulnerable minorities, its Rope-like coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_dilemma_resolution, empirical, 'Whether judicial supremacy fulfills its stated purpose of rights protection or becomes a vehicle for judicial policy-making.').

omega_variable(
    amendment_process_efficacy,
    'Is the constitutional amendment process a viable democratic check on judicial supremacy, or is it too difficult to be an effective counter-balance to judicial power?',
    'Comparative analysis of amendment rates and processes across different constitutional systems, combined with historical analysis of successful and failed amendment attempts within this specific system.',
    'If amendments are a viable and accessible check, the suppression of democratic will is less absolute, making the constraint less extractive. If amendments are effectively impossible, suppression is higher, reinforcing the constraint''s extractive nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_process_efficacy, empirical, 'The effectiveness of the amendment process as a democratic check on judicial power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t44, constitutional_text__judicial_supremacy_reading, theater_ratio, 44, 0.12).
narrative_ontology:measurement(cons_tr_t88, constitutional_text__judicial_supremacy_reading, theater_ratio, 88, 0.13).
narrative_ontology:measurement(cons_tr_t132, constitutional_text__judicial_supremacy_reading, theater_ratio, 132, 0.14).
narrative_ontology:measurement(cons_tr_t176, constitutional_text__judicial_supremacy_reading, theater_ratio, 176, 0.15).
narrative_ontology:measurement(cons_tr_t220, constitutional_text__judicial_supremacy_reading, theater_ratio, 220, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cons_be_t44, constitutional_text__judicial_supremacy_reading, base_extractiveness, 44, 0.55).
narrative_ontology:measurement(cons_be_t88, constitutional_text__judicial_supremacy_reading, base_extractiveness, 88, 0.6).
narrative_ontology:measurement(cons_be_t132, constitutional_text__judicial_supremacy_reading, base_extractiveness, 132, 0.63).
narrative_ontology:measurement(cons_be_t176, constitutional_text__judicial_supremacy_reading, base_extractiveness, 176, 0.64).
narrative_ontology:measurement(cons_be_t220, constitutional_text__judicial_supremacy_reading, base_extractiveness, 220, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cons_su_t44, constitutional_text__judicial_supremacy_reading, suppression_requirement, 44, 0.65).
narrative_ontology:measurement(cons_su_t88, constitutional_text__judicial_supremacy_reading, suppression_requirement, 88, 0.7).
narrative_ontology:measurement(cons_su_t132, constitutional_text__judicial_supremacy_reading, suppression_requirement, 132, 0.73).
narrative_ontology:measurement(cons_su_t176, constitutional_text__judicial_supremacy_reading, suppression_requirement, 176, 0.74).
narrative_ontology:measurement(cons_su_t220, constitutional_text__judicial_supremacy_reading, suppression_requirement, 220, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
