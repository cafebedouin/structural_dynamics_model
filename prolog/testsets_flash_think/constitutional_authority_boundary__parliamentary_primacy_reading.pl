% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy Reading of Constitutional Authority
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'parliamentary primacy' reading of
 *   the 'constitutional_authority_boundary' kernel. It posits that the
 *   constitutional text, where it exists, is subordinate to parliamentary
 *   sovereignty, meaning the elected legislature retains final authority to
 *   define constitutional meaning through ordinary or entrenched legislation.
 *   This reading emphasizes democratic accountability and legislative
 *   supremacy over judicial review.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.75).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '949bdbf4-5912-43e4-956c-d8e45b45fb63').
narrative_ontology:cs_kernel_codification('949bdbf4-5912-43e4-956c-d8e45b45fb63', formalized).
narrative_ontology:cs_authority_grounding('949bdbf4-5912-43e4-956c-d8e45b45fb63', lineage).
narrative_ontology:cs_interpretation_layer_present('949bdbf4-5912-43e4-956c-d8e45b45fb63').
narrative_ontology:cs_reading_relation('949bdbf4-5912-43e4-956c-d8e45b45fb63', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('949bdbf4-5912-43e4-956c-d8e45b45fb63', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('949bdbf4-5912-43e4-956c-d8e45b45fb63', foundational, democratic_accountability_is_supreme).
narrative_ontology:cs_axiom_status(democratic_accountability_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('949bdbf4-5912-43e4-956c-d8e45b45fb63', democratic_accountability_is_supreme, deontological).
narrative_ontology:cs_axiom('949bdbf4-5912-43e4-956c-d8e45b45fb63', foundational, legislative_will_is_final).
narrative_ontology:cs_axiom_status(legislative_will_is_final, holdable).
narrative_ontology:cs_axiom_grounding('949bdbf4-5912-43e4-956c-d8e45b45fb63', legislative_will_is_final, conventional).
narrative_ontology:cs_reference_frame('949bdbf4-5912-43e4-956c-d8e45b45fb63', westminster_model_sovereignty).
narrative_ontology:cs_drift_state('949bdbf4-5912-43e4-956c-d8e45b45fb63', contemporary_constitutional_debates, gap(stable, minor, true)).
narrative_ontology:cs_created_at('949bdbf4-5912-43e4-956c-d8e45b45fb63', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, electorate).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_review_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains final authority to define constitutional meaning, ensuring its legislative acts are supreme. It benefits from unconstrained power to enact the popular will and is the primary beneficiary of interpretive authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Is constrained to an advisory role or easily-overridden review, lacking final authority to invalidate legislative acts. It bears the cost of diminished interpretive power and institutional influence.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Benefits from direct democratic accountability, as their elected representatives have the final say on constitutional matters, reflecting the popular will without judicial obstruction.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, electorate, beneficiary,
    organized, biographical, mobile, national).

% Are structurally marginalized, as their arguments for strong-form judicial review or a more distributed interpretive model are denied by the principle of parliamentary supremacy. Their influence is limited to persuasion rather than legal challenge.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_review_advocates, excluded,
    organized, generational, constrained, national).

% Operates under laws passed by a supreme parliament, potentially with less judicial interference in policy implementation. It benefits from a clearer, less contested legislative mandate.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, executive_branch, beneficiary,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the final interpretation of constitutional meaning rests with the democratically elected body, coordinating legislative action with the popular will and providing a clear locus of ultimate authority.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the constitution from the judiciary (or a distributed model) to the elected legislature, centralizing the power to define constitutional meaning.
% ABSENT_VOICES: Advocates for strong-form judicial review or a more distributed model of constitutional interpretation are structurally marginalized; they would argue for judicial checks on legislative power but are kept out of the final decision-making process.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy vanished, the balance of power would shift dramatically, likely empowering the judiciary or leading to a more fragmented interpretive landscape, fundamentally altering the constitutional order and the relationship between branches of government.
% FOUNDING_PROBLEM: To ensure that the will of the people, expressed through their elected representatives, is supreme in defining the nation's laws and constitutional framework, preventing unelected bodies from thwarting democratic decisions and ensuring accountability.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists, historical constitutional documents from parliamentary systems (e.g., UK constitutional history), and contemporary legislative debates corroborate this founding problem, emphasizing democratic accountability over judicial oversight. Independent legal scholars often analyze the trade-offs of this model.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is low (0.20) because, from this reading's perspective, the constraint primarily coordinates the democratic will through the legislature, rather than extracting from it. However, it is highly suppressive (0.75) of strong judicial review, as it actively denies the judiciary final interpretive authority. The theater ratio is low (0.20) because the legislative process is genuinely functional, and any judicial review is understood to be advisory or easily overridden, not performative. Accessibility collapse is high (0.80) as alternatives like strong judicial review are structurally foreclosed. Resistance is moderate (0.40) due to ongoing advocacy for stronger judicial checks.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the elected legislature and the electorate, this constraint is a legitimate coordination mechanism ensuring democratic will. From the judiciary and constitutional review advocates, it is an extractive mechanism that suppresses essential checks and balances, concentrating power in the legislative branch.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature is the primary beneficiary and agenda-setter, gaining ultimate interpretive authority. The electorate also benefits from direct democratic accountability. The executive branch benefits from a clearer legislative mandate. The judiciary is the primary target/payer, bearing the cost of its constrained interpretive power. Advocates for strong constitutional review are excluded, as their claims are structurally denied.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it represents a live and actively defended principle of constitutional design. The contest is over which constitutional reading is legitimate, not whether the constraint's original function has atrophied. The classification as a Tangled Rope reflects its dual nature: coordinating democratic will while extracting interpretive power from the judiciary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_primacy_vs_judicial_review_legitimacy,
    'Is parliamentary primacy truly more democratic, or does it risk majoritarian tyranny by removing effective checks on legislative power?',
    'Comparative constitutional analysis of systems with varying degrees of judicial review, assessing long-term outcomes for minority rights, rule of law, and democratic stability.',
    'If it risks majoritarian tyranny, the effective extractiveness from vulnerable groups (not explicitly modeled here) would be higher than currently assessed, potentially reclassifying the constraint as a Snare for those groups. If it consistently upholds democratic values, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_primacy_vs_judicial_review_legitimacy, conceptual, 'Debate over the democratic legitimacy and potential risks of parliamentary supremacy.').

omega_variable(
    codification_vs_practice_primacy,
    'Does the written constitutional text (if any) truly constrain parliament under this reading, or is parliamentary practice the ultimate source of constitutional meaning, making the text merely symbolic?',
    'Analysis of historical instances where parliamentary action has directly contradicted or significantly reinterpreted explicit constitutional text without formal amendment, and the subsequent legal and political consequences.',
    'If practice consistently overrides text, the ''formalized'' kernel codification might be re-evaluated towards ''implicit'', and the authority grounding might shift more towards ''practice'', potentially altering the CS pattern and its resilience to drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_vs_practice_primacy, empirical, 'The relative authority of written text versus parliamentary practice in defining constitutional meaning.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''parliamentary_primacy_reading'' of the ''constitutional_authority_boundary'' kernel?',
    'Expert consensus from constitutional scholars on the distinct structural features and normative claims of this reading compared to its siblings (''judicial_supremacy_reading'', ''coordinate_construction_reading'').',
    'If misidentified, the entire structural analysis and classification would be invalid, requiring re-authoring under the correct kernel reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint is a specific reading of a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(cons_tr_t1920, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(cons_tr_t1940, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(cons_tr_t1960, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(cons_tr_t1980, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(cons_tr_t2000, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(cons_tr_t2020, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(cons_be_t1920, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1920, 0.16).
narrative_ontology:measurement(cons_be_t1940, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1940, 0.17).
narrative_ontology:measurement(cons_be_t1960, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement(cons_be_t1980, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1980, 0.19).
narrative_ontology:measurement(cons_be_t2000, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(cons_be_t2020, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 2020, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(cons_su_t1920, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(cons_su_t1940, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(cons_su_t1960, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1960, 0.72).
narrative_ontology:measurement(cons_su_t1980, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1980, 0.74).
narrative_ontology:measurement(cons_su_t2000, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(cons_su_t2020, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, legislative_process_rules).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, executive_power_limits).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_review_scope).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_authority_boundary' kernel. The other readings are 'judicial_supremacy_reading' and 'coordinate_construction_reading', each representing a different structural claim about ultimate constitutional interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
