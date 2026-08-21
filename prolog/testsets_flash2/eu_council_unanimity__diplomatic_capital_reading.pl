% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity (Diplomatic Capital Reading)
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story analyzes the EU Council's unanimity requirement
 *   from the perspective of 'diplomatic capital,' where unanimity is seen as
 *   a necessary coordination cost that forces iterative negotiation, builds
 *   consensus, and ultimately strengthens the legitimacy and durability of EU
 *   policy. It is one reading of the broader 'eu_council_unanimity' kernel,
 *   which also includes 'sovereignty_guarantor_reading' and
 *   'veto_trap_reading'. This reading emphasizes the positive function of the
 *   veto in fostering deeper integration through consensus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.25).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.1).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity (Diplomatic Capital Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__diplomatic_capital_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '8c9aad63-b5ea-4d4d-a5ff-48fbdb134773').
narrative_ontology:cs_kernel_codification('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', formalized).
narrative_ontology:cs_authority_grounding('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', lineage).
narrative_ontology:cs_interpretation_layer_present('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773').
narrative_ontology:cs_reading_relation('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', foundational, consensus_enhances_legitimacy).
narrative_ontology:cs_axiom_status(consensus_enhances_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', consensus_enhances_legitimacy, instrumental).
narrative_ontology:cs_axiom('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', foundational, diplomatic_investment_yields_stability).
narrative_ontology:cs_axiom_status(diplomatic_investment_yields_stability, holdable).
narrative_ontology:cs_axiom_grounding('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', diplomatic_investment_yields_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', post_maastricht_integration_framework).
narrative_ontology:cs_drift_state('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', contemporary_multi_crisis_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8c9aad63-b5ea-4d4d-a5ff-48fbdb134773', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each member state holds a veto, forcing extensive negotiation to achieve consensus. This process requires significant diplomatic effort but ensures that decisions have broad buy-in, making them more legitimate and durable. States invest diplomatic capital to build coalitions and find common ground.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_member_states, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from the enhanced legitimacy and stability of unanimously adopted policies. While the negotiation process can be slow, the resulting policies are less likely to be challenged or undermined by member states, strengthening the overall coherence and effectiveness of the Union.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_institutions, beneficiary,
    institutional, generational, constrained, continental).

% Benefit from policies that reflect a broad consensus among member states, leading to greater stability and predictability in European governance. The perceived legitimacy of decisions, even if slow to arrive, can foster greater public trust in the EU project.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_citizens, beneficiary,
    moderate, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that all member states, despite their diverse interests, coordinate their policy preferences to reach a common, mutually acceptable position, thereby preventing unilateral action or deep divisions.
% TRANSFER_FUNCTION: Transfers diplomatic capital and negotiation effort from individual member states into collective policy legitimacy and durability, reducing the likelihood of future policy reversals or non-compliance.
% ABSENT_VOICES: While all member states are present, the voices of smaller states might be less influential in shaping the final consensus compared to larger states, despite their equal veto power. Citizens who desire faster decision-making might feel their preferences are sidelined by the lengthy negotiation process.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement vanished, the EU's decision-making process would fundamentally change, likely shifting to qualified majority voting. This would accelerate decisions but potentially reduce buy-in from dissenting states, leading to more frequent challenges, opt-outs, or even exits, fundamentally altering the nature of European integration.
% FOUNDING_PROBLEM: The need to ensure that collective decisions in a union of sovereign states are perceived as legitimate and are genuinely supported by all members, preventing the imposition of policies that could undermine national interests or sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Many political scientists and international relations scholars, as well as statements from various EU member state governments (especially smaller ones), corroborate that ensuring broad legitimacy and preventing majoritarian overreach remains a live and critical problem for the EU. This perspective is often articulated in academic analyses of EU governance and in diplomatic discourse.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because the 'cost' of unanimity (lengthy negotiation, diplomatic effort) is viewed as an investment that yields a return in policy legitimacy and reduced defection, rather than pure extraction. Suppression is low (0.1) as the mechanism is about empowering all members, not coercing them. Theater ratio is minimal (0.05) because the negotiation process is genuinely functional in building consensus. The claimed type is 'rope' because it's a coordination mechanism where all participants are net beneficiaries of the outcome, despite the high transaction costs.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of EU unanimity, such as the 'veto_trap_reading,' would experience this constraint as highly extractive, with significant suppression, as it enables minoritarian blocking and rent-seeking. This 'diplomatic_capital_reading' focuses on the collective benefit of consensus-building, leading to a 'rope' classification, while the 'veto_trap_reading' would likely classify it as a 'snare' or 'tangled_rope' due to the asymmetric power dynamics it can create.
 *
 * DIRECTIONALITY LOGIC:
 *   All EU member states are considered agenda-setters and beneficiaries in this reading, as they all possess the veto power and benefit from the enhanced legitimacy of unanimous decisions. There are no direct 'victims' in this framing, as the costs are seen as shared investments in collective governance. EU institutions and citizens are also beneficiaries of the resulting stable and legitimate policies.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the coordination function as pure extraction by highlighting the long-term benefits of consensus-building. While the process can be slow and costly, the 'diplomatic capital' reading argues that these are necessary investments for the EU's unique form of integration. The low extractiveness and high perceived legitimacy distinguish it from a 'snare' or 'piton' where the mandate has atrophied or is merely cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_efficiency_tradeoff,
    'At what point do the transaction costs of unanimity (slow decision-making, diplomatic effort) outweigh the benefits of enhanced legitimacy and policy durability?',
    'Empirical studies comparing policy implementation rates, compliance levels, and public approval for unanimous vs. QMV decisions, alongside economic analysis of negotiation costs.',
    'If costs consistently outweigh benefits, the ''rope'' classification would shift towards ''tangled_rope'' or ''snare'', indicating that the coordination function is being undermined by excessive extraction of time and resources without commensurate legitimacy gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_efficiency_tradeoff, empirical, 'The optimal balance between decision-making speed and policy legitimacy in the EU context.').

omega_variable(
    minority_veto_abuse_potential,
    'Does the unanimity requirement, even when intended for consensus-building, create structural opportunities for individual member states to extract disproportionate concessions or block essential policies for narrow self-interest?',
    'Case studies of specific policy deadlocks and their resolution, analyzing whether the outcomes primarily served collective EU interests or disproportionately benefited a single veto-wielding state.',
    'If such ''veto traps'' are frequent and demonstrably extractive, this reading''s ''rope'' classification would be challenged, potentially shifting towards ''tangled_rope'' or ''snare'', aligning more with the ''veto_trap_reading'' of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_veto_abuse_potential, empirical, 'The extent to which unanimity enables minoritarian extraction rather than genuine consensus.').

omega_variable(
    reading_framing_impact,
    'Is the ''diplomatic capital'' framing a genuine reflection of the constraint''s function, or a legitimizing narrative that obscures underlying power dynamics and potential for extraction?',
    'Comparative analysis of this reading''s claims against the ''sovereignty_guarantor_reading'' and ''veto_trap_reading'', examining which framing best explains observed policy outcomes and member state behavior over time, particularly during crises.',
    'If the ''diplomatic capital'' framing is found to systematically understate extraction or power asymmetries, the classification would shift to reflect a more extractive reality, potentially aligning with a ''tangled_rope'' or ''snare'' type, even if the formal structure remains unchanged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'The influence of the chosen analytical frame on the perceived nature of the unanimity constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 1993, 0.03).
narrative_ontology:measurement(eu_c_tr_t2003, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2003, 0.04).
narrative_ontology:measurement(eu_c_tr_t2013, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2013, 0.05).
narrative_ontology:measurement(eu_c_tr_t2023, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2023, 0.05).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 1993, 0.2).
narrative_ontology:measurement(eu_c_be_t2003, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2003, 0.22).
narrative_ontology:measurement(eu_c_be_t2013, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2013, 0.24).
narrative_ontology:measurement(eu_c_be_t2023, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2023, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1993, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 1993, 0.08).
narrative_ontology:measurement(eu_c_su_t2003, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2003, 0.09).
narrative_ontology:measurement(eu_c_su_t2013, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2013, 0.1).
narrative_ontology:measurement(eu_c_su_t2023, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2023, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eu_council_unanimity' kernel. This 'diplomatic_capital_reading' focuses on the consensus-building and legitimacy-enhancing aspects, leading to a 'rope' classification, distinct from the 'sovereignty_guarantor_reading' (also a rope, but focused on protection) and the 'veto_trap_reading' (likely a snare, focused on extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
