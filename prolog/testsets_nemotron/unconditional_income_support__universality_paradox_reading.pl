% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support — Universality Paradox Reading
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   The 'universality paradox' reading of unconditional income support
 *   identifies a constraint whose central mechanism is political ambiguity.
 *   The UBI label functions as a Trojan horse: it attracts cross-ideological
 *   support because left and right factions project incompatible meanings
 *   onto it (autonomy floor vs. welfare state elimination vs. poverty
 *   eradication), but the fiscal architecture of every serious proposal —
 *   tax-back/clawback mechanisms that phase out the benefit as income rises —
 *   converges on similar distributional outcomes. This is not a bug but the
 *   constraint's function: the ambiguity allows coalition formation without
 *   normative resolution. The constraint is a tangled rope because it
 *   genuinely coordinates a fragmented welfare reform debate (coordination
 *   function) while asymmetrically extracting clarity from discourse and
 *   targeted benefits from vulnerable recipients (extraction function), and
 *   requires active enforcement through the policy design process to maintain
 *   the universal label over a targeted fiscal reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.18).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.35).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support — Universality Paradox Reading").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '099aadfe-5772-432d-95dd-ab92a6a25424').
narrative_ontology:cs_kernel_codification('099aadfe-5772-432d-95dd-ab92a6a25424', distributed).
narrative_ontology:cs_authority_grounding('099aadfe-5772-432d-95dd-ab92a6a25424', distributed).
narrative_ontology:cs_reading_relation('099aadfe-5772-432d-95dd-ab92a6a25424', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('099aadfe-5772-432d-95dd-ab92a6a25424', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('099aadfe-5772-432d-95dd-ab92a6a25424', foundational, ambiguity_is_the_mechanism).
narrative_ontology:cs_axiom_status(ambiguity_is_the_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('099aadfe-5772-432d-95dd-ab92a6a25424', ambiguity_is_the_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('099aadfe-5772-432d-95dd-ab92a6a25424', foundational, fiscal_convergence_holds_across_designs).
narrative_ontology:cs_axiom_status(fiscal_convergence_holds_across_designs, holdable).
narrative_ontology:cs_axiom_grounding('099aadfe-5772-432d-95dd-ab92a6a25424', fiscal_convergence_holds_across_designs, empirically_contingent).
narrative_ontology:cs_reference_frame('099aadfe-5772-432d-95dd-ab92a6a25424', post_1970s_welfare_legitimacy_crisis).
narrative_ontology:cs_drift_state('099aadfe-5772-432d-95dd-ab92a6a25424', contemporary_pilot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('099aadfe-5772-432d-95dd-ab92a6a25424', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political actors across the ideological spectrum who use the ambiguity of 'universal basic income' to build cross-cutting coalitions. Left entrepreneurs frame it as anti-poverty; right entrepreneurs frame it as bureaucratic replacement. Both benefit from the label's vagueness because it allows coalition-building without resolving the implementation contradiction. They can exit by pivoting to other policy vehicles if the UBI label becomes toxic, but gain substantial political capital while the ambiguity persists.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary,
    organized, biographical, mobile, national).

% Technocrats and fiscal architects who design the actual transfer mechanisms. The 'tax-back' or 'clawback' mechanisms in most UBI proposals allow them to present a universal-looking program that functionally operates as a targeted negative income tax. This gives them rhetorical flexibility — they can claim universality while delivering targeted fiscal outcomes. They are institutionally embedded and face no meaningful exit pressure; their role is to translate political ambiguity into implementable design.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, policy_designers, agenda_setter).

% Current recipients of means-tested programs (housing assistance, food stamps, disability benefits) who would see their specialized supports folded into a universal payment. Because universality requires either higher total spending or lower per-person amounts, the fiscal convergence research suggests the latter — a universal payment large enough to replace targeted supports is fiscally implausible, so the universal amount is set lower and targeted recipients lose the supplemental wraparound services that addressed their specific needs. They have no exit: they cannot opt out of the policy change and lack political power to resist it.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, national).

% The capacity for coherent normative evaluation of social policy. The UBI label's cross-ideological appeal depends on different factions projecting incompatible meanings onto the same term — 'freedom from coercion' vs. 'elimination of the welfare state' vs. 'poverty eradication'. This semantic inflation prevents any faction from being held accountable for the actual distributional consequences of their preferred design. It is not an agent but a public good (clear policy discourse) that is degraded by the constraint's operation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Finance ministries, treasury departments, and legislative budget offices that must score and fund any UBI proposal. They are constrained by the fiscal convergence finding: regardless of rhetorical framing, the net fiscal cost converges toward a narrow band once tax-back mechanisms are accounted for. This limits their ability to expand or contract the program — they become the enforcers of the convergence, not its authors. They cannot easily exit because they are the ones who must make the numbers work.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Researchers who document the tax-back equivalence and fiscal convergence across UBI designs. Their work establishes the low-epsilon finding — that the constraint's extractiveness is low because fiscal outcomes are similar — but also reveals the political mechanism: ambiguity is the feature, not the bug. They are analytical observers with full exit; their situation is to measure the constraint, not to be governed by it.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, welfare_state_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared policy vehicle that allows ideologically opposed factions to coordinate on a single legislative proposal without resolving their normative disagreement — a 'big tent' coordination mechanism for welfare reform.
% TRANSFER_FUNCTION: Transfers political capital and coalition-building capacity to political entrepreneurs; transfers design flexibility to policy designers; transfers fiscal risk and benefit reduction to targeted program recipients; transfers discursive clarity from the public sphere into ambiguity.
% ABSENT_VOICES: Direct beneficiaries of means-tested programs (disability recipients, families with children in deep poverty, housing-insecure populations) are structurally excluded from the UBI design conversation because their specific needs contradict the universality premise. They would object to the folding of specialized supports into a flat universal payment, but the constraint's framing positions their programs as 'bureaucracy to be eliminated' rather than 'needs to be met.'
% DISAPPEARANCE_RATIONALE: If the universality-paradox framing vanished, the cross-ideological coalition for UBI would fracture — left and right versions would be recognized as distinct policies with different fiscal and distributional profiles. Targeted programs would retain their specialized logic. Policy designers would lose the 'tax-back' rhetorical cover. The political entrepreneurs would need new coalition vehicles. The welfare reform debate would restructure around specific, evaluable proposals rather than a shared ambiguous label.
% FOUNDING_PROBLEM: The post-1970s welfare state faced a legitimacy crisis: means-tested programs created poverty traps and stigma; universal programs (like family allowances) were fiscally expansive and politically vulnerable to retrenchment. UBI emerged as a synthesis concept — universal enough to avoid stigma, targeted enough (via tax-back) to be fiscally credible.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by welfare state historians (Esping-Andersen, van Parijs) from outside the political entrepreneur class. However, the status is contested: left scholars argue the poverty trap/stigma problem persists and UBI remains a live solution; right scholars argue the fiscal credibility was always illusory and the 'synthesis' was a rhetorical trap; fiscal sociologists document that the tax-back equivalence was known at the founding but suppressed to maintain the coalition.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.18) because the fiscal convergence finding means the constraint does not substantially redistribute resources beyond what a conventional negative income tax would — the 'extraction' is primarily discursive and political, not fiscal. Suppression (0.35) is moderate: the constraint suppresses alternative framings (targeted reform, job guarantee, universal services) by occupying the 'radical reform' semantic space, but does not use coercive force. Theater ratio (0.42) is significant and rising: an increasing share of UBI advocacy is performative coalition-maintenance rather than policy design, as evidenced by the proliferation of pilots that test the label's popularity rather than the fiscal mechanics. Accessibility collapse (0.30) is low because targeted programs and alternative reform pathways remain intellectually and politically available — the constraint does not foreclose them, it merely crowds them out. Resistance (0.55) is moderate: targeted recipients and their advocates resist, but the cross-ideological coalition creates a pincer movement that marginalizes resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the political entrepreneur's seat, the constraint is a rope — a genuine coordination solution that unifies a fragmented reform space. From the targeted recipient's seat, it is a snare — the universality label is cover for cutting specialized supports. From the policy designer's seat, it is a scaffold — a transitional vehicle that will resolve into a conventional negative income tax once the political moment passes. The engine computes this seat divergence from the structural data; the claimed_type (tangled_rope) reflects the constraint's aggregate structure as experienced by the system as a whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers are structural beneficiaries (d ≈ 0.15-0.25): they gain coalition capacity and design flexibility from the ambiguity. Targeted program recipients are structural victims (d ≈ 0.85-0.95): they bear the fiscal convergence risk with no exit. Ideological clarity is a non-agent victim — the constraint degrades a public good. Fiscal authorities sit near symmetric (d ≈ 0.5): they enforce the convergence but do not author it. Welfare state scholars are analytical observers (d = 0.5 by definition). The derivation chain from beneficiary/victim declarations + exit options produces these directionalities without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (welfare state legitimacy crisis) is contested — not clearly live or dead. The constraint persists not because the problem is solved, but because the ambiguity itself became a political resource. This is mandatrophy in a distinctive form: not a solved problem leaving a vestigial institution, but an unsolved problem whose ambiguity is actively maintained because it serves coalition-building. The classification as tangled_rope (not piton) captures this: the coordination function is real and active, not atrophied; the extraction is structural, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_structure,
    'How does this reading''s structural assignment of beneficiaries/victims differ from the sibling readings, and what does that reveal about the kernel''s contestation?',
    'Compare the beneficiary/victim arrays across all three readings of the unconditional_income_support kernel. The freedom_floor_reading likely names ''precarious_workers'' and ''caregivers'' as beneficiaries; the dependency_trap_reading likely names ''taxpayers'' and ''labor_market_discipline'' as victims. This reading''s assignment (political_entrepreneurs, policy_designers as beneficiaries; ideological_clarity, targeted_recipients as victims) reveals that the kernel''s contestation is not merely normative but structural: different readings instantiate different constraints with different extraction topologies.',
    'Confirms that the kernel is a genuine commitment-system kernel — a stabilized commitment that generates structurally distinct constraints depending on which reading''s axioms are adopted. Validates the committer-frame approach: one kernel, multiple ε-invariant constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_structure, conceptual, 'Committer-frame structural delta across sibling readings of the same kernel.').

omega_variable(
    fiscal_convergence_empirical_robustness,
    'How robust is the tax-back equivalence finding across different UBI designs, funding mechanisms, and behavioral response assumptions?',
    'Systematic review of microsimulation studies (OECD, IMF, national fiscal institutes) comparing net distributional outcomes of UBI vs. NIT vs. existing transfer systems under varying labor supply elasticities and take-up rates.',
    'If the convergence is robust, this reading''s low epsilon (0.18) is justified and the constraint''s extraction is primarily political/discursive. If convergence fails under plausible parameters, epsilon should be higher and the constraint may be more extractive fiscally — potentially shifting classification toward snare for targeted recipients.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_convergence_empirical_robustness, empirical, 'Empirical robustness of the tax-back equivalence / fiscal convergence thesis.').

omega_variable(
    ambiguity_as_coordination_vs_extraction,
    'Is the cross-ideological ambiguity a genuine coordination mechanism (enabling reform that would otherwise be impossible) or a pure extraction mechanism (enabling politicians to claim credit for a reform that delivers little)?',
    'Counterfactual analysis: in jurisdictions where UBI-adjacent reforms passed (Alaska Permanent Fund, Iran''s subsidy reform, Finland/Canada pilots), did the ambiguity enable passage, and did the implemented policy deliver the promised benefits to the promised beneficiaries?',
    'If ambiguity is primarily coordination, the tangled_rope classification is apt — genuine coordination + asymmetric extraction. If ambiguity is primarily extraction (coalition built on false premises, delivering nothing), the constraint may be a snare with a coordination cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguity_as_coordination_vs_extraction, conceptual, 'Whether the constraint''s coordination function is genuine or performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_tr_t1975, unconditional_income_support__universality_paradox_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_tr_t1985, unconditional_income_support__universality_paradox_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_tr_t1995, unconditional_income_support__universality_paradox_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_tr_t2005, unconditional_income_support__universality_paradox_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_tr_t2015, unconditional_income_support__universality_paradox_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_tr_t2025, unconditional_income_support__universality_paradox_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_be_t1975, unconditional_income_support__universality_paradox_reading, base_extractiveness, 1975, 0.08).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_be_t1985, unconditional_income_support__universality_paradox_reading, base_extractiveness, 1985, 0.1).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_be_t1995, unconditional_income_support__universality_paradox_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_be_t2005, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2005, 0.14).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_be_t2015, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2015, 0.16).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_be_t2025, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_su_t1975, unconditional_income_support__universality_paradox_reading, suppression_requirement, 1975, 0.15).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_su_t1985, unconditional_income_support__universality_paradox_reading, suppression_requirement, 1985, 0.2).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_su_t1995, unconditional_income_support__universality_paradox_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_su_t2005, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_su_t2015, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement(unconditional_income_support__universality_paradox_reading_su_t2025, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__universality_paradox_reading, 0.15).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, negative_income_tax).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, means_tested_welfare_programs).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, universal_basic_services).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unconditional_income_support kernel. The freedom_floor_reading and dependency_trap_reading are sibling constraints with different beneficiary/victim structures, different epsilon values, and different claimed types. All three are linked via affects_constraints. The universality_paradox_reading is the meta-reading that identifies the kernel's ambiguity as the constraint's mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
