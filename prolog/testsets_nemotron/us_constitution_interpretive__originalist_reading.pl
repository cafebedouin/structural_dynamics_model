% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation
 *   domain: legal/political
 *
 * SUMMARY:
 *   This constraint story captures the originalist reading of the U.S.
 *   Constitution as a single instantiation of the contested kernel
 *   'us_constitution_interpretive'. Originalism claims constitutional meaning
 *   was fixed at ratification (1787 for the original Constitution, 1791 for
 *   the Bill of Rights, etc.) and that interpretive authority derives solely
 *   from fidelity to the framers' intent or the original public meaning. The
 *   reading functions as a tangled rope: it provides genuine coordination (a
 *   stable rule of recognition limiting judicial discretion) while
 *   simultaneously extracting by foreclosing rights claims and regulatory
 *   authority that would exist under alternative readings. Beneficiaries
 *   include originalist judges (who gain institutional legitimacy),
 *   federalism advocates, religious liberty claimants under the original
 *   understanding, and property rights defenders. Victims include
 *   unenumerated rights claimants, federal regulatory expansion advocates,
 *   and living constitutionalism proponents. The constraint requires active
 *   enforcement through judicial appointments, law school curricula, and the
 *   Federalist Society network.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.45).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.65).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Constitutional Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, 'cc5a708e-96a2-4bc3-bc96-01e94006affc').
narrative_ontology:cs_kernel_codification('cc5a708e-96a2-4bc3-bc96-01e94006affc', fixed_text).
narrative_ontology:cs_authority_grounding('cc5a708e-96a2-4bc3-bc96-01e94006affc', lineage).
narrative_ontology:cs_interpretation_layer_present('cc5a708e-96a2-4bc3-bc96-01e94006affc').
narrative_ontology:cs_reading_relation('cc5a708e-96a2-4bc3-bc96-01e94006affc', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc5a708e-96a2-4bc3-bc96-01e94006affc', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('cc5a708e-96a2-4bc3-bc96-01e94006affc', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('cc5a708e-96a2-4bc3-bc96-01e94006affc', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('cc5a708e-96a2-4bc3-bc96-01e94006affc', foundational, judicial_duty_fidelity_to_original_meaning).
narrative_ontology:cs_axiom_status(judicial_duty_fidelity_to_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('cc5a708e-96a2-4bc3-bc96-01e94006affc', judicial_duty_fidelity_to_original_meaning, deontological).
narrative_ontology:cs_reference_frame('cc5a708e-96a2-4bc3-bc96-01e94006affc', founding_era_original_meaning).
narrative_ontology:cs_drift_state('cc5a708e-96a2-4bc3-bc96-01e94006affc', contemporary_originalist_majority, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cc5a708e-96a2-4bc3-bc96-01e94006affc', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_originalist_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, originalist_judges).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, living_constitution_advocates).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, enumerated_powers_federalism).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the institutional authority to authoritatively declare constitutional meaning through judicial opinions. They benefit from the constraint by gaining a methodological anchor that limits judicial discretion and insulates decisions from political attack. Their exit from originalism would mean adopting a methodology that exposes them to charges of judicial activism, threatening their legitimacy and influence.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__originalist_reading, originalist_judges, beneficiary).

% Use originalist interpretation to resist federal regulatory expansion and protect state autonomy. They benefit because originalism's fixed meaning constrains the Commerce Clause and Necessary and Proper Clause to their 1787 understanding. Their exit would mean accepting broader federal power under living constitutionalism, which they view as the loss of the constitutional structure itself.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, biographical, constrained, national).

% Seek exemption from generally applicable laws under the Free Exercise Clause as originally understood. They benefit from originalism's narrow scope of judicial power because it limits courts to historical understandings rather than expanding rights. Their exit options are constrained because alternative frameworks (living constitutionalism, popular constitutionalism) tend to prioritize anti-discrimination norms over religious exemptions.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_originalist_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Rely on originalist readings of the Takings Clause and Contracts Clause to challenge regulatory takings and economic regulation. They benefit from the constraint's fixed meaning, which treats property rights as robust and historically grounded. Exit would mean accepting the modern regulatory state's broader latitude, which they see as the erosion of constitutional protection.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    moderate, biographical, constrained, national).

% Seek recognition of rights not explicitly enumerated (privacy, bodily autonomy, marriage equality, etc.). They pay the cost of originalism because its fixed historical scope forecloses judicial recognition of new rights. Their exit options are constrained because the constraint's institutional authority (originalist judges) controls the interpretive gateway; legislative alternatives are blocked by the same federalism structure originalism protects.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    organized, biographical, constrained, national).

% Seek to maintain or expand federal regulatory authority under the Commerce Clause and spending power. They bear the cost of originalism because its constrained reading of enumerated powers limits the regulatory state. Their exit options are constrained because the constraint operates through the Supreme Court's institutional authority; court-packing or jurisdiction stripping are structurally difficult and politically costly.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    institutional, generational, constrained, national).

% Argue that constitutional meaning must adapt to contemporary values and conditions. They pay the cost of originalism's dominance because it marginalizes their interpretive methodology in the courts. They are also partially excluded because originalism's claim to unique legitimacy (fidelity to the constitutional text) structurally positions living constitutionalism as illegitimate judicial activism. Exit would require a fundamental shift in judicial appointments or a constitutional amendment.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitution_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__originalist_reading, living_constitution_advocates, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, originalist_judges).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, determinate rule of recognition for constitutional meaning that limits judicial discretion and coordinates expectations across branches and generations by anchoring interpretation to a fixed historical reference point.
% TRANSFER_FUNCTION: Moves interpretive authority and policy-making power from unenumerated rights claimants and federal regulatory advocates to originalist judges, federalism advocates, and property/religious liberty claimants who benefit from the constraint's fixed historical scope.
% ABSENT_VOICES: Future generations whose constitutional rights cannot be anticipated by 1787 understandings; marginalized groups whose protections depend on evolving constitutional interpretation (e.g., LGBTQ+ individuals, reproductive rights claimants); state and local officials who would exercise broader police powers under a different federalism balance.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, the Supreme Court would immediately adopt a living constitutionalist or pluralist methodology, expanding judicial recognition of unenumerated rights, broadening federal regulatory authority, and narrowing state autonomy. The entire architecture of modern constitutional law — from substantive due process to Commerce Clause doctrine — would shift within a single term.
% FOUNDING_PROBLEM: The problem of judicial legitimacy in a democratic republic: how can unelected judges exercise the power of judicial review without substituting their own policy preferences for the people's constitution? Originalism was built to solve this by constraining judges to a fixed historical meaning they did not choose.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Scalia, Bork, Barnett) attest the problem remains live — judicial activism persists and originalism is the only constraint. Living constitutionalist scholars (Brennan, Dworkin, Strauss) and popular constitutionalists (Kramer, Tushnet) attest the problem is misconceived — the Constitution was designed to be adapted, and originalism itself is a policy choice masquerading as constraint. No neutral arbiter corroborates either side; the dispute is structural.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).
:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects that originalism transfers interpretive authority and policy outcomes from one coalition to another — it is not neutral coordination. Suppression (0.65) is substantial because the constraint's persistence depends on controlling judicial appointments and marginalizing alternative methodologies; the Federalist Society pipeline and originalist judicial hegemony are active enforcement mechanisms. Theater ratio (0.30) is moderate because the coordination function (stable rule of recognition) is real but increasingly performs as cover for substantive policy preferences. The measurement series shows rising extractiveness and suppression from 1787 to 2025 as originalism evolved from a marginal academic theory to the dominant methodology of a Supreme Court majority.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judge's seat, the constraint is a rope — genuine coordination solving the counter-majoritarian difficulty. From the unenumerated rights claimant's seat, it is a snare — pure extraction foreclosing their claims under cover of neutrality. From the federalism advocate's seat, it is a tangled rope — coordination (stable federalism) with extraction (foreclosing regulatory responses to modern problems). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges are the structural agenda-setters and partial beneficiaries (d ≈ 0.2) — they control the constraint and gain legitimacy from it. Federalism advocates, religious liberty claimants, and property rights defenders are beneficiaries (d ≈ 0.25-0.35) — they gain policy outcomes but do not control the constraint. Unenumerated rights claimants and federal regulatory advocates are payers (d ≈ 0.7-0.8) — they bear the cost of foreclosed claims. Living constitutionalism advocates are payers and excluded (d ≈ 0.85) — they pay the cost and are structurally marginalized by originalism's legitimacy claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial legitimacy) remains contested. Originalists argue it is live and their methodology is the only solution. Critics argue the problem was misconceived or that originalism has become the very judicial activism it claimed to prevent. The constraint persists not because the founding problem is solved, but because it has built an institutional enforcement apparatus (judicial pipeline, intellectual infrastructure) that sustains it regardless. This is mandatrophy: the original function (constraining judges) has arguably degraded into a mechanism for entrenching a particular substantive constitutional vision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel, or a standalone constraint?',
    'Structural analysis: if the constraint''s ε, beneficiaries, and victims change when evaluated from a different interpretive methodology applied to the same constitutional text, it is a kernel reading.',
    'If a kernel reading, its classification is reading-indexed — the same constitutional text instantiates different constraints under different readings. The engine must not average across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commiter-frame: this is the originalist_reading of kernel us_constitution_interpretive.').

omega_variable(
    originalism_vs_living_constitutionalism_foreclosure,
    'Does originalism logically foreclose living constitutionalism within a single judge''s framework, or do they coexist as competing methodologies?',
    'Analyze whether a judge could coherently hold both: originalism for some provisions, living constitutionalism for others, or whether the core premises (fixed meaning vs. evolving meaning) are mutually exclusive.',
    'If forecloses, the relation is ''forecloses'' in cs_structure. If coexists, the relation is ''coexists_with''. The current declaration uses ''coexists_with'' based on the observation that different justices hold different methodologies simultaneously on the same Court.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_living_constitutionalism_foreclosure, conceptual, 'Structural relation between originalist and living constitution readings.').

omega_variable(
    originalism_vs_popular_constitutionalism_influence,
    'Does originalism create structural pressure on popular constitutionalism without foreclosing it?',
    'Trace whether originalism''s judicial supremacy claim (courts have final interpretive authority) undermines popular constitutionalism''s claim that the people, not courts, are the ultimate interpreters.',
    'If yes, the relation is ''influences''. Originalism''s institutionalization in the courts creates downstream pressure on popular movements by raising the stakes of judicial appointments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_vs_popular_constitutionalism_influence, conceptual, 'Structural relation between originalist and popular constitutionalism readings.').

omega_variable(
    counter_majoritarian_difficulty_genuine,
    'Is the counter-majoritarian difficulty (unelected judges overriding democratic majorities) a genuine coordination problem that originalism solves, or a cover story for substantive policy preferences?',
    'Compare originalist outcomes to originalist methodology: if originalist judges consistently reach conservative policy results regardless of historical evidence, the coordination claim is undermined.',
    'If cover story, the constraint''s claimed_type (tangled_rope) is validated — coordination is real but extraction is the dominant function. If genuine, the coordination function is stronger than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_difficulty_genuine, empirical, 'Whether the coordination function is genuine or pretextual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1787, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_interpretive__originalist_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(us_c_tr_t1865, us_constitution_interpretive__originalist_reading, theater_ratio, 1865, 0.1).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_interpretive__originalist_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_interpretive__originalist_reading, theater_ratio, 1973, 0.25).
narrative_ontology:measurement(us_c_tr_t2008, us_constitution_interpretive__originalist_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_interpretive__originalist_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_interpretive__originalist_reading, base_extractiveness, 1787, 0.1).
narrative_ontology:measurement(us_c_be_t1865, us_constitution_interpretive__originalist_reading, base_extractiveness, 1865, 0.15).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_interpretive__originalist_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_interpretive__originalist_reading, base_extractiveness, 1973, 0.35).
narrative_ontology:measurement(us_c_be_t2008, us_constitution_interpretive__originalist_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_interpretive__originalist_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_interpretive__originalist_reading, suppression_requirement, 1787, 0.2).
narrative_ontology:measurement(us_c_su_t1865, us_constitution_interpretive__originalist_reading, suppression_requirement, 1865, 0.35).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_interpretive__originalist_reading, suppression_requirement, 1937, 0.55).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_interpretive__originalist_reading, suppression_requirement, 1973, 0.6).
narrative_ontology:measurement(us_c_su_t2008, us_constitution_interpretive__originalist_reading, suppression_requirement, 2008, 0.63).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_interpretive__originalist_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__originalist_reading, 0.1).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is the originalist_reading of kernel us_constitution_interpretive. The living_constitution_reading and popular_constitutionalism_reading are sibling constraints with different ε, beneficiaries, and victims. The ε-invariance principle requires separate stories because the 'same' constitutional text produces structurally different constraints under different interpretive commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__originalist_reading, institutional, 0.2).
constraint_indexing:directionality_override(us_constitution_interpretive__originalist_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
