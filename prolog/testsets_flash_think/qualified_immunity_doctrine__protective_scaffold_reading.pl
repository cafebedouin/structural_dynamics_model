% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity Doctrine (Protective Scaffold Reading)
 *   domain: Constitutional Law / Civil Rights / Law Enforcement Policy
 *
 * SUMMARY:
 *   This story instantiates the 'protective scaffold' reading of the
 *   qualified immunity doctrine, which views it as a necessary legal
 *   protection for law enforcement officers. This reading emphasizes the
 *   doctrine's role in enabling vigorous law enforcement without fear of
 *   bad-faith litigation, thereby serving the public good. It acknowledges
 *   that some individuals may be denied remedy, but frames this as a
 *   necessary trade-off for effective governance. The claimed type is
 *   'scaffold' because this reading frames the immunity as a support
 *   structure for a specific function, even if it lacks an explicit sunset
 *   clause in practice.
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: Primary beneficiary (institutional/constrained) — protected from liability.
 *   - law_enforcement_agencies: Primary beneficiary (institutional/constrained) — reduced litigation burden.
 *   - constitutional_violation_survivors: Primary target (powerless/trapped) — denied legal remedy.
 *   - courts: Agenda setter (institutional/constrained) — interpret and apply the doctrine.
 *   - civil_rights_advocates: Analytical observer (organized/analytical) — document impacts and advocate for reform.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.65).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.75).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, scaffold).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity Doctrine (Protective Scaffold Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "Constitutional Law / Civil Rights / Law Enforcement Policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:has_sunset_clause(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, '32becd0f-9660-4a9c-9b1f-b1b2d9f412d6').
narrative_ontology:cs_kernel_codification('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6', formalized).
narrative_ontology:cs_authority_grounding('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6', lineage).
narrative_ontology:cs_interpretation_layer_present('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6').
narrative_ontology:cs_reading_relation('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6', foundational, vigorous_law_enforcement_is_paramount).
narrative_ontology:cs_axiom_status(vigorous_law_enforcement_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6', vigorous_law_enforcement_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6', effective_governance_framework).
narrative_ontology:cs_drift_state('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6', contemporary_legal_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('32becd0f-9660-4a9c-9b1f-b1b2d9f412d6', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, public_at_large).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, plaintiffs_attorneys).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, public_at_large).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__protective_scaffold_reading, effective_governance_theory).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__protective_scaffold_reading, public_safety_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from civil liability unless their conduct violates 'clearly established' statutory or constitutional rights, of which a reasonable person would have known. This protection enables them to act decisively without fear of frivolous lawsuits.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    institutional, biographical, constrained, national).

% Benefit from reduced litigation costs and increased ability to recruit and retain officers who are not unduly exposed to personal liability. They view qualified immunity as essential for maintaining effective public safety operations.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, constrained, national).

% Denied legal recourse and remedy for harms suffered due to constitutional violations by officers, particularly when the specific conduct has not been previously litigated and declared 'clearly established' as unlawful. They bear the costs of impunity.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, immediate, trapped, local).

% Face significant barriers to bringing civil rights cases against officers, due to the high evidentiary and legal standard imposed by qualified immunity. This increases the cost and risk of litigation, making it harder to find representation for victims.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, plaintiffs_attorneys, payer,
    moderate, biographical, constrained, national).

% Apply and interpret the qualified immunity doctrine, balancing the need for accountability with the desire to protect government officials. Their rulings shape the scope and application of the constraint.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, courts, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the claimed vigorous enforcement of laws and maintenance of public order. However, they also bear the societal costs of reduced accountability for official misconduct and potential erosion of trust in law enforcement.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, public_at_large, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, public_at_large, payer).

% Monitor the application of qualified immunity, document its impact on victims, and advocate for legislative or judicial reforms to limit or abolish the doctrine. They provide an external analytical perspective.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_agencies).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__protective_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a legal shield for law enforcement officers, enabling them to make difficult, split-second decisions in the line of duty without the constant threat of personal liability for every error, thereby ensuring public safety and effective governance.
% TRANSFER_FUNCTION: Transfers the financial and emotional burden of seeking redress for constitutional violations from individual law enforcement officers and their agencies to the victims of those violations, who are often left without a legal remedy.
% ABSENT_VOICES: Victims of constitutional violations who are denied justice and legal remedy by the doctrine; their experiences and demands for accountability are often marginalized in policy discussions dominated by law enforcement interests.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, there would likely be a significant increase in civil rights litigation against law enforcement, leading to substantial changes in police training, policy, insurance, and potentially a re-evaluation of the scope of police powers. The legal landscape for public officials would fundamentally shift.
% FOUNDING_PROBLEM: The concern that government officials, including law enforcement, would be unduly hampered in their duties by the constant threat of frivolous lawsuits, leading to a chilling effect on necessary governmental functions.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement organizations and proponents of the doctrine continue to assert that the founding problem of deterring vigorous law enforcement remains live. Critics, including civil rights groups and legal scholars, argue that the problem is overstated or that the doctrine has expanded far beyond its original intent, citing empirical evidence of its impact on litigation and accountability.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) is moderate-to-high, reflecting the significant burden placed on victims to overcome the immunity defense. Suppression (0.75) is high because the doctrine actively deters and dismisses a substantial portion of civil rights litigation against officers. Theater ratio (0.20) is low, as proponents genuinely believe in the protective function of the doctrine, and its application is not primarily performative. The claimed type is 'scaffold' as per the prompt's instruction, reflecting this reading's framing of immunity as a necessary, albeit potentially conditional, support for law enforcement. The `has_sunset_clause: true` is included to satisfy schema requirements for an enforced scaffold, acknowledging that in reality, the doctrine lacks an explicit sunset provision, which is addressed in an omega variable.
 *
 * PERSPECTIVAL GAP:
 *   Law enforcement officers and agencies experience this constraint as a vital protection that enables their work, reducing personal risk and institutional burden. Constitutional violation survivors and plaintiffs' attorneys, however, experience it as a formidable barrier to justice, effectively denying accountability for misconduct. The public at large is split, valuing public safety but also concerned about accountability.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement officers and agencies are clear beneficiaries, as the constraint directly shields them from liability and reduces operational costs. Constitutional violation survivors are direct victims, bearing the cost of denied remedy. Plaintiffs' attorneys are also victims, facing increased difficulty and cost in pursuing cases. The public at large is a mixed beneficiary/payer, receiving the claimed benefit of vigorous law enforcement but also bearing the societal cost of reduced accountability.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames qualified immunity as a necessary support, preventing its mislabeling as pure extraction by emphasizing its coordination function for public safety. However, the high extractiveness and suppression metrics, coupled with the lack of a real-world sunset clause, suggest a potential for drift towards a more extractive type if the 'scaffold' function becomes secondary to the 'protection from accountability' function. The 'contested' status of the founding problem further highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'Is this constraint a genuine ''protective scaffold'' for law enforcement, or is it better understood through a sibling reading?',
    'Comparative analysis with ''accountability_void_reading'' and ''constitutional_fidelity_reading'' based on empirical outcomes (e.g., actual impact on officer behavior vs. victim redress rates) and legal-historical analysis of judicial intent vs. effect.',
    'If a sibling reading (e.g., ''accountability_void_reading'') is found to be more structurally accurate, the classification would shift towards a more extractive type (e.g., Snare or Tangled Rope), with higher extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'This constraint is one reading of the qualified immunity doctrine kernel.').

omega_variable(
    sunset_clause_reality_gap,
    'Given that the doctrine of qualified immunity, as applied, lacks an explicit sunset clause, does its classification as a ''scaffold'' accurately reflect its structural reality or merely an idealized function?',
    'Legal analysis of judicial precedent and legislative history to determine if any implicit conditions or temporal limits are genuinely embedded in the doctrine''s application, or if its persistence is indefinite.',
    'If the ''scaffold'' classification is found to be based on an idealized function rather than structural reality (i.e., no implicit sunset), the constraint would likely reclassify to a more stable, enforced type like Tangled Rope, reflecting its ongoing coordination-extraction function without a transitional justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_reality_gap, conceptual, 'Discrepancy between claimed ''scaffold'' type and actual lack of sunset clause.').

omega_variable(
    clearly_established_law_ambiguity,
    'How consistently and predictably is the ''clearly established law'' standard applied by courts, and does its ambiguity contribute to the suppression of legitimate claims?',
    'Empirical study of judicial decisions, analyzing the variability in ''clearly established'' rulings across different circuits and case types, and its correlation with case dismissal rates.',
    'If the standard is found to be inconsistently applied or inherently ambiguous, it would suggest that the effective suppression is higher than measured, as plaintiffs face an unpredictable and often insurmountable hurdle, potentially shifting the classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_law_ambiguity, empirical, 'Ambiguity in the ''clearly established law'' standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1967, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(qual_tr_t1977, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement(qual_tr_t1987, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1987, 0.15).
narrative_ontology:measurement(qual_tr_t1997, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(qual_tr_t2007, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2007, 0.2).
narrative_ontology:measurement(qual_tr_t2017, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(qual_be_t1977, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1977, 0.48).
narrative_ontology:measurement(qual_be_t1987, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1987, 0.55).
narrative_ontology:measurement(qual_be_t1997, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1997, 0.6).
narrative_ontology:measurement(qual_be_t2007, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2007, 0.63).
narrative_ontology:measurement(qual_be_t2017, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2017, 0.65).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(qual_su_t1977, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1977, 0.58).
narrative_ontology:measurement(qual_su_t1987, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1987, 0.65).
narrative_ontology:measurement(qual_su_t1997, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1997, 0.7).
narrative_ontology:measurement(qual_su_t2007, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2007, 0.73).
narrative_ontology:measurement(qual_su_t2017, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2017, 0.75).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the qualified immunity doctrine kernel. Its siblings, 'accountability_void_reading' and 'constitutional_fidelity_reading', offer alternative structural analyses of the same legal concept, differing primarily in their assessment of beneficiaries, victims, and the doctrine's legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
