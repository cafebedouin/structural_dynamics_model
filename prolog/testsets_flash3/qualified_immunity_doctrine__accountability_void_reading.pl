% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine (Accountability Void Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'accountability void' reading of
 *   the qualified immunity doctrine. In this reading, qualified immunity
 *   functions as a systematic extraction mechanism that guarantees impunity
 *   for law enforcement officers who commit constitutional violations. It
 *   effectively shields officers from liability, leaving victims without
 *   remedy and undermining civil rights. The doctrine, as interpreted by the
 *   federal judiciary, has evolved from a limited defense to a near-absolute
 *   bar to accountability, creating a structural snare for victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.92).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.88).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine (Accountability Void Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, 'ecc876b5-47e8-4b26-9163-e9cf4c90d4a3').
narrative_ontology:cs_kernel_codification('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', formalized).
narrative_ontology:cs_authority_grounding('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', lineage).
narrative_ontology:cs_interpretation_layer_present('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3').
narrative_ontology:cs_reading_relation('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', foundational, impunity_for_constitutional_violations_is_systemic).
narrative_ontology:cs_axiom_status(impunity_for_constitutional_violations_is_systemic, holdable).
narrative_ontology:cs_axiom_grounding('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', impunity_for_constitutional_violations_is_systemic, empirically_contingent).
narrative_ontology:cs_axiom('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', foundational, victims_lack_effective_remedy).
narrative_ontology:cs_axiom_status(victims_lack_effective_remedy, holdable).
narrative_ontology:cs_axiom_grounding('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', victims_lack_effective_remedy, empirically_contingent).
narrative_ontology:cs_reference_frame('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', post_harlow_v_fitzgerald_era).
narrative_ontology:cs_drift_state('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', contemporary_judicial_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ecc876b5-47e8-4b26-9163-e9cf4c90d4a3', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, municipal_governments).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, victims_of_constitutional_violations).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shielded from liability for constitutional violations unless their conduct violates 'clearly established statutory or constitutional rights of which a reasonable person would have known.' This reading sees them as direct beneficiaries of impunity, allowing them to act without fear of consequence for rights violations.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    institutional, biographical, arbitrage, national).

% Benefit from reduced litigation costs and payouts for officer misconduct, as the doctrine often shifts the burden of proof to victims, making successful lawsuits rare. This reading views them as indirect beneficiaries of the accountability void.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, municipal_governments, beneficiary,
    institutional, generational, constrained, local).

% Bear the full cost of constitutional violations without effective legal recourse. The doctrine creates a near-absolute bar to liability, leaving them with no remedy path and perpetuating harm. Their options are to endure the violation or engage in costly, often futile, litigation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, victims_of_constitutional_violations, payer,
    powerless, immediate, trapped, local).

% Expend significant resources attempting to challenge the doctrine and secure accountability for victims, often facing insurmountable legal hurdles. They bear the cost of a system that systematically denies justice.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_advocates, payer,
    organized, generational, constrained, national).

% The architects and enforcers of the doctrine, having created and expanded it through case law. They set the legal standards that determine when immunity applies, effectively controlling the flow of accountability.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Have the power to reform or abolish qualified immunity but have largely failed to do so, despite public pressure. Their inaction allows the judicial doctrine to persist, effectively excluding legislative will from the accountability process.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, legislative_bodies, excluded,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the doctrine coordinates impunity for state actors, ensuring that constitutional violations by law enforcement rarely result in personal liability, thereby protecting officers and municipalities from financial and professional consequences.
% TRANSFER_FUNCTION: Transfers the cost of constitutional violations from individual law enforcement officers and their employing municipalities to the victims of those violations, who are denied legal remedy.
% ABSENT_VOICES: Victims of constitutional violations are systematically silenced by the doctrine's high bar for liability; their experiences and demands for justice are effectively excluded from the legal framework that determines accountability.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, there would be a dramatic increase in lawsuits against law enforcement officers and municipalities, leading to significant changes in police training, accountability mechanisms, and potentially a re-evaluation of policing practices to minimize constitutional violations. The legal landscape for civil rights would be fundamentally altered.
% FOUNDING_PROBLEM: The doctrine was ostensibly created to protect government officials from frivolous lawsuits and to ensure they could perform their duties without fear of constant litigation, particularly in areas where the law is not clearly defined.
% FOUNDING_PROBLEM_CORROBORATION: While proponents (law enforcement unions, some legal scholars) argue the problem of frivolous lawsuits is still live, a broad coalition of civil rights organizations, legal academics, and victims' advocates attest that the doctrine has far exceeded its original intent, creating an accountability void that is no longer justified by the founding problem. Independent legal analysis and empirical studies on litigation outcomes corroborate that the problem of frivolous lawsuits is largely solved, while the problem of impunity for rights violations has escalated.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.92) reflects the complete denial of remedy for victims, who bear the full cost of constitutional violations. Suppression (0.88) is high because the legal hurdles (e.g., 'clearly established law' standard) are nearly insurmountable, actively suppressing litigation and accountability. The low theater ratio (0.15) indicates that the doctrine's stated purpose (protecting officers from frivolous lawsuits) is largely a cover for its actual function of shielding officers from legitimate accountability, with minimal genuine coordination benefit remaining. Resistance is high (0.75) due to ongoing efforts by civil rights groups and legal scholars to reform or abolish the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of law enforcement and municipalities, qualified immunity might be seen as a necessary protection (a 'protective scaffold' or even a 'rope' for effective governance). However, from the perspective of victims and civil rights advocates, it is a 'snare' that systematically denies justice. This story focuses on the latter, highlighting the structural asymmetry and the accountability void.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement officers and municipal governments are clear beneficiaries, as they are shielded from liability and associated costs. Victims of constitutional violations and civil rights advocates are the primary targets, bearing the costs of impunity and the burden of challenging an entrenched legal barrier. The federal judiciary acts as the agenda-setter, having shaped and expanded the doctrine through case law.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's mandate has atrophied from its original intent to protect against frivolous lawsuits to a mechanism that grants near-absolute impunity. The classification as a snare prevents mislabeling this as a legitimate coordination mechanism or a temporary support, instead highlighting its function as a pure extraction of accountability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_current_function,
    'To what extent does the current application of qualified immunity align with its original judicial intent to protect against frivolous lawsuits, versus functioning as a blanket shield against accountability?',
    'Empirical studies comparing the rate of frivolous lawsuits before and after the doctrine''s expansion, and analysis of successful vs. unsuccessful lawsuits against officers for constitutional violations.',
    'If the doctrine primarily functions as a blanket shield, it strengthens the ''snare'' classification and supports calls for abolition. If it still primarily screens frivolous lawsuits, it might suggest a ''tangled rope'' or ''scaffold'' classification, requiring targeted reform rather than abolition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_current_function, empirical, 'Assessing the functional drift of qualified immunity from its stated purpose.').

omega_variable(
    judicial_vs_legislative_authority,
    'Is the expansion of qualified immunity by the judiciary an appropriate exercise of judicial power, or does it overstep into legislative authority, effectively creating law rather than interpreting it?',
    'Legal scholarship analyzing the constitutional basis (or lack thereof) for the doctrine, and legislative action to codify or abolish immunity.',
    'If deemed an overreach, it would strengthen the ''constitutional_fidelity_reading'' and challenge the doctrine''s legitimacy regardless of its policy outcomes. If deemed appropriate, it would reinforce the judiciary''s role as the primary agenda-setter for this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_vs_legislative_authority, conceptual, 'Ambiguity regarding the source and legitimacy of the doctrine''s authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, high litigation costs) or internalized (victims'' belief that seeking justice is futile, fear of retaliation)?',
    'Post-exit suppression trajectory: if suppression persists after the legal barriers are removed (e.g., through legislative reform), reclassify as partially internalized. Surveys of victims'' perceptions and reasons for not pursuing legal action.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — victims carry the suppression with them after exit, making reform more complex. If purely structural, legal reforms would be more immediately effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for victims of constitutional violations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1967, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(qual_tr_t2000, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(qual_tr_t2010, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(qual_be_t2000, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(qual_be_t2010, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2010, 0.88).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(qual_su_t2000, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(qual_su_t2010, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
