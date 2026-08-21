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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine (Accountability Void Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story analyzes the qualified immunity doctrine as a
 *   systematic extraction mechanism that guarantees impunity for
 *   constitutional violations, focusing on the 'accountability void' reading.
 *   The doctrine, developed through judicial precedent, shields government
 *   officials from liability in civil lawsuits unless their conduct violates
 *   'clearly established' statutory or constitutional rights. While
 *   ostensibly designed to protect officials from frivolous litigation, this
 *   reading argues its primary effect is to deny victims of constitutional
 *   violations any effective remedy, thereby extracting accountability and
 *   transferring the costs of misconduct to the public. The claimed type is
 *   'snare' because the coordination story (protecting public servants)
 *   serves as cover for a structure that primarily extracts from victims
 *   through impunity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.85).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.9).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine (Accountability Void Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '2ddb4bf2-816c-40e0-92cc-5e01c542ec6f').
narrative_ontology:cs_kernel_codification('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', formalized).
narrative_ontology:cs_authority_grounding('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', lineage).
narrative_ontology:cs_interpretation_layer_present('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f').
narrative_ontology:cs_reading_relation('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', foundational, impunity_enables_misconduct).
narrative_ontology:cs_axiom_status(impunity_enables_misconduct, holdable).
narrative_ontology:cs_axiom_grounding('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', impunity_enables_misconduct, empirically_contingent).
narrative_ontology:cs_axiom('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', foundational, constitutional_rights_require_remedy).
narrative_ontology:cs_axiom_status(constitutional_rights_require_remedy, holdable).
narrative_ontology:cs_axiom_grounding('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', constitutional_rights_require_remedy, deontological).
narrative_ontology:cs_reference_frame('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', original_judicial_intent_balance).
narrative_ontology:cs_drift_state('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', contemporary_judicial_application, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2ddb4bf2-816c-40e0-92cc-5e01c542ec6f', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, victims_of_constitutional_violations).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shielded from personal liability for constitutional violations unless their conduct violates 'clearly established statutory or constitutional rights of which a reasonable person would have known.' This high bar effectively grants impunity for many actions, reducing personal risk.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    institutional, biographical, constrained, national).

% Benefit from reduced litigation costs and liability exposure for their officers' actions. They administer policies under the doctrine, which can implicitly encourage aggressive tactics by minimizing accountability. They actively lobby for the doctrine's preservation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Suffer physical, emotional, and financial harm due to constitutional violations by law enforcement, often without any legal recourse or compensation. The doctrine creates a near-absolute bar to holding officers accountable in civil court.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, victims_of_constitutional_violations, payer,
    powerless, immediate, trapped, local).

% Bear the significant costs of litigation and advocacy to challenge the doctrine, often facing insurmountable legal hurdles. They represent victims and work to reform or abolish qualified immunity, but their efforts are consistently suppressed by judicial precedent.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_advocates, payer,
    organized, generational, constrained, national).

% Interpret and apply the doctrine, having expanded its scope over decades through case law, making it increasingly difficult for plaintiffs to overcome. They are the primary enforcers and shapers of the constraint.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Possesses the power to reform or abolish qualified immunity through statutory changes but has largely failed to act, leaving the doctrine's evolution to the judiciary. They observe the public outcry but are often gridlocked.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, legislature, observer,
    institutional, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The doctrine ostensibly coordinates law enforcement by providing a clear, albeit high, standard for officer conduct, aiming to protect officials from 'frivolous lawsuits' and ensure they can act decisively without fear of personal liability.
% TRANSFER_FUNCTION: Transfers the cost and burden of constitutional violations from individual law enforcement officers and their agencies to victims, who bear the harm without compensation or accountability. It also transfers legal risk from state actors to the public.
% ABSENT_VOICES: The voices of victims of constitutional violations are systematically marginalized in the legal and political discourse surrounding qualified immunity. Their experiences of unredressed harm are often dismissed as isolated incidents or necessary costs of law enforcement, rather than evidence of systemic impunity.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, there would be a significant increase in civil lawsuits against law enforcement officers and agencies. This would likely lead to substantial changes in police training, internal accountability mechanisms, insurance markets for municipalities, and potentially a re-evaluation of law enforcement tactics and policies across the nation. The legal landscape for civil rights litigation would be fundamentally altered.
% FOUNDING_PROBLEM: The doctrine was ostensibly created to balance the need for effective government action with the protection of individual rights, specifically to prevent 'undue interference with government operations' and protect officials from 'frivolous lawsuits and harassing litigation' that could deter public service.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (law enforcement organizations, some legal scholars) argue the problem of frivolous lawsuits and the chilling effect on public service remain live. Opponents (civil rights groups, other legal scholars, victims' advocates) attest that the doctrine has expanded far beyond its original intent, creating widespread impunity, and that the original problem is either overstated or adequately addressed by other legal mechanisms. Independent legal analysis and empirical studies on litigation rates support the shifted-function reading.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the near-absolute bar to liability for officers, effectively extracting the right to remedy from victims. Suppression (0.90) is very high because the doctrine actively suppresses legal challenges and accountability mechanisms, making it extremely difficult for victims to pursue justice. The theater ratio (0.45) is moderate and rising, indicating that while there's a genuine, albeit diminishing, concern for protecting officials from truly frivolous lawsuits, a growing portion of the doctrine's application serves to shield officials from legitimate claims, turning its stated purpose into a performance. Accessibility collapse (0.90) is severe for victims, as legal avenues for redress are largely foreclosed. Resistance (0.75) is high, reflecting significant public and legal efforts to reform or abolish the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of law enforcement officers and agencies, qualified immunity is a necessary protection that enables them to perform their duties without fear of constant litigation. From the perspective of victims and civil rights advocates, it is a mechanism of impunity that systematically denies justice and undermines constitutional rights. The engine's computation of per-seat classifications will reflect this divergence, showing a 'snare' for victims and a 'rope' or 'scaffold' for officers, despite the overall 'snare' classification from the analytical seat of this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement officers and agencies are clear beneficiaries, as they are shielded from liability and litigation costs (low directionality). Victims of constitutional violations and civil rights advocates are the primary targets, bearing the costs of unredressed harm and the burden of challenging the doctrine (high directionality). Courts act as agenda-setters, actively shaping and expanding the doctrine, which benefits law enforcement. The legislature, while having the power to intervene, largely remains an observer.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_vs_impunity,
    'Does qualified immunity primarily deter frivolous lawsuits against public officials, or does it primarily enable misconduct by shielding officials from accountability?',
    'Empirical studies analyzing changes in officer behavior, litigation rates, and misconduct complaints in jurisdictions that have reformed or abolished qualified immunity, compared to those that have not.',
    'If it primarily enables misconduct, the doctrine''s extractiveness and suppression are higher than currently estimated, and its coordination function is largely theatrical. If it primarily deters frivolous lawsuits, the ''protective_scaffold_reading'' gains stronger empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_impunity, empirical, 'The actual behavioral impact of qualified immunity on law enforcement conduct and litigation.').

omega_variable(
    constitutional_authorization_ambiguity,
    'Is qualified immunity a judicially created doctrine with sufficient constitutional or statutory authorization, or is it an illegitimate judicial fabrication?',
    'Legal scholarship and judicial opinions that rigorously trace the doctrine''s historical and textual grounding, or legislative action explicitly authorizing or prohibiting the doctrine.',
    'If found to lack sufficient authorization, the doctrine''s legitimacy collapses, strengthening the ''constitutional_fidelity_reading'' and potentially leading to its judicial or legislative abolition. If found to be well-grounded, its persistence is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_authorization_ambiguity, conceptual, 'The legal and constitutional legitimacy of the qualified immunity doctrine''s origins.').

omega_variable(
    scope_of_clearly_established_law,
    'How broadly or narrowly should ''clearly established law'' be interpreted, and what impact does this interpretation have on accountability?',
    'A Supreme Court ruling providing a clearer, more consistent standard for ''clearly established law,'' or legislative guidance defining the scope.',
    'A broader interpretation would increase accountability and reduce extractiveness, moving the constraint closer to a ''tangled_rope'' or ''rope''. A narrower interpretation would further entrench impunity, reinforcing its ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_clearly_established_law, preference, 'The interpretive flexibility of ''clearly established law'' and its effect on officer liability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1967, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(qual_tr_t1977, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement(qual_tr_t1987, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1987, 0.25).
narrative_ontology:measurement(qual_tr_t1997, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1997, 0.33).
narrative_ontology:measurement(qual_tr_t2007, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2007, 0.39).
narrative_ontology:measurement(qual_tr_t2017, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2017, 0.43).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(qual_be_t1977, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1977, 0.6).
narrative_ontology:measurement(qual_be_t1987, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1987, 0.7).
narrative_ontology:measurement(qual_be_t1997, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1997, 0.78).
narrative_ontology:measurement(qual_be_t2007, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2007, 0.82).
narrative_ontology:measurement(qual_be_t2017, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2017, 0.84).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1967, 0.6).
narrative_ontology:measurement(qual_su_t1977, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1977, 0.7).
narrative_ontology:measurement(qual_su_t1987, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1987, 0.8).
narrative_ontology:measurement(qual_su_t1997, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1997, 0.85).
narrative_ontology:measurement(qual_su_t2007, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2007, 0.88).
narrative_ontology:measurement(qual_su_t2017, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2017, 0.89).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, police_accountability_mechanisms).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, civil_rights_litigation_standards).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, constitutional_fidelity_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, protective_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'qualified_immunity_doctrine' kernel, focusing on its role as an accountability void. It is linked to sibling readings that offer alternative interpretations of the same doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
