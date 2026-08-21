% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine: Constitutional Fidelity Reading
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'constitutional_fidelity_reading'
 *   of the 'qualified_immunity_doctrine' kernel. From this perspective,
 *   qualified immunity is a judicially fabricated doctrine that lacks
 *   constitutional or statutory authorization. It is viewed as illegitimate
 *   regardless of any purported policy outcomes, as its very existence
 *   represents an overreach of judicial power and a distortion of the
 *   constitutional framework for civil rights. The doctrine's expansion over
 *   time has systematically eroded accountability for state actors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.95).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.9).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine: Constitutional Fidelity Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '3384d780-7f6e-42f6-ba28-d343375e9f78').
narrative_ontology:cs_kernel_codification('3384d780-7f6e-42f6-ba28-d343375e9f78', formalized).
narrative_ontology:cs_authority_grounding('3384d780-7f6e-42f6-ba28-d343375e9f78', extraction).
narrative_ontology:cs_interpretation_layer_present('3384d780-7f6e-42f6-ba28-d343375e9f78').
narrative_ontology:cs_reading_relation('3384d780-7f6e-42f6-ba28-d343375e9f78', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('3384d780-7f6e-42f6-ba28-d343375e9f78', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('3384d780-7f6e-42f6-ba28-d343375e9f78', foundational, judicial_overreach_illegitimate).
narrative_ontology:cs_axiom_status(judicial_overreach_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('3384d780-7f6e-42f6-ba28-d343375e9f78', judicial_overreach_illegitimate, deontological).
narrative_ontology:cs_axiom('3384d780-7f6e-42f6-ba28-d343375e9f78', foundational, constitutional_text_supremacy).
narrative_ontology:cs_axiom_status(constitutional_text_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('3384d780-7f6e-42f6-ba28-d343375e9f78', constitutional_text_supremacy, deontological).
narrative_ontology:cs_reference_frame('3384d780-7f6e-42f6-ba28-d343375e9f78', constitutional_textualism_originalism).
narrative_ontology:cs_drift_state('3384d780-7f6e-42f6-ba28-d343375e9f78', contemporary_judicial_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3384d780-7f6e-42f6-ba28-d343375e9f78', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_constitutional_violations).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Expands its own institutional power by creating and applying a doctrine (qualified immunity) that lacks explicit constitutional or statutory authorization, effectively legislating from the bench and shaping the scope of civil rights enforcement.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Shielded from personal liability for constitutional violations unless the law was 'clearly established' and they violated it in a way that every reasonable officer would know was unlawful. Benefits from reduced personal risk and increased operational latitude.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, beneficiary,
    powerful, biographical, constrained, local).

% Denied effective legal recourse for harms suffered due to official misconduct, facing an almost insurmountable legal standard that often grants immunity even for clear violations. Bears the unredressed costs of constitutional breaches.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_constitutional_violations, payer,
    powerless, immediate, trapped, local).

% Work to challenge the doctrine through litigation and legislative efforts, bearing significant legal and political costs in a system designed to protect state actors. Their efforts are often frustrated by the high bar set by qualified immunity.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_advocates, payer,
    organized, generational, constrained, national).

% Has the constitutional power to codify, modify, or abolish qualified immunity but has largely deferred to the judiciary, allowing the judicially fabricated doctrine to persist and expand without direct democratic accountability.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, legislature, excluded,
    institutional, generational, mobile, national).

% Analyze the doctrine's origins, constitutional basis, and impact, arguing for its illegitimacy based on principles of textual and historical fidelity to the Constitution and the Civil Rights Act of 1871.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, legal_scholars_constitutionalists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. From this constitutional fidelity reading, the doctrine serves no legitimate coordination function; it is a judicial fabrication that distorts the constitutional order.
% TRANSFER_FUNCTION: Transfers accountability for constitutional violations from individual law enforcement officers and their employing agencies to victims, who bear the costs of unredressed harm. It also transfers legislative power from the elected legislature to the unelected judiciary.
% ABSENT_VOICES: The framers of the Constitution and the original intent of the Civil Rights Act of 1871 (Section 1983) are absent, as their principles are overridden by judicial invention. The voices of those denied justice are systematically marginalized by the doctrine's high bar.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished, the legal landscape for civil rights litigation would fundamentally shift. Law enforcement agencies would face increased liability, likely leading to changes in training, oversight, and insurance. Victims would have a clearer path to redress, and the balance of power between the judiciary and legislature would be altered, restoring legislative supremacy in this domain.
% FOUNDING_PROBLEM: The problem of holding government officials accountable for constitutional violations while preventing frivolous lawsuits against them.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and civil rights organizations argue that the doctrine has overshot its original intent, creating an accountability void. The judiciary and law enforcement agencies maintain it is necessary to prevent 'chilling' legitimate police work. Independent legal analysis from constitutional historians and legal ethicists supports the view that the doctrine lacks a legitimate constitutional or statutory basis.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) because the doctrine fundamentally extracts the right to redress for constitutional violations, which is seen as an illegitimate taking of a foundational right. Suppression is also very high (0.90) as the legal standard actively suppresses legitimate claims and prevents victims from holding officials accountable. Theater ratio is moderate (0.40) because while there is a performance of legal process, the outcome is often predetermined by the high bar of qualified immunity, making the process itself a form of theatrical maintenance for an illegitimate structure. The increasing values over time reflect the doctrine's expansion and hardening through case law.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's and law enforcement's perspective, qualified immunity is often framed as a necessary protection (a 'protective_scaffold_reading'). However, from the perspective of constitutional fidelity, the same structure is an illegitimate fabrication that undermines the rule of law and constitutional rights, operating as a 'snare'. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary benefits from the doctrine by expanding its own institutional power and influence over constitutional interpretation. Law enforcement officers are direct beneficiaries, shielded from liability. Victims of constitutional violations and civil rights advocates are the primary targets, bearing the costs of unredressed harm and the burden of challenging an entrenched legal barrier. The legislature is excluded, as its proper role in defining such immunities has been usurped.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_of_common_law_evolution,
    'Is the judicial creation and evolution of qualified immunity a legitimate exercise of common law development, or an unconstitutional overreach into legislative authority?',
    'A Supreme Court decision explicitly overturning the doctrine on constitutional grounds, or comprehensive legislative action to abolish or codify it, clarifying the boundaries of judicial power.',
    'If deemed legitimate common law, the ''fabricated'' aspect of this reading would be weakened, potentially lowering the perceived extractiveness. If deemed overreach, it would reinforce the ''snare'' classification and the illegitimacy claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_legitimacy_of_common_law_evolution, conceptual, 'Ambiguity regarding the constitutional legitimacy of judicial common law development in this domain.').

omega_variable(
    empirical_impact_on_officer_conduct,
    'Does qualified immunity actually achieve its stated goal of preventing ''chilling'' legitimate law enforcement activity, or does it primarily shield misconduct?',
    'Empirical studies comparing officer behavior, misconduct rates, and civil rights litigation outcomes in jurisdictions with and without qualified immunity, or before and after its expansion.',
    'If empirical evidence shows it primarily shields misconduct without significantly preventing frivolous lawsuits, it would strengthen the ''snare'' classification by undermining its purported coordination function. If it genuinely prevents chilling, it would challenge the ''pure extraction'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_impact_on_officer_conduct, empirical, 'Uncertainty about the actual policy effects of qualified immunity on law enforcement behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(qual_tr_t1978, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1978, 0.25).
narrative_ontology:measurement(qual_tr_t1987, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement(qual_tr_t1999, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1999, 0.35).
narrative_ontology:measurement(qual_tr_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(qual_be_t1978, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1978, 0.7).
narrative_ontology:measurement(qual_be_t1987, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1987, 0.8).
narrative_ontology:measurement(qual_be_t1999, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1999, 0.88).
narrative_ontology:measurement(qual_be_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2010, 0.92).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(qual_su_t1978, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1978, 0.75).
narrative_ontology:measurement(qual_su_t1987, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1987, 0.8).
narrative_ontology:measurement(qual_su_t1999, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1999, 0.85).
narrative_ontology:measurement(qual_su_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, police_accountability_mechanisms).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_litigation).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_training_standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
