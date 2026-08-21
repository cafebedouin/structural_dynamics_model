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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine (Accountability Void Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This story describes the qualified immunity doctrine as a systematic
 *   extraction mechanism that guarantees impunity for constitutional
 *   violations by law enforcement, creating an accountability void. This
 *   reading focuses on the practical effects of the doctrine, where victims
 *   are denied remedy and officers are shielded from consequences, regardless
 *   of the constitutional merits of their actions. The doctrine, initially
 *   conceived to protect officials from frivolous lawsuits, has expanded
 *   through judicial interpretation to become a near-absolute bar to
 *   liability, effectively extracting accountability from the public and
 *   transferring it to state actors.
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: Primary beneficiary (institutional/arbitrage) — shielded from liability
 *   - law_enforcement_agencies: Secondary beneficiary (institutional/constrained) — reduced litigation costs, avoids systemic reform
 *   - victims_of_constitutional_violations: Primary target (powerless/trapped) — denied remedy and justice
 *   - civil_rights_advocates: Secondary target (organized/constrained) — efforts to secure accountability are nullified
 *   - federal_judiciary: Agenda setter (institutional/identity_locked) — interprets and enforces the doctrine
 *   - legislative_bodies: Excluded (institutional/constrained) — could reform but fail to act
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
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine (Accountability Void Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '62424a08-7b0f-4584-bd38-38cd4b6c7e9a').
narrative_ontology:cs_kernel_codification('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', formalized).
narrative_ontology:cs_authority_grounding('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', lineage).
narrative_ontology:cs_interpretation_layer_present('62424a08-7b0f-4584-bd38-38cd4b6c7e9a').
narrative_ontology:cs_reading_relation('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', foundational, impunity_for_constitutional_violations_is_systemic).
narrative_ontology:cs_axiom_status(impunity_for_constitutional_violations_is_systemic, holdable).
narrative_ontology:cs_axiom_grounding('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', impunity_for_constitutional_violations_is_systemic, empirically_contingent).
narrative_ontology:cs_axiom('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', secondary, judicial_doctrine_supersedes_constitutional_accountability).
narrative_ontology:cs_axiom_status(judicial_doctrine_supersedes_constitutional_accountability, holdable).
narrative_ontology:cs_axiom_grounding('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', judicial_doctrine_supersedes_constitutional_accountability, conventional).
narrative_ontology:cs_reference_frame('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', constitutional_accountability_framework).
narrative_ontology:cs_drift_state('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', contemporary_judicial_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('62424a08-7b0f-4584-bd38-38cd4b6c7e9a', '').
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

% Shielded from liability for constitutional violations unless their conduct violates 'clearly established statutory or constitutional rights of which a reasonable person would have known.' This high bar effectively grants impunity for many actions, reducing personal risk and accountability.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    institutional, biographical, arbitrage, national).

% Benefit from reduced litigation costs and public scrutiny, as individual officers are rarely held liable. This allows agencies to avoid systemic reforms that might otherwise be compelled by successful lawsuits.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, constrained, national).

% Bear the full cost of constitutional violations (e.g., excessive force, unlawful arrest) with virtually no legal recourse. The doctrine creates an accountability void, denying them justice and remedy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, victims_of_constitutional_violations, payer,
    powerless, immediate, trapped, local).

% Expend significant resources attempting to challenge the doctrine and secure accountability for victims, often facing insurmountable legal barriers. Their efforts are systematically suppressed by the doctrine's high bar for liability.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_advocates, payer,
    organized, generational, constrained, national).

% The primary enforcer and interpreter of qualified immunity, having created and expanded the doctrine through case law. Judges are bound by precedent but face increasing pressure to reconsider the doctrine's scope.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Could abolish or reform qualified immunity through legislation but have largely failed to act due to political gridlock and lobbying from law enforcement interests. Their inaction perpetuates the doctrine's effects.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, legislative_bodies, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading, the doctrine coordinates impunity for state actors, ensuring that constitutional violations by law enforcement rarely result in personal liability, thereby protecting officers and agencies from accountability.
% TRANSFER_FUNCTION: Transfers the cost of constitutional violations from individual law enforcement officers and their agencies to the victims of those violations, by denying legal remedy and accountability.
% ABSENT_VOICES: Victims of constitutional violations, who are systematically denied justice, and civil rights organizations, whose efforts to secure accountability are largely nullified by the doctrine. They would demand an end to impunity and a restoration of constitutional accountability.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, there would be a dramatic increase in lawsuits against officers for constitutional violations, leading to greater accountability, potential reforms in law enforcement practices, and a shift in the balance of power between citizens and the state. The legal landscape for civil rights would fundamentally reorganize.
% FOUNDING_PROBLEM: The doctrine was ostensibly created to protect government officials from frivolous lawsuits and the chilling effect of potential liability, allowing them to perform their duties without undue fear of litigation.
% FOUNDING_PROBLEM_CORROBORATION: While law enforcement groups and some legal scholars still argue the problem is live, a broad coalition of civil rights organizations, legal academics, and victims' advocates attest that the doctrine has far exceeded its original intent, creating an accountability void that is no longer justified by the 'frivolous lawsuit' concern. Independent legal analysis and empirical studies of litigation outcomes corroborate this shifted function.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.92) because the doctrine almost completely eliminates the possibility of holding officers accountable for constitutional violations, effectively extracting the right to remedy from victims. Suppression is also very high (0.88) as the legal system actively enforces this immunity, making it nearly impossible for victims to win lawsuits. Theater ratio is low (0.15) because the doctrine's primary function, from this reading, is to shield officers, not to perform a coordination function that has atrophied. The doctrine is actively and effectively doing what it was expanded to do: protect officers from liability. The temporal measurements show a clear trend of increasing extractiveness and suppression as the doctrine has been expanded by judicial rulings over decades.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary, as the agenda setter, experiences this as a necessary legal principle for effective governance, while victims and civil rights advocates experience it as a profound injustice and an active mechanism of impunity. The engine's classification will highlight this divergence, showing a snare from the victims' seat and potentially a scaffold (or even rope, if the 'frivolous lawsuit' rationale is accepted) from the judiciary's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement officers and agencies are clear beneficiaries (d near 0.0) as they are directly protected from liability. Victims of constitutional violations and civil rights advocates are clear targets (d near 1.0) as they bear the costs of violations without remedy and face systemic barriers to justice. The federal judiciary, while appearing to be a neutral arbiter, is structurally aligned with the doctrine's persistence, making it an agenda setter with a directionality that favors the doctrine's continued operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading classifies qualified immunity as a snare because its original mandate (protecting against frivolous lawsuits) has largely atrophied, while its function has shifted to systematically denying accountability for constitutional violations. The classification prevents mislabeling it as a legitimate coordination mechanism (rope or scaffold) by highlighting the high extraction and suppression, and the identifiable victims. The 'dead' status of the founding problem further supports this snare classification, indicating that the constraint persists not due to its original purpose, but due to the benefits it provides to its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint best understood as the ''accountability void'' reading of qualified immunity, or as a ''protective scaffold'' or ''constitutional fidelity'' reading?',
    'Analysis of judicial opinions, legislative debates, and public discourse to identify the dominant framing and its structural implications for accountability and constitutional rights.',
    'If a ''protective scaffold'' reading were dominant, the constraint might be reclassified as a scaffold or tangled_rope, emphasizing its coordination function. If a ''constitutional fidelity'' reading were dominant, the focus would shift to the doctrine''s legitimacy as a judicial creation, potentially leading to a mountain (if seen as inherent) or snare (if seen as illegitimate fabrication).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the primary interpretation of qualified immunity.').

omega_variable(
    judicial_intent_vs_effect,
    'To what extent does the federal judiciary''s intent in applying qualified immunity align with the doctrine''s actual effects on accountability and constitutional rights?',
    'Empirical studies comparing judicial reasoning in qualified immunity cases with the outcomes for victims, and analysis of dissenting opinions that highlight the gap between intent and effect.',
    'If intent and effect diverge significantly, it strengthens the ''snare'' classification by highlighting the doctrine''s function as an extraction mechanism despite stated protective aims. If they align, it suggests a more direct, albeit still extractive, coordination of impunity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_intent_vs_effect, empirical, 'Gap between judicial intent and the doctrine''s practical consequences.').


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
narrative_ontology:measurement(qual_tr_t1995, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(qual_tr_t2008, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2008, 0.14).
narrative_ontology:measurement(qual_tr_t2015, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(qual_be_t1995, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(qual_be_t2008, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2008, 0.85).
narrative_ontology:measurement(qual_be_t2015, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2015, 0.9).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(qual_su_t1995, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(qual_su_t2008, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2008, 0.85).
narrative_ontology:measurement(qual_su_t2015, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2015, 0.87).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, police_misconduct_reporting_standards).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, civil_rights_litigation_funding).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'qualified_immunity_doctrine' kernel. Other readings include 'protective_scaffold_reading' and 'constitutional_fidelity_reading', which offer different structural interpretations of the same legal principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
