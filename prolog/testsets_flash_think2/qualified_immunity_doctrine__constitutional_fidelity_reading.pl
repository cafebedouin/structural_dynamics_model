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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine (Constitutional Fidelity Reading)
 *   domain: Constitutional Law / Civil Rights / Law Enforcement Policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'constitutional fidelity' reading
 *   of the qualified immunity doctrine. From this perspective, qualified
 *   immunity is a judicially fabricated doctrine that lacks constitutional or
 *   statutory authorization. It is illegitimate regardless of any purported
 *   policy benefits, as it represents an overreach of judicial power and
 *   undermines the constitutional rights it purports to protect. The doctrine
 *   is seen as a snare, extracting accountability and expanding judicial
 *   authority under the guise of protecting public servants.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda_setter (institutional/arbitrage) — expands its own power
 *   - law_enforcement_officers: Primary beneficiary (powerful/constrained) — shielded from liability
 *   - civil_rights_plaintiffs: Primary payer (powerless/trapped) — denied redress
 *   - legislature: Excluded actor (institutional/constrained) — authority usurped
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — critiques legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.95).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.85).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "Constitutional Law / Civil Rights / Law Enforcement Policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '6d1eb95c-defc-4944-9519-9a6dea91f4d7').
narrative_ontology:cs_kernel_codification('6d1eb95c-defc-4944-9519-9a6dea91f4d7', formalized).
narrative_ontology:cs_authority_grounding('6d1eb95c-defc-4944-9519-9a6dea91f4d7', extraction).
narrative_ontology:cs_interpretation_layer_present('6d1eb95c-defc-4944-9519-9a6dea91f4d7').
narrative_ontology:cs_reading_relation('6d1eb95c-defc-4944-9519-9a6dea91f4d7', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d1eb95c-defc-4944-9519-9a6dea91f4d7', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('6d1eb95c-defc-4944-9519-9a6dea91f4d7', foundational, judicial_power_limited_to_constitution_and_statute).
narrative_ontology:cs_axiom_status(judicial_power_limited_to_constitution_and_statute, holdable).
narrative_ontology:cs_axiom_grounding('6d1eb95c-defc-4944-9519-9a6dea91f4d7', judicial_power_limited_to_constitution_and_statute, deontological).
narrative_ontology:cs_axiom('6d1eb95c-defc-4944-9519-9a6dea91f4d7', foundational, no_immunity_without_explicit_authorization).
narrative_ontology:cs_axiom_status(no_immunity_without_explicit_authorization, holdable).
narrative_ontology:cs_axiom_grounding('6d1eb95c-defc-4944-9519-9a6dea91f4d7', no_immunity_without_explicit_authorization, deontological).
narrative_ontology:cs_reference_frame('6d1eb95c-defc-4944-9519-9a6dea91f4d7', constitutional_text_supremacy).
narrative_ontology:cs_drift_state('6d1eb95c-defc-4944-9519-9a6dea91f4d7', contemporary_judicial_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6d1eb95c-defc-4944-9519-9a6dea91f4d7', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_state_misconduct).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The branch of government that fabricated and continues to apply the doctrine, thereby expanding its own power over constitutional interpretation and shielding state actors from accountability without explicit constitutional or statutory authorization.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Are shielded from civil liability for constitutional violations unless their conduct violates 'clearly established statutory or constitutional rights of which a reasonable person would have known.' This protection enables aggressive tactics and reduces personal accountability.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, beneficiary,
    powerful, biographical, constrained, local).

% Individuals whose constitutional rights have been violated by state actors, but who are denied effective legal redress due to the high bar set by qualified immunity, often leaving them without a remedy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs, payer,
    powerless, immediate, trapped, local).

% A broader group encompassing those harmed by state actors, who find their avenues for justice blocked by the doctrine, leading to a perception of impunity for government overreach.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_state_misconduct, payer,
    powerless, immediate, trapped, local).

% The branch constitutionally empowered to create laws, including immunities. From this reading, the judiciary's creation of qualified immunity usurps legislative authority, leaving the legislature to react to judicially-created doctrine rather than setting policy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, legislature, excluded,
    institutional, generational, constrained, national).

% Academics and legal experts who critically analyze the doctrine's origins, constitutional basis, and impact, often arguing for its illegitimacy based on fidelity to constitutional text and separation of powers.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading, the doctrine has no legitimate coordination function, as its very existence is an illegitimate exercise of judicial power. Any 'coordination' it achieves (e.g., protecting officers) is a byproduct of an unconstitutional framework.
% TRANSFER_FUNCTION: Transfers accountability for constitutional violations from state actors to victims, effectively granting impunity. It also transfers legislative power over immunities to the judiciary.
% ABSENT_VOICES: The framers of the Constitution, whose intent did not include such a judicially-created immunity; and the public, whose constitutional rights are undermined without legislative consent or clear constitutional basis.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, the legal landscape for civil rights litigation would fundamentally shift. Law enforcement practices would likely be re-evaluated, and the balance of power between the judiciary and legislature regarding immunities would be reset, requiring legislative action to define officer liability.
% FOUNDING_PROBLEM: To protect government officials from frivolous lawsuits and excessive liability when performing discretionary functions, ensuring they can act decisively without fear of litigation.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement agencies and their advocates attest the problem is still live and the doctrine is necessary. Civil rights groups, legal scholars, and victims' advocates argue the founding problem is either exaggerated or that the doctrine's solution is unconstitutional, citing historical legal precedent and legislative intent. Independent legal analysis from outside the benefiting parties supports the view that the doctrine lacks constitutional or statutory basis.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is very high (0.95) because the doctrine is viewed as fundamentally illegitimate, meaning any 'cost' it imposes (denial of justice) is pure extraction. Suppression is high (0.85) due to the effective closure of legal avenues for victims. The theater ratio is moderate (0.40) as there is a performance of legal process and balancing, but the core function is seen as maintaining an unauthorized power structure. Accessibility collapse is high (0.80) as it severely limits legal alternatives for plaintiffs. Resistance is high (0.70) due to ongoing legal challenges and public debate.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, qualified immunity is a necessary evolution of common law to balance rights and governance. From the perspective of civil rights plaintiffs and constitutional scholars, it is an illegitimate barrier to justice. The engine's classification as a snare reflects the structural asymmetry and lack of legitimate authorization from this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary benefits from the expansion of its interpretive power, placing it near the beneficiary end. Law enforcement officers are direct beneficiaries, shielded from liability. Civil rights plaintiffs and victims of state misconduct are the clear targets, bearing the costs of denied justice. The legislature is excluded from its rightful role in defining immunities. Constitutional scholars act as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the doctrine as a legitimate 'rope' or 'scaffold' by highlighting its lack of constitutional or statutory basis and its function in expanding judicial power and shielding state actors. It emphasizes that the 'coordination' (protection of officers) is achieved through an illegitimate mechanism, making it a snare rather than a genuine collective-action solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_judicial_creation,
    'Is the judiciary constitutionally authorized to create and maintain a doctrine of immunity that lacks explicit statutory or constitutional basis?',
    'A Supreme Court ruling explicitly overturning the doctrine on separation of powers grounds, or a constitutional amendment clarifying judicial authority regarding immunities.',
    'If not authorized, the doctrine is fundamentally illegitimate, reinforcing its classification as a snare and demanding its abolition. If some implicit authority is found, the extractiveness might be re-evaluated downward, though still likely high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_creation, conceptual, 'Ambiguity regarding the source of judicial authority for qualified immunity.').

omega_variable(
    constitutional_text_ambiguity,
    'Does the constitutional text, or its original understanding, provide any implicit basis for a doctrine like qualified immunity, or is it a pure judicial invention?',
    'Exhaustive historical and textual analysis by a consensus of constitutional scholars, or a definitive Supreme Court interpretation based solely on originalist principles.',
    'If no textual basis exists, the claim of constitutional fidelity is severely undermined, strengthening the argument for illegitimacy. If a plausible textual hook is found, the ''fabricated'' aspect of this reading would be challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_ambiguity, empirical, 'Whether qualified immunity has any grounding in the constitutional text or original intent.').

omega_variable(
    policy_outcomes_vs_legality,
    'Can the purported policy benefits of qualified immunity (e.g., enabling vigorous law enforcement) justify a doctrine that is otherwise deemed constitutionally unauthorized?',
    'A societal consensus or legislative act that explicitly prioritizes policy outcomes over strict constitutional authorization for judicial doctrines, or a clear judicial precedent establishing such a hierarchy.',
    'If policy outcomes are deemed insufficient to legitimize an unauthorized doctrine, this reading''s core premise holds. If policy outcomes are given significant weight, the ''illegitimate regardless of policy outcomes'' claim would be challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_outcomes_vs_legality, preference, 'The tension between policy outcomes and constitutional legality in justifying judicial doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qual_tr_t8, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(qual_tr_t16, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(qual_tr_t24, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(qual_tr_t32, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(qual_tr_t40, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(qual_be_t8, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 8, 0.88).
narrative_ontology:measurement(qual_be_t16, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 16, 0.91).
narrative_ontology:measurement(qual_be_t24, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 24, 0.93).
narrative_ontology:measurement(qual_be_t32, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 32, 0.94).
narrative_ontology:measurement(qual_be_t40, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 40, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(qual_su_t8, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(qual_su_t16, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(qual_su_t24, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(qual_su_t32, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 32, 0.84).
narrative_ontology:measurement(qual_su_t40, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
