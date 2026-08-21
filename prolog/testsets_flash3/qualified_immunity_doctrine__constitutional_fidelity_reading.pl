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
 *   human_readable: Qualified Immunity Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story analyzes the doctrine of qualified immunity from a
 *   'constitutional fidelity' reading. This reading asserts that qualified
 *   immunity is a judicially fabricated doctrine lacking constitutional or
 *   statutory authorization, rendering it illegitimate regardless of its
 *   policy outcomes. It views the doctrine as an overreach of judicial power,
 *   creating a barrier to justice for victims of constitutional violations
 *   and undermining the separation of powers. The high extractiveness and
 *   suppression reflect the systemic denial of legal recourse and the
 *   judicial entrenchment of the doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.95).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.9).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, 'f67dfa0f-f286-4d5b-8267-fe856bfd8c06').
narrative_ontology:cs_kernel_codification('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', implicit).
narrative_ontology:cs_authority_grounding('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', extraction).
narrative_ontology:cs_interpretation_layer_present('f67dfa0f-f286-4d5b-8267-fe856bfd8c06').
narrative_ontology:cs_reading_relation('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', foundational, judicial_power_limited_to_text_and_history).
narrative_ontology:cs_axiom_status(judicial_power_limited_to_text_and_history, holdable).
narrative_ontology:cs_axiom_grounding('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', judicial_power_limited_to_text_and_history, deontological).
narrative_ontology:cs_axiom('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', foundational, constitutional_rights_self_executing).
narrative_ontology:cs_axiom_status(constitutional_rights_self_executing, holdable).
narrative_ontology:cs_axiom_grounding('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', constitutional_rights_self_executing, deontological).
narrative_ontology:cs_reference_frame('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', constitutional_text_and_original_intent).
narrative_ontology:cs_drift_state('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', contemporary_judicial_practice, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f67dfa0f-f286-4d5b-8267-fe856bfd8c06', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary_as_policy_maker).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_rights_claimants).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_as_unprotected).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The judiciary, particularly the Supreme Court, created and expanded qualified immunity without clear constitutional or statutory basis. This reading sees them as benefiting from an expansion of judicial power into policy-making, effectively legislating from the bench and insulating their own decisions from challenge.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary_as_policy_maker, agenda_setter,
    institutional, generational, identity_locked, national).

% Individuals whose constitutional rights have been violated by state actors find their claims routinely dismissed due to qualified immunity. They are denied a legitimate legal avenue for redress, effectively bearing the cost of a judicially created barrier to justice.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_rights_claimants, payer,
    powerless, immediate, trapped, local).

% Officers are placed in a legally ambiguous position where their actions are judged by an evolving, unpredictable standard of 'clearly established law.' This reading argues they are denied a clear, constitutionally or statutorily defined framework for their duties, leading to uncertainty and potential for arbitrary judgment, rather than genuine protection.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_as_unprotected, payer,
    moderate, biographical, constrained, local).

% The branch constitutionally empowered to define immunities and remedies is bypassed by judicial action. This reading views the legislature as excluded from its proper role in shaping law enforcement accountability, with its attempts to legislate often undermined by judicial interpretation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, legislative_branch, excluded,
    institutional, generational, constrained, national).

% Academics and legal experts who analyze the doctrine's origins and impact, often critiquing its lack of textual or historical grounding and its implications for the separation of powers.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the doctrine serves no legitimate coordination function, as it lacks constitutional or statutory basis. Any perceived 'coordination' is an illegitimate judicial overreach.
% TRANSFER_FUNCTION: Transfers the burden of constitutional violations from state actors and the state itself to individual victims, by denying them legal recourse. It also transfers legislative authority from Congress to the judiciary.
% ABSENT_VOICES: The framers of the Constitution and the legislative branch are effectively absent from the process of defining immunities, as the judiciary has usurped this role. Their 'voice' would demand a return to constitutionally authorized processes.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, the legal landscape for civil rights litigation would fundamentally shift. Courts would be forced to adjudicate constitutional claims on their merits, potentially leading to a surge in lawsuits against state actors and a re-evaluation of law enforcement training and accountability mechanisms. The balance of power between the judiciary and legislature would also be reconfigured.
% FOUNDING_PROBLEM: The doctrine was judicially created to address concerns about excessive litigation against public officials and to ensure officials could perform their duties without fear of frivolous lawsuits.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary (as agenda-setter) claims the problem is live, citing the need to protect public officials. Constitutional scholars and civil rights advocates (outside the benefiting parties) argue that the 'problem' was either exaggerated or that the judicial 'solution' is illegitimate and has created greater problems for constitutional fidelity.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is high (0.95) because the doctrine effectively nullifies constitutional rights for victims, denying them any meaningful remedy. Suppression is also high (0.90) as the judiciary actively enforces this barrier, making it extremely difficult to overcome. The theater ratio is low (0.10) because, from this reading, the doctrine's primary function is to shield state actors, not to perform a legitimate coordination or protective role. Accessibility collapse is high (0.90) as legal avenues for redress are almost entirely foreclosed. Resistance is also high (0.80) due to ongoing legal challenges and advocacy against the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective (as policy-maker), the doctrine might be framed as a necessary 'scaffold' for effective governance. However, from the constitutional fidelity reading, this is a 'snare' that illegitimately expands judicial power and denies fundamental rights. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary, as the creator and enforcer of the doctrine, is the primary beneficiary, gaining expanded policy-making power. Constitutional rights claimants are the clear victims, bearing the full cost of denied justice. Law enforcement officers, while seemingly protected, are also victims in this reading, as they operate under an illegitimate and unpredictable legal framework. The legislative branch is excluded from its proper role.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently views the doctrine as having no legitimate mandate from its inception, thus it is a 'snare' rather than a 'piton' or 'scaffold' that has atrophied. The question is not one of mandate decay, but of foundational illegitimacy. The classification prevents mislabeling an illegitimate power grab as a decaying coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_basis_ambiguity,
    'Does qualified immunity have any implicit constitutional or statutory basis, or is it purely a judicial fabrication?',
    'A definitive Supreme Court ruling explicitly identifying a textual or historical basis, or comprehensive legislative action codifying or rejecting the doctrine.',
    'If a legitimate basis is found, the ''fabricated doctrine'' claim weakens, potentially shifting the constraint''s classification towards a ''tangled_rope'' or ''scaffold'' (from other readings'' perspectives). If no basis is found, the ''snare'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_basis_ambiguity, conceptual, 'Ambiguity regarding the legal foundation of qualified immunity.').

omega_variable(
    judicial_power_legitimacy,
    'Is the judiciary''s role in creating and expanding qualified immunity a legitimate exercise of common law development, or an illegitimate usurpation of legislative power?',
    'A shift in judicial philosophy or a constitutional amendment clarifying the boundaries of judicial power in creating immunities.',
    'If deemed legitimate common law, the ''judiciary_as_policy_maker'' beneficiary role might be re-evaluated. If confirmed as usurpation, the ''snare'' classification and high extractiveness are further solidified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_power_legitimacy, preference, 'Debate over the legitimacy of judicial policy-making in this area.').

omega_variable(
    officer_protection_vs_illegitimacy,
    'Does the doctrine genuinely protect law enforcement officers, or does its lack of legitimate grounding expose them to greater legal uncertainty and moral hazard?',
    'Empirical studies on officer behavior and legal outcomes under different immunity regimes, or a clear legislative framework that provides protection through statutory means.',
    'If officers are found to be genuinely protected by a legitimate framework, their ''victim'' status in this reading would be challenged. If uncertainty and moral hazard are confirmed, their ''victim'' status is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_protection_vs_illegitimacy, empirical, 'Whether officers are truly protected or harmed by the doctrine''s illegitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(qual_be_t1980, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1980, 0.78).
narrative_ontology:measurement(qual_be_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1995, 0.85).
narrative_ontology:measurement(qual_be_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2010, 0.9).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(qual_su_t1980, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(qual_su_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1995, 0.82).
narrative_ontology:measurement(qual_su_t2010, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_accountability_mechanisms).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_litigation_access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
