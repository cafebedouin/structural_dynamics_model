% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy over Constitutional Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of
 *   constitutional interpretive authority, where courts hold final, binding
 *   authority over the meaning of the constitution. This reading emphasizes
 *   judicial independence, legal expertise, and the protection of rights from
 *   majoritarian impulses. It is one of several competing readings of the
 *   'basic_law_interpretive_authority' kernel. The metrics reflect the
 *   institutional power and extraction inherent in this arrangement,
 *   particularly as it has evolved over time.
 *
 * KEY AGENTS:
 *   - Judiciary: Primary beneficiary and agenda-setter, wielding final interpretive authority.
 *   - Legal Profession: Secondary beneficiary, profiting from the specialized nature of constitutional law.
 *   - Legislature: Primary payer, subject to judicial review and potential invalidation of its laws.
 *   - Electoral Majorities: Payer, whose democratic will can be frustrated by judicial decisions.
 *   - Executive Branch: Payer, constrained in policy implementation by judicial interpretations.
 *   - Constitutional Scholars: Analytical observers, studying the system's dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy over Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, 'fe1d648e-f3a1-484f-8f37-a044be2076a1').
narrative_ontology:cs_kernel_codification('fe1d648e-f3a1-484f-8f37-a044be2076a1', fixed_text).
narrative_ontology:cs_authority_grounding('fe1d648e-f3a1-484f-8f37-a044be2076a1', lineage).
narrative_ontology:cs_interpretation_layer_present('fe1d648e-f3a1-484f-8f37-a044be2076a1').
narrative_ontology:cs_reading_relation('fe1d648e-f3a1-484f-8f37-a044be2076a1', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe1d648e-f3a1-484f-8f37-a044be2076a1', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('fe1d648e-f3a1-484f-8f37-a044be2076a1', foundational, judicial_finality_for_constitutional_meaning).
narrative_ontology:cs_axiom_status(judicial_finality_for_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('fe1d648e-f3a1-484f-8f37-a044be2076a1', judicial_finality_for_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('fe1d648e-f3a1-484f-8f37-a044be2076a1', foundational, judicial_expertise_in_constitutional_law).
narrative_ontology:cs_axiom_status(judicial_expertise_in_constitutional_law, holdable).
narrative_ontology:cs_axiom_grounding('fe1d648e-f3a1-484f-8f37-a044be2076a1', judicial_expertise_in_constitutional_law, empirically_contingent).
narrative_ontology:cs_reference_frame('fe1d648e-f3a1-484f-8f37-a044be2076a1', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('fe1d648e-f3a1-484f-8f37-a044be2076a1', contemporary_political_polarization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fe1d648e-f3a1-484f-8f37-a044be2076a1', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the final say on constitutional meaning, interpreting the basic law and striking down legislation deemed unconstitutional. Benefits from enhanced institutional authority and prestige. Its independence from political pressure is a core tenet of this reading.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefits from the complexity and specialized nature of constitutional interpretation, which requires legal training and expertise. This enhances their professional standing and economic opportunities.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Bears the cost of having its democratically enacted laws subject to judicial review and potential invalidation. Its legislative agenda can be blocked or altered by judicial decisions, leading to gridlock and frustration.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, immediate, constrained, national).

% Their policy preferences, expressed through elected representatives, can be overturned by unelected judges. This creates a democratic deficit and can lead to a sense of disempowerment, as their will is not final.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, immediate, constrained, national).

% Must implement laws as interpreted by the judiciary, even if it disagrees with the interpretation. Its policy initiatives can be constrained by judicial rulings, affecting its ability to govern effectively.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    institutional, immediate, constrained, national).

% Analyze the implications of judicial supremacy, its historical development, and its impact on democratic governance. They provide critical commentary and alternative theoretical frameworks.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative interpretation of the constitution, preventing legislative overreach and ensuring consistency in legal application across different political cycles. It aims to protect fundamental rights and minority interests from transient majoritarian impulses.
% TRANSFER_FUNCTION: Transfers final interpretive power over the constitution from the democratically elected branches to the judiciary, along with the associated institutional prestige and influence. It also transfers the cost of gridlock and policy frustration to the legislative and executive branches, and ultimately to electoral majorities.
% ABSENT_VOICES: Proponents of parliamentary sovereignty and popular constitutionalism are structurally excluded from the final interpretive act. They would argue for democratic accountability and direct popular engagement in constitutional meaning-making, but their claims are subordinated to judicial finality.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the legislature would immediately assert its own interpretive authority, leading to a period of intense constitutional contestation. Laws previously struck down might be re-enacted, and the balance of power between branches would fundamentally shift, reorganizing the entire political system.
% FOUNDING_PROBLEM: To prevent legislative tyranny, protect individual rights, and ensure the long-term stability and coherence of the constitutional order against political expediency and transient majoritarianism.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and many legal scholars attest that the founding problem of protecting rights and constitutional stability remains live. However, proponents of parliamentary sovereignty and popular constitutionalism argue that the problem has shifted, and judicial supremacy now creates a democratic deficit, with corroboration from political scientists and public opinion surveys.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant power transferred to the judiciary, allowing it to shape policy outcomes without direct democratic accountability. Suppression (0.70) is high because alternative interpretive paths (e.g., legislative or popular finality) are actively suppressed by the institutional structure and legal precedent. Theater ratio (0.20) is relatively low, as the judiciary's interpretive function is largely genuine, though some performativity exists in framing policy choices as purely legal determinations. The increasing trend in extractiveness and suppression over time reflects the historical expansion of judicial power and the hardening of its institutional defenses.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary coordination mechanism for constitutional stability and rights protection. From the legislature and electoral majorities' perspective, it is an extractive mechanism that centralizes power in an unelected body, frustrating democratic self-governance. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and legal profession are clear beneficiaries, gaining authority, prestige, and economic opportunity (low directionality). The legislature, executive, and electoral majorities are targets, bearing the costs of constrained policy-making and democratic frustration (high directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling judicial supremacy as pure coordination. While it provides a coordination function (stable constitutional meaning), the significant extraction from democratic processes and active suppression of alternative interpretive authorities indicate it is a Tangled Rope, not a pure Rope. The 'contested' status of the founding problem further supports this, suggesting the original mandate may have been superseded by institutional self-preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_judicial_independence,
    'Is the democratic deficit created by judicial supremacy a necessary cost for constitutional stability and rights protection, or an illegitimate usurpation of popular sovereignty?',
    'This is a normative question, resolvable only through societal preference shifts or constitutional amendment processes that reallocate interpretive authority.',
    'If deemed an illegitimate usurpation, the constraint would be reclassified closer to a Snare from the perspective of democratic actors; if deemed a necessary cost, its Tangled Rope classification would be reinforced as a functional, albeit extractive, coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_judicial_independence, preference, 'Normative trade-off between democratic accountability and judicial counter-majoritarianism.').

omega_variable(
    judicial_expertise_objectivity,
    'To what extent is judicial interpretation truly based on specialized legal expertise and objective constitutional principles, versus reflecting the policy preferences or ideological biases of judges?',
    'Empirical studies of judicial behavior, analysis of dissenting opinions, and comparison of rulings across different judicial appointments over time. However, full ''objectivity'' is conceptually elusive.',
    'If interpretation is found to be largely policy-driven, the ''expertise'' justification for judicial supremacy weakens, increasing the perceived extractiveness and theater ratio, pushing the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_expertise_objectivity, empirical, 'The degree to which judicial decisions are driven by law vs. policy preferences.').

omega_variable(
    alternative_interpretive_mechanisms,
    'Are there viable alternative institutional designs for constitutional interpretation that could achieve similar stability and rights protection with less democratic cost or extraction?',
    'Comparative constitutional studies, political theory proposals (e.g., constitutional councils, legislative review mechanisms), and real-world experiments in different jurisdictions.',
    'The demonstration of viable, less extractive alternatives would significantly undermine the ''necessity'' claim of judicial supremacy, potentially reclassifying it as a Snare by highlighting suppressed alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_interpretive_mechanisms, conceptual, 'Feasibility and desirability of alternative models for constitutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_process_efficiency).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, executive_policy_implementation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'basic_law_interpretive_authority' kernel. Its extractiveness and suppression metrics reflect the institutional power of the judiciary relative to other branches, which differs significantly from sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
