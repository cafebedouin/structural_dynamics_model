% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Border Normative Status: Sovereignty Primary Reading
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty_primary' reading of
 *   the contested 'border_normative_status' kernel. It asserts that
 *   territorial boundaries are legitimate instruments of collective
 *   self-determination and that states possess foundational authority to
 *   exclude non-members. This reading frames border enforcement as a
 *   legitimate state function, treating excluded migrants as a victim set
 *   whose displacement is an externality or non-issue from the perspective of
 *   state self-determination. The metrics reflect a highly extractive and
 *   suppressive constraint, with low theatricality, as the function is direct
 *   and coercive.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.85).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.9).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Border Normative Status: Sovereignty Primary Reading").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, '08489b22-ba2c-4887-a790-65cb2a2772e8').
narrative_ontology:cs_kernel_codification('08489b22-ba2c-4887-a790-65cb2a2772e8', formalized).
narrative_ontology:cs_authority_grounding('08489b22-ba2c-4887-a790-65cb2a2772e8', lineage).
narrative_ontology:cs_interpretation_layer_present('08489b22-ba2c-4887-a790-65cb2a2772e8').
narrative_ontology:cs_reading_relation('08489b22-ba2c-4887-a790-65cb2a2772e8', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('08489b22-ba2c-4887-a790-65cb2a2772e8', border_normative_status__qualified_sovereignty, forecloses).
narrative_ontology:cs_axiom('08489b22-ba2c-4887-a790-65cb2a2772e8', foundational, state_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(state_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('08489b22-ba2c-4887-a790-65cb2a2772e8', state_territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('08489b22-ba2c-4887-a790-65cb2a2772e8', foundational, collective_self_determination_priority).
narrative_ontology:cs_axiom_status(collective_self_determination_priority, holdable).
narrative_ontology:cs_axiom_grounding('08489b22-ba2c-4887-a790-65cb2a2772e8', collective_self_determination_priority, conventional).
narrative_ontology:cs_reference_frame('08489b22-ba2c-4887-a790-65cb2a2772e8', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('08489b22-ba2c-4887-a790-65cb2a2772e8', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('08489b22-ba2c-4887-a790-65cb2a2772e8', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, member_citizens).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, state_apparatus).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, non_member_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional body (government, border agencies) that defines, enforces, and benefits from the state's foundational authority to control its borders and exclude non-members. It collects resources (taxes, political capital) for this function.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the collective self-determination and security afforded by defined borders and controlled membership. They experience the border as a protective mechanism, preserving resources and cultural identity, and legitimizing their exclusive access to state benefits.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, member_citizens, beneficiary,
    organized, generational, mobile, national).

% Bear the full cost of exclusion, facing legal barriers, physical dangers, and economic hardship due to their inability to legally cross borders. Their movement is criminalized or severely restricted, leading to exploitation and denial of basic rights.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, non_member_migrants, payer,
    powerless, immediate, trapped, global).

% Despite international legal protections, they often face significant barriers to entry and due process, as the state's foundational authority to exclude is prioritized. They are often detained or denied safe passage, experiencing the border as a direct threat to their survival.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, asylum_seekers, excluded).

% Monitor border practices and challenge the absolute nature of state sovereignty, arguing for the primacy of human rights and international law. They face an uphill battle against the entrenched legal and political frameworks of state authority.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, human_rights_advocates, observer,
    organized, biographical, constrained, global).

% Analyze the historical and legal foundations of state sovereignty and its interaction with evolving human rights norms. They provide critical analysis but have no direct power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a collective to define its membership, territory, and shared resources, providing a framework for self-governance, social cohesion, and the management of public goods for its members.
% TRANSFER_FUNCTION: Transfers the right to reside, access resources, and participate in political life from non-members to members, enforced by the state's control over territorial entry and exit.
% ABSENT_VOICES: Non-member migrants, asylum seekers, and those advocating for open borders or universal human rights are structurally excluded from the decision-making processes regarding border policy, despite being directly impacted.
% DISAPPEARANCE_RATIONALE: If states lost foundational authority to exclude, borders would become porous, leading to massive population shifts, a fundamental redefinition of citizenship, and a complete reordering of global political, economic, and social systems.
% FOUNDING_PROBLEM: The need for distinct political communities to define themselves, manage shared resources, and protect their collective identity and security from external threats or uncontrolled influx, particularly in the post-Westphalian era.
% FOUNDING_PROBLEM_CORROBORATION: States and many member citizens attest to the ongoing need for border control for security, economic stability, and cultural preservation. Critics (human rights groups, some economists) contest the severity of the 'threats' and the efficacy or ethics of exclusion, but the foundational problem of collective self-definition remains.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) and suppression (0.90) are high because this reading legitimizes severe restrictions on movement and access for non-members, backed by state power. The accessibility collapse (0.95) is near-total for those without legal pathways. Resistance (0.70) is significant from migrants and advocates, but the state's enforcement capacity is robust. Theater ratio is low (0.10) because the constraint's function is direct and coercive, not performative. The claimed type is Tangled Rope because it provides a coordination function for member citizens (self-determination, resource management) while simultaneously extracting from and suppressing non-members.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member citizens and the state apparatus, this constraint is a legitimate and necessary mechanism for collective self-determination and security. From the perspective of non-member migrants and human rights advocates, it is a highly extractive and suppressive barrier that violates fundamental freedoms. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and member citizens are clear beneficiaries, gaining security, identity, and resource control. Non-member migrants and asylum seekers are the primary targets, bearing the costs of exclusion and suppression. Human rights advocates and international law scholars act as observers, analyzing and challenging the constraint without direct control over its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_determination_vs_elite_control,
    'Is ''collective self-determination'' truly a democratic expression of the populace, or is it primarily an instrument of elite control and state power?',
    'Analysis of policy-making processes, public discourse, and the distribution of benefits/costs within the ''member_citizens'' group. If benefits are concentrated and costs externalized, it suggests elite capture.',
    'If elite-driven, the coordination function for ''member_citizens'' is weakened, potentially shifting the constraint closer to a Snare, as the ''collective'' benefit becomes a cover for concentrated extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_vs_elite_control, conceptual, 'Ambiguity of ''collective self-determination'' as a coordination justification.').

omega_variable(
    sovereignty_vs_human_rights_primacy,
    'Which normative framework holds ultimate primacy: state sovereignty and the right to exclude, or universal human rights and freedom of movement?',
    'Legal and philosophical adjudication of conflicting claims, or empirical observation of which framework consistently overrides the other in practice and international jurisprudence.',
    'If human rights gain primacy, the foundational authority to exclude is undermined, shifting the constraint towards a ''qualified_sovereignty'' or ''freedom_primary'' reading, drastically reducing extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_primacy, preference, 'Contest between state sovereignty and human rights as foundational principles.').

omega_variable(
    necessity_of_exclusion_for_security,
    'To what extent is the exclusion of non-members genuinely necessary for the security and stability of the state, as opposed to being a response to perceived threats or economic protectionism?',
    'Empirical studies on the correlation between border openness/closure and national security/economic stability, disaggregating actual threats from political rhetoric.',
    'If exclusion is found to be largely unnecessary for genuine security, the justification for high extractiveness and suppression is weakened, challenging the ''tangled_rope'' classification and pushing it towards ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_exclusion_for_security, empirical, 'Empirical basis for the necessity of exclusion for state security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_normative_status__sovereignty_primary, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(bord_tr_t1965, border_normative_status__sovereignty_primary, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(bord_tr_t1985, border_normative_status__sovereignty_primary, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(bord_tr_t2005, border_normative_status__sovereignty_primary, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__sovereignty_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_normative_status__sovereignty_primary, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(bord_be_t1965, border_normative_status__sovereignty_primary, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(bord_be_t1985, border_normative_status__sovereignty_primary, base_extractiveness, 1985, 0.8).
narrative_ontology:measurement(bord_be_t2005, border_normative_status__sovereignty_primary, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__sovereignty_primary, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_normative_status__sovereignty_primary, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(bord_su_t1965, border_normative_status__sovereignty_primary, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(bord_su_t1985, border_normative_status__sovereignty_primary, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(bord_su_t2005, border_normative_status__sovereignty_primary, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__sovereignty_primary, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
