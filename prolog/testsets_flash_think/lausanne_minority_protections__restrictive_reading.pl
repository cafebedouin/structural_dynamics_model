% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Minority Protections (Restrictive Reading)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents a restrictive interpretation of the Treaty of
 *   Lausanne's minority protections, asserting that these protections apply
 *   only to individual worship rights. Under this reading, the institutional
 *   autonomy, property ownership, and theological education of non-Muslim
 *   minority communities are considered domestic matters, fully subject to
 *   general Turkish law. This interpretation has historically led to
 *   significant extraction from minority institutions and communities, as the
 *   state apparatus consolidates control over their capacity and resources.
 *   This is one reading of the 'lausanne_minority_protections' kernel,
 *   distinct from more expansive or internationally-enforceable
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.85).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.9).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Minority Protections (Restrictive Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '3adf9f67-20ba-4e56-8428-3c0e58c9a8de').
narrative_ontology:cs_kernel_codification('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', fixed_text).
narrative_ontology:cs_authority_grounding('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', extraction).
narrative_ontology:cs_interpretation_layer_present('3adf9f67-20ba-4e56-8428-3c0e58c9a8de').
narrative_ontology:cs_reading_relation('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', lausanne_minority_protections__guarantor_reading, forecloses).
narrative_ontology:cs_axiom('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', foundational, state_sovereignty_over_religious_institutions).
narrative_ontology:cs_axiom_status(state_sovereignty_over_religious_institutions, holdable).
narrative_ontology:cs_axiom_grounding('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', state_sovereignty_over_religious_institutions, conventional).
narrative_ontology:cs_axiom('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', foundational, individual_worship_only_protection).
narrative_ontology:cs_axiom_status(individual_worship_only_protection, holdable).
narrative_ontology:cs_axiom_grounding('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', individual_worship_only_protection, conventional).
narrative_ontology:cs_reference_frame('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', domestic_legal_supremacy).
narrative_ontology:cs_drift_state('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3adf9f67-20ba-4e56-8428-3c0e58c9a8de', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_communities).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, state_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Treaty of Lausanne's provisions, asserting that institutional matters of minority communities fall under general domestic law. Benefits from consolidating control over minority assets and educational systems, reinforcing national sovereignty.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the direct costs of this restrictive interpretation, including property confiscation, denial of legal personality, and severe restrictions on theological education and self-governance. Their existence is constrained by state policy, with no viable exit.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_religious_institutions, payer,
    powerless, generational, trapped, national).

% Experience the degradation of their cultural and religious infrastructure due to the weakening of their institutions. Their identity is deeply tied to these institutions, making 'exit' from the community or its traditions unthinkable, despite the costs.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_communities, payer,
    powerless, generational, identity_locked, national).

% Monitor and report on the treatment of minorities under the Treaty of Lausanne, often critiquing the restrictive interpretation as inconsistent with international human rights law. They can exert diplomatic pressure but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Signatories to the Treaty of Lausanne, with a historical role in overseeing its implementation. Their current influence is limited by the restrictive interpretation, which denies international enforceability, but they retain diplomatic leverage.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the state's perspective, it coordinates the integration of non-Muslim minorities into the national legal framework, asserting state sovereignty over all institutions within its borders.
% TRANSFER_FUNCTION: Transfers control over institutional assets (property, endowments), educational curricula, and the appointment of religious leaders from minority communities to the Turkish state apparatus.
% ABSENT_VOICES: International law scholars advocating for an expansive reading of minority rights, and guarantor states seeking to enforce international oversight, are largely excluded from the domestic legal and political discourse that shapes this interpretation.
% DISAPPEARANCE_RATIONALE: If this restrictive reading vanished overnight, minority religious institutions would immediately assert their autonomy, property rights, and educational freedom, leading to significant legal and political challenges, and a fundamental reorganization of state-minority relations and the legal landscape concerning religious minorities.
% FOUNDING_PROBLEM: To define the status and rights of non-Muslim minorities in the newly formed Turkish Republic following the collapse of the Ottoman Empire, aiming to balance the protection of minority communities with the consolidation of national sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish state apparatus attests that the founding problem of balancing sovereignty and minority rights remains live, justifying its current interpretation. Minority communities, international human rights bodies, and independent historians attest that while the initial problem of definition is largely settled, the arrangement now functions to suppress minority institutional life, supported by historical records of property confiscations and educational restrictions.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because this reading enables the state to seize or control significant assets and functions (property, education) from minority institutions. Suppression is also very high (0.90) due to the state's legal and administrative power, which effectively collapses alternatives for minority institutions and communities. Theater ratio is low (0.15) as the enforcement is direct and functional, not performative; the state actively implements this restrictive interpretation. Resistance is moderate (0.40) as minority communities and international bodies continue to advocate for broader rights, but direct, effective resistance is severely constrained.
 *
 * PERSPECTIVAL GAP:
 *   The Turkish state apparatus views this constraint as a legitimate exercise of national sovereignty and domestic legal order, consistent with the Treaty's intent. Minority religious institutions and communities, however, experience it as a snare that systematically undermines their collective existence and cultural continuity, despite formal individual worship rights. International human rights bodies often critique this interpretation as falling short of contemporary human rights standards.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state apparatus is the primary beneficiary and agenda-setter, gaining control over minority institutional capacity and property, and reinforcing its sovereignty (low directionality). Minority religious institutions and communities are the primary victims and payers, bearing the costs of lost autonomy, property confiscation, and educational restrictions (high directionality, trapped/identity_locked exit). International human rights bodies and guarantor states act as observers, critiquing the constraint but often lacking direct enforcement power (analytical directionality).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine interpretation of the Treaty of Lausanne, or a strategic reading to consolidate state power?',
    'Comparative legal analysis of historical intent vs. contemporary application, and analysis of state actions in other minority contexts.',
    'If a strategic reading, the classification as Snare is reinforced; if a genuine interpretation, it might suggest a different foundational problem or a more complex Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between genuine interpretation and strategic power consolidation.').

omega_variable(
    expansive_reading_impact,
    'How would the structural reality of minority rights change if the ''expansive_reading'' of Lausanne protections were adopted?',
    'Legal and policy analysis of hypothetical implementation of the expansive reading, including its impact on property restitution, educational autonomy, and legal personality.',
    'The victim set would shrink or disappear, extractiveness would drop to near zero, and the constraint would likely reclassify as a Rope or even a Mountain (if the protections were genuinely self-enforcing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansive_reading_impact, conceptual, 'Impact of adopting the ''expansive_reading'' on the constraint''s structure.').

omega_variable(
    guarantor_reading_impact,
    'How would the structural reality of minority rights change if the ''guarantor_reading'' of Lausanne protections were adopted?',
    'Analysis of the enforcement mechanisms and diplomatic pressure that would be applied by guarantor states and international human rights bodies under this reading.',
    'The suppression metric would likely decrease due to external pressure, and the state''s ability to unilaterally enforce the restrictive reading would be curtailed, potentially shifting the constraint towards a Tangled Rope or Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantor_reading_impact, conceptual, 'Impact of adopting the ''guarantor_reading'' on the constraint''s structure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, property confiscation) or internalized (minority communities'' resignation to state control)?',
    'Post-exit suppression trajectory: if suppression persists after legal barriers are removed, reclassify as partially internalized. Analysis of community resilience and advocacy efforts.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making resistance harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for minority communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__restrictive_reading, theater_ratio, 1923, 0.2).
narrative_ontology:measurement(laus_tr_t1945, lausanne_minority_protections__restrictive_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(laus_tr_t1970, lausanne_minority_protections__restrictive_reading, theater_ratio, 1970, 0.16).
narrative_ontology:measurement(laus_tr_t1995, lausanne_minority_protections__restrictive_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(laus_tr_t2010, lausanne_minority_protections__restrictive_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(laus_tr_t2023, lausanne_minority_protections__restrictive_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1923, 0.6).
narrative_ontology:measurement(laus_be_t1945, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(laus_be_t1970, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1970, 0.78).
narrative_ontology:measurement(laus_be_t1995, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1995, 0.82).
narrative_ontology:measurement(laus_be_t2010, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(laus_be_t2023, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1923, 0.7).
narrative_ontology:measurement(laus_su_t1945, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1945, 0.78).
narrative_ontology:measurement(laus_su_t1970, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1970, 0.85).
narrative_ontology:measurement(laus_su_t1995, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1995, 0.88).
narrative_ontology:measurement(laus_su_t2010, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(laus_su_t2023, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2023, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, minority_religious_freedom_in_turkey).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, minority_property_rights_in_turkey).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, minority_educational_autonomy_in_turkey).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
