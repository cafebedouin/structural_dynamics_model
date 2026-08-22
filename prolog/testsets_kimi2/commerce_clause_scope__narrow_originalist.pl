% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Narrow Originalist Commerce Clause Scope
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   The Commerce Clause of the U.S. Constitution grants Congress power to
 *   regulate commerce among the several states. This constraint story
 *   instantiates the narrow_originalist reading: 'commerce' means trade
 *   crossing state lines, 'regulate' means to make regular (facilitate), and
 *   federal power extends only to removing state-imposed barriers to
 *   interstate trade and ensuring uniform commercial rules. Under this
 *   reading, federal environmental, labor, and civil rights laws that
 *   regulate non-commercial or purely local activity are unconstitutional.
 *   State governments and local businesses are structural beneficiaries;
 *   federal regulators and civil rights claimants in recalcitrant states are
 *   structural payers. The constraint is one reading of the
 *   commerce_clause_scope kernel; siblings broad_effects_test and
 *   intermediate_channels produce structurally different constraints with
 *   different victim sets and Îµ values.
 *
 * KEY AGENTS:
 *   - state_governments (beneficiary/institutional/constrained) â retain intrastate regulatory autonomy
 *   - local_businesses (beneficiary/moderate/constrained) â operate under lighter federal burdens
 *   - federal_regulators (payer/institutional/constrained) â lose national regulatory reach
 *   - civil_rights_claimants (payer/powerless/trapped) â lose federal protection in recalcitrant states
 *   - narrow_originalist_jurists (agenda_setter/institutional/analytical) â enforce the narrow interpretive framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.38).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.55).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.38).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Narrow Originalist Commerce Clause Scope").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional/law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '51140135-c34c-4ca7-9dae-a568b5090e6a').
narrative_ontology:cs_kernel_codification('51140135-c34c-4ca7-9dae-a568b5090e6a', fixed_text).
narrative_ontology:cs_authority_grounding('51140135-c34c-4ca7-9dae-a568b5090e6a', lineage).
narrative_ontology:cs_interpretation_layer_present('51140135-c34c-4ca7-9dae-a568b5090e6a').
narrative_ontology:cs_reading_relation('51140135-c34c-4ca7-9dae-a568b5090e6a', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('51140135-c34c-4ca7-9dae-a568b5090e6a', commerce_clause_scope__intermediate_channels, forecloses).
narrative_ontology:cs_axiom('51140135-c34c-4ca7-9dae-a568b5090e6a', foundational, commerce_means_trade_crossing_lines).
narrative_ontology:cs_axiom_status(commerce_means_trade_crossing_lines, holdable).
narrative_ontology:cs_axiom_grounding('51140135-c34c-4ca7-9dae-a568b5090e6a', commerce_means_trade_crossing_lines, empirically_contingent).
narrative_ontology:cs_axiom('51140135-c34c-4ca7-9dae-a568b5090e6a', foundational, regulate_means_make_regular).
narrative_ontology:cs_axiom_status(regulate_means_make_regular, holdable).
narrative_ontology:cs_axiom_grounding('51140135-c34c-4ca7-9dae-a568b5090e6a', regulate_means_make_regular, empirically_contingent).
narrative_ontology:cs_reference_frame('51140135-c34c-4ca7-9dae-a568b5090e6a', original_public_meaning_1787).
narrative_ontology:cs_drift_state('51140135-c34c-4ca7-9dae-a568b5090e6a', contemporary_federal_practice, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('51140135-c34c-4ca7-9dae-a568b5090e6a', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_regulators).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain exclusive authority over intrastate economic and non-commercial activity; shielded from federal preemption in manufacturing, agriculture, and local labor conditions under this reading. They benefit from regulatory autonomy but remain bound by federal judicial interpretation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Operate under state and local regulatory regimes without federal minimum standards for wages, hours, or environmental compliance when activity is deemed purely local; benefit from regulatory competition between states and lower compliance costs relative to national standards.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    moderate, biographical, constrained, national).

% Cannot deploy comprehensive national regulatory schemes in environmental, labor, and civil rights domains to non-commercial intrastate activity; must rely on alternative constitutional powers or state cooperation; bear the institutional cost of fragmented enforcement and jurisdictional uncertainty.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_regulators, payer,
    institutional, generational, constrained, national).

% Residents of states that decline to protect civil rights locally lose federal recourse when the regulated activity is classified as non-commercial and intrastate; dependent on state political majorities for protection and structurally unable to exit to a more protective jurisdiction without significant personal cost.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Occupy the interpretive seat that enforces the narrow reading through judicial review; derive professional authority and ideological coherence from claims of fidelity to original public meaning; set the constitutional boundaries within which Congress may legislate.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, narrow_originalist_jurists, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_scope__narrow_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates interstate trade by preventing state-level protectionism and ensuring uniform commercial rules across state boundaries; authorizes Congress to remove state-imposed barriers to goods and services crossing state lines.
% TRANSFER_FUNCTION: Transfers regulatory authority over non-commercial intrastate economic activity from the federal government to state governments and local polities; transfers the costs of regulatory fragmentation and lost federal protection to civil rights claimants and national regulatory agencies.
% ABSENT_VOICES: Civil rights claimants in recalcitrant states and national regulatory uniformity advocates are heard in litigation but structurally disadvantaged by the interpretive framework; their preferred policy outcomes require constitutional amendment or a changed judicial majority to achieve.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished overnight, federal environmental, labor, and civil rights statutes would extend to previously exempt local activity, state regulatory experiments would be preempted by national uniformity, and the balance of federal-state power would shift dramatically toward Congress.
% FOUNDING_PROBLEM: Conflicting state commercial regulations and protectionist trade barriers under the Articles of Confederation impeded national economic union and interstate trust.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and the Federalist Papers (No. 42) attest the trade-barrier problem from outside the benefiting state governments; modern federal judges across interpretive camps acknowledge the historical founding problem while disputing its implications for contemporary scope.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate-low: the constraint genuinely coordinates interstate trade but asymmetrically limits federal capacity to protect civil rights and the environment. Suppression (0.55) reflects that alternatives (broad federal regulation) are legally foreclosed by constitutional interpretation. Theater ratio (0.30) acknowledges that originalist methodology performs fidelity to history while selection of historical evidence can be strategic. Accessibility collapse (0.70) is high because overriding a Supreme Court constitutional interpretation requires amendment or a new doctrinal regime. Resistance (0.60) is substantial from federal legislators, civil rights groups, and progressive jurists. Measurements share a single time grid spanning the modern contested period.
 *
 * PERSPECTIVAL GAP:
 *   The state_governments seat experiences the constraint as protective sovereignty; the civil_rights_claimants seat experiences the same constraint as abandonment to local majorities. The federal_regulators seat sees lost coordination capacity; the narrow_originalist_jurists seat sees constitutional fidelity. The engine should compute divergent per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local businesses are beneficiaries (low d) because the constraint subsidizes their regulatory autonomy. Federal regulators and civil rights claimants are payers (high d) because the constraint extracts national enforcement capacity and protective federal law from them. Narrow originalist jurists sit near symmetric or moderate beneficiary: they do not collect material rents but derive professional authority from administering the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both coordination and extraction. The narrow reading is not a pure rope because civil rights claimants bear real costs when federal protection is removed. It is not a pure snare because the coordination functionâpreventing state trade barriersâis genuine and historically documented. The founding problem is corroborated by historians outside the beneficiary set, preventing a cover-story capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_relation,
    'This constraint is the narrow_originalist reading of the commerce_clause_scope kernel; siblings broad_effects_test and intermediate_channels produce different epsilon values and victim structures. How does the original-public-meaning method affect the structural classification relative to siblings?',
    'Compare historical linguistic evidence of ''commerce'' and ''regulate'' in 1787-1789 against the doctrinal requirements of sibling readings; assess whether the narrow reading is a distinct constraint or merely a metric re-framing.',
    'If historical evidence robustly supports the narrow reading, the classification as low-extraction coordination strengthens; if evidence is indeterminate, the narrow reading functions as a constructed constraint benefiting states, raising epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_relation, conceptual, 'Committer structure of the commerce clause kernel').

omega_variable(
    original_public_meaning_empirical_status,
    'Does the empirical historical record support the claim that ''commerce'' in 1787 was limited to trade crossing lines and ''regulate'' meant make regular rather than restrict?',
    'Corpus linguistics analysis of founding-era usage; archival research into ratification debates, state constitutional conventions, and colonial statutory language.',
    'If the empirical claim is weak, the narrow reading''s authority grounding shifts from lineage toward extraction (maintaining a state-favoring distribution against textual evidence), raising epsilon and potentially shifting computed type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_empirical_status, empirical, 'Empirical foundation of the narrow reading''s axioms').

omega_variable(
    state_autonomy_extraction_ambiguity,
    'Does state regulatory autonomy under this reading function as genuine decentralized experimentation, or as a cover for state-level extraction (e.g., Jim Crow labor regimes, environmental race-to-bottom)?',
    'Cross-state comparative analysis of regulatory outcomes for labor, environment, and civil rights in periods when the narrow reading was judicially dominant versus when broad federal power was exercised.',
    'If state autonomy systematically produces extraction from vulnerable populations, the coordination story is cover and the constraint shifts toward snare; if outcomes show genuine policy innovation without systematic exploitation, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_autonomy_extraction_ambiguity, empirical, 'Whether state autonomy is coordination or cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccsn_tr_t0, commerce_clause_scope__narrow_originalist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ccsn_tr_t6, commerce_clause_scope__narrow_originalist, theater_ratio, 6, 0.18).
narrative_ontology:measurement(ccsn_tr_t12, commerce_clause_scope__narrow_originalist, theater_ratio, 12, 0.22).
narrative_ontology:measurement(ccsn_tr_t18, commerce_clause_scope__narrow_originalist, theater_ratio, 18, 0.26).
narrative_ontology:measurement(ccsn_tr_t24, commerce_clause_scope__narrow_originalist, theater_ratio, 24, 0.29).
narrative_ontology:measurement(ccsn_tr_t30, commerce_clause_scope__narrow_originalist, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(ccsn_be_t0, commerce_clause_scope__narrow_originalist, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ccsn_be_t6, commerce_clause_scope__narrow_originalist, base_extractiveness, 6, 0.31).
narrative_ontology:measurement(ccsn_be_t12, commerce_clause_scope__narrow_originalist, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(ccsn_be_t18, commerce_clause_scope__narrow_originalist, base_extractiveness, 18, 0.35).
narrative_ontology:measurement(ccsn_be_t24, commerce_clause_scope__narrow_originalist, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(ccsn_be_t30, commerce_clause_scope__narrow_originalist, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ccsn_su_t0, commerce_clause_scope__narrow_originalist, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ccsn_su_t6, commerce_clause_scope__narrow_originalist, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(ccsn_su_t12, commerce_clause_scope__narrow_originalist, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(ccsn_su_t18, commerce_clause_scope__narrow_originalist, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(ccsn_su_t24, commerce_clause_scope__narrow_originalist, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(ccsn_su_t30, commerce_clause_scope__narrow_originalist, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the commerce_clause_scope kernel. The kernel decomposes into structurally distinct claims because the colloquial label 'Commerce Clause' conflates narrow originalist scope, intermediate channels doctrine, and broad effects-test scope. Each reading has different epsilon, beneficiaries, and victim sets. The narrow reading extracts from federal regulatory capacity; the broad reading extracts from state sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
