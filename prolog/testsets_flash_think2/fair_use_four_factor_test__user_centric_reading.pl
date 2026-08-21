% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use Four-Factor Test (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint story instantiates a 'user-centric' reading of the fair
 *   use four-factor test, viewing fair use as an affirmative right for users
 *   to access and build upon copyrighted works. This reading prioritizes
 *   public access, education, and cultural production, interpreting the four
 *   factors (purpose and character of the use, nature of the copyrighted
 *   work, amount and substantiality of the portion used, and effect of the
 *   use upon the potential market) to favor users over rights holders. The
 *   low extractiveness and suppression reflect this interpretation's intent
 *   to minimize barriers to use, while the moderate theater ratio
 *   acknowledges the inherent subjectivity and balancing act involved in
 *   applying the test.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.25).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.3).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, 'f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f').
narrative_ontology:cs_kernel_codification('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', fixed_text).
narrative_ontology:cs_authority_grounding('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', lineage).
narrative_ontology:cs_interpretation_layer_present('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f').
narrative_ontology:cs_reading_relation('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', foundational, public_access_is_primary_good).
narrative_ontology:cs_axiom_status(public_access_is_primary_good, holdable).
narrative_ontology:cs_axiom_grounding('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', public_access_is_primary_good, deontological).
narrative_ontology:cs_axiom('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', foundational, copyright_is_limited_monopoly).
narrative_ontology:cs_axiom_status(copyright_is_limited_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', copyright_is_limited_monopoly, conventional).
narrative_ontology:cs_reference_frame('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', public_domain_enrichment).
narrative_ontology:cs_drift_state('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', contemporary_digital_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('f8fea6a2-691e-465d-8ce4-53cb9fc4bb6f', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_producers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from broad access to copyrighted works for personal, educational, and non-commercial uses, enabling participation in cultural discourse and knowledge acquisition. Without fair use, access would be severely restricted by licensing fees or outright prohibitions.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_users, beneficiary,
    moderate, biographical, constrained, global).

% Relies on fair use to incorporate copyrighted materials into curricula, lectures, and research without prohibitive licensing costs, facilitating teaching and scholarship. Exit means significantly reduced pedagogical resources or increased institutional legal risk.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educators, beneficiary,
    moderate, biographical, constrained, national).

% Benefits from the ability to build upon, critique, parody, and transform existing copyrighted works to create new cultural products. Fair use provides a legal safe harbor for derivative creation, fostering innovation. Exit means stifled creativity and increased legal exposure.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, cultural_producers, beneficiary,
    moderate, biographical, constrained, global).

% Bears the cost of reduced exclusive control over their works and potentially diminished licensing revenue for uses deemed 'fair.' They often advocate for a narrower interpretation of fair use to maximize their property rights and compensation. Their exit options are limited to litigation or lobbying for legislative changes.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_holders, payer,
    powerful, generational, constrained, global).

% Interprets and applies the four-factor test in specific cases, shaping the boundaries of fair use. This reading emphasizes the public benefit and user rights in their balancing decisions. Their role is to adjudicate disputes and set precedent.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyzes judicial decisions, proposes theoretical frameworks, and critiques the application of fair use, often advocating for interpretations that align with public interest and cultural commons. They influence legal discourse but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the tension between copyright holders' exclusive rights and the public's interest in accessing and building upon creative works, ensuring a dynamic cultural commons while incentivizing creation.
% TRANSFER_FUNCTION: Transfers limited rights to use copyrighted material from copyright holders to users (public, educators, creators), reducing direct compensation to rights holders for such uses in favor of broader public access and cultural production.
% ABSENT_VOICES: While copyright holders are present in the discourse, this reading subordinates their arguments for absolute property rights. Stricter property rights advocates would argue for greater compensation and control, but their perspective is de-emphasized here.
% DISAPPEARANCE_RATIONALE: If fair use vanished, public access to copyrighted works for education, criticism, and new creation would be severely restricted, leading to a chilling effect on cultural production, academic discourse, and public engagement with creative works. The digital economy would be fundamentally reshaped.
% FOUNDING_PROBLEM: To prevent copyright from becoming an absolute monopoly that stifles subsequent creativity, education, and public discourse, by establishing a flexible doctrine that balances creator incentives with public benefit.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, educators, public interest groups, and many cultural producers consistently attest to the ongoing necessity of fair use to prevent copyright overreach, especially in the digital age. This corroboration comes from outside the primary benefiting parties (e.g., independent legal analysis, academic studies).
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because this reading aims to minimize the burden on users, allowing them to utilize copyrighted material without significant cost or permission. Suppression is low as it actively enables uses that might otherwise be prohibited. Theater ratio is moderate because while the four-factor test is a genuine legal mechanism, its application often involves subjective judicial balancing, leading to some performative aspects in legal arguments. Resistance is moderate as copyright holders consistently challenge this broad interpretation. Accessibility collapse is moderate, as fair use opens up access but doesn't make all content universally free.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public users, educators, and cultural producers, this reading of fair use is a vital enabling mechanism, a 'rope' that facilitates their activities. For copyright holders, however, the same structure is perceived as an erosion of their property rights, potentially a 'snare' that diminishes their control and revenue. The engine will compute these divergent classifications based on the declared structural relationships and directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Public users, educators, and cultural producers are beneficiaries (low directionality) as the constraint enables their activities and reduces their costs. Copyright holders are victims/payers (high directionality) as they bear the cost of reduced control and potential revenue. Courts act as agenda-setters, interpreting and enforcing the balance. The low extractiveness for users translates to a subsidy, while for rights holders, it represents a loss of potential extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This user-centric reading prevents mislabeling genuine public benefit as extraction. By emphasizing the coordination function of balancing rights for societal good, it resists the narrative that any unauthorized use is inherently extractive. The 'live' status of the founding problem (balancing rights) further supports its ongoing relevance, though the specific interpretation is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_factor_subjectivity,
    'How consistently and predictably is the four-factor test applied across different courts and cases, given its inherent subjectivity?',
    'Empirical analysis of judicial decisions, identifying patterns and divergences in how each factor is weighed in user-centric cases. Development of clearer guidelines or presumptions for specific types of uses.',
    'Higher consistency would strengthen the ''rope'' classification by reducing uncertainty for users; high inconsistency would increase the ''theater_ratio'' and potentially shift towards a ''tangled_rope'' due to unpredictable enforcement costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_factor_subjectivity, empirical, 'The degree of judicial subjectivity in applying the fair use test.').

omega_variable(
    creator_incentive_impact,
    'What is the actual empirical impact of a broad, user-centric fair use doctrine on creators'' incentives to produce new works?',
    'Longitudinal economic studies comparing creative output and revenue in jurisdictions with different fair use interpretations, or before/after significant legal shifts.',
    'If broad fair use demonstrably reduces creation, it would challenge the ''rope'' classification by revealing a hidden cost to the ''beneficiaries'' (public) in the long run, potentially increasing the effective extraction from creators. If creation remains robust, it reinforces the current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_incentive_impact, empirical, 'Empirical trade-off between user rights and creator incentives.').

omega_variable(
    property_rights_vs_public_good,
    'Is copyright fundamentally a property right to be maximized, or a limited monopoly designed to serve a broader public good?',
    'This is a conceptual/preference question, resolvable through philosophical and policy debates, legislative action, or shifts in societal values regarding intellectual property.',
    'Resolution towards a property-centric view would fundamentally reframe fair use as an ''extraction'' from rights holders, increasing its perceived extractiveness. Resolution towards a public-good view reinforces the current ''rope'' classification and low extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_rights_vs_public_good, conceptual, 'The foundational conceptual framing of copyright law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1976, 0.3).
narrative_ontology:measurement(fair_tr_t1988, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1988, 0.35).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(fair_tr_t2012, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2012, 0.42).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.3).
narrative_ontology:measurement(fair_be_t1988, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1988, 0.28).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement(fair_be_t2012, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2012, 0.24).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1976, 0.35).
narrative_ontology:measurement(fair_su_t1988, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1988, 0.32).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(fair_su_t2012, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2012, 0.28).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, digital_rights_management).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, dmca_safe_harbors).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the 'fair_use_four_factor_test' kernel, each with different structural properties and classifications. This reading emphasizes user rights and public access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
