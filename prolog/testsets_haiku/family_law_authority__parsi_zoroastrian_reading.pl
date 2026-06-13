% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Marriage Community Preservation under Zoroastrian Religious Law (Parsi Reading)
 *   domain: religious/political/social
 *
 * SUMMARY:
 *   This constraint is ONE READING of a contested kernel:
 *   family_law_authority. The Parsi Zoroastrian reading frames marriage as a
 *   community-preserving institution governed by religious law, with endogamy
 *   as the primary mechanism and priestly authority as the enforcer. This
 *   reading competes with four sibling readings (Christian canonical, Hindu
 *   dharmashastra, Muslim shariat, and secular contractual) that ground the
 *   family-law authority kernel differently. The Parsi reading's distinctive
 *   structural delta is the explicit fusion of community preservation with
 *   marital constraint: the endogamy rule is justified not as personal
 *   salvation (Christian), not as dharmic duty (Hindu), not as contractual
 *   arrangement (secular), but as the survival mechanism for a small diaspora
 *   religion. The reading is historically grounded in the Parsi experience of
 *   numerical vulnerability and assimilative pressure after the Islamic
 *   conquest of Persia and subsequent diaspora to India.
 *
 * KEY AGENTS:
 *   - Parsi priestly authority (Dastur and high priests): sets doctrine, controls ritual validity, enforces membership standards
 *   - Parsi community collective: framed as beneficiary of preservation, but also enforces through social sanction and participation exclusion
 *   - Parsi individuals seeking intermarriage: bear the constraint directly, face identity-locked suppression (religious identity fused with family identity)
 *   - Non-Parsi spouses: systematically excluded from ritual participation and governance; have no seat in the rule that excludes them
 *   - Reform-minded Parsis: challenge the rule as boundary-maintenance rather than preservation necessity, excluded from priestly governance but exert organized pressure
 *   - Diaspora Parsi individuals: experience the constraint differently due to weaker enforcement machinery in pluralistic host societies; higher exit optionality but internalized identity-lock
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.68).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Marriage Community Preservation under Zoroastrian Religious Law (Parsi Reading)").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "religious/political/social").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7').
narrative_ontology:cs_kernel_codification('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', fixed_text).
narrative_ontology:cs_authority_grounding('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', lineage).
narrative_ontology:cs_interpretation_layer_present('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7').
narrative_ontology:cs_reading_relation('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', foundational, community_descent_determines_membership).
narrative_ontology:cs_axiom_status(community_descent_determines_membership, holdable).
narrative_ontology:cs_axiom_grounding('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', community_descent_determines_membership, deontological).
narrative_ontology:cs_axiom('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', foundational, priestly_authority_preserves_doctrinal_purity).
narrative_ontology:cs_axiom_status(priestly_authority_preserves_doctrinal_purity, holdable).
narrative_ontology:cs_axiom_grounding('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', priestly_authority_preserves_doctrinal_purity, deontological).
narrative_ontology:cs_reference_frame('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', avestan_priestly_transmission).
narrative_ontology:cs_drift_state('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', contemporary_diaspora_legal_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e4b8f63-6dce-40b6-8b78-1aec9dafd7d7', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_priestly_authority).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_community_collective).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_individuals_seeking_intermarriage).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as TANGLED ROPE, not pure snare or rope, because it combines genuine coordination (small-community preservation in diaspora conditions) with asymmetric extraction (authority consolidation in priesthood, autonomy transfer from individuals to collective, burden concentration on payers). Extractiveness is moderate-high (0.68 final) because the rule persistently transfers individual reproductive autonomy to collective gatekeepers, yet it is not pure extraction because the coordinated outcome (community continuity) is real and valued by many Parsis. Suppression is high (0.72) because enforcement depends heavily on exclusion machinery: excommunication, ritual participation bars, status demotion, and family pressure create structural barriers to exit that persist even when legal alternatives exist. Theater ratio is moderate (0.42): priestly justifications remain coded as theological necessity (preservation of Avestan knowledge, ritual purity), but the functional mechanism is increasingly recognizable as boundary maintenance and authority consolidation. The measurement series shows extractiveness plateauing around midpoint and suppression stabilizing, indicating the constraint has settled into a steady enforcement state rather than ratcheting upward. Theater remains below 0.5, indicating the functional preservation logic is not yet wholly displaced by performative maintenance, but the trajectory is upward. Time grid is shared and aligned: all three metrics are authored at all eight time points.
 *
 * PERSPECTIVAL GAP:
 *   The priestly seat and the constrained-payer seats experience this constraint radically differently. From the priestly perspective, the rule is governance necessary for community survival, and the authority it consolidates is legitimate by religious transmission. From the payer seats, particularly parsi_individuals_seeking_intermarriage and non_parsi_spouses, the rule appears as enforced extraction: loss of reproductive choice, identity-based exclusion, and suppression through gatekeeping machinery. The engine computes d separately for each power atom, so priestly-institutional d will be near 0.0 (beneficiary, arbitrage-exit-capable), while payer d will be near 1.0 (trapped or identity-locked, bearing extraction). This divergence IS the measurement: a constraint whose claim diverges from computed seat classifications reveals misalignment between the constraint's presented function and its actual operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Priestly authority (institutional, arbitrage-exit) derives d near 0.0 (full beneficiary): they set rules, collect authority, and face no meaningful suppression because the constraint's enforcement is their prerogative. Parsi community collective (organized, identity-locked) has dual positioning: they are framed as beneficiary (community is 'preserved'), but many individual members are payers. This ambiguity reflects the reading's structure itself: the collective benefit is real but purchased at individual cost. Parsi individuals seeking intermarriage (moderate power, identity-locked exit) derive d near 1.0 (full target): they face systematic suppression, constricted choice set, and cannot exit without identity cost. Non-Parsi spouses (moderate, trapped exit) also derive d near 1.0: they are excluded by rules they did not consent to and cannot change. Reform-minded Parsis (powerful, but excluded from governance) face constrained but not trapped exit: they can exit the traditional reading by joining reform movements or leaving the community, but at organizational and identity cost. The directionality derivation follows from beneficiary/victim declarations plus exit modulation: no overrides are needed because structural data drives the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested between traditional and reform readings. Orthodox priesthood maintains the founding problem is LIVE: diaspora conditions, assimilative pressure, declining Parsi demographics mean endogamy is still necessary for survival. Reform and secular Parsis argue the founding problem is DEAD: external persecution ceased, legal equality in host states is achieved, and the rule now functions as boundary-maintenance and authority consolidation rather than preservation necessity. The constraint's persistence despite the disputed founding problem suggests institutional inertia and theater: if the problem were universally recognized as live, we would expect lower theater_ratio; if universally recognized as dead, we would expect higher theater_ratio and lower extractiveness (pure snare). The moderate theater and moderate-high extraction suggest the reading is genuinely contested — some Parsis see it as necessary coordination, others as obsolete extraction. This is NOT mandatrophy resolved in the schema sense (the rule has not been formally declared obsolete), but the divergence between founding-problem-status and measured theater indicates the condition the mandatrophy warning would flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_live_or_dead,
    'Is the founding problem (small diaspora community facing assimilative pressure and extinction through intermarriage) still live, or has it been superseded by changed historical conditions (legal equality in host states, economic security, voluntary religious affiliation)?',
    'Demographic analysis: compare Parsi community size, intermarriage rates, and intergenerational religious transmission in contexts with strong enforcement (traditional communities in India) vs. weak enforcement (diaspora in North America/Europe). If transmission rates are comparable, the problem is largely solved and rule persistence indicates inertia rather than necessity. If rates diverge sharply, the problem remains live.',
    'If live, the rule approaches Tangled Rope with genuine coordination value; if dead, it drifts toward Snare (extraction masked by obsolete preservation narrative). The classification hinges on whether the measured suppression and extraction serve a real coordination function or only boundary maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_or_dead, empirical, 'Genealogy of the founding problem: has the historical condition it was built to address persisted, been solved, or been transformed into something else?').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (legal exclusion from ritual, institutional enforcement, family pressure within the community) or internalized (individuals have fused their identity with the religious boundary such that exit itself is psychologically experienced as apostasy or self-betrayal)?',
    'Post-exit trajectories: track Parsi individuals who marry outside and their subsequent relationship to suppression. If suppression persists after structural enforcement is escaped (diaspora context), it indicates internalized mechanism. If suppression drops sharply, it is primarily structural.',
    'Internalized suppression is more robust to legal challenges and creates stickier identity-lock; it suggests the constraint''s persistence depends on identity fusion, not just gatekeeping machinery. Structural suppression is more amenable to legal remedy. The distinction affects remedial strategy and classification stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression operates through external institutional machinery or through internalized identity fusion.').

omega_variable(
    priestly_authority_lineage_claim,
    'Is the priesthood''s claim to authority grounded in received transmission of Avestan doctrine (as the reading asserts) or is it a constructed authority that developed through institutional practice and is now defended by appeals to ancient lineage?',
    'Textual and historical analysis: examine Avestan sources for explicit endogamy commands (likely sparse or absent), compare with later priestly texts and community practice (increasingly elaborate), and assess whether authority developed incrementally through practice rather than from foundational text.',
    'If authority is received-lineage-grounded, the reading''s epistemic structure is secure (authority rooted in external source); if constructed-and-defended, the reading''s legitimacy claim is weaker and the constraint drifts toward pure extraction (authority asserting its own necessity). This maps to the cs_structure authority_grounding field.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(priestly_authority_lineage_claim, empirical, 'Is priestly authority grounded in received transmission or constructed through institutional practice?').

omega_variable(
    endogamy_necessity_vs_identity_boundary,
    'Does endogamy function as a mechanism that enables the coordination outcome (community preservation through ritual practice and doctrinal transmission), or is the coordination outcome achievable through other mechanisms and endogamy functions primarily as identity boundary maintenance?',
    'Comparative case: examine mixed-descent Parsis (non-Zoroastrian mother or father) and their offspring''s religious practice and community participation. If they show equivalent transmission and practice, endogamy is not functionally necessary for preservation. If they show sharply reduced transmission, endogamy is functionally tied to the coordination outcome.',
    'If endogamy is necessary, the constraint is Tangled Rope (genuine coordination with extraction cost). If it is a proxy for boundary maintenance, the constraint is Snare (extraction defended by obsolete coordination story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_necessity_vs_identity_boundary, empirical, 'Is endogamy the mechanism enabling community preservation, or is it identity boundary maintenance defended through preservation rhetoric?').

omega_variable(
    reform_reading_foreclosure,
    'Does the Orthodox priestly reading functionally foreclose the Reform reading within a single framework, or do the two readings coexist as live alternatives held by different Parsi factions?',
    'Organizational structure: the Zoroastrian Reform community operates separate institutions (fire temples, courts, priesthoods) in some jurisdictions. If both readings maintain separate institutional structures and neither can adjudicate claims against the other, they coexist; if Orthodox institutions can formally exclude Reform members and render Reform practice invalid, Orthodox forecloses Reform within the community framework.',
    'If foreclosing, the reading has stronger authority legitimacy but faces greater internal conflict; if coexisting, the reading is contested but more stable institutionally. Affects the cs_structure reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_reading_foreclosure, conceptual, 'Whether the Orthodox and Reform readings of Zoroastrian family law are logically incompatible or institutionally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(fami_tr_t35, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(fami_be_t5, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(fami_be_t10, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(fami_be_t15, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(fami_be_t20, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(fami_be_t25, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(fami_be_t30, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fami_be_t35, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fami_su_t5, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(fami_su_t10, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(fami_su_t15, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(fami_su_t20, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(fami_su_t25, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(fami_su_t30, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(fami_su_t35, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the family_law_authority kernel. It is structurally distinct from sibling readings because it grounds authority in Zoroastrian priestly lineage and justifies enforcement through diaspora-community-preservation logic. The ε value (0.68 extractiveness, 0.72 suppression) is specific to THIS reading and would differ substantially for sibling readings. The family_law_authority kernel is instantiated in five separate constraint files (this one plus four siblings). Each reading has independent beneficiary/victim structure and directionality derivation. The network links indicate which other readings share the kernel; they are not causal dependencies but structural siblings that compete for legitimacy in the same domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
