% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Orthographic Legitimacy (Instrumentalist Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the instrumentalist reading of orthographic
 *   legitimacy, where the choice and enforcement of a national script are
 *   justified by their utility in maximizing literacy rates and
 *   administrative efficiency. This reading views orthography as a pragmatic
 *   tool for state-building and modernization, rather than a marker of
 *   historical continuity or civilizational alignment. It often involves a
 *   break from older, more complex scripts in favor of simplified, often
 *   Latin-based, alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.45).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.7).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Orthographic Legitimacy (Instrumentalist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, '23e9413f-519d-4de9-a2c1-1a60758aff26').
narrative_ontology:cs_kernel_codification('23e9413f-519d-4de9-a2c1-1a60758aff26', formalized).
narrative_ontology:cs_authority_grounding('23e9413f-519d-4de9-a2c1-1a60758aff26', extraction).
narrative_ontology:cs_interpretation_layer_present('23e9413f-519d-4de9-a2c1-1a60758aff26').
narrative_ontology:cs_reading_relation('23e9413f-519d-4de9-a2c1-1a60758aff26', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_reading_relation('23e9413f-519d-4de9-a2c1-1a60758aff26', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_axiom('23e9413f-519d-4de9-a2c1-1a60758aff26', foundational, literacy_is_primary_good).
narrative_ontology:cs_axiom_status(literacy_is_primary_good, holdable).
narrative_ontology:cs_axiom_grounding('23e9413f-519d-4de9-a2c1-1a60758aff26', literacy_is_primary_good, instrumental).
narrative_ontology:cs_axiom('23e9413f-519d-4de9-a2c1-1a60758aff26', foundational, administrative_efficiency_is_state_goal).
narrative_ontology:cs_axiom_status(administrative_efficiency_is_state_goal, holdable).
narrative_ontology:cs_axiom_grounding('23e9413f-519d-4de9-a2c1-1a60758aff26', administrative_efficiency_is_state_goal, instrumental).
narrative_ontology:cs_reference_frame('23e9413f-519d-4de9-a2c1-1a60758aff26', efficient_modern_state).
narrative_ontology:cs_drift_state('23e9413f-519d-4de9-a2c1-1a60758aff26', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('23e9413f-519d-4de9-a2c1-1a60758aff26', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, linguistic_reformers).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, traditional_religious_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, international_development_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary driver and enforcer of orthographic reform, benefiting from increased administrative efficiency and a more unified national communication system. They implement the new script through education and official documents.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_bureaucracy, agenda_setter,
    institutional, generational, mobile, national).

% Gains access to education, state services, and modern literature through the simplified, standardized orthography. Their literacy rates increase, but they are dependent on the new system and lack access to older texts.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    powerless, biographical, constrained, national).

% Their cultural capital, built on mastery of the older, often Arabic-based script, is devalued. They face a loss of status and influence as the new orthography becomes dominant, and their skills are no longer central to state administration or modern education.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite, payer,
    organized, biographical, identity_locked, national).

% Advocates for and implements the orthographic changes, seeing them as essential for national progress and modernization. They gain professional standing and influence through the successful adoption of their reforms.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, linguistic_reformers, beneficiary,
    organized, generational, mobile, national).

% Supports literacy initiatives and administrative modernization in developing nations, aligning with the instrumentalist goals of orthographic reform. They provide funding and expertise, validating the reform's utility-driven rationale.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, international_development_agencies, beneficiary,
    institutional, generational, arbitrage, global).

% Often tied to the preservation of older scripts (e.g., Arabic for religious texts) and resist reforms that devalue this tradition. They are excluded from the decision-making process and their concerns are sidelined in favor of secular, utilitarian goals.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, traditional_religious_institutions, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To standardize written communication, increase national literacy rates, and streamline state administration by adopting a more phonetically consistent and accessible orthography.
% TRANSFER_FUNCTION: Transfers linguistic capital, administrative power, and educational access from an elite group proficient in an older, complex script to a broader population and a modernizing state bureaucracy, at the cost of devaluing existing traditional skills.
% ABSENT_VOICES: Traditional religious institutions and cultural conservatives, who would argue for the preservation of historical and religious script traditions, are excluded from the instrumentalist discourse that prioritizes utility over continuity.
% DISAPPEARANCE_RATIONALE: If the instrumentalist justification for orthographic legitimacy vanished, the state's authority to enforce a single, modern script would collapse. This would lead to fragmentation in education, administration, and national identity, forcing a fundamental reorganization of the public sphere.
% FOUNDING_PROBLEM: Low national literacy rates, administrative inefficiencies due to complex or multiple writing systems, and a perceived barrier to modernization and integration into global systems.
% FOUNDING_PROBLEM_CORROBORATION: International literacy statistics, economic development reports, and historical accounts of administrative challenges in nations undergoing such reforms corroborate the existence and persistence of these problems, supporting the instrumentalist rationale from outside the benefiting state apparatus.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while the new orthography genuinely benefits a large segment of the population by increasing literacy, it simultaneously devalues the linguistic capital of those proficient in the older script. Suppression (0.70) is high because the state actively enforces the new orthography through education, administration, and often by banning or discouraging the use of older scripts in public life. The theater ratio (0.10) is low, as the stated goals of literacy and efficiency are genuinely pursued and largely achieved, making the constraint functional rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   The newly literate population and state bureaucracy experience this as a beneficial coordination mechanism, opening new avenues for participation and governance. In contrast, the Arabic-literate elite and traditional religious institutions experience it as a coercive extraction, as their skills and cultural heritage are marginalized. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The state bureaucracy and linguistic reformers are clear beneficiaries, gaining efficiency and influence. The newly literate population also benefits significantly from increased access. The Arabic-literate elite and traditional religious institutions are victims, bearing the cost of devalued skills and cultural marginalization. International development agencies are indirect beneficiaries, as the reform aligns with their goals.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the victims) or a Snare (which would ignore the genuine coordination benefits of increased literacy and efficiency). The constraint's mandate (literacy, efficiency) remains live, but its implementation involves an unavoidable, asymmetric transfer of linguistic capital, requiring active enforcement to maintain against resistance from those whose skills are devalued.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utility_vs_identity_legitimacy,
    'Is orthographic legitimacy fundamentally about maximizing utility (literacy, efficiency) or about preserving cultural identity and historical continuity?',
    'Analysis of long-term societal outcomes in nations that adopted instrumentalist reforms versus those that prioritized continuity, assessing social cohesion, cultural resilience, and economic development.',
    'If identity/continuity proves more foundational for long-term stability, the instrumentalist reading''s legitimacy would be undermined, potentially reclassifying it as more extractive or even a Snare if the utility claims are found to be cover for cultural erasure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utility_vs_identity_legitimacy, conceptual, 'The core conceptual tension between utilitarian and identity-based justifications for orthographic choice.').

omega_variable(
    devaluation_as_necessary_cost,
    'Is the devaluing of existing linguistic capital (e.g., Arabic literacy) a necessary and justified cost of achieving widespread literacy and administrative efficiency, or an unjust extraction?',
    'Ethical and economic analysis comparing the aggregate societal gains from reform against the specific losses incurred by the affected elite, considering alternative reform pathways that might have mitigated these losses.',
    'If deemed an unjust extraction, the constraint''s effective extractiveness would be higher, pushing it further towards a Snare classification for the affected groups. If deemed a necessary cost, the Tangled Rope classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devaluation_as_necessary_cost, preference, 'Whether the costs borne by the old elite are ethically justifiable in the pursuit of instrumentalist goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 1928, 1978).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement(orth_tr_t1938, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1938, 0.08).
narrative_ontology:measurement(orth_tr_t1948, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1948, 0.07).
narrative_ontology:measurement(orth_tr_t1958, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1958, 0.08).
narrative_ontology:measurement(orth_tr_t1968, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1968, 0.09).
narrative_ontology:measurement(orth_tr_t1978, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1978, 0.1).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1928, 0.35).
narrative_ontology:measurement(orth_be_t1938, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1938, 0.4).
narrative_ontology:measurement(orth_be_t1948, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1948, 0.43).
narrative_ontology:measurement(orth_be_t1958, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1958, 0.45).
narrative_ontology:measurement(orth_be_t1968, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1968, 0.46).
narrative_ontology:measurement(orth_be_t1978, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1978, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1928, 0.6).
narrative_ontology:measurement(orth_su_t1938, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1938, 0.7).
narrative_ontology:measurement(orth_su_t1948, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(orth_su_t1958, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1958, 0.72).
narrative_ontology:measurement(orth_su_t1968, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1968, 0.68).
narrative_ontology:measurement(orth_su_t1978, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1978, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'orthographic_legitimacy_kernel'. This instrumentalist reading focuses on utility (literacy, efficiency), while the modernist reading emphasizes alignment with Western modernity, and the continuity reading prioritizes historical and religious tradition. Each reading instantiates a distinct constraint with different ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
