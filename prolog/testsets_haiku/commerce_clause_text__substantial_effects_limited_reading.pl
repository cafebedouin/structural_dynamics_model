% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause Substantial Effects Doctrine with Economic/Noneconomic Boundary
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The Commerce Clause grants Congress power to regulate interstate
 *   commerce. The substantial effects doctrine allows federal regulation of
 *   intrastate economic activity when it substantially affects interstate
 *   commerce, but claims to preserve state police power by excluding
 *   noneconomic regulation (health, safety, morals) from federal reach via
 *   the Commerce Clause. This constraint is ONE READING of a contested kernel
 *   (the Commerce Clause text itself). The
 *   substantial_effects_limited_reading carves out a middle ground: federal
 *   authority is broad over genuinely economic intrastate activity, but the
 *   economic/noneconomic distinction serves as a boundary supposed to prevent
 *   federal police-power assertion disguised as commerce regulation. Three
 *   sibling readings contest this: the expansive_federal_reading treats all
 *   substantial-effects activity as federal domain; the
 *   originalist_narrow_reading confines interstate commerce to trade crossing
 *   borders and instrumentalities of interstate movement. The
 *   substantial_effects_limited_reading is instantiated here as a constraint
 *   because it describes a functioning interpretive doctrine that allocates
 *   authority, requires enforcement (litigation to police the boundary), and
 *   produces asymmetric extraction (state regulatory autonomy lost; federal
 *   authority gained; regulated actors face dual compliance). The
 *   constraint's extractiveness is moderate-high because the doctrine grants
 *   broad federal reach while the economic/noneconomic boundary is internally
 *   contested, creating opportunities for category manipulation and
 *   litigation costs.
 *
 * KEY AGENTS:
 *   - Supreme Court: enforces the economic/noneconomic boundary via case decisions; stewards the doctrine
 *   - Federal regulators: beneficiary — gain authority over intrastate economic activity; gain legitimacy from apparent constraint
 *   - National market integrationists: beneficiary — gain uniform regulation, collective-action solutions
 *   - States: victim — lose regulatory autonomy over intrastate economic affairs; police power nominally preserved but practically constrained
 *   - Regulated intrastate actors: victim — face federal regulation if classified as economic; litigation costs to contest classification
 *   - Originalist jurists & federalism advocates: excluded — cannot authoritative participate; remain a live alternative jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.58).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.62).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause Substantial Effects Doctrine with Economic/Noneconomic Boundary").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '49e08424-957c-4e68-a9b4-6dbdbf54ef8c').
narrative_ontology:cs_kernel_codification('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', fixed_text).
narrative_ontology:cs_authority_grounding('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', lineage).
narrative_ontology:cs_interpretation_layer_present('49e08424-957c-4e68-a9b4-6dbdbf54ef8c').
narrative_ontology:cs_reading_relation('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', foundational, genuine_economic_activity_federally_reachable).
narrative_ontology:cs_axiom_status(genuine_economic_activity_federally_reachable, holdable).
narrative_ontology:cs_axiom_grounding('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', genuine_economic_activity_federally_reachable, empirically_contingent).
narrative_ontology:cs_axiom('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', foundational, noneconomic_regulation_state_reserved).
narrative_ontology:cs_axiom_status(noneconomic_regulation_state_reserved, holdable).
narrative_ontology:cs_axiom_grounding('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', noneconomic_regulation_state_reserved, deontological).
narrative_ontology:cs_reference_frame('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', post_1937_integrated_commerce_settlement).
narrative_ontology:cs_drift_state('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', contemporary_originalist_insurgency, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('49e08424-957c-4e68-a9b4-6dbdbf54ef8c', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_economic_integrationists).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_regulatory_autonomy).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, regulated_intrastate_actors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.58) reflects the doctrine's grant of substantial federal reach while the economic/noneconomic boundary appears to constrain it. The boundary is the locus of extraction: it is claimed as a constraint but applied expansively (what counts as genuinely economic is settled via litigation favoring federal reach). Theater ratio (0.41) increases over time because the doctrine's performative function grows: the boundary policing appears to protect federalism but has become ritualized—the Court rarely strikes down federal commerce regulation on the grounds that it is noneconomic or lacks substantial effects (post-Lopez and Morrison, two rare boundary cases, the boundary reasserted itself as permissive). Suppression rises from 0.45 to 0.62 because maintaining the doctrine's coherence requires active judicial policing: the economic/noneconomic distinction must be enforced through precedent-setting, and alternative readings (expansive, originalist) must be contained. All measurements are on a single shared time grid (1937–2024) at six points spanning the doctrine's post-New Deal development.
 *
 * PERSPECTIVAL GAP:
 *   The federal-regulator and state seats should compute very differently. From the federal seat, the doctrine is a necessary, modest expansion of enumerated power to reach genuine economic integration; restraint is visible in the economic/noneconomic boundary, and coordination problems justify the reach. From the state seat, the doctrine is a Tenth Amendment erosion disguised as boundary-policing; the economic/noneconomic line is incoherent (what counts as economic is determined by federal courts favoring federal reach) and theater obscures preemption. The engine computes these per-seat; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators sit at the beneficiary end (d near 0.0): they gain authority and legitimacy without needing to defend police-power assertions. National integrationists sit as moderate beneficiaries (d ~0.2–0.3): they gain coordination solutions but depend on federal regulatory will. States occupy the victim end (d ~0.75–0.85): they lose autonomy over intrastate economic affairs; the Tenth Amendment police-power reserve is nominally preserved but substantially eroded. Regulated intrastate actors sit at high-target end (d ~0.80): they face federal compliance costs and litigation risk without the exit options of states (cannot amend the Constitution; must litigate or comply). The Supreme Court stewards the doctrine and could revise the boundary, but its institutional position is to apply law as settled, not to question the foundational reading. No directionality overrides are necessary: the structural relationships derive cleanly from the beneficiary/victim declarations and exit constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (economic integration made pure intrastate regulation a fiction post-1937) is contested but widely acknowledged. The doctrine's function is to bridge constitutional contradiction: allow federal reach into integrated markets while preserving Tenth Amendment federalism language. The constraint is classified as tangled_rope (not snare) because there is a genuine coordination function: uniform national regulation of interstate-affecting activity solves real collective-action problems (race-to-bottom, supply-chain externalities). It is not pure extraction because the beneficiaries (federal regulators, national integrationists) are not solely extracting rent; they are solving coordination failures that states cannot. However, the mechanism is asymmetric: states lose discretion unilaterally; regulated actors face compliance costs and litigation uncertainty; the Supreme Court maintains the boundary through active enforcement. The economic/noneconomic distinction is the locus of both coordination (distinguishes genuine commerce from police-power overreach) and extraction (the boundary is applied expansively, favoring federal reach). This hybrid structure—real coordination function + asymmetric allocation of the coordination's benefits + active enforcement required to maintain the boundary against challenge—defines tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_coherence,
    'Is the economic/noneconomic distinction a coherent, text-grounded boundary, or is it a policy-driven category whose membership is determined by federal courts favoring federal reach?',
    'Systematic analysis of boundary cases (López, Morrison, Gonzalez, contemporary litigation): do courts apply a consistent principled rule, or does the boundary shift to accommodate federal authority? Examination of what gets classified as genuinely economic when federal interest is high vs. low.',
    'If incoherent and policy-driven, the doctrine''s extractiveness and theater components are higher than authored (the boundary policing is performance, not constraint). If coherent, the doctrine is closer to a genuine coordination mechanism with negotiated constraints. This resolves the doctrine''s legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_coherence, empirical, 'Whether the economic/noneconomic boundary is principled or malleable.').

omega_variable(
    substantial_effects_manipulation,
    'Can the substantial effects test be manipulated to reach noneconomic regulation by reclassifying the regulation''s purpose as economic (e.g., claiming a gun-control law regulates ''economic activity'' because possession affects insurance and commerce)?',
    'Examination of post-López jurisprudence: did López and Morrison durably protect noneconomic regulation from federal Commerce Clause reach, or have lower courts found methods to extend substantial effects language into noneconomic domains?',
    'If manipulation is possible, the doctrine provides weaker real constraint than claimed, and extraction is higher. If the boundary holds, the doctrine''s coordination function is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantial_effects_manipulation, empirical, 'Whether the boundary actually protects noneconomic regulation from federal reach.').

omega_variable(
    federalism_doctrine_vs_text_divergence,
    'Does the substantial effects reading faithfully represent the original public meaning of the Commerce Clause text, or is it a mid-20th-century accommodation that privileges pragmatic integration over textual fidelity?',
    'Originalist textual analysis of the Commerce Clause against the substantial effects doctrine. Comparison of originalist scholarship and dissenting opinions (Alito, Thomas) against the doctrine''s foundational premises.',
    'If text-divergent, this reading is a constructed constraint benefiting federal authority and integration, not an inevitable reading of constitutional law. It is one choice among live alternatives (expansive, originalist), not the only defensible reading. This supports the omega''s existence and the three-reading kernel decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalism_doctrine_vs_text_divergence, conceptual, 'Whether the doctrine''s reading is texturally grounded or judicially constructed.').

omega_variable(
    kernel_reading_alternative_dominance,
    'Within a single contemporary decision-making seat (e.g., the current Supreme Court), can all three readings (expansive, originalist, substantial_effects_limited) coexist, or does the institutional structure force selection of one dominant reading?',
    'Analysis of majority coalitions on the current Court and recent constitutional litigation. Do judges apply the readings contextually (substantial effects for economic regulation, originalism for police power), or is there a pressure toward one unified reading?',
    'If coexistence is possible, the three readings genuinely coexist (per the `coexists_with` relation). If institutional structure forces selection, the dominant reading will tend to foreclose or influence others over time. This affects the Long Maginot estimate for the doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_dominance, conceptual, 'Whether the three readings can coexist in practice or whether one tends to dominate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1937, 0.25).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1964, 0.32).
narrative_ontology:measurement(comm_tr_t1985, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1937, 0.42).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1964, 0.52).
narrative_ontology:measurement(comm_be_t1985, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1937, 0.45).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1964, 0.55).
narrative_ontology:measurement(comm_su_t1985, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__substantial_effects_limited_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, dormant_commerce_clause_preemption).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, federalism_structural_reserve).

% DUAL FORMULATION NOTE:
% The commerce_clause_text kernel decomposes into three constraint stories, one per reading. Each reading instantiates a different constraint with its own ε, beneficiary/victim structure, and type. The substantial_effects_limited_reading is the operative post-1937 doctrine. The expansive_federal_reading represents the outer boundary of federal reach (all substantially affecting activity = federal domain). The originalist_narrow_reading represents the alternative interpretation that would constrain federal reach most severely. These three stories form a constraint family linked by network.affects_constraints. The substantial_effects_limited_reading influences the other two because it is institutionally dominant and shapes what the alternatives must argue against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
