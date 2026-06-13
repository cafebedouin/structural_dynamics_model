% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support as Universality Paradox
 *   domain: political_economy/welfare_state
 *
 * SUMMARY:
 *   Unconditional income support (UIS) has become a politically ambiguous
 *   policy vehicle that attracts cross-ideological coalition support by
 *   deferring the fundamental normative choice between
 *   universality-as-dignity (left reading: decommodification, unconditional
 *   floor, protection against market shocks) and universality-as-efficiency
 *   (right reading: elimination of means-testing bureaucracy, work-incentive
 *   preservation through taxing-back, fiscal discipline). The CLAIM is
 *   tangled_rope: the constraint entangles incompatible normative commitments
 *   in a single policy vehicle, enabled by the taxing-back architecture's
 *   formal equivalence to multiple designs. The METRICS describe a constraint
 *   whose extractiveness is moderate but rising over time (theater_ratio
 *   climbs from 0.35 to 0.70 over 40 years), indicating increasing gap
 *   between the policy's rhetorical framing (unconditional, universal,
 *   dignity-affirming) and its actual operation (conditional on income, taxed
 *   back aggressively, implementing covert work requirements via benefit
 *   design). This is the universality_paradox_reading: one reading of the
 *   contested unconditional_income_support kernel, distinguished from the
 *   freedom_floor_reading and dependency_trap_reading by its focus on
 *   political ambiguity as the constraint's actual function, not as a
 *   communication problem to be solved.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.42).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.38).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support as Universality Paradox").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/welfare_state").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, 'e3c91c28-dc5e-491a-8fc3-6a8f90c0b542').
narrative_ontology:cs_kernel_codification('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', distributed).
narrative_ontology:cs_authority_grounding('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', extraction).
narrative_ontology:cs_interpretation_layer_present('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542').
narrative_ontology:cs_reading_relation('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', foundational, universality_as_ambiguity_management).
narrative_ontology:cs_axiom_status(universality_as_ambiguity_management, holdable).
narrative_ontology:cs_axiom_grounding('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', universality_as_ambiguity_management, deontological).
narrative_ontology:cs_axiom('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', foundational, taxback_equivalence_preserves_coalition_choice).
narrative_ontology:cs_axiom_status(taxback_equivalence_preserves_coalition_choice, holdable).
narrative_ontology:cs_axiom_grounding('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', taxback_equivalence_preserves_coalition_choice, empirically_contingent).
narrative_ontology:cs_axiom('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', secondary, administrative_discretion_defers_ideology).
narrative_ontology:cs_axiom_status(administrative_discretion_defers_ideology, holdable).
narrative_ontology:cs_axiom_grounding('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', administrative_discretion_defers_ideology, conventional).
narrative_ontology:cs_reference_frame('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', welfare_state_legitimacy_crisis).
narrative_ontology:cs_drift_state('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', contemporary_implementation_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3c91c28-dc5e-491a-8fc3-6a8f90c0b542', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers_with_taxback_mechanisms).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity_requirement).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.25 at t0) because fiscal outcomes are approximately equivalent across design choices—the taxing-back research literature establishes this equivalence, which is the constraint's structural foundation. However, extractiveness rises over time to 0.42 (by t30–t40) because the gap between framing and operation grows: as implementation hardens into specific marginal tax rates and eligibility criteria, the rhetorical claim of 'unconditional' becomes less tenable, yet the ambiguous universality framing persists. The constraint extracts from targeted-program recipients (their specificity and advocacy voice are absorbed into universality) and from low-income wage workers (high effective marginal rates remain but are obscured by unconditional framing). Theater_ratio rises sharply from 0.35 to 0.70, indicating that the proportion of policy activity devoted to rhetorical maintenance (defending the ambiguous framing, explaining why design differs from framing) grows over time—as the policy ages, more energy is spent explaining the ambiguity than solving welfare problems. Suppression is moderate (0.38 by t30) because the constraint's enforcement is primarily informational: the suppression of alternative framings (explicit left vs. right choice) is maintained by the ambiguous universality label and by the administrative apparatus's complex implementation (which deters scrutiny). The constraint does not require coercive enforcement of compliance; it requires suppression of clarity about what is being chosen. Accessibility_collapse is moderate (0.45) because while actors within the system recognize the ambiguity and choices available to them, actors outside (voters, targeted-program beneficiaries with no organized representation) cannot easily see that they have been displaced. Resistance is high (0.72) because the constraint meets substantial ideological resistance from both left (claiming the design is too conditional, insufficient unconditional floor) and right (claiming it provides insufficient work incentives, excessive marginal rates). The resistance is real but ineffective because it is distributed across incompatible critiques—each side blames the implementation for failing to match its reading, not recognizing that the ambiguity is structural.
 *
 * PERSPECTIVAL GAP:
 *   From the left reading's seat (freedom-floor agenda setter): the constraint should deliver decommodified security and unconditional dignity; the actual operation (high marginal tax rates, aggressive taxing-back) is a betrayal caused by right-wing capture of implementation details. The response is to demand higher UIS floors and lower marginal rates. From the right reading's seat (work-incentive agenda setter): the constraint should preserve work incentives and replace bureaucratic means-testing; the actual operation (high unconditional floor, apparent universality) is a betrayal caused by left-wing capture of framing. The response is to demand more aggressive taxing-back and stricter eligibility criteria. From the administrative apparatus seat: both are correct in observing the gap, but reconciling them requires continuous tuning that expands bureaucratic scope. From the victim seats (targeted-program recipients, low-income wage workers): the constraint operates as designed to extract from them—the gap between framing and operation is not a bug, it is the mechanism. Seat divergence should compute per-seat types from these structural differences: beneficiary seats should compute as rope or tangled_rope (coordination with side-benefits), payer seats should compute as snare or tangled_rope (extraction disguised as coordination), and observer seats should compute as mountain or neutral (the ambiguity is structural, not a perception problem to be solved). The engine computes this divergence from the directionality values and the structural data; the authored claim (tangled_rope) does not adjudicate it, but the per-seat computations should validate the claim's directional logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers sit at d near 0.0 (beneficiaries): they collect authority, professional advancement, and coalition-spanning capability from managing the constraint's ambiguity. Their directionality is beneficiary because the constraint subsidizes their position—without the ambiguous framing, they would have to choose, and either choice would alienate part of their coalition. Targeted-program recipients sit at d near 1.0 (full target): their programs are consolidated away and their specific advocacy voice is absorbed into universal framing. They cannot exit (they depend on income support) and their exit options are constrained (relocation, program switching are costly and limited). Low-income wage workers sit at d near 0.85 (target): they bear high effective marginal tax rates under UIS-with-taxback, which are obscured by the unconditional framing. Their exit options are constrained (labor market mobility is limited) and identity-locked (they are expected to work to maintain social respect, which makes exit via non-participation unavailable). Ideological traditions (left and right) sit at d near 0.8 (target): their voices are appropriated by the universality framing but neither controls the implementation. They are trapped because accepting UIS framing provides rhetorical legitimacy and coalition inclusion, but only if they accept the ambiguity that prevents their reading from determining policy. Administrative apparatus sits at d near 0.15 (beneficiary): the constraint subsidizes bureaucratic expansion because the ambiguity between readings requires continuous administrative tuning and interpretation. The apparatus gains resources and authority from managing the divergence between framing and operation. Cross-coalition veto players sit at d near 0.2 (beneficiary): the constraint allows them to maintain coalition cohesion without resolving internal incompatibility, which preserves their institutional position and prevents coalition dissolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate is the founding problem: welfare legitimacy crisis requiring escape from means-testing bureaucracy (left) and work-disincentive tradeoffs (right). The constraint's mandate would be satisfied if either the freedom-floor design (pure left) or the dependency-constraint design (pure right) had been coherently implemented. However, the universality_paradox reading claims the constraint's actual function is NOT mandate satisfaction—it is political ambiguity management. The mandate has therefore atrophied: the constraint persists not because it solves the welfare problem, but because it allows incompatible parties to coexist in the same coalition without resolving their incompatibility. The founding problem's status is 'contested' precisely because the ambiguity prevents its resolution. Mandatrophy is resolved by recognizing that the constraint is a tangled_rope, not a rope: it performs political coordination (coalition cohesion across ideological difference) by entangling incompatible normative commitments. The mandate-atrophy concern is answered by the observation that the new function (political coordination) is real and sustained, even though it differs from the founding intent (welfare optimization). The constraint persists because political entrepreneurs and policy designers benefit from the ambiguity; the founding problem remains unresolved because resolving it would destroy the coalition. This is tangled_rope, not piton: the extraction from targeted-program recipients and low-income wage workers is real and ongoing, but the coordination function (keeping the coalition together) is also real, even though it prevents mandate satisfaction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the UIS kernel is structurally correct: the universality paradox (this reading), the freedom floor, or the dependency trap? Or are all three live readings held simultaneously by different coalitions?',
    'Resolution of the contested kernel is not empirical—it depends on which normative commitment (universality framing, unconditional dignity, or work-incentive discipline) one takes as foundational. A ''resolution'' would require explicit choice among incompatible values, which is precisely what the constraint''s ambiguity defers.',
    'If the universality paradox reading is accepted as correct, the constraint''s function is political ambiguity management, not welfare optimization. If either sibling reading displaces it, the constraint''s classification changes from tangled_rope (ambiguity entangles incompatible paths) to either rope (freedom-floor reading: genuine coordination) or snare (dependency-trap reading: pure extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Fundamental contest over which reading of the UIS kernel is structurally correct and whether all three are simultaneously live.').

omega_variable(
    taxback_mechanism_distributional_equivalence,
    'Are the distributional outcomes of UIS with aggressive taxing back actually equivalent to the targeted programs they replace, or does the measurement of equivalence obscure subtle shifts in who bears the marginal burden?',
    'Detailed incidence analysis comparing marginal tax rates facing low-income households across old and new systems, disaggregated by household type and region. Comparison of actual labor supply responses, not predicted responses.',
    'If equivalence is confirmed, the constraint operates as claimed: pure political ambiguity with fiscal outcomes independent of framing. If equivalence is illusory (low-income wage workers face higher effective marginal rates under UIS than under prior means-tested systems), extractiveness increases and the constraint shifts toward snare (wage workers become clear victims, not just ambiguous targets).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxback_mechanism_distributional_equivalence, empirical, 'Whether fiscal equivalence research''s claim that UIS and targeted programs produce identical outcomes withstands scrutiny at the household level.').

omega_variable(
    ambiguity_maintenance_mechanism,
    'Is the constraint''s ambiguity actively maintained by political entrepreneurs and policy designers, or does it arise passively from genuine conceptual incommensurability between left and right readings?',
    'Qualitative analysis of policy design choices: when designers must choose a marginal tax rate, phase-out threshold, or eligibility criterion, do they choose to preserve ambiguity (avoid decisions that would force explicit ideological choice) or do they choose based on coherent principle (left reading or right reading)? Analysis of political rhetoric and coalition documents.',
    'If ambiguity is actively maintained, the agenda-setter seats (political entrepreneurs, policy designers) gain more authority and are more clearly beneficiaries (they capture rents from the absence of resolution). If ambiguity is passive, the constraint is less of a tangled rope and more of a institutional accident—the incommensurability between traditions is real and neither reading can displace the other without destroying the coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_maintenance_mechanism, empirical, 'Whether the constraint''s political ambiguity is strategic or emergent from genuine value incommensurability.').

omega_variable(
    targeted_program_vulnerability,
    'Are targeted program recipients and their advocacy communities actually disempowered by universality framing, or do they retain voice and veto power through side-payments, compromise designs, and coalition participation?',
    'Analysis of policy design processes: did targeted program advocates successfully block consolidation or negotiate carve-outs (disability support maintained separately, child allowances preserved as supplements to UIS)? Or were they systematically overridden by universality framing?',
    'If targeted advocates retained effective voice, the victimhood is less clear and the constraint is less tangled (the extraction is visible and contested). If they were overridden by universality framing that they opposed, the victimhood is confirmed and the tangled_rope classification is strengthened—the ambiguity specifically disabled their ability to defend their constituency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeted_program_vulnerability, empirical, 'Whether targeted program constituencies experienced actual disempowerment or retained effective voice in the constraint''s operation.').

omega_variable(
    ideological_reading_foreclosure,
    'Does the universality paradox reading foreclose either the freedom-floor reading or the dependency-trap reading, or are all three simultaneously holdable by different parties?',
    'Logical analysis: if the universality paradox is correct (ambiguity entangles incompatible paths), can a single coherent party hold either the freedom-floor reading (unconditional floor is real and primary) or the dependency-trap reading (work incentives are primary)? Or does accepting the universality paradox force acknowledgment that both readings are appropriated fictions?',
    'If the universality paradox forecloses either sibling, the reading_relations should be marked ''forecloses'' rather than ''coexists_with''. If all three are simultaneously live, the relation is ''coexists_with'' and the kernel remains contested at the deep level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ideological_reading_foreclosure, conceptual, 'Whether the universality paradox reading''s core claim logically eliminates either sibling reading''s core claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__universality_paradox_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__universality_paradox_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__universality_paradox_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.61).
narrative_ontology:measurement(unco_tr_t25, unconditional_income_support__universality_paradox_reading, theater_ratio, 25, 0.65).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__universality_paradox_reading, theater_ratio, 30, 0.68).
narrative_ontology:measurement(unco_tr_t35, unconditional_income_support__universality_paradox_reading, theater_ratio, 35, 0.69).
narrative_ontology:measurement(unco_tr_t40, unconditional_income_support__universality_paradox_reading, theater_ratio, 40, 0.7).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__universality_paradox_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__universality_paradox_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__universality_paradox_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(unco_be_t25, unconditional_income_support__universality_paradox_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__universality_paradox_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(unco_be_t35, unconditional_income_support__universality_paradox_reading, base_extractiveness, 35, 0.42).
narrative_ontology:measurement(unco_be_t40, unconditional_income_support__universality_paradox_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__universality_paradox_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__universality_paradox_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__universality_paradox_reading, suppression_requirement, 15, 0.34).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__universality_paradox_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(unco_su_t25, unconditional_income_support__universality_paradox_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(unco_su_t30, unconditional_income_support__universality_paradox_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(unco_su_t35, unconditional_income_support__universality_paradox_reading, suppression_requirement, 35, 0.38).
narrative_ontology:measurement(unco_su_t40, unconditional_income_support__universality_paradox_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__universality_paradox_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% The constraint family unconditional_income_support consists of three structurally distinct readings of a contested kernel: (1) universality_paradox_reading (this constraint)—UIS as politically ambiguous Trojan horse, tangled_rope, ε≈0.42; (2) freedom_floor_reading—UIS as autonomy-enabling, rope or mountain depending on empirical labor-supply response, ε low; (3) dependency_trap_reading—UIS as incentive-distorting subsidy, snare, ε high. Each reading decomposes from a single natural-language label ('unconditional income support') into a separate constraint with its own kernel reading, beneficiary/victim structure, and classification. The universality_paradox_reading affects both siblings by establishing that their core premises are appropriable by the same policy vehicle—the universality framing allows both left and right to claim victory simultaneously, which forecloses neither reading's core premise but influences both by creating ambiguity about which is actually being implemented. The freedom_floor_reading and dependency_trap_reading influence the universality_paradox_reading by providing the incompatible implementation paths that the paradox reading claims are entangled: the constraint's actual function is to make the choice between these readings deferrable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__universality_paradox_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
