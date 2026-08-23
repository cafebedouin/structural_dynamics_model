% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint story captures the parliamentary supremacy reading of
 *   constitutional interpretive authority: the elected legislature possesses
 *   final say over constitutional meaning, and courts lack authority to void
 *   parliamentary acts. It is a contested kernel reading (siblings: judicial
 *   supremacy, coordinate construction). The legislature is the structural
 *   beneficiary of interpretive discretion; the judiciary and rights-holders
 *   bear the costs of excluded judicial review. The arrangement coordinates
 *   governance by preventing inter-branch deadlock but extracts
 *   asymmetrically by concentrating ultimate power in the legislative
 *   majority.
 *
 * KEY AGENTS:
 *   - Legislature (institutional/agenda-setter): possesses and exercises final interpretive authority, benefits from unreviewable discretion.
 *   - Judiciary (institutional/payer): structurally subordinated, lacks nullification power, bears cost of deferred constitutional role.
 *   - Constitutional rights holders (powerless/payer): lack judicial recourse against parliamentary override, bear risk of majoritarian extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.55).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0').
narrative_ontology:cs_kernel_codification('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', formalized).
narrative_ontology:cs_authority_grounding('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', lineage).
narrative_ontology:cs_interpretation_layer_present('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0').
narrative_ontology:cs_reading_relation('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', constitutional_interpretive_authority__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', foundational, parliamentary_sovereignty_as_fundamental).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_as_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', parliamentary_sovereignty_as_fundamental, conventional).
narrative_ontology:cs_axiom('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', foundational, judicial_nullification_ultra_vires).
narrative_ontology:cs_axiom_status(judicial_nullification_ultra_vires, holdable).
narrative_ontology:cs_axiom_grounding('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', judicial_nullification_ultra_vires, conventional).
narrative_ontology:cs_reference_frame('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', westminster_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4d1cc31a-c4ea-43d2-8ad6-6b577e8ad8e0', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses final interpretive authority over constitutional meaning and enacts legislation without risk of judicial nullification. Derives structural benefit from unreviewable discretion to determine the content and limits of law, legitimated through electoral mandate.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% Courts are required to defer to parliamentary acts and lack authority to void legislation even when it conflicts with constitutional norms or fundamental rights. Bears the structural cost of a subordinated constitutional role and loss of interpretive finality.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Individuals and groups whose rights are recognized in legal or political text but who lack judicial recourse against legislation that overrides those protections. They bear the risk of majoritarian override and unreviewable parliamentary sovereignty.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_rights_holders, payer,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislature).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__parliamentary_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves inter-branch conflict over constitutional meaning by concentrating final interpretive authority in a single elected institution, preventing deadlock and establishing a clear hierarchy in the constitutional order.
% TRANSFER_FUNCTION: Moves final constitutional interpretive authority and the power to give supreme legal effect to norms from the judiciary to the elected legislature.
% ABSENT_VOICES: Proponents of judicial supremacy and constitutional rights claimants who would benefit from justiciable review are structurally marginalized; common-law constitutionalist judges and international human rights institutions are excluded from the authoritative interpretive framework.
% DISAPPEARANCE_RATIONALE: If legislative interpretive supremacy vanished overnight, courts would acquire (or share) authority to nullify legislation, shifting the locus of constitutional legitimacy and fundamentally altering how statutes are drafted, challenged, and enforced.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: unelected judges overriding democratically enacted legislation, creating a democratic deficit and inter-branch conflict over the ultimate source of constitutional authority.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary sovereignty theorists within the Diceyan tradition attest the problem remains salient. Critics from rights-based constitutionalism and comparative law attest the problem is overstated or that the cure produces worse democratic pathologies. Corroboration from fully external seats is limited because the dispute is internal to constitutional theory and institutional identity.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the asymmetric concentration of ultimate authority in one branch. Suppression (0.55) reflects the doctrinal exclusion of judicial review alternatives. Theater ratio is low (0.20) because the doctrine is substantively operative, not merely performative. Accessibility collapse (0.70) is high because judicial review alternatives are constitutionally closed off once the doctrine is accepted. Resistance (0.45) captures ongoing contestation from rights-based and judicial-supremacy traditions.
 *
 * PERSPECTIVAL GAP:
 *   The legislature experiences this as democratic coordination (the people's representatives have the final say); the judiciary and rights holders experience it as structural exclusion from constitutional protection. The engine will compute divergent seat classifications from this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature is the beneficiary (d near the beneficiary end), receiving interpretive discretion and immunity from judicial nullification. The judiciary and constitutional rights holders are targets (d near the target end), paying through subordinated authority and unreviewable risk. The directionality derives from beneficiary/victim declarations combined with institutional exit options: the judiciary is constrained by constitutional role, and rights holders are trapped in the jurisdiction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â the counter-majoritarian difficulty of unelected judges overriding democratic legislation â remains contested, but the solution (parliamentary supremacy) may have outlived its necessity in an era of mature democratic constitutionalism and international human rights law. The doctrine persists because it coordinates British-derived governance effectively, yet it asymmetrically empowers legislative majorities. The Tangled Rope classification prevents mislabeling it as pure extraction (it solves a real coordination problem) or pure coordination (it asymmetrically benefits the legislative majority at cost to rights-protective review).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does strict parliamentary supremacy absolutely foreclose judicial constitutional interpretation, or can it coexist with weak-form rights dialogue without nullification?',
    'Comparative constitutional analysis of Westminster-derived systems to determine whether judicial rights-dialogue mechanisms are structurally compatible with legislative finality.',
    'If weak-form review is compatible, the reading''s victim set shrinks and the constraint shifts toward rope; if absolute, it remains tangled_rope with full exclusion of judicial authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Ambiguity about whether parliamentary supremacy excludes all judicial constitutional interpretation or only nullification.').

omega_variable(
    majoritarian_extraction_vs_democratic_coordination,
    'Is the legislature''s final authority a genuine democratic coordination mechanism or a majoritarian extraction device that systematically disadvantages permanent minorities?',
    'Empirical analysis of legislative behavior toward minority rights in parliamentary supremacy regimes compared to systems with judicial review, controlling for electoral system and party structure.',
    'If minorities fare systematically worse, extraction is higher and snare-like features strengthen; if legislatures internalize rights constraints, the coordination function dominates and rope features strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_extraction_vs_democratic_coordination, empirical, 'Whether parliamentary supremacy operates as coordination or majoritarian extraction.').

omega_variable(
    sibling_reading_divergence,
    'How would classification change if the constitutional_interpretive_authority kernel were stabilized under judicial_supremacy_reading or coordinate_construction_reading instead of this reading?',
    'Cross-reading corpus analysis comparing the epsilon and stakeholder structures of the sibling constraints generated from the same kernel.',
    'If the sibling readings show lower extraction and broader coordination, the kernel is inherently contested and this reading''s high extraction is reading-dependent rather than kernel-necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_divergence, conceptual, 'Structural uncertainty about whether high extraction belongs to the kernel or to this specific reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parl_sup_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(parl_sup_tr_t16, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement(parl_sup_tr_t32, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 32, 0.17).
narrative_ontology:measurement(parl_sup_tr_t48, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 48, 0.2).
narrative_ontology:measurement(parl_sup_tr_t64, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 64, 0.22).
narrative_ontology:measurement(parl_sup_tr_t80, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 80, 0.25).

% Extraction over time
narrative_ontology:measurement(parl_sup_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(parl_sup_be_t16, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(parl_sup_be_t32, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(parl_sup_be_t48, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 48, 0.65).
narrative_ontology:measurement(parl_sup_be_t64, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 64, 0.65).
narrative_ontology:measurement(parl_sup_be_t80, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 80, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(constitutional_interpretive_authority__parliamentary_supremacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
