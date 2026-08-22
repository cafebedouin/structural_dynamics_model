% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta's Feudal Obsolescence (Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'feudal obsolescence' reading of Magna
 *   Carta's authority, arguing that the 13th-century baronial compact has no
 *   binding force over modern sovereignty structures. It is a reading that
 *   maximizes executive and parliamentary discretion by dismissing historical
 *   constraints as anachronistic. The constraint is claimed as a Piton
 *   because its persistence relies more on institutional inertia and
 *   rhetorical performance (e.g., celebrating its historical significance
 *   while denying its legal force) than on active, concentrated benefit,
 *   though it enables extraction by removing potential checks on power. The
 *   metrics reflect a constraint that is largely performative in its
 *   'maintenance' (high theater_ratio) but still enables significant
 *   extraction by nullifying historical checks on power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.65).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.7).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta's Feudal Obsolescence (Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '8aca83d4-79f2-4f9f-acd6-ee9f06e46f96').
narrative_ontology:cs_kernel_codification('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', fixed_text).
narrative_ontology:cs_authority_grounding('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', extraction).
narrative_ontology:cs_interpretation_layer_present('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96').
narrative_ontology:cs_reading_relation('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', foundational, feudal_compacts_lack_modern_binding_force).
narrative_ontology:cs_axiom_status(feudal_compacts_lack_modern_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', feudal_compacts_lack_modern_binding_force, conventional).
narrative_ontology:cs_axiom('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', secondary, sovereignty_resides_in_contemporary_institutions).
narrative_ontology:cs_axiom_status(sovereignty_resides_in_contemporary_institutions, holdable).
narrative_ontology:cs_axiom_grounding('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', sovereignty_resides_in_contemporary_institutions, conventional).
narrative_ontology:cs_reference_frame('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', modern_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8aca83d4-79f2-4f9f-acd6-ee9f06e46f96', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_majority).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the argument that Magna Carta is historically obsolete, thereby removing ancient constraints on executive power and maximizing discretion in modern governance. This reading allows for more flexible and unconstrained action.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch, beneficiary,
    institutional, biographical, mobile, national).

% Gains from the obsolescence reading by asserting that any enduring principles of Magna Carta have been fully absorbed into and superseded by modern statute law, which Parliament can freely amend or repeal. This reinforces parliamentary sovereignty.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_majority, beneficiary,
    institutional, biographical, mobile, national).

% Bear the cost of this reading as it undermines claims for a broader, enduring constitutional tradition that empowers citizens and limits state power. Their arguments for popular sovereignty and fundamental rights are weakened.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates, payer,
    organized, generational, constrained, national).

% Suffer from this reading as it diminishes the role of courts in interpreting and enforcing ancient constitutional principles against legislative or executive overreach. It reduces the scope for judicial review based on historical charters.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_advocates, payer,
    organized, generational, constrained, national).

% Analyze the historical context and original intent of Magna Carta, often providing evidence that supports or refutes claims of its modern relevance. Their work informs the debate but does not directly set policy.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the understanding of constitutional authority by asserting a clear break between medieval feudal compacts and modern sovereign power, simplifying the legal landscape by removing ancient, ambiguous constraints.
% TRANSFER_FUNCTION: Transfers interpretive authority over fundamental law from historical documents and judicial precedent to contemporary legislative and executive bodies, from advocates of inherited rights to those asserting modern state power.
% ABSENT_VOICES: Those who believe in an unbroken chain of constitutional tradition, or who see Magna Carta as a foundational text for universal human rights, are marginalized by this reading. Their arguments for enduring principles are dismissed as anachronistic.
% DISAPPEARANCE_RATIONALE: If the 'feudal obsolescence' reading vanished, the world would largely remain unchanged in terms of practical governance, as modern sovereignty structures already operate largely independent of Magna Carta's direct authority. The debate would shift, but the underlying power dynamics would persist.
% FOUNDING_PROBLEM: The problem of reconciling an ancient feudal document with the demands of modern, centralized, and democratically accountable (or at least, electorally legitimized) state power.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and political theorists outside the direct beneficiaries (executive/parliamentary power) corroborate that the tension between historical documents and modern governance is a live and complex problem, though they may dispute this reading's resolution.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_unchanged).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high theater_ratio (0.80) reflects that while Magna Carta is often celebrated symbolically, this reading denies its practical legal force, making its 'maintenance' largely performative. Extractiveness (0.65) is moderate-to-high because dismissing Magna Carta's authority removes a potential check on state power, allowing for greater discretion and potential extraction by the executive and legislature. Suppression (0.70) is also high, as this reading actively suppresses alternative interpretations that would assert enduring constitutional limits. Accessibility collapse is low (0.30) because alternative readings and arguments for enduring constitutionalism still exist and are actively pursued, though they face significant rhetorical and institutional barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the executive and parliamentary majority, this reading is a pragmatic recognition of modern sovereignty. From the perspective of those advocating for popular constitutionalism and juridical restraint, it is a rhetorical maneuver to enable unchecked power. The engine's classification will highlight this divergence by showing different effective extraction for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and parliamentary majority are beneficiaries, as this reading grants them greater freedom from historical constraints. Advocates of popular constitutionalism and juridical restraint are payers, as their arguments for limited government and fundamental rights are undermined. Legal historians act as observers, analyzing the historical context without directly benefiting or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_continuity_vs_discontinuity,
    'Is there a genuine historical discontinuity between medieval feudal compacts and modern constitutionalism, or is there an unbroken, albeit evolving, tradition of constitutional restraint?',
    'Extensive historical and legal scholarship, potentially leading to a consensus on the nature of constitutional evolution and the role of foundational documents.',
    'If continuity is established, this reading''s premise of obsolescence is weakened, potentially reclassifying the constraint towards a more ''tangled_rope'' or ''snare'' type if its persistence is found to be purely extractive. If discontinuity is affirmed, the ''piton'' classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_continuity_vs_discontinuity, conceptual, 'Ambiguity regarding the historical relationship between Magna Carta and modern constitutional structures.').

omega_variable(
    rhetorical_performance_vs_functional_obsolescence,
    'To what extent is the ''obsolescence'' claim a genuine reflection of Magna Carta''s diminished legal force, versus a rhetorical performance to justify expanded state power?',
    'Analysis of legislative and judicial decisions where Magna Carta is invoked or dismissed, and the actual impact on state power. Examination of the ''theater_ratio'' in practice.',
    'If primarily rhetorical, the ''theater_ratio'' is accurately high, but the underlying extractiveness might be higher than currently assessed, pushing it towards a ''snare''. If genuinely obsolete, the ''piton'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_performance_vs_functional_obsolescence, empirical, 'Distinguishing genuine obsolescence from strategic rhetorical use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1900, 0.6).
narrative_ontology:measurement(magn_tr_t1930, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1930, 0.68).
narrative_ontology:measurement(magn_tr_t1960, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1960, 0.75).
narrative_ontology:measurement(magn_tr_t1990, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1990, 0.78).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2024, 0.8).

% Extraction over time
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(magn_be_t1930, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1930, 0.55).
narrative_ontology:measurement(magn_be_t1960, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(magn_be_t1990, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement(magn_su_t1930, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(magn_su_t1960, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(magn_su_t1990, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, identity_coordination).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'magna_carta_constraint_authority' kernel, focusing on its feudal obsolescence. The other readings ('living_constitutionalism_reading' and 'parliamentary_sovereignty_reading') offer alternative interpretations of its modern relevance and binding force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
