% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Displacement Reading of Practice Standardization Legitimacy
 *   domain: political/historical/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous_displacement_reading of the
 *   contested kernel legitimacy_of_practice_standardization. The doctrine
 *   holds that practice changes such as calendar, dress, or measurement
 *   standardization are legitimate only when they emerge from voluntary
 *   adoption driven by perceived utility or cultural evolution. It constrains
 *   state modernizers and colonial reformers by stripping legitimacy from
 *   top-down decrees, while coordinating academics and traditional elites
 *   around evolutionary frameworks. The claim of tangled_rope is maintained
 *   independently from metrics: the framework coordinates historiographical
 *   analysis but asymmetrically extracts legitimacy from state-led reformers.
 *
 * KEY AGENTS:
 *   - evolutionary_theorists (institutional/analytical): Primary agenda-setter and beneficiary â administers the legitimacy framework through disciplinary gatekeeping.
 *   - state_modernizers (institutional/constrained): Primary payer â bears the cost of delegitimized reform authority.
 *   - colonial_reformers (powerful/constrained): Secondary payer â excluded from legitimate historiography of change.
 *   - local_elites (moderate/mobile): Beneficiary â their adoption behavior is treated as the legitimate vanguard.
 *   - rural_communities (powerless/trapped): Excluded voice â experience of coercion is erased by the voluntary-adoption narrative.
 *   - comparative_historians (organized/analytical): Observer seat â tracks diffusion without enforcing the frame.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.76).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.68).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement Reading of Practice Standardization Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political/historical/institutional").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__endogenous_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '7e9e7b41-d509-4a67-a997-17309297e590').
narrative_ontology:cs_kernel_codification('7e9e7b41-d509-4a67-a997-17309297e590', distributed).
narrative_ontology:cs_authority_grounding('7e9e7b41-d509-4a67-a997-17309297e590', diffuse_epistemic).
narrative_ontology:cs_reading_relation('7e9e7b41-d509-4a67-a997-17309297e590', legitimacy_of_practice_standardization__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('7e9e7b41-d509-4a67-a997-17309297e590', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('7e9e7b41-d509-4a67-a997-17309297e590', foundational, voluntary_adoption_as_legitimacy_source).
narrative_ontology:cs_axiom_status(voluntary_adoption_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('7e9e7b41-d509-4a67-a997-17309297e590', voluntary_adoption_as_legitimacy_source, empirically_contingent).
narrative_ontology:cs_axiom('7e9e7b41-d509-4a67-a997-17309297e590', secondary, imposition_produces_fragile_practices).
narrative_ontology:cs_axiom_status(imposition_produces_fragile_practices, holdable).
narrative_ontology:cs_axiom_grounding('7e9e7b41-d509-4a67-a997-17309297e590', imposition_produces_fragile_practices, instrumental).
narrative_ontology:cs_reference_frame('7e9e7b41-d509-4a67-a997-17309297e590', organic_cultural_evolution).
narrative_ontology:cs_drift_state('7e9e7b41-d509-4a67-a997-17309297e590', post_colonial_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7e9e7b41-d509-4a67-a997-17309297e590', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, evolutionary_theorists).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, local_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_institutions).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_modernizers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, colonial_reformers).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntary_adoption_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and police the boundary between authentic cultural evolution and artificial state imposition in modernization studies. They administer peer review, curriculum design, and citation networks that reward endogenous-framed research and marginalize exogenous-framed alternatives. Collect paradigm dominance, citation premiums, and disciplinary authority.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, evolutionary_theorists, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, evolutionary_theorists, beneficiary).

% Their adoption behavior is treated as the legitimate vanguard of practice change. They benefit from a historiographical framework that privileges their choices over state command, validating their role as cultural brokers and insulating their status from top-down reform.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, local_elites, beneficiary,
    moderate, biographical, mobile, regional).

% Religious, kinship, and customary institutions benefit from a legitimacy rule that privileges continuity and gradual adaptation over state decree. The endogenous reading shields their jurisdictional autonomy by treating their practices as organically evolved rather than subject to legislative override.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_institutions, beneficiary,
    organized, generational, constrained, national).

% Seek to legitimate practice reforms such as calendar adoption, dress codes, and land tenure systems through state decree. Their preferred path to legitimacy is structurally delegitimized by the endogenous reading; they must either manufacture adoption appearances or accept classification as coercive modernizers.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_modernizers, payer,
    institutional, biographical, constrained, national).

% Colonial administrations imposing new practices are framed as illegitimate exogenous forces regardless of local outcomes. Their reform narratives and archival self-justifications are excluded from the legitimate historiography of change, and their policy experiments are read as extraction rather than coordination.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, colonial_reformers, payer,
    powerful, biographical, constrained, global).

% Their experience of transitional coercion during so-called voluntary adoption is not represented in the endogenous narrative. They would object to the retrospective framing of their compliance as organic cultural choice, but their testimony is absent from the academic conversation that defines legitimacy.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_communities, excluded,
    powerless, biographical, trapped, local).

% Track and compare adoption curves across regions and periods. They observe whether practice change correlates with utility or follows elite-to-mass diffusion without necessarily enforcing the endogenous frame, serving as an analytical seat that can corroborate or challenge the founding problem.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, comparative_historians, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, evolutionary_theorists).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared historiographical framework for distinguishing legitimate from illegitimate practice change, allowing scholars to classify reforms as authentic evolution or artificial imposition without recourse to normative state theory.
% TRANSFER_FUNCTION: Moves the power to legitimize practices from state decree and colonial authority to evolutionary social processes and elite-to-mass diffusion patterns, transferring analytical authority from political institutions to historical sociologists and anthropologists.
% ABSENT_VOICES: State modernizers, colonial administrators, and subaltern rural populations are structurally marginalized; their accounts of negotiated or coerced adoption are treated as epiphenomenal noise while the endogenous narrative is treated as structurally real.
% DISAPPEARANCE_RATIONALE: If the endogenous displacement norm vanished overnight, the evaluation of historical reforms like calendar or dress changes would shift from assessing adoption curves and regional variation to assessing decree efficacy and state capacity; historiography, museum curation, and policy legitimacy would reorganize around institutional command rather than social reception.
% FOUNDING_PROBLEM: How to legitimate practice change without relying on arbitrary state authority, colonial imposition, or culturally destructive top-down reform.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historians corroborate that arbitrary imposition was a genuine historiographical problem in the nineteenth and twentieth centuries. State modernizers and post-colonial critics dispute that the endogenous frame accurately resolves it, arguing that the voluntary-adoption narrative is itself a retrospective construction that masks structural coercion.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.76 over the interval because the endogenous frame progressively denies legitimacy to a widening range of state and colonial reforms. Suppression rises from 0.35 to 0.72 as peer review, curriculum boundaries, and citation networks harden against exogenous-framed scholarship. Theater_ratio crosses 0.50 by interval end, indicating that authenticity-policing has become partially performative â a diagnostic signal that the constraint may be drifting toward piton territory. Accessibility_collapse is moderate (0.48) because ex_override and dual-practice readings persist in policy and legal scholarship despite historiographical marginalization. Resistance is substantial (0.60) because state actors and post-colonial critics actively contest the frame.
 *
 * PERSPECTIVAL GAP:
 *   From the evolutionary_theorist seat, the constraint appears as a necessary analytical correction against teleological state-centrism. From the state_modernizer seat, the same structure appears as a historiographical snare that retrospectively delegitimizes reforms that had genuine coordination benefits. The engine computes this divergence from the structural asymmetry in exit options and beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Evolutionary_theorists sit near the beneficiary end: they subsidize the constraint's operation through disciplinary labor and collect paradigm rents. Local_elites and traditional_institutions are pure beneficiaries â the constraint subsidizes their status and autonomy. State_modernizers and colonial_reformers sit near the full-target end: the constraint extracts legitimacy from their preferred policy instruments. Rural_communities are structurally excluded rather than targeted; their directionality is ambiguous but their exclusion means they do not feed into the beneficiary derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â arbitrary imposition â was genuine in the colonial and high-modernist contexts. However, as the endogenous frame hardened into disciplinary orthodoxy, its coordination function shifted toward legitimizing any practice change that could be narrated as evolutionary while pathologizing state action as such. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags that the arrangement may have outlived its original problem, though the contested status prevents automatic mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint a genuine analytical framework or a normative weapon in a disciplinary contest over the legitimacy of state action?',
    'Corpus-level comparison of epsilon values across the three sibling readings of this kernel; if endogenous and exogenous readings show symmetrically opposed extraction profiles with identical beneficiaries/victims inverted, the kernel is a pure contest arena rather than a constraint with independent structure.',
    'If the reading is purely contestual, classification should reflect the kernel''s debate dynamics rather than treating any single reading as a stable constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Uncertainty about whether this reading is an independent constraint or a move in a kernel-level contest.').

omega_variable(
    voluntary_vs_manufactured_adoption,
    'Can historical methods reliably distinguish voluntary adoption from state-manufactured consent or structurally coerced compliance in the cases this reading treats as endogenous?',
    'Archival excavation of adoption campaigns showing state incentives, penalties, or elite brokerage; if such mechanisms are pervasive, the endogenous reading''s empirical foundation is undermined.',
    'If manufactured consent is widespread, the constraint''s coordination function collapses into a legitimating ideology for elite brokerage, and extractiveness should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_manufactured_adoption, empirical, 'Empirical ambiguity about whether adopted practices were genuinely voluntary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(legi_tr_t45, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(legi_tr_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 75, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(legi_be_t45, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 45, 0.64).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(legi_be_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 75, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(legi_su_t45, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(legi_su_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.08).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the legitimacy_of_practice_standardization kernel. Each reading carries a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family because the natural-language label conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
