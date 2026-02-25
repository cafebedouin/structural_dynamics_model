% ============================================================================
% CONSTRAINT STORY: inverse_spin_valve_signature
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inverse_spin_valve_signature, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: inverse_spin_valve_signature
 *   human_readable: Inverse Spin Valve Signature in Noncentrosymmetric Superconductors
 *   domain: condensed_matter_physics/superconductivity/quantum_materials
 *
 * SUMMARY:
 *   The inverse spin valve signature refers to the observation that critical
 *   temperature Tc is suppressed in antiparallel ferromagnet alignment
 *   relative to parallel alignment in noncentrosymmetric superconductors —
 *   opposite to conventional singlet superconductor behavior. First observed
 *   in NbRe with ΔTc ~ 30mK, this signature has been interpreted as evidence
 *   for triplet pairing enabled by antisymmetric spin-orbit coupling (ASOC).
 *   The constraint operates as a coordination mechanism for the triplet
 *   pairing research program while extracting from the conventional singlet
 *   interpretation framework. KEY AGENTS (by structural relationship): -
 *   Singlet theorists: Primary victims (powerless/trapped) — institutional
 *   commitment to BCS framework threatened by inverse signature - Triplet
 *   pairing researchers: Primary beneficiaries (institutional/arbitrage) —
 *   inverse signature validates research program and secures funding -
 *   Experimental materials physicists: Secondary actors (organized/mobile) —
 *   navigate between frameworks, experience both coordination and extraction
 *   - Analytical observers: Framework evaluators (analytical/analytical) —
 *   assess structural validity of inverse signature as pairing symmetry probe
 *
 * KEY AGENTS:
 *   - Singlet theorists: Primary victims (powerless/trapped) — bears extraction through forced reinterpretation of conventional framework
 *   - Triplet pairing researchers: Primary beneficiaries (institutional/arbitrage) — benefits from inverse signature as coordination mechanism
 *   - Experimental materials physicists: Secondary actors (organized/mobile) — experience both coordination value and extraction pressure
 *   - Analytical observers: Framework evaluators (analytical/analytical) — see full structure of coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inverse_spin_valve_signature, 0.38).
domain_priors:suppression_score(inverse_spin_valve_signature, 0.45).
domain_priors:theater_ratio(inverse_spin_valve_signature, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inverse_spin_valve_signature, extractiveness, 0.38).
narrative_ontology:constraint_metric(inverse_spin_valve_signature, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(inverse_spin_valve_signature, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(inverse_spin_valve_signature, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(inverse_spin_valve_signature, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inverse_spin_valve_signature, tangled_rope).
narrative_ontology:human_readable(inverse_spin_valve_signature, "Inverse Spin Valve Signature in Noncentrosymmetric Superconductors").
narrative_ontology:topic_domain(inverse_spin_valve_signature, "condensed_matter_physics/superconductivity/quantum_materials").

domain_priors:requires_active_enforcement(inverse_spin_valve_signature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inverse_spin_valve_signature, triplet_pairing_researchers).
narrative_ontology:constraint_beneficiary(inverse_spin_valve_signature, noncentrosymmetric_materials_community).
narrative_ontology:constraint_victim(inverse_spin_valve_signature, singlet_interpretation_advocates).
narrative_ontology:constraint_victim(inverse_spin_valve_signature, conventional_bcs_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SINGLET THEORIST (TANGLED ROPE) — Trapped by institutional commitment to conventional BCS framework, experiences both coordination value (new pairing symmetry probe) and extraction (forced reinterpretation)
constraint_indexing:constraint_classification(inverse_spin_valve_signature, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE TRIPLET PAIRING ADVOCATE (ROPE) — Benefits from inverse signature as coordination mechanism validating triplet superconductivity research program
constraint_indexing:constraint_classification(inverse_spin_valve_signature, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE EXPERIMENTAL MATERIALS PHYSICIST (TANGLED ROPE) — Sees both coordination value (new research direction) and extraction (forced reinterpretation of existing data)
constraint_indexing:constraint_classification(inverse_spin_valve_signature, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes genuine coordination function (resolving pairing symmetry) coupled with asymmetric extraction from conventional framework
constraint_indexing:constraint_classification(inverse_spin_valve_signature, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inverse_spin_valve_signature_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inverse_spin_valve_signature, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inverse_spin_valve_signature, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(inverse_spin_valve_signature_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the moderate cost imposed on conventional singlet interpretation advocates who must either reinterpret existing data or accept framework inadequacy. Suppression (0.45) captures the difficulty of maintaining singlet interpretation in face of inverse signature evidence, though alternative explanations (multiband effects, strong SOC modifications) remain possible. Theater ratio (0.32) indicates growing but stabilizing performative emphasis on inverse signature as triplet evidence, with the field recognizing remaining ambiguities. The constraint requires active enforcement through experimental replication and theoretical elaboration — it does not emerge naturally from first principles but depends on specific material properties and measurement protocols. The measurements show initial extraction accumulation (2005-2010) followed by stabilization and slight decline (2010-2015) as the field reached equilibrium between coordination value and extraction costs, with growing recognition of interpretive ambiguities tempering extractive pressure.
 *
 * PERSPECTIVAL GAP:
 *   The singlet theorist (powerless/trapped) experiences the inverse signature as Tangled Rope — genuine coordination value in pairing symmetry determination coupled with significant extraction through framework reinterpretation demands. The triplet pairing advocate (institutional/arbitrage) experiences it as a Rope — a low-extraction coordination mechanism that solves the pairing symmetry ambiguity problem and enables productive research. The experimental physicist (organized/mobile) sees Tangled Rope — genuine coordination value in resolving pairing symmetry coupled with extraction through forced framework adoption. The analytical observer recognizes the dual structure: real coordination function (pairing symmetry probe) entangled with asymmetric extraction from conventional framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are triplet pairing researchers who gain institutional validation, funding access, and framework dominance from inverse signature interpretation. Their arbitrage exit options (ability to work across multiple material systems and theoretical frameworks) combined with beneficiary status yields low directionality (d ~ 0.10), producing negative effective extraction (χ < 0). Victims are singlet interpretation advocates who bear costs through framework obsolescence and data reinterpretation demands. Their trapped exit options (institutional commitment to conventional BCS) combined with victim status yields high directionality (d ~ 0.85), producing elevated but moderate effective extraction (χ ~ 0.55). Experimental physicists occupy intermediate position with mobile exit options and mixed beneficiary/victim status, yielding moderate directionality (d ~ 0.50) and balanced extraction (χ ~ 0.38).
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this as pure extraction (Snare) or pure coordination (Rope). The inverse signature provides genuine coordination value — it offers an experimental probe for pairing symmetry that addresses a real scientific question. However, this coordination function is entangled with asymmetric extraction: the conventional singlet framework bears significant reinterpretation costs, while the triplet pairing program captures the coordination benefits. The constraint is not a natural law (Mountain) because the inverse signature interpretation depends on specific theoretical assumptions about ASOC-enabled triplet pairing that remain contested. Alternative mechanisms (multiband effects, proximity-induced pairing) could potentially explain the same experimental signature. The Tangled Rope classification captures this dual structure: real coordination entangled with framework-specific extraction. The stabilization and slight decline of extraction and theater metrics after 2010 indicates the field has reached an equilibrium where the coordination value is recognized while extraction costs are contained, with growing awareness of interpretive limitations preventing runaway extraction accumulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pairing_symmetry_ambiguity,
    'Does the inverse spin valve signature uniquely identify triplet pairing, or can singlet mechanisms with strong spin-orbit coupling produce the same effect?',
    'Phase-sensitive measurements (Josephson interferometry), angle-resolved thermal conductivity, or direct observation of odd-frequency pairing correlations',
    'If uniquely triplet: Rope (coordination mechanism). If singlet-compatible: Snare (extractive reinterpretation)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pairing_symmetry_ambiguity, empirical, 'Whether inverse signature uniquely identifies triplet pairing versus singlet with strong SOC').

omega_variable(
    material_specificity,
    'Is the 30mK ΔTc magnitude in NbRe representative of a universal noncentrosymmetric superconductor property, or is it material-specific and potentially an artifact?',
    'Systematic survey across multiple noncentrosymmetric superconductors (CePt3Si, Li2Pt3B, UIr) with varying ASOC strength',
    'If universal: Mountain (intrinsic property). If material-specific: Piton (NbRe-specific artifact)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_specificity, empirical, 'Whether inverse signature is universal or material-specific').

omega_variable(
    theoretical_framework_lock_in,
    'Does the inverse signature interpretation create path dependence that suppresses alternative theoretical frameworks (e.g., multiband effects, proximity-induced pairing)?',
    'Comparative analysis of citation patterns, funding allocation, and theoretical diversity in noncentrosymmetric superconductivity research post-2010',
    'If path-dependent: Snare (extractive lock-in). If framework-neutral: Rope (coordination)',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theoretical_framework_lock_in, conceptual, 'Whether inverse signature creates theoretical framework lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inverse_spin_valve_signature, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isv_theater_2005, inverse_spin_valve_signature, theater_ratio, 0, 0.15).
narrative_ontology:measurement(isv_theater_2010, inverse_spin_valve_signature, theater_ratio, 5, 0.26).
narrative_ontology:measurement(isv_theater_2015, inverse_spin_valve_signature, theater_ratio, 10, 0.32).

% Extraction over time
narrative_ontology:measurement(isv_extract_2005, inverse_spin_valve_signature, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(isv_extract_2010, inverse_spin_valve_signature, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(isv_extract_2015, inverse_spin_valve_signature, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inverse_spin_valve_signature, information_standard).
narrative_ontology:boltzmann_floor_override(inverse_spin_valve_signature, 0.18).
narrative_ontology:affects_constraint(inverse_spin_valve_signature, triplet_pairing_universality).
narrative_ontology:affects_constraint(inverse_spin_valve_signature, asoc_pairing_mechanism).

% DUAL FORMULATION NOTE:
% The inverse spin valve signature is downstream of noncentrosymmetric_asoc_coupling (Mountain) — the ASOC provides the physical mechanism that enables triplet pairing and inverse signature. However, the inverse signature itself is a Tangled Rope because its interpretation as definitive triplet evidence remains contested. The upstream Mountain (ASOC exists) does not guarantee the downstream interpretation (inverse signature uniquely identifies triplet pairing). This decomposition separates the physical mechanism (Mountain) from the interpretive framework (Tangled Rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(inverse_spin_valve_signature, institutional, 0.1).
constraint_indexing:directionality_override(inverse_spin_valve_signature, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
