% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Standard Persistence via Incumbent Defense
 *   domain: technology_history/industrial_standards
 *
 * SUMMARY:
 *   The QWERTY keyboard layout, originally designed to prevent mechanical
 *   typebar jams on Sholes & Glidden typewriters (1874), became the dominant
 *   standard through a combination of early market capture (Remington's
 *   commercial success), network effects (typist training infrastructure),
 *   and active defense by incumbent manufacturers and institutions. This
 *   reading holds that persistence is primarily driven by beneficiary
 *   defense: manufacturers protecting tooling investments, trained typists
 *   protecting human capital, training institutions protecting curriculum
 *   investments, and supply chains protecting complementary asset
 *   specificity. The constraint operates as a Tangled Rope — it solves a
 *   genuine coordination problem (universal key layout enabling interoperable
 *   typing labor market) while extracting from alternative-adopters and
 *   efficiency-seekers through defensive suppression (patent enforcement,
 *   exclusive contracts, lobbying). The lapsed_alternatives_reading is the
 *   sibling constraint, attributing persistence to coordination value and
 *   alternative failure to reach critical mass without incumbent malice.
 *
 * KEY AGENTS:
 *   - typewriter_manufacturers: Primary beneficiary (institutional/trapped) — protects tooling and production capital
 *   - trained_typist_workforce: Beneficiary (organized/identity_locked) — protects human capital investment in QWERTY motor patterns
 *   - typing_training_institutions: Beneficiary (organized/constrained) — protects curriculum and certification investments
 *   - office_equipment_supply_chain: Beneficiary (institutional/constrained) — protects complementary asset specificity
 *   - alternative_keyboard_adopters: Victim (moderate/trapped) — bears switching costs and compatibility penalties
 *   - efficiency_seeking_organizations: Victim (powerful/constrained) — bears productivity loss from suboptimal layout
 *   - new_entrants_to_typing_labor_market: Victim (powerless/identity_locked) — forced into QWERTY training with no historical choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.55).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Standard Persistence via Incumbent Defense").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '3e9805d0-ee81-4fd1-8e14-065c2598ca36').
narrative_ontology:cs_kernel_codification('3e9805d0-ee81-4fd1-8e14-065c2598ca36', implicit).
narrative_ontology:cs_authority_grounding('3e9805d0-ee81-4fd1-8e14-065c2598ca36', practice).
narrative_ontology:cs_reading_relation('3e9805d0-ee81-4fd1-8e14-065c2598ca36', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('3e9805d0-ee81-4fd1-8e14-065c2598ca36', foundational, incumbent_defense_drives_persistence).
narrative_ontology:cs_axiom_status(incumbent_defense_drives_persistence, holdable).
narrative_ontology:cs_axiom_grounding('3e9805d0-ee81-4fd1-8e14-065c2598ca36', incumbent_defense_drives_persistence, empirically_contingent).
narrative_ontology:cs_axiom('3e9805d0-ee81-4fd1-8e14-065c2598ca36', secondary, coordination_value_insufficient_without_enforcement).
narrative_ontology:cs_axiom_status(coordination_value_insufficient_without_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('3e9805d0-ee81-4fd1-8e14-065c2598ca36', coordination_value_insufficient_without_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('3e9805d0-ee81-4fd1-8e14-065c2598ca36', mechanical_typewriter_era).
narrative_ontology:cs_drift_state('3e9805d0-ee81-4fd1-8e14-065c2598ca36', electronic_keyboard_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3e9805d0-ee81-4fd1-8e14-065c2598ca36', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typist_workforce).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, office_equipment_supply_chain).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_organizations).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, new_entrants_to_typing_labor_market).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remington, Underwood, Royal, Smith-Corona, later IBM — invested heavily in QWERTY tooling, production lines, and parts supply chains. Defended standard through patent enforcement, exclusive dealer agreements, and later through IBM's dominance in electric typewriters and early computing. Their capital investments created the initial enforcement capacity; they profit from the installed base and switching costs they helped create.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typewriter_manufacturers, beneficiary,
    institutional, generational, trapped, global).

% Clerical workers, secretaries, typists who invested months/years in QWERTY touch-typing motor patterns. This human capital is specific to QWERTY — switching layouts requires retraining with significant productivity loss during transition. They benefit from the standard's universality (portable skill across employers) but are locked into it. Professional identity fuses with QWERTY proficiency; alternative layouts threaten occupational competence.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typist_workforce, beneficiary,
    organized, biographical, identity_locked, global).

% Business schools, vocational programs, typing academies that built curricula, certification systems, and teaching materials around QWERTY. Their product is QWERTY proficiency; switching would invalidate their pedagogical capital. They lobby standards bodies and education departments to maintain QWERTY requirements. In the 1930s-1950s, they were a coordinated political bloc resisting Dvorak adoption in public schools.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, generational, constrained, national).

% Ribbon manufacturers, typewriter repair networks, later keyboard OEMs and keycap producers — all built complementary assets specific to QWERTY layout. Their products interoperate only with QWERTY; a standard shift would require massive retooling. They benefit from the stable, universal demand QWERTY creates. In computing era, this extended to keyboard firmware, keycap legends, and OS-level keyboard drivers all assuming QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, office_equipment_supply_chain, beneficiary,
    institutional, generational, constrained, global).

% Early Dvorak adopters (1930s-1950s), ergonomic keyboard users (Kinesis, Maltron, split keyboards), Colemak/Workman enthusiasts. They bear the full cost of non-standard layouts: incompatibility with shared machines, inability to use standard training, custom keycap sourcing, OS configuration friction, social friction in collaborative typing. The constraint's enforcement machinery (standards, procurement specs, educational requirements) actively penalizes their choice.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_adopters, payer,
    moderate, biographical, trapped, global).

% Large employers, government agencies, military organizations that would gain measurable productivity from alternative layouts but face prohibitive switching costs at scale. The US Navy's 1944 Dvorak trial showed 74% faster training and 20% higher throughput, but adoption was blocked by typing school lobby and procurement standardization. Modern enterprises face the same calculus: the productivity gain is real but the coordination cost of a mixed-layout workforce is higher.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_organizations, payer,
    powerful, biographical, constrained, global).

% Children learning to type, career-changers entering clerical work, global south workers entering digital labor markets. They never chose QWERTY; it is the only option presented in schools, on devices, in job requirements. Their motor patterns form around QWERTY from first exposure, creating identity-lock that makes later switching psychologically and physiologically costly. They bear the cumulative extraction of a standard chosen for 1870s mechanical constraints.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, new_entrants_to_typing_labor_market, payer,
    powerless, biographical, identity_locked, global).

% ANSI, ISO, ECMA committees that codified QWERTY as the universal standard (ISO 9995, ANSI INCITS 154). They legitimize the constraint through formal standardization, making it procurement-ready and legally defensible. Their process is open to alternative proposals but the installed base and beneficiary coalition make alternatives practically non-viable. They observe the constraint's operation but do not directly collect or pay its extraction.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, standards_bodies, observer,
    institutional, generational, analytical, global).

% FTC, DOJ Antitrust, EU Commission — have investigated whether keyboard standard constitutes anti-competitive tying or monopolization. Never pursued enforcement because the standard is not owned by a single firm (unlike the OS marketplace case) and the coordination defense is strong. They observe the constraint's market effects but lack a clear remedial lever.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal key layout enabling a portable typing skill across all machines, employers, and geographies — a single learned motor pattern works everywhere. Solves the interoperability problem for the global typing labor market and device ecosystem.
% TRANSFER_FUNCTION: Moves productivity (speed, accuracy, reduced fatigue) from alternative-adopters, efficiency-seekers, and new labor market entrants to incumbent manufacturers, trained typists, training institutions, and supply chain — the latter collect rents from the installed base and avoid retraining/retooling costs.
% ABSENT_VOICES: Would-be alternative keyboard entrepreneurs (blocked by standards and procurement specs), global south digital labor entrants (forced into QWERTY with no historical path dependence), RSI sufferers who would benefit from ergonomic layouts but face switching barriers. These voices are structurally excluded by the constraint's enforcement machinery — they are not in the standardization room, not in the procurement process, not in the curriculum committee.
% DISAPPEARANCE_RATIONALE: If QWERTY enforcement vanished overnight: keyboard manufacturers would ship multiple layouts by default; OS vendors would make layout switching one-click; typing curricula would offer layout choice; ergonomic layouts would gain market share rapidly. The typing labor market would fracture temporarily (mixed-layout workplaces) then likely converge on a more efficient standard (Dvorak/Colemak) within 10-15 years. The incumbent beneficiary coalition would lose their protective rents; new entrants would gain efficiency.
% FOUNDING_PROBLEM: Mechanical typebar interference on early typewriters (Sholes & Glidden, 1874) — adjacent typebars would clash and jam at typing speed. QWERTY separated common letter pairs to reduce jams.
% FOUNDING_PROBLEM_CORROBORATION: The mechanical jam problem is objectively resolved: electric typewriters (1920s), Selectric ball (1961), and electronic keyboards (1970s+) eliminated typebar interference entirely. No engineering authority claims QWERTY serves a mechanical function today. The beneficiary coalition (manufacturers, typing schools) acknowledges the original problem is gone but argues the coordination value justifies persistence. Independent historians of technology (David 1985, Liebowitz & Margolis 1990, 1995) confirm the founding problem is dead and the standard persists via path dependence and network effects — corroboration from outside the beneficiary set.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the sustained productivity penalty of QWERTY vs. demonstrated alternatives (Dvorak: ~5-10% speed advantage, ~50% finger travel reduction) multiplied by the global typing workforce — a massive aggregate extraction. Suppression (0.55) captures active enforcement: Remington's patent litigation (1880s-1900s), exclusive contracts with typing schools (1910s-1930s), IBM's 1961 Selectric cementing QWERTY in computing, ANSI/ISO standardization locking out alternatives. Theater ratio (0.38) reflects the increasing performative share: early period had genuine mechanical justification (anti-jam), later period's 'standardization' rhetoric increasingly covers extraction. Accessibility collapse (0.62) — alternatives exist but face massive switching barriers. Resistance (0.47) — periodic challenges (Dvorak 1936, various ergonomic layouts) but never sustained enough to displace.
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturer seat (institutional power, arbitrage exit), the constraint appears as coordination infrastructure they built and maintain. From the new-entrant seat (powerless, identity-locked), it appears as an imposed tax on labor market entry. The engine computes this divergence from power/exit asymmetry — the same constraint structure produces different experienced types. The incumbent_preservation_reading emphasizes the beneficiary defense mechanism; the lapsed_alternatives_reading would emphasize the coordination function and alternative inferiority.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers and supply chain are structural beneficiaries (d ~ 0.15) — they collect rents from installed base and control enforcement. Trained typists and training institutions are beneficiaries with identity-locked exit (d ~ 0.25) — they benefit from incumbent status but would bear retraining costs if standard changed. Alternative-adopters and efficiency-seekers are targets (d ~ 0.85) — they pay the extraction and face suppression. New labor market entrants are identity-locked targets (d ~ 0.9) — they never consented to the standard but inherit its costs. Competition authorities and standards bodies are observers (d ~ 0.5, analytical exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate (preventing mechanical typebar jams) died with the Selectric (1961) and electronic keyboards. The constraint persists because beneficiaries control the enforcement machinery and the coordination function (universal layout) provides cover. The founding problem is dead; the arrangement persists as extraction. This is a classic mandatrophy case: the constraint's mandate is resolved but the constraint remains via beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'Is the QWERTY persistence phenomenon one kernel with multiple readings, or structurally distinct constraints?',
    'Compare the beneficiary/victim structures and epsilon referents across incumbent_preservation_reading and lapsed_alternatives_reading. If they describe different standing arrangements with different extraction profiles, they are distinct constraints linked by network.affects_constraints. If they describe the same arrangement from different observational angles, they are readings of one kernel.',
    'If distinct constraints: each gets independent classification. If readings of one kernel: the committer structure (cs_structure.reading_relations, axioms, reference_frame, drift_state) is the correct representation, and the engine''s kernel-aware consumers will evaluate the reading set as a unit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Whether QWERTY persistence is a single contested kernel or multiple constraints').

omega_variable(
    defensive_suppression_vs_coordination_cost,
    'How much of the measured suppression (0.55) is defensive action by incumbents protecting capital investments versus necessary coordination cost of maintaining a universal standard?',
    'Historical analysis of enforcement actions: patent litigation against alternative keyboard makers, exclusive dealing contracts with typing schools, lobbying against government adoption of alternatives. Compare to coordination-maintenance costs (interoperability testing, standard-setting body operations).',
    'If defensive suppression dominates, the constraint''s extraction profile is higher and its Tangled Rope classification strengthens toward Snare. If coordination cost dominates, the arrangement is closer to Rope with incidental extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_suppression_vs_coordination_cost, empirical, 'Decomposition of suppression into defensive vs. coordination-maintenance components').

omega_variable(
    alternative_viability_counterfactual,
    'Would Dvorak or other alternatives have achieved critical mass absent incumbent defensive action, or did they fail on intrinsic merits (learning curve, switching cost, network effects)?',
    'Natural experiments: contexts where incumbents lacked enforcement power (e.g., early PC era before IBM standardization, non-Western markets with different incumbent structures). Compare adoption trajectories.',
    'If alternatives failed primarily on intrinsic merits, the lapsed_alternatives_reading gains empirical support and this reading''s extraction claim weakens. If defensive action was decisive, this reading''s Tangled Rope classification with active enforcement is structurally accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_viability_counterfactual, empirical, 'Counterfactual viability of alternative keyboard standards without incumbent suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 1878, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_preservation_tr_t1878, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1878, 0.1).
narrative_ontology:measurement(qwerty_preservation_tr_t1890, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(qwerty_preservation_tr_t1905, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1905, 0.22).
narrative_ontology:measurement(qwerty_preservation_tr_t1920, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(qwerty_preservation_tr_t1940, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1940, 0.33).
narrative_ontology:measurement(qwerty_preservation_tr_t1965, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1965, 0.36).
narrative_ontology:measurement(qwerty_preservation_tr_t1985, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(qwerty_preservation_tr_t2000, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2000, 0.38).

% Extraction over time
narrative_ontology:measurement(qwerty_preservation_be_t1878, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1878, 0.25).
narrative_ontology:measurement(qwerty_preservation_be_t1890, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(qwerty_preservation_be_t1905, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1905, 0.45).
narrative_ontology:measurement(qwerty_preservation_be_t1920, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(qwerty_preservation_be_t1940, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1940, 0.62).
narrative_ontology:measurement(qwerty_preservation_be_t1965, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1965, 0.68).
narrative_ontology:measurement(qwerty_preservation_be_t1985, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(qwerty_preservation_be_t2000, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_preservation_su_t1878, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1878, 0.15).
narrative_ontology:measurement(qwerty_preservation_su_t1890, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1890, 0.25).
narrative_ontology:measurement(qwerty_preservation_su_t1905, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1905, 0.35).
narrative_ontology:measurement(qwerty_preservation_su_t1920, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1920, 0.45).
narrative_ontology:measurement(qwerty_preservation_su_t1940, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1940, 0.5).
narrative_ontology:measurement(qwerty_preservation_su_t1965, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1965, 0.53).
narrative_ontology:measurement(qwerty_preservation_su_t1985, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(qwerty_preservation_su_t2000, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2000, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__incumbent_preservation_reading, 0.08).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, computer_keyboard_standardization).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, typing_education_curriculum_lockin).

% DUAL FORMULATION NOTE:
% The QWERTY persistence kernel decomposes into (at least) two readings: incumbent_preservation_reading (this constraint) and lapsed_alternatives_reading. They share the same referent (QWERTY's 140+ year dominance) but disagree on the structural mechanism — beneficiary defense vs. coordination value. This story authors the incumbent_preservation_reading with epsilon=0.68 (defensive suppression costs included). The sibling reading would author a lower epsilon (coordination cost only) and likely classify as Rope. Network link enables contamination analysis: if empirical evidence shifts toward lapsed_alternatives, this constraint's extraction profile should degrade.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence__incumbent_preservation_reading, organized, 0.25).
constraint_indexing:directionality_override(qwerty_persistence__incumbent_preservation_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
