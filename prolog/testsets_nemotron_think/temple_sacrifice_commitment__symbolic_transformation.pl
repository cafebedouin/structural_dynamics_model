% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Authorized Symbolic Transformation of Temple Sacrifice
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint story instantiates the symbolic_transformation reading of
 *   the temple_sacrifice_commitment kernel. The reading asserts that the
 *   rabbinic authority structure legitimately authorized the transformation
 *   of the sacrificial commandment into prayer and study as its new
 *   instantiation — not as substitutes for a suspended practice, but as the
 *   commandment's fulfilled form in the post-Temple era. This claim carries
 *   high extractiveness if the authorization is contested: the authority
 *   structure asserts power to redefine what the divine command requires, and
 *   those who hold the original material performance as non-negotiable
 *   (material_performance_adherents) bear the cost of this redefinition. The
 *   constraint operates as a tangled rope: it coordinates Jewish communal
 *   practice across two millennia (genuine coordination function) while
 *   simultaneously extracting definitional authority from dissenters
 *   (asymmetric extraction). Active enforcement is required — the halakhic
 *   consensus maintains the transformation through social, institutional, and
 *   theological gatekeeping.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: agenda_setter (institutional/generational/analytical/universal) — defines and enforces the authorized transformation
 *   - prayer_communities: beneficiary (organized/biographical/mobile/global) — practice accessible, authorized worship
 *   - material_performance_adherents: payer (organized/biographical/identity_locked/global) — bear cost of redefinition, identity-fused to material performance
 *   - messianic_restorationists: excluded (moderate/generational/trapped/global) — hybrid_preparatory view, structurally excluded
 *   - academic_observers: observer (analytical/civilizational/analytical/universal) — analyze the dispute from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.72).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.65).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.72).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Authorized Symbolic Transformation of Temple Sacrifice").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'd4b9abab-3ba9-4a98-bf29-c3ee9af426cc').
narrative_ontology:cs_kernel_codification('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', formalized).
narrative_ontology:cs_authority_grounding('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', lineage).
narrative_ontology:cs_interpretation_layer_present('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc').
narrative_ontology:cs_reading_relation('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_axiom('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', foundational, authorized_transformation_legitimacy).
narrative_ontology:cs_axiom_status(authorized_transformation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', authorized_transformation_legitimacy, conventional).
narrative_ontology:cs_axiom('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', foundational, rabbinic_legislative_power_over_divine_command).
narrative_ontology:cs_axiom_status(rabbinic_legislative_power_over_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', rabbinic_legislative_power_over_divine_command, conventional).
narrative_ontology:cs_reference_frame('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', talmudic_sacrificial_substitution).
narrative_ontology:cs_drift_state('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', contemporary_zionist_restoration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d4b9abab-3ba9-4a98-bf29-c3ee9af426cc', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, prayer_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, material_performance_adherents).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, authorized_transformation_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, rabbinic_legislative_authority).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, prayer_as_service_substitute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rabbinic leadership (Sanhedrin lineage, poskim, halakhic consensus) that declared prayer and study as the authorized fulfillment of the sacrificial commandment. They maintain this transformation through halakhic decision-making, communal policy, ordination standards, and educational curricula. Their authority to redefine the commandment's instantiation is the constraint's central claim. Exit from this role means leaving the rabbinic office — analytically possible but structurally identity-dissolving.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority, agenda_setter,
    institutional, generational, analytical, universal).

% Mainstream Jewish communities (Orthodox, Conservative, Reform, Reconstructionist) that practice prayer and study as the authorized form of divine service. They benefit from an accessible, portable, authorized worship practice that maintains continuity with tradition. Exit options are mobile — they can change denominations or leave Judaism, but within the framework they are coordinated beneficiaries.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, prayer_communities, beneficiary,
    organized, biographical, mobile, global).

% Groups and individuals (Temple Mount activists, Third Temple movements, some Religious Zionist factions, Karaite-influenced positions) who hold that the sacrificial commandment requires material instantiation and cannot be transformed. Their religious identity is fused to material performance — exit from this position means not just changing practice but dissolving the self-concept constituted by 'awaiting restoration.' They bear the cost of the transformation: marginalization, halakhic delegitimization, exclusion from mainstream communal institutions.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, material_performance_adherents, payer,
    organized, biographical, identity_locked, global).

% Those holding the hybrid_preparatory view: study maintains the commitment in suspended state as preparatory exercise for messianic restoration. They are structurally excluded from the authorized consensus — their framing is neither the authorized transformation nor the rejected performance_only position. They cannot exit the tension: trapped between the authorized practice they participate in and the restoration they await.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, messianic_restorationists, excluded,
    moderate, generational, trapped, global).

% Scholars of halakha, history of religions, anthropology of Judaism, and philosophy of law who analyze the dispute from outside. They neither collect from nor pay into the constraint. Their analytical seat sees the full structural field: the authority claim, the coordination function, the extraction asymmetry, the identity-lock dynamics.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Jewish sacrificial commitment across 2000 years without Temple by providing an authorized substitute practice (prayer/study) that preserves communal continuity, liturgical structure, and theological coherence.
% TRANSFER_FUNCTION: Moves legitimization from material Temple service (animal offerings, priestly mediation, geographic centralization) to rabbinically-authorized prayer and study (verbal offering, lay participation, portable practice). The authority structure gains definitional power over what the divine command requires; material_performance_adherents lose the legitimacy of their non-negotiable commitment.
% ABSENT_VOICES: Those who would rebuild the Temple and resume material sacrifice immediately (Third Temple activists, some messianic groups) are structurally excluded by both political reality (Muslim control of Temple Mount) and rabbinic consensus (halakhic prohibitions on ascending Temple Mount, uncertainty of priestly lineage). They would argue the transformation is unauthorized drift but are kept out of the halakhic conversation by the same authority structure.
% DISAPPEARANCE_RATIONALE: If the authorized transformation vanished overnight, mainstream Jewish practice would face an existential crisis: either demand material performance (impossible without Temple, priesthood, political sovereignty) or accept that the sacrificial commandment is unfulfillable (theological rupture). The Jewish religious world would fundamentally reorganize around this void.
% FOUNDING_PROBLEM: How to maintain the divine commandment of sacrificial worship when the Temple's destruction (70 CE) made material performance impossible — threatening the continuity of the covenantal relationship itself.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by classical sources outside the beneficiary set: Josephus (Jewish War 6.2.1) documents the crisis; Talmudic sages (Yoma 39b, Rosh Hashanah 31b) record the trauma; Maimonides (Guide 3:32, 3:46) analyzes the philosophical problem. However, the 'authorized transformation' RESOLUTION is attested primarily by the authority structure itself (Talmud Berakhot 26b: 'tefillot k'neged temidim tiknum' — prayers instituted corresponding to daily offerings). No external corroborator confirms the authorization; the authorization claim is self-warranted by the authority.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the authority structure claims power to redefine divine command — if unauthorized, this is authority overreach extracting legitimacy from those who hold the command as immutable. Suppression (0.65) is moderate-high: the halakhic consensus actively marginalizes performance_only and hybrid_preparatory views through institutional gatekeeping (rabbinic ordination, communal acceptance, conversion standards). Theater ratio (0.45) reflects performative continuity rhetoric ('prayer replaces sacrifice') masking the substantive transformation. Accessibility collapse (0.82) is high because the authoritative declaration collapses the alternative (material performance) as halakhically impossible/forbidden. Resistance (0.48) is moderate: ongoing contestation from Temple Mount movements, Third Temple ideology, and academic critique, but contained within minority positions. The claimed type tangled_rope reflects genuine coordination (maintaining Jewish worship continuity) fused with asymmetric extraction (authority redefining command).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic_authority) experiences this as legitimate coordination solving an existential crisis (Temple destruction). The payer seat (material_performance_adherents) experiences it as authority overreach extracting their non-negotiable commitment. The beneficiary seat (prayer_communities) experiences it as accessible authorized practice. The engine computes this divergence from the structural data — the declared roles, power levels, exit options, and identity-lock dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic_authority sits at the beneficiary end (d ≈ 0.15): it gains definitional power, institutional centrality, and legitimacy from the transformation. Prayer_communities are near-symmetric beneficiaries (d ≈ 0.35): they gain accessible authorized practice but cede interpretive autonomy. Material_performance_adherents are at the target end (d ≈ 0.85): identity-locked to material performance, they bear the full cost of the redefinition with trapped exit (leaving mainstream Orthodoxy severs communal/religious identity). Messianic_restorationists are excluded (d ≈ 0.9): their preparatory framing is structurally excluded from the authorized consensus. Academic_observers are analytical (d = 0.5): neither collecting nor paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining sacrificial commitment without Temple) was live at T=0 (70 CE). The symbolic_transformation reading claims the problem is SOLVED by authorized transformation (founding_problem_status: contested — the authority says solved; material_adherents say persists until Temple rebuilt). Mandatrophy is NOT resolved: the arrangement persists but the founding problem's status is contested. The transformation's authorization claim prevents mislabeling coordination as pure extraction — if authorization is genuine, it's tangled_rope; if unauthorized drift, it trends toward snare. The omega variables capture this irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorization_legitimacy,
    'Was the transformation from material sacrifice to prayer/study genuinely authorized by legitimate halakhic authority, or does it constitute unauthorized drift by an authority structure claiming power to redefine divine command?',
    'Historical-philological analysis of Talmudic sources (Berakhot 26b, Ta''anit 27b, Megillah 31b) and Maimonidean codification (Hilkhot Tefillah 1:1-5) to determine whether the sources describe authorization or post-hoc rationalization.',
    'If unauthorized drift, extractiveness is higher (authority overreach); if genuinely authorized, the coordination function is legitimate and extraction lower. Changes classification from snare toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authorization_legitimacy, conceptual, 'Whether the symbolic transformation carries authentic halakhic authorization or represents authority overreach.').

omega_variable(
    victim_status_of_material_adherents,
    'Do those who hold material performance as non-negotiable constitute genuine victims of extraction, or have they self-excluded by rejecting the authorized halakhic consensus?',
    'Sociological study of Temple Mount activist groups, Third Temple movements, and their relationship to mainstream Orthodox authority structures; analysis of whether exclusion is imposed or chosen.',
    'If genuine victims, the constraint extracts from a coherent group with identity-locked exit; if self-excluding, the victim set is constructed and extraction metrics overstate asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_of_material_adherents, empirical, 'Whether material_performance_adherents are extracted-from victims or self-excluding dissenters.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of material performance views structural (halakhic consensus, social ostracism, institutional gatekeeping) or internalized (theological conviction that material performance is currently forbidden)?',
    'Post-exit trajectory analysis: if material performance advocates who leave mainstream Orthodoxy continue to experience suppression, it is structural; if suppression dissolves upon exit, it is substantially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the target carries the suppression with them. Affects theater_ratio and resistance calibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for material performance dissent.').

omega_variable(
    kernel_reading_framing,
    'Does the symbolic_transformation reading frame the kernel as (a) the sacrificial commandment itself transformed, or (b) the rabbinic authority''s legislative power to transform commandments?',
    'Comparative analysis of how each sibling reading (hybrid_preparatory, performance_only, study_as_exercise) locates the kernel — in the divine command vs. in the authority structure — and whether this reading''s axioms commit it to one framing.',
    'If framing (b), the constraint is about authority power (higher extractiveness); if framing (a), it is about commandment continuity (lower extractiveness). Routes to different cs_pattern classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Commitment-system framing ambiguity: commandment-continuity vs. authority-legitimacy as the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(temp_tr_t130, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 130, 0.2).
narrative_ontology:measurement(temp_tr_t1030, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1030, 0.3).
narrative_ontology:measurement(temp_tr_t1868, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1868, 0.4).
narrative_ontology:measurement(temp_tr_t1948, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1948, 0.42).
narrative_ontology:measurement(temp_tr_t1967, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1967, 0.44).
narrative_ontology:measurement(temp_tr_t2024, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(temp_be_t130, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 130, 0.35).
narrative_ontology:measurement(temp_be_t1030, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1030, 0.55).
narrative_ontology:measurement(temp_be_t1868, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1868, 0.65).
narrative_ontology:measurement(temp_be_t1948, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement(temp_be_t1967, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(temp_be_t2024, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(temp_su_t130, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 130, 0.35).
narrative_ontology:measurement(temp_su_t1030, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1030, 0.5).
narrative_ontology:measurement(temp_su_t1868, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1868, 0.58).
narrative_ontology:measurement(temp_su_t1948, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1948, 0.62).
narrative_ontology:measurement(temp_su_t1967, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1967, 0.64).
narrative_ontology:measurement(temp_su_t2024, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__symbolic_transformation, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'sacrifice after Temple destruction' colloquial label into four structurally distinct readings. symbolic_transformation claims authorized redefinition (high extractiveness if unauthorized). study_as_exercise claims study IS the command (lower extractiveness). hybrid_preparatory claims suspended state (lowest extractiveness). performance_only rejects transformation (mountain-like if genuine natural law, but extractive if enforced consensus). They share the kernel but instantiate different constraints with different ε, beneficiaries, victims, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__symbolic_transformation, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
