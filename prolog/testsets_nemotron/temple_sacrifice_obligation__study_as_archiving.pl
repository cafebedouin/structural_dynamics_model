% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Study as Archival Preservation of Temple Sacrifice Obligation
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   After the Second Temple's destruction (70 CE), the halakhic obligation to
 *   offer sacrifices became physically unperformable. The study-as-archiving
 *   reading maintains that the obligation remains binding in principle but
 *   can only be fulfilled through study of its laws — study preserves the
 *   knowledge for future messianic restoration while acknowledging that the
 *   entire post-Temple period constitutes non-compliance with the divine
 *   command. This reading instantiates moderate extractiveness: the authority
 *   structure (rabbinic courts, scholarly institutions) maintains the binding
 *   status of an unperformable law, extracting scholarly labor and communal
 *   deference while the obligation-bearing community bears the violation. The
 *   constraint coordinates preservation of restorable knowledge but
 *   asymmetrically extracts from those who cannot fulfill what remains
 *   binding.
 *
 * KEY AGENTS:
 *   - halakhic_authority_structure: agenda_setter (institutional/analytical) — maintains binding status, defines study as fulfillment-surrogate
 *   - scholarly_institutions: beneficiary (organized/biographical) — receive resources, status, and authority from managing the archival project
 *   - continuity_practitioners: beneficiary (moderate/biographical) — gain identity coherence from participation in the preservation project
 *   - divine_command_itself: victim (analytical/universal) — the unfulfilled command bears the violation across the entire post-Temple period
 *   - obligation_bearing_community: payer (powerless/biographical) — bears the violation and defers to authority structure's maintenance of binding status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.52).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.38).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.52).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Study as Archival Preservation of Temple Sacrifice Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '470ca067-fad9-46a3-a342-c1c6985e403b').
narrative_ontology:cs_kernel_codification('470ca067-fad9-46a3-a342-c1c6985e403b', fixed_text).
narrative_ontology:cs_authority_grounding('470ca067-fad9-46a3-a342-c1c6985e403b', lineage).
narrative_ontology:cs_interpretation_layer_present('470ca067-fad9-46a3-a342-c1c6985e403b').
narrative_ontology:cs_reading_relation('470ca067-fad9-46a3-a342-c1c6985e403b', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('470ca067-fad9-46a3-a342-c1c6985e403b', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('470ca067-fad9-46a3-a342-c1c6985e403b', foundational, obligation_remains_binding_despite_unperformability).
narrative_ontology:cs_axiom_status(obligation_remains_binding_despite_unperformability, holdable).
narrative_ontology:cs_axiom_grounding('470ca067-fad9-46a3-a342-c1c6985e403b', obligation_remains_binding_despite_unperformability, deontological).
narrative_ontology:cs_axiom('470ca067-fad9-46a3-a342-c1c6985e403b', foundational, study_preserves_but_does_not_fulfill).
narrative_ontology:cs_axiom_status(study_preserves_but_does_not_fulfill, holdable).
narrative_ontology:cs_axiom_grounding('470ca067-fad9-46a3-a342-c1c6985e403b', study_preserves_but_does_not_fulfill, deontological).
narrative_ontology:cs_axiom('470ca067-fad9-46a3-a342-c1c6985e403b', secondary, violation_is_structural_not_accidental).
narrative_ontology:cs_axiom_status(violation_is_structural_not_accidental, holdable).
narrative_ontology:cs_axiom_grounding('470ca067-fad9-46a3-a342-c1c6985e403b', violation_is_structural_not_accidental, deontological).
narrative_ontology:cs_reference_frame('470ca067-fad9-46a3-a342-c1c6985e403b', sinai_mandate_comprehensive_halakhic_authority).
narrative_ontology:cs_drift_state('470ca067-fad9-46a3-a342-c1c6985e403b', post_temple_rabbinic_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('470ca067-fad9-46a3-a342-c1c6985e403b', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, halakhic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, scholarly_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, continuity_practitioners).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, divine_command_itself).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, obligation_bearing_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rabbinic court system and poskim hierarchy that adjudicates halakhic status. Defines study as the archival fulfillment-surrogate, maintains the obligation's binding status, and controls which alternative readings (suspension, occupation) are treated as legitimate. Collects interpretive authority and communal deference from this role. Exit is analytical — they could revise the framework but doing so would dissolve their authority's grounding in the kernel.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, halakhic_authority_structure, agenda_setter,
    institutional, generational, analytical, universal).

% Yeshivot, kollelim, and academic centers that organize sacrificial law study. Receive communal resources, institutional status, and student bodies from managing the archival project. Their coordination function is genuine (preserving complex technical knowledge), but their institutional survival depends on the obligation's binding status. Exit is constrained — they could pivot to other curricula but would lose the distinctive authority-granting role.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, scholarly_institutions, beneficiary,
    organized, biographical, constrained, global).

% Individuals and families whose religious identity is constituted through participation in the sacrificial study system. Gain identity coherence, communal belonging, and meaning from the preservation project. Their exit is identity-locked: leaving means losing the self-concept and relational world built around the obligation system. They bear the violation personally but experience it as meaningful participation rather than pure extraction.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, continuity_practitioners, beneficiary,
    moderate, biographical, identity_locked, global).

% The Torah command to offer sacrifices, which remains binding but unfulfilled across the entire post-Temple period. In this reading, the command itself is the structural victim — it is violated continuously while the authority structure maintains its binding status. It has no exit, no voice, and no agency; its victim-status is the reading's structural claim about what the constraint extracts from.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, divine_command_itself, payer,
    analytical, civilizational, trapped, universal).

% The collective of halakhically-obligated Jews who bear the violation of an unperformable command. They defer to the authority structure's maintenance of binding status, fund the scholarly institutions, and participate in the study system. Their exit is identity-locked — leaving the halakhic framework means losing communal, familial, and personal identity. They pay the extraction (the violation itself) while the coordination benefits (preserved knowledge) accrue to the system that maintains their subjection.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, obligation_bearing_community, payer,
    powerless, biographical, identity_locked, global).

% Historical and contemporary voices arguing that the obligation is suspended rather than binding-unfulfilled. They are structurally excluded from the authority structure's interpretive control — their reading is not engaged on merits but marginalized as 'not halakhically operative.' Their exclusion is what the enforcement machinery maintains.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, messianic_suspension_advocates, excluded,
    moderate, generational, constrained, global).

% Voices (including some rishonim and contemporary scholars) arguing that sacrificial study constitutes legitimate fulfillment, not mere preservation. They are excluded from the binding-status maintenance because their reading would reduce extractiveness (violation ends) and thus undermine the authority structure's extraction basis. Their exclusion is structural, not merely disagreement.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, study_as_occupation_proponents, excluded,
    moderate, generational, constrained, global).

% Scholars of halakha and religious studies who analyze the constraint from outside the obligation-bearing community. They see the full structural picture — the kernel contest, the three readings' divergent extraction profiles, the authority structure's maintenance mechanisms — but bear no costs and collect no benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, academic_halakhic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the complex technical knowledge of sacrificial law (orders, quantities, procedures, disqualifications) across the post-Temple period so that restoration is possible when the Temple is rebuilt. Solves the genuine coordination problem of intergenerational knowledge transmission for a practice that cannot be performed but must not be lost.
% TRANSFER_FUNCTION: Moves scholarly labor, communal resources, and deference-to-authority from the obligation-bearing community and scholarly practitioners to the halakhic authority structure and its institutions, as the price of maintaining the unperformable obligation's binding status. The divine command receives the violation (extraction target); the authority structure receives the coordination rents.
% ABSENT_VOICES: The messianic suspension reading (which would end the violation by suspending the obligation) and the study-as-occupation reading (which would end the violation by redefining study as fulfillment) are both structurally excluded from the authority structure's interpretive control. They would object that the binding-status maintenance serves authority extraction, not divine command, but they are kept out by the same interpretive gatekeeping the constraint rides on.
% DISAPPEARANCE_RATIONALE: If the study-as-archiving constraint vanished overnight, the obligation's binding status would collapse: either the community would adopt messianic suspension (obligation paused), study-as-occupation (study = fulfillment), or abandon the sacrificial framework entirely. The scholarly institutions would lose their distinctive authority-granting role. The halakhic authority structure would lose a primary anchor of its interpretive legitimacy. The entire post-Temple halakhic architecture would reorganize around a different reading of the kernel.
% FOUNDING_PROBLEM: After the Temple's destruction, the sacrificial system — the center of biblical worship — became physically impossible. The founding problem was how to preserve the Torah's sacrificial commandments as binding law while acknowledging their current unperformability, without dissolving the halakhic system's claim to comprehensive divine authority.
% FOUNDING_PROBLEM_CORROBORATION: The halakhic authority structure attests the problem is live (Temple not yet rebuilt, knowledge must be preserved). Messianic suspension advocates attest the problem is misframed (suspension solves it without violation). Study-as-occupation proponents attest the problem is solved differently (study IS fulfillment). Academic historians of halakha corroborate that the archival framing emerged historically as the dominant but not inevitable solution — the Yerushalmi and Bavli record competing framings. No single corroborating source outside the beneficiary set resolves the contest.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects the structural fact that an unperformable obligation remains binding, extracting compliance-cost (the violation itself) from the community while the authority structure and scholarly institutions collect the coordination benefits. Suppression (0.38) is moderate: the binding status is maintained through interpretive authority and communal normativity rather than overt coercion, but alternatives (messianic suspension, occupational fulfillment) are structurally marginalized. Theater ratio (0.28) captures that much scholarly activity is genuine preservation work, but a growing share serves to legitimate the authority structure's maintenance of binding status. Accessibility collapse (0.42) reflects that alternatives exist conceptually but are excluded by the authority structure's interpretive control. Resistance (0.35) is present but channeled: messianic suspension and study-as-occupation are live readings but cannot displace the archival framing within the authority structure.
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic authority seat, the constraint is genuine coordination (preserving restorable knowledge) with moderate extraction (the violation is acknowledged). From the obligation-bearing community seat, the same structure operates as extraction: they bear an unfulfillable violation while the authority structure collects the benefits of binding-status maintenance. The engine computes this divergence from the structural data — the scholarly institutions and continuity practitioners sit between, benefiting from the coordination while sharing the violation's burden.
 *
 * DIRECTIONALITY LOGIC:
 *   The halakhic authority structure is the primary beneficiary (collects interpretive authority, defines the obligation's terms, maintains binding status — d near beneficiary end). Scholarly institutions and continuity practitioners are secondary beneficiaries (receive resources, status, identity coherence — d moderately low). The divine command itself is the structural victim (bears the violation across the entire period — d at target end). The obligation-bearing community are the payers (bear the violation, defer to authority, constrained exit — d near target end). The community's exit is identity-locked: leaving the halakhic framework means losing the communal-religious identity constituted by the obligation system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving sacrificial knowledge for restoration) remains live in principle but has shifted: the coordination function (knowledge preservation) is genuine, but the extraction function (maintaining binding status of unperformable law) has accumulated. The mandate has not atrophied — the restoration horizon keeps the coordination live — but the extraction-to-coordination ratio has risen over the post-Temple period. This is tangled_rope, not snare, because the coordination function would persist even if extraction were reduced (knowledge would still need preserving for restoration), and not scaffold because no sunset clause exists (restoration is indefinite).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the study-as-archiving reading the only defensible framing of the post-Temple sacrificial obligation, or does it compete with study-as-occupation and messianic-suspension as live alternatives?',
    'Comparative structural analysis of the three readings'' internal coherence and their divergent implications for obligation status, authority structure, and community practice.',
    'If study-as-archiving is uniquely coherent, it anchors the obligation''s classification; if it coexists with alternatives, the kernel''s binding force is contested and extraction metrics reflect authority maintenance rather than obligation fulfillment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the archival framing forecloses or merely competes with sibling readings of the same kernel.').

omega_variable(
    divine_command_as_victim,
    'Does treating the unfulfilled divine command as a structural victim category make analytic sense, or does it reify a theological claim into a constraint-measurement primitive?',
    'Test whether the victim-status of the divine command generates measurable extraction effects on the obligation-bearing community independent of the authority structure''s enforcement.',
    'If the divine command is a genuine victim, extractiveness tracks theological violation; if it is a cover for authority extraction, the constraint is snare-flavored regardless of claimed coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_command_as_victim, conceptual, 'Ontological status of the divine command as victim in the constraint structure.').

omega_variable(
    binding_status_maintenance_mechanism,
    'Is the obligation''s binding status maintained by genuine communal acceptance, or by the authority structure''s active suppression of suspension/fulfillment alternatives?',
    'Historical analysis of halakhic discourse: when alternatives (messianic suspension, occupational fulfillment) were proposed, were they engaged on merits or excluded by structural authority?',
    'If maintained by acceptance, the constraint leans rope/tangled_rope with genuine coordination; if maintained by suppression, it leans snare with authority extraction as the persistence engine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_status_maintenance_mechanism, empirical, 'Mechanism sustaining the obligation''s binding status across the post-Temple period.').

omega_variable(
    study_as_coordination_vs_extraction,
    'Does the scholarly infrastructure of sacrificial study serve a genuine coordination function (preserving restorable knowledge) or does it primarily extract scholarly labor and communal resources to maintain an unfulfillable obligation''s binding status?',
    'Measure the ratio of preservation-directed scholarly activity to authority-maintenance activity across the post-Temple period; assess whether the coordination function would persist if the binding-status claim were relaxed.',
    'If coordination is genuine and extraction moderate, tangled_rope holds; if coordination is cover for extraction, the constraint reclassifies toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_coordination_vs_extraction, empirical, 'Whether the study infrastructure''s coordination function is genuine or a cover for authority extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.12).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_tr_t50, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 50, 0.18).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_tr_t100, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 100, 0.22).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_tr_t150, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 150, 0.25).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_tr_t200, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 200, 0.27).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_tr_t250, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 250, 0.28).

% Extraction over time
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_be_t50, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_be_t100, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_be_t150, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 150, 0.5).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_be_t200, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_be_t250, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 250, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_su_t50, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 50, 0.3).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_su_t100, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 100, 0.34).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_su_t150, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 150, 0.36).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_su_t200, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 200, 0.38).
narrative_ontology:measurement(temple_sacrifice_obligation__study_as_archiving_su_t250, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 250, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_archiving, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% Part of the temple_sacrifice_obligation constraint family. This reading (study_as_archiving) treats the post-Temple period as violation-with-preservation; study_as_occupation treats study as fulfillment (lower extractiveness); messianic_suspension treats obligation as suspended (near-zero extractiveness). All three share the same kernel but instantiate different constraints with different ε values, beneficiary/victim structures, and authority-maintenance mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_archiving, analytical, 0.95).
constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_archiving, powerless, 0.85).
constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_archiving, institutional, 0.15).
constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_archiving, organized, 0.25).
constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_archiving, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
