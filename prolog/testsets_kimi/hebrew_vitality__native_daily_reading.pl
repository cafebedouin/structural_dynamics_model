% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Hebrew Vitality as Native Daily Generation
 *   domain: sociolinguistic/cultural/religious
 *
 * SUMMARY:
 *   This constraint instantiates the native_daily_reading of the
 *   hebrew_vitality kernel: the claim that only native daily generation
 *   constitutes linguistic vitality, while ritual liturgical recitation is
 *   merely preservation. The reading emerged from the Zionist language
 *   revival project and was institutionalized through state education,
 *   military language policy, and lexical engineering. It functions as a
 *   tangled rope because it genuinely coordinated the unprecedented
 *   reconstruction of a modern Hebrew vernacular, but simultaneously extracts
 *   legitimacy and resources from the pre-existing liturgical tradition by
 *   declaring it not life. The structural delta is moderate Îµ: the
 *   institutional enforcement required to construct and maintain a native
 *   speaker community in a non-mother-tongue context involved real coercion,
 *   while the liturgical tradition pays the cost of desacralization and
 *   delegitimization.
 *
 * KEY AGENTS:
 *   - Zionist state-building project: agenda-setter (institutional/generational) â sets the vitality criterion and captures state-building legitimacy
 *   - Liturgical tradition: payer (organized/civilizational) â bears the cost of being reclassified as preservation rather than life
 *   - Hebrew renaissance planners: agenda-setter (powerful/generational) â operationalized the criterion through language planning
 *   - Secular Israeli speakers: beneficiary (moderate/biographical) â receive the vernacular coordination benefit
 *   - Mizrahi religious communities: excluded (moderate/generational) â their pre-existing Hebrew-textual daily practice was written out of the vitality narrative
 *   - Comparative sociolinguists: observer (analytical/civilizational) â see the criterion as one contested reading among many
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.58).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.68).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Vitality as Native Daily Generation").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistic/cultural/religious").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '3b599754-9e19-4272-bf62-7ccbd25896fa').
narrative_ontology:cs_kernel_codification('3b599754-9e19-4272-bf62-7ccbd25896fa', distributed).
narrative_ontology:cs_authority_grounding('3b599754-9e19-4272-bf62-7ccbd25896fa', extraction).
narrative_ontology:cs_interpretation_layer_present('3b599754-9e19-4272-bf62-7ccbd25896fa').
narrative_ontology:cs_reading_relation('3b599754-9e19-4272-bf62-7ccbd25896fa', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('3b599754-9e19-4272-bf62-7ccbd25896fa', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('3b599754-9e19-4272-bf62-7ccbd25896fa', foundational, native_generation_sole_vitality_criterion).
narrative_ontology:cs_axiom_status(native_generation_sole_vitality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('3b599754-9e19-4272-bf62-7ccbd25896fa', native_generation_sole_vitality_criterion, empirically_contingent).
narrative_ontology:cs_axiom('3b599754-9e19-4272-bf62-7ccbd25896fa', secondary, liturgical_practice_non_generative).
narrative_ontology:cs_axiom_status(liturgical_practice_non_generative, holdable).
narrative_ontology:cs_axiom_grounding('3b599754-9e19-4272-bf62-7ccbd25896fa', liturgical_practice_non_generative, empirically_contingent).
narrative_ontology:cs_reference_frame('3b599754-9e19-4272-bf62-7ccbd25896fa', hebrew_native_speaker_statehood).
narrative_ontology:cs_drift_state('3b599754-9e19-4272-bf62-7ccbd25896fa', contemporary_post_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3b599754-9e19-4272-bf62-7ccbd25896fa', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, secular_israeli_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established and enforces the institutional and ideological framework defining Hebrew vitality exclusively through native daily generation. Funds vernacular schools, military language units, and lexical modernization while compartmentalizing liturgical study as heritage preservation. Derives state legitimacy from the revival narrative.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, constrained, national).

% Bears the epistemic cost of being reclassified from co-equal Hebrew practice to mere preservation. Liturgical continuity across diaspora communities is delegitimized as insufficient for vitality, forcing defensive justification or assimilation to secular vernacular norms.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition, payer,
    organized, civilizational, identity_locked, global).

% Lexicographers, educators, and poets who engineered the modern Hebrew vocabulary and grammatical norms necessary for native daily use. They operationalized the native-generation criterion through institutional language planning and pedagogical design.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, hebrew_renaissance_planners, agenda_setter,
    powerful, generational, mobile, national).

% Inherit a functional modern vernacular enabling full participation in economic, military, and political life. Their daily speech is cited as empirical proof that Hebrew is alive, reinforcing the exclusivity claim.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, secular_israeli_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Maintained Hebrew textual and liturgical practices embedded in daily religious life prior to and alongside the Ashkenazi-led revival. Their model of Hebrew vitality was marginalized by the state-building project's exclusivity criterion and written out of the national narrative.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, mizrahi_religious_communities, excluded,
    moderate, generational, identity_locked, regional).

% Analyze Hebrew revitalization against Irish, Welsh, and Sanskrit cases. They note that the native-generation criterion is one contested index among many and that its institutional dominance reflects political projects rather than linguistic necessity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, comparative_sociolinguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the massive institutional project of modern Hebrew reconstruction by providing a single, measurable success criterion: the presence of native daily speakers. Aligns lexicographers, educators, state bureaucracies, and military institutions toward a unified vernacular standard.
% TRANSFER_FUNCTION: Moves definitional authority, state funding, and cultural prestige from liturgical continuity to vernacular generation; transfers the burden of proof onto religious communities to justify their practice as vitality rather than preservation.
% ABSENT_VOICES: Mizrahi and Sephardic communities for whom Hebrew textual practice was already woven into daily religious life; anti-Zionist religious scholars who view vernacular Hebrew as profane; Arabic-speaking Jewish communities whose multilingualism was erased by the monolingual native-speaker ideal.
% DISAPPEARANCE_RATIONALE: If the native-generation exclusivity vanished, state language funding and prestige would redistribute toward liturgical maintenance, the ideological justification for specific lexical engineering would weaken, and the hierarchy between living vernacular and preserved liturgy would flatten. Hebrew would still be spoken, but its vitality would be contestable through multiple criteria.
% FOUNDING_PROBLEM: Jewish communities in the late 19th and early 20th centuries lacked a shared modern vernacular for governance, military, commerce, and secular literature; Hebrew existed primarily as a liturgical and textual language across dispersed communities.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and state educational institutions corroborate the historical problem from within the beneficiary framework. Independent sociolinguists and diaspora religious historians outside the state-building project corroborate the historical lack of a modern vernacular but dispute that native-generation exclusivity was the necessary or only solution, citing alternative models of functional expansion.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint genuinely produced a coordinated vernacular revival, but a substantial fraction of its persistence serves to maintain an ideological monopoly over what counts as living Hebrew. Suppression (0.68) is high because the criterion required active institutional suppression of competing vitality modelsâparticularly the liturgical model and diaspora multilingualismâto secure its dominance. Theater ratio (0.45) is moderate: much state language activity is functional, but a growing share performs the revival narrative for legitimacy purposes even after native speakerhood is achieved. Accessibility collapse (0.60) reflects that alternative vitality criteria are institutionally marginalized but not erased; resistance (0.55) comes from religious communities and post-Zionist scholars who contest the exclusivity claim.
 *
 * PERSPECTIVAL GAP:
 *   From the state-building seat, the constraint is a rope: it solved an extraordinary coordination problem and produced a shared national language. From the liturgical seat, it is a snare: the coordination story is cover for delegitimizing a millennial practice. The engine computes this divergence from the structural data. The authored claim of tangled_rope does not adjudicate the dispute but identifies that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project sits at the beneficiary end: the constraint subsidizes its legitimacy and territorial-cultural claims by producing proof of national rebirth through living language. Secular Israeli speakers are near-symmetric beneficiaries of coordination. The liturgical tradition is the primary target: its identity-locked exit amplifies effective extraction. Mizrahi communities experience differentiated directionality as structurally excluded rather than directly taxed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine collective-action problem solved: Hebrew did lack a modern vernacular, and the native-generation criterion coordinated thousands of agents toward a shared standard. It prevents mislabeling as pure coordination (rope) by requiring the victim declaration: the liturgical tradition is not merely a non-beneficiary but a payer whose practice is actively devalued. The temporal measurements show extraction peaking mid-century and slightly declining as the constraint naturalizes, consistent with successful tangled-rope institutionalization rather than snare intensification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_naturalness,
    'Is native-generation exclusivity a natural law of language vitality, or a constructed criterion serving the Zionist state-building project?',
    'Comparative analysis with Irish, Welsh, and Sanskrit revitalization or preservation models to determine whether native generation is universally necessary or one index among many.',
    'If empirically universal, the constraint shifts toward Mountain or Rope; if constructed and contingent, it confirms Tangled Rope or Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_naturalness, empirical, 'Whether the native-generation criterion is a linguistic universal or an ideological construct').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of liturgical vitality structural (funding, institutional gatekeeping, legal status) or internalized (shame, self-censorship within religious communities)?',
    'Post-exit trajectory analysis: tracking liturgical Hebrew practice and self-identification among communities that have left the Israeli state framework.',
    'If internalized, effective suppression exceeds the structural measure; the constraint operates through identity capture rather than only institutional coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of liturgical vitality claims').

omega_variable(
    kernel_reading_scope,
    'Does this reading''s exclusivity claim apply universally to all language vitality assessments, or is it one authority''s interpretation of the Hebrew vitality kernel?',
    'Cross-linguistic corpus analysis of vitality indices; examination of whether the same institutions apply the criterion consistently to other languages.',
    'If universal, the reading claims Mountain-like status; if kernel-specific, it confirms distributed kernel with authority grounded in extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Scope of the native-generation criterion across kernels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__native_daily_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__native_daily_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__native_daily_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__native_daily_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__native_daily_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__native_daily_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(hebr_tr_t120, hebrew_vitality__native_daily_reading, theater_ratio, 120, 0.46).
narrative_ontology:measurement(hebr_tr_t140, hebrew_vitality__native_daily_reading, theater_ratio, 140, 0.45).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__native_daily_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__native_daily_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__native_daily_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__native_daily_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__native_daily_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__native_daily_reading, base_extractiveness, 100, 0.59).
narrative_ontology:measurement(hebr_be_t120, hebrew_vitality__native_daily_reading, base_extractiveness, 120, 0.57).
narrative_ontology:measurement(hebr_be_t140, hebrew_vitality__native_daily_reading, base_extractiveness, 140, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__native_daily_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hebr_su_t20, hebrew_vitality__native_daily_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__native_daily_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(hebr_su_t60, hebrew_vitality__native_daily_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(hebr_su_t80, hebrew_vitality__native_daily_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(hebr_su_t100, hebrew_vitality__native_daily_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(hebr_su_t120, hebrew_vitality__native_daily_reading, suppression_requirement, 120, 0.65).
narrative_ontology:measurement(hebr_su_t140, hebrew_vitality__native_daily_reading, suppression_requirement, 140, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The hebrew_vitality kernel decomposes into three structurally distinct constraints: native_daily_reading (this file), liturgical_reading, and hybrid_continuity_reading. Each reading has a different epsilon, beneficiary-victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
