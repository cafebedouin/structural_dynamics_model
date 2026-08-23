% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Native Generation as Sole Criterion of Hebrew Vitality
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint story models the 'native daily reading' of the contested
 *   Hebrew vitality kernel: the claim that only native, intergenerational,
 *   daily use constitutes true language vitality, while liturgical recitation
 *   is mere preservation. This reading was constructed by the Zionist
 *   state-building project (Ben-Yehuda, the Yishuv leadership, later the
 *   State of Israel) as the operational criterion for Hebrew's successful
 *   revival. It coordinates massive institutional effort — education,
 *   language planning, immigration absorption — around a single measurable
 *   benchmark. Simultaneously, it extracts from liturgical tradition
 *   communities by defining their primary mode of Hebrew engagement as 'not
 *   vitality,' thereby desacralizing the language and transferring its
 *   authority to state institutions. The constraint requires active
 *   enforcement (school curricula, Academy of Hebrew Language prescriptions,
 *   media regulation) and persists because the coordination function (unified
 *   national language) remains live while the extraction (liturgical
 *   marginalization) is structural.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.55).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.65).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Native Generation as Sole Criterion of Hebrew Vitality").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '3918550a-b1a8-43b1-b036-671dfe7d00e2').
narrative_ontology:cs_kernel_codification('3918550a-b1a8-43b1-b036-671dfe7d00e2', formalized).
narrative_ontology:cs_authority_grounding('3918550a-b1a8-43b1-b036-671dfe7d00e2', extraction).
narrative_ontology:cs_interpretation_layer_present('3918550a-b1a8-43b1-b036-671dfe7d00e2').
narrative_ontology:cs_reading_relation('3918550a-b1a8-43b1-b036-671dfe7d00e2', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('3918550a-b1a8-43b1-b036-671dfe7d00e2', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('3918550a-b1a8-43b1-b036-671dfe7d00e2', foundational, native_generation_sole_vitality_criterion).
narrative_ontology:cs_axiom_status(native_generation_sole_vitality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('3918550a-b1a8-43b1-b036-671dfe7d00e2', native_generation_sole_vitality_criterion, deontological).
narrative_ontology:cs_axiom('3918550a-b1a8-43b1-b036-671dfe7d00e2', secondary, vernacular_reconstruction_as_national_imperative).
narrative_ontology:cs_axiom_status(vernacular_reconstruction_as_national_imperative, holdable).
narrative_ontology:cs_axiom_grounding('3918550a-b1a8-43b1-b036-671dfe7d00e2', vernacular_reconstruction_as_national_imperative, instrumental).
narrative_ontology:cs_reference_frame('3918550a-b1a8-43b1-b036-671dfe7d00e2', native_intergenerational_transmission).
narrative_ontology:cs_drift_state('3918550a-b1a8-43b1-b036-671dfe7d00e2', contemporary_israeli_hebrew, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3918550a-b1a8-43b1-b036-671dfe7d00e2', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, native_hebrew_speakers).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, vernacular_reconstruction_as_national_imperative).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, intergenerational_transmission_as_vitality_benchmark).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established and enforces the native-generation criterion through education policy, the Academy of the Hebrew Language, and state institutions. Benefits from a unified national vernacular that serves state-building, military cohesion, and economic integration. Can redirect resources across linguistic domains; exit from the constraint would mean abandoning the linguistic foundation of the state project.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, zionist_state_building_project, beneficiary).

% Bear the cost of Hebrew's desacralization: the liturgical language becomes ordinary vernacular, losing its ritual distinctiveness and sacred boundary function. Their religious identity is fused with Hebrew-as-liturgical-vehicle; adopting the native-generation criterion means accepting that their primary mode of Hebrew use 'does not count' as vitality. Exit requires either abandoning liturgical Hebrew (identity rupture) or rejecting the state's linguistic authority (political marginalization).
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition_communities, payer,
    organized, generational, identity_locked, national).

% Gain a fully functional national language for daily life, education, and work. Their native acquisition validates the constraint's criterion. They can exit linguistically (emigrate, adopt English) without identity rupture, but the constraint's benefits (linguistic infrastructure, economic participation) make exit costly in practical terms.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, native_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% Maintain Hebrew primarily through liturgical and textual engagement. The native-generation criterion renders their Hebrew use 'preservation not vitality' by definition. They would object to being classified as non-vital, but lack standing in Israeli language policy. Their exit options are constrained by communal identity and the centrality of Israeli Hebrew in global Jewish discourse.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_jewish_communities, excluded,
    organized, generational, constrained, global).

% Analyze Hebrew as a case study in language revitalization. The native-generation criterion shapes theoretical frameworks (e.g., Fishman's GIDS scale) but scholars can apply alternative vitality metrics. Their exit is analytical — they can choose frameworks — but the Hebrew case's prominence makes the constraint difficult to ignore in the field.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, language_revitalization_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single, unambiguous, state-actionable criterion for language vitality that enables institutional coordination: curriculum design, language planning, immigration policy, and national cohesion all reference the native-generation benchmark.
% TRANSFER_FUNCTION: Moves definitional authority over 'what counts as Hebrew vitality' from rabbinic/traditional custodians to state institutions; moves institutional resources (education budget, media, academic positions) toward vernacular expansion and away from liturgical maintenance; transfers the sacred status of Hebrew from its ritual function to its national-vernacular function.
% ABSENT_VOICES: Pre-state Jewish communities in Ottoman Palestine who maintained Hebrew as a lingua franca without state apparatus; Yemenite and other Mizrahi communities whose liturgical pronunciation traditions were marginalized by the standardized 'native' accent; contemporary Haredi communities for whom Hebrew remains primarily liturgical and who reject the Zionist vitality framework entirely.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion vanished overnight, Israeli language policy would lose its foundational benchmark: the Academy of the Hebrew Language's mandate would shift, education curricula would need new justification, vitality assessments would pluralize to include liturgical and textual competence, and the Zionist narrative of 'revival' would lose its empirical criterion. Liturgical communities would regain standing as 'vital' Hebrew users. The sociolinguistic field would lose its paradigmatic success case.
% FOUNDING_PROBLEM: How to transform Hebrew from a liturgical and literary language with no native speakers into a living national vernacular capable of serving a modern sovereign state — including military, legal, scientific, and daily life domains — within a single generation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Zionism (e.g., Anita Shapira, Derek Penslar) attest the founding problem was real and urgent from outside the benefiting project. Liturgical communities (Haredi leadership, Sephardic chief rabbinate) attest it was never their problem — they maintained Hebrew continuity without state vernacularization. Sociolinguists (e.g., Bernard Spolsky, Joshua Fishman) corroborate the problem's structural uniqueness: no other language has undergone deliberate vernacularization from zero native speakers to national majority language.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.55) is moderate: the constraint transfers definitional authority and resources from liturgical to state domains, but the coordination function (creating a functional national vernacular) is genuine and substantial. Suppression (0.65) is moderately high: the criterion is enforced through state education, media, and institutional gatekeeping; alternatives (liturgical-only vitality) are structurally excluded from policy recognition. Theater ratio (0.3) is low-moderate: enforcement is largely functional (producing native speakers), though performative 'language purity' campaigns exist. Accessibility collapse (0.6) reflects that liturgical communities cannot easily adopt the native-generation criterion without identity rupture, yet they persist. Resistance (0.55) captures ongoing Haredi rejection and diaspora ambivalence. The claimed type 'tangled_rope' reflects genuine coordination (state-building needed a unified language) plus asymmetric extraction (liturgical tradition bears the cost of desacralization).
 *
 * PERSPECTIVAL GAP:
 *   From the state-builder seat, the constraint is a Rope: genuine coordination solving the collective-action problem of creating a national language from scratch. From the liturgical-community seat, it is a Snare: the coordination story covers extraction of sacred status and definitional authority. The engine computes this divergence from the structural data — the same constraint, different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project sits at the beneficiary end (d ~0.15): it sets the agenda, collects the gains (national cohesion, institutional control), and has arbitrage-grade exit (could redefine vitality but chooses not to). Liturgical tradition communities sit at the target end (d ~0.85): identity-locked, organized but politically subordinate on this issue, bearing the desacralization cost. Native Hebrew speakers are near-symmetric beneficiaries (d ~0.4): they gain a functional language but pay conformity costs. Diaspora communities are excluded (d undefined by derivation): structurally absent from the constraint's operational domain but affected by its global discourse dominance. Scholars are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a vernacular from zero) was live in 1880-1948 and remains live in a transformed sense: maintaining vernacular vitality against English dominance, preserving Hebrew as a majority language amid demographic shifts. The constraint has not atrophied into a Piton — its coordination function is actively maintained and expanded. However, the extraction component (liturgical marginalization) has become structural: the state no longer needs to actively suppress liturgical Hebrew; the native-generation criterion itself renders it 'non-vital' by definition. This is the tangled_rope dynamic: coordination and extraction are now inextricable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s classification change if the kernel ''hebrew_vitality'' is framed as a single contested commitment versus three independent constraints?',
    'Compare engine outputs when (a) all three readings are authored as separate constraints linked by network.affects_constraints, versus (b) a single constraint with measurement-dependent ε. The ε-invariance principle requires (a).',
    'If the kernel framing is correct, this reading''s ε is stable at 0.55 and its tangled_rope classification holds. If the kernel is an analytical artifact, the extraction may be higher (liturgical communities bear more cost) or lower (coordination function dominates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing: is hebrew_vitality a genuine kernel with multiple readings, or a post-hoc grouping?').

omega_variable(
    naturalness_ambiguity_vitality_criterion,
    'Is the native-generation criterion a natural property of language vitality (like biological reproduction for species) or a constructed benchmark serving state-building?',
    'Cross-linguistic comparison: do other revitalized languages (Māori, Welsh, Basque) treat native generation as the sole vitality criterion, or do they recognize liturgical/textual continuity as partial vitality? If the criterion is cross-linguistically unique to Hebrew, it is constructed.',
    'If natural, the constraint trends toward Mountain (low ε, emerges_naturally). If constructed, tangled_rope or snare classification is confirmed and FSM may trigger if claimed as mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_ambiguity_vitality_criterion, empirical, 'Natural-law vs. constructed criterion for language vitality').

omega_variable(
    suppression_mechanism_liturgical_communities,
    'Is the suppression of liturgical Hebrew vitality structural (state policy, institutional gatekeeping) or internalized (liturgical communities accepting ''preservation not vitality'' framing)?',
    'Post-policy-change observation: if Israeli state explicitly recognized liturgical Hebrew as a vitality domain, would Haredi communities accept the designation or reject it as Zionist co-optation? Current resistance suggests internalized suppression is low; structural suppression dominates.',
    'If internalized suppression is significant, effective suppression is higher than structural measure suggests — the constraint operates inside the target''s self-conception. If purely structural, resistance remains available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_liturgical_communities, empirical, 'Structural vs. internalized suppression of liturgical Hebrew communities').

omega_variable(
    reading_relation_forecloses_liturgical,
    'Does the native_daily_reading''s core premise (''only native generation constitutes vitality'') logically foreclose the liturgical_reading (''ritual preservation constitutes vitality'') within any single framework?',
    'Test whether a single institutional framework (e.g., a language policy body) could simultaneously adopt both criteria as valid without contradiction. If the criteria are definitionally mutually exclusive (vitality = X OR vitality = Y, where X and Y are disjoint), forecloses holds.',
    'If forecloses, the two readings cannot coexist in one policy framework — one must displace the other. If coexists_with, pluralistic vitality metrics are structurally possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relation_forecloses_liturgical, conceptual, 'Logical foreclosure between native-generation and liturgical-preservation vitality criteria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_vitality_native_daily_tr_t1880, hebrew_vitality__native_daily_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hebrew_vitality_native_daily_tr_t1904, hebrew_vitality__native_daily_reading, theater_ratio, 1904, 0.15).
narrative_ontology:measurement(hebrew_vitality_native_daily_tr_t1922, hebrew_vitality__native_daily_reading, theater_ratio, 1922, 0.2).
narrative_ontology:measurement(hebrew_vitality_native_daily_tr_t1948, hebrew_vitality__native_daily_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(hebrew_vitality_native_daily_tr_t1967, hebrew_vitality__native_daily_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(hebrew_vitality_native_daily_tr_t1990, hebrew_vitality__native_daily_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(hebrew_vitality_native_daily_tr_t2024, hebrew_vitality__native_daily_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(hebrew_vitality_native_daily_be_t1880, hebrew_vitality__native_daily_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(hebrew_vitality_native_daily_be_t1904, hebrew_vitality__native_daily_reading, base_extractiveness, 1904, 0.25).
narrative_ontology:measurement(hebrew_vitality_native_daily_be_t1922, hebrew_vitality__native_daily_reading, base_extractiveness, 1922, 0.4).
narrative_ontology:measurement(hebrew_vitality_native_daily_be_t1948, hebrew_vitality__native_daily_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(hebrew_vitality_native_daily_be_t1967, hebrew_vitality__native_daily_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(hebrew_vitality_native_daily_be_t1990, hebrew_vitality__native_daily_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(hebrew_vitality_native_daily_be_t2024, hebrew_vitality__native_daily_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_vitality_native_daily_su_t1880, hebrew_vitality__native_daily_reading, suppression_requirement, 1880, 0.2).
narrative_ontology:measurement(hebrew_vitality_native_daily_su_t1904, hebrew_vitality__native_daily_reading, suppression_requirement, 1904, 0.35).
narrative_ontology:measurement(hebrew_vitality_native_daily_su_t1922, hebrew_vitality__native_daily_reading, suppression_requirement, 1922, 0.5).
narrative_ontology:measurement(hebrew_vitality_native_daily_su_t1948, hebrew_vitality__native_daily_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(hebrew_vitality_native_daily_su_t1967, hebrew_vitality__native_daily_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(hebrew_vitality_native_daily_su_t1990, hebrew_vitality__native_daily_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(hebrew_vitality_native_daily_su_t2024, hebrew_vitality__native_daily_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__native_daily_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint decomposes the colloquial label 'Hebrew vitality' into three structurally distinct claims with different ε values and beneficiary/victim structures. The native_daily_reading (this file) has moderate ε (0.55) and tangled_rope classification. The liturgical_reading would have low ε (ritual continuity requires minimal enforcement) and rope/mountain classification. The hybrid_continuity_reading would have moderate ε with different beneficiary/victim structure. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_vitality__native_daily_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
