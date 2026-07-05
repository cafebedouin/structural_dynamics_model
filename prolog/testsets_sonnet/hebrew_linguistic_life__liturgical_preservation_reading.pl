% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew as Living Liturgical Language (Unbroken Recitational Chain Standard)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   Among ultra-Orthodox and traditionalist diaspora communities, the claim
 *   that 'Hebrew was a dead language before Ben-Yehuda' is itself contested —
 *   not as a historical detail but as a foundational legitimating myth for
 *   Zionist linguistic nationalism. This reading holds that Hebrew's
 *   continuous liturgical, halakhic, and exegetical use across two thousand
 *   years of diaspora constituted genuine linguistic life, and that
 *   Ben-Yehuda's revival project did not restore a dead language but rather
 *   displaced a living sacred register with a profane, ideologically
 *   motivated vernacular substitute. The constraint governs who gets to say
 *   what 'Hebrew being alive' means, and therefore who gets credit — and
 *   blame — for its continuity.
 *
 * KEY AGENTS:
 *   - rabbinic_scholarly_authorities: institutional agenda-setter administering canonical/textual criteria for linguistic life
 *   - diaspora_religious_communities: beneficiary whose non-territorial religious identity is validated by this standard
 *   - hebrew_revivalist_movement: payer whose historical legitimacy is structurally denied under this reading
 *   - secular_israeli_vernacular_speakers: payer whose achievement (native secular Hebrew) is reframed as irrelevant or degrading
 *   - sacred_liturgical_tradition_itself: non-agent victim, the entity claimed to be harmed by secularization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.42).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.55).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew as Living Liturgical Language (Unbroken Recitational Chain Standard)").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'fbcefce5-c3aa-47e7-b555-dde94e830fdf').
narrative_ontology:cs_kernel_codification('fbcefce5-c3aa-47e7-b555-dde94e830fdf', fixed_text).
narrative_ontology:cs_authority_grounding('fbcefce5-c3aa-47e7-b555-dde94e830fdf', lineage).
narrative_ontology:cs_interpretation_layer_present('fbcefce5-c3aa-47e7-b555-dde94e830fdf').
narrative_ontology:cs_reading_relation('fbcefce5-c3aa-47e7-b555-dde94e830fdf', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('fbcefce5-c3aa-47e7-b555-dde94e830fdf', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('fbcefce5-c3aa-47e7-b555-dde94e830fdf', foundational, recitational_continuity_constitutes_life).
narrative_ontology:cs_axiom_status(recitational_continuity_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('fbcefce5-c3aa-47e7-b555-dde94e830fdf', recitational_continuity_constitutes_life, conventional).
narrative_ontology:cs_axiom('fbcefce5-c3aa-47e7-b555-dde94e830fdf', secondary, vernacular_acquisition_unnecessary_for_vitality).
narrative_ontology:cs_axiom_status(vernacular_acquisition_unnecessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('fbcefce5-c3aa-47e7-b555-dde94e830fdf', vernacular_acquisition_unnecessary_for_vitality, conventional).
narrative_ontology:cs_reference_frame('fbcefce5-c3aa-47e7-b555-dde94e830fdf', diaspora_textual_transmission_standard).
narrative_ontology:cs_drift_state('fbcefce5-c3aa-47e7-b555-dde94e830fdf', post_zionist_revival_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fbcefce5-c3aa-47e7-b555-dde94e830fdf', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_religious_communities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, traditional_yeshiva_institutions).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_revivalist_movement).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, secular_israeli_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_liturgical_tradition_itself).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, continuous_transmission_constitutes_linguistic_life).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, sacred_register_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the criteria by which Hebrew's liveness is adjudicated inside religious institutions: what counts as correct recitation, which texts are canonical, how study lineages are certified. They hold interpretive authority over the chain of transmission and their institutional standing depends on that chain being recognized as unbroken and sufficient in itself.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Maintain Hebrew liturgical practice across dispersed communities without ever needing a territorial vernacular base. The liturgical-preservation standard validates their form of Jewish life as complete and legitimate without requiring migration to a Hebrew-speaking homeland or acquisition of Hebrew as a mother tongue.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_religious_communities, beneficiary,
    organized, generational, constrained, global).

% Run the pedagogical infrastructure that trains students in textual Hebrew — reading, chanting, exegesis — divorced from conversational fluency. Their curricula, funding, and prestige are built on the premise that this mode of engagement IS linguistic life, not a substitute for it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, traditional_yeshiva_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, traditional_yeshiva_institutions, agenda_setter).

% Ben-Yehuda and successive generations of revivalists who worked to make Hebrew a spoken vernacular for daily secular life. Under this reading, their entire project is reframed as unnecessary at best and desecrating at worst — since the language was never dead by the liturgical-preservation standard, the revival narrative that grounds their historical legitimacy is structurally denied.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_revivalist_movement, payer,
    moderate, biographical, constrained, national).

% Speak Hebrew as a native, fully secular daily language — the outcome the revivalist project achieved. Under this reading their vernacular fluency is irrelevant to the language's aliveness and, in stronger formulations, their casual and profane use of the sacred tongue is treated as a degradation rather than an achievement.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_israeli_vernacular_speakers, payer,
    organized, generational, mobile, national).

% The textual and recitational tradition named as the bearer of Hebrew's life. Cited as the entity whose continuity proves the language's aliveness, and — per the expected structural delta — cast as the party harmed when the language is redirected toward secular vernacular purposes it was never meant to serve.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_liturgical_tradition_itself, payer,
    analytical, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_liturgical_tradition_itself).

% Study the documentary record of Hebrew's liturgical, scholarly, and epistolary use across the diaspora centuries. They can assess whether the recitational chain was in fact unbroken and whether 'aliveness' talk tracks a real linguistic property or a retrospective legitimation project.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_authorities).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a criterion for linguistic survival that lets dispersed, non-territorial religious communities recognize their own textual and liturgical practice as sufficient continuation of the language, without requiring a common vernacular or shared homeland.
% TRANSFER_FUNCTION: Moves interpretive and definitional authority over 'what counts as Hebrew being alive' to the institutions that administer liturgical and scholarly transmission, and moves legitimacy away from vernacular revival as the measure of a language's survival.
% ABSENT_VOICES: The Hebrew revivalist movement and secular vernacular speakers are structurally present as payers but their own criterion for aliveness — daily spoken use — is excluded from this reading's definition; they would object that a language read but not spoken as mother tongue is preserved, not alive, but that objection is answered elsewhere (see sibling readings), not inside this constraint.
% DISAPPEARANCE_RATIONALE: If the liturgical-preservation standard were abandoned, rabbinic and yeshiva institutions would lose their claim that centuries of diaspora practice constituted unbroken linguistic life, diaspora religious identity built on textual continuity without vernacular Hebrew would need new grounding, and the revivalist narrative (Hebrew was dead, then resurrected) would become the uncontested account rather than one reading among several.
% FOUNDING_PROBLEM: How to certify that a language survives across a millennia-long diaspora with no contiguous territory or continuous native-speaker population — the problem of grounding linguistic (and thereby communal-religious) continuity in the absence of the normal conditions for language survival.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and yeshiva institutions attest the standard remains live and sufficient. Linguistic historians outside those institutions are divided: some corroborate that liturgical Hebrew maintained real linguistic continuity (phonology, morphology, textual transmission) distinct from vernacular death; others, citing sociolinguistic definitions of language vitality tied to intergenerational mother-tongue transmission, hold the founding problem was answered by revival, not by liturgical continuity, and that the liturgical-preservation reading is a retrospective legitimation constructed partly in response to and partly in competition with the revivalist narrative.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).
:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the coordination function is real — dispersed communities genuinely need a criterion for continuity that doesn't require a shared territory, and rabbinic authorities do maintain a documented, textually rigorous transmission chain. But the standard also performs definitional work that transfers legitimacy away from the revivalist project and its beneficiaries (secular vernacular speakers), which is where the extractive component sits. Suppression (0.55) reflects the real but not overwhelming institutional pressure within traditionalist communities against treating vernacular fluency as sufficient or superior; it is enforced through communal and educational structures, not state coercion, so it stops short of the higher end. Theater ratio (0.28) is modest — the recitational and study practices are functionally real, not merely performed, though a growing share of polemical assertion ('Hebrew was never dead') serves boundary-maintenance against the rival Zionist narrative more than it serves transmission itself, hence the slow upward drift over the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and yeshiva institutions are structural beneficiaries: they administer and profit institutionally from the criterion, and their arbitrage-grade exit (they can operate across any diaspora jurisdiction) puts them near the beneficiary end. Diaspora religious communities benefit from having their form of Jewish continuity validated without needing Hebrew fluency or territorial return. The revivalist movement and secular vernacular speakers are targets: the reading directly delegitimizes the historical narrative and communicative achievement that ground their position, and their constrained exit (they cannot simply exit Hebrew's contested history) pushes their directionality toward the target end. The sacred tradition itself is marked non-agent and trapped — it cannot exit its own instrumentalization by either side of the kernel dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — certifying linguistic continuity without territorial contiguity — was a live problem across the diaspora centuries. Whether it remains live today, after the successful establishment of a Hebrew-speaking nation-state with tens of millions of native speakers, is exactly the contested question the six-questions genealogy surfaces: rabbinic authorities say the liturgical standard remains the truer measure of aliveness (status: contested, not dead), while the historical record most historians outside the benefiting institutions cite (mother-tongue transmission achieved through revival) suggests the founding problem was, in the vernacular sense, actually solved by the sibling reading. This divergence — the mismatch between the founding-problem status claimed here and the world's apparent rearrangement around vernacular Hebrew — is the corpus-relevant signal, not a defect to resolve within this single story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unbroken_chain_empirical_status,
    'Was Hebrew''s liturgical and scholarly use across the diaspora centuries genuinely ''unbroken'' in a linguistically meaningful sense (continuous productive use, phonological and morphological stability), or is ''unbroken chain'' itself a retrospective, idealized construction built partly in reaction to the revivalist narrative it competes with?',
    'Comparative philological analysis of diaspora-era responsa, poetry, and correspondence for evidence of productive (not merely liturgical-recitational) Hebrew use, cross-checked against sociolinguistic vitality criteria independent of either reading''s stakes.',
    'If the chain was genuinely unbroken in a strong sense, this reading''s claim that Hebrew never died is well-grounded and the revival narrative is the constructed one. If the diaspora use was narrowly liturgical/textual with no productive register, this reading''s ''aliveness'' criterion is doing more definitional work than descriptive work, and the extraction from the revivalist movement''s legitimacy is harder to justify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbroken_chain_empirical_status, empirical, 'Whether the unbroken-chain premise is linguistically substantiated or retrospectively constructed.').

omega_variable(
    kernel_reading_selection_stakes,
    'This story adopts the liturgical-preservation reading of the hebrew_linguistic_life kernel rather than the native_generational_reading or marketplace_pidgin_reading. What determines which reading a given community or scholar adopts, and is the choice itself neutral or does it track institutional self-interest?',
    'Cross-reference which communities/scholars hold which reading against their institutional position (rabbinic authority vs. secular linguist vs. Zionist historian) to test whether reading-adoption correlates with beneficiary status under each reading.',
    'If reading-adoption tracks institutional self-interest cleanly, all three readings in the kernel are partly self-serving constructions rather than neutral linguistic definitions, which would argue for treating ''is Hebrew alive/was it dead'' as an essentially contested question rather than an empirical one resolvable by any single reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_stakes, conceptual, 'Whether kernel-reading choice is analytically neutral or interest-tracking.').

omega_variable(
    victim_status_of_sacred_tradition,
    'Can ''the sacred liturgical tradition itself'' coherently be a victim, or is this a rhetorical personification that smuggles rabbinic authorities'' institutional interest into the appearance of a harm to an abstract entity?',
    'Examine whether claimed harms to ''the tradition'' cash out as identifiable harms to specific institutions/practitioners (loss of students, funding, authority) versus genuinely diffuse harms with no institutional beneficiary of the harm-claim.',
    'If the harm fully cashes out as institutional loss to rabbinic/yeshiva authorities, the true victim/beneficiary structure collapses toward a simpler tangled_rope or even snare reading with those institutions as sole beneficiaries; if some genuinely diffuse harm to a non-institutional tradition survives the analysis, the current three-way victim structure holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_status_of_sacred_tradition, conceptual, 'Whether the non-agent victim is a real diffuse harm or an institutional interest in personified form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(hebr_tr_t80, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(hebr_tr_t100, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(hebr_be_t80, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(hebr_be_t100, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(hebr_su_t80, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement(hebr_su_t100, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hebrew_linguistic_life kernel, decomposed per the epsilon-invariance principle rather than represented as a single constraint with a variable observable. liturgical_preservation_reading (this story) claims moderate, identity-coordination-flavored extraction with rabbinic/yeshiva institutions as beneficiaries and the revivalist movement as victim. native_generational_reading (sibling) would measure aliveness by mother-tongue transmission and likely inverts the beneficiary/victim structure — treating the revival as the achievement and treating liturgical-only use as insufficient, arguably a form of linguistic death-in-practice. marketplace_pidgin_reading (sibling) measures aliveness by practical inter-communal function and would likely register low extraction on either institutional axis, since it does not center either sacred authority or nationalist vernacular achievement. Each story's ε and beneficiary/victim structure is authored independently; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
