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
 *   human_readable: Native Daily Generation as the Sole Criterion of Hebrew Vitality
 *   domain: sociolinguistics/nationalism/religious_studies
 *
 * SUMMARY:
 *   This story instantiates the native_daily_reading of the contested
 *   hebrew_vitality kernel: the claim that ONLY intergenerational native
 *   acquisition counts as linguistic vitality, and that centuries of
 *   continuous liturgical, textual, and scholarly Hebrew use — however
 *   unbroken — constitute mere preservation of a dead register, not life.
 *   This is the reading that underwrote the ivrit be-ivrit pedagogical
 *   revolution, the Hebrew Language Committee's prescriptive lexical
 *   expansion, and the Zionist historiographical narrative of 'reviving a
 *   dead language.' It is a distinct constraint from the liturgical_reading
 *   (which holds that unbroken ritual use already constitutes vitality and
 *   needs no vernacular supplement) and from the hybrid_continuity_reading
 *   (which holds liturgical continuity was a necessary substrate but
 *   insufficient alone). Each reading has its own beneficiary/victim
 *   structure and its own epsilon; they are linked here only through
 *   network.affects_constraints, not merged.
 *
 * KEY AGENTS:
 *   - zionist_state_building_project: primary agenda-setter and beneficiary (institutional/arbitrage) — writes and enforces the native-generation criterion
 *   - sabra_native_speaker_generation: direct beneficiary (moderate/mobile) — the embodied proof of the criterion
 *   - vernacular_hebrew_pedagogy_institutions: administering beneficiary (organized/arbitrage) — coins vocabulary, certifies competence, funded by the criterion's dominance
 *   - liturgical_hebrew_tradition: primary victim (moderate/constrained) — its continuous sacred use is redefined as non-vitality
 *   - diaspora_yiddish_and_ladino_vernaculars: suppressed victim (powerless/trapped) — actively displaced living languages
 *   - traditional_religious_educators: displaced-prestige victim (moderate/constrained)
 *   - comparative_linguists_and_revival_scholars: analytical observer (analytical/analytical) — studies the case as the paradigm of language revival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.52).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.61).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Native Daily Generation as the Sole Criterion of Hebrew Vitality").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/nationalism/religious_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, 'aa415972-170a-40e3-bf2f-6bc6abfcdfd6').
narrative_ontology:cs_kernel_codification('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', distributed).
narrative_ontology:cs_authority_grounding('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', practice).
narrative_ontology:cs_interpretation_layer_present('aa415972-170a-40e3-bf2f-6bc6abfcdfd6').
narrative_ontology:cs_reading_relation('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', foundational, native_intergenerational_acquisition_is_necessary_and_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(native_intergenerational_acquisition_is_necessary_and_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', native_intergenerational_acquisition_is_necessary_and_sufficient_for_vitality, empirically_contingent).
narrative_ontology:cs_axiom('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', secondary, ritual_recitation_without_native_transmission_constitutes_preservation_not_life).
narrative_ontology:cs_axiom_status(ritual_recitation_without_native_transmission_constitutes_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', ritual_recitation_without_native_transmission_constitutes_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', pre_revival_diglossic_hebrew_liturgical_register).
narrative_ontology:cs_drift_state('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', contemporary_israeli_hebrew, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aa415972-170a-40e3-bf2f-6bc6abfcdfd6', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, vernacular_hebrew_pedagogy_institutions).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, sabra_native_speaker_generation).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_hebrew_tradition).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_yiddish_and_ladino_vernaculars).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, traditional_religious_educators).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, vernacularization_thesis_of_language_revival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the criterion of vitality itself: institutes Hebrew-only instruction (the ivrit be-ivrit method), builds school curricula, army service, and civil administration entirely in vernacular Hebrew, and treats native daily generation among children as proof of national rebirth. It writes the standard against which every other form of Hebrew use is now measured, and it collects the legitimacy dividend of having produced a 'living' national language.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, zionist_state_building_project, beneficiary).

% The first generation of children raised with Hebrew as a mother tongue. They acquire native fluency, full social and economic access within the Yishuv/Israeli society, and inherit the prestige of embodying the revival's proof-of-concept. Their exit options are wide: they hold the dominant vernacular and are not disadvantaged by the criterion.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, sabra_native_speaker_generation, beneficiary,
    moderate, biographical, mobile, national).

% Teacher-training colleges, the Hebrew Language Committee (later Academy), and the ulpan system administer and continually re-legitimate the native-generation standard. They coin vocabulary, standardize grammar for child acquisition, and certify what counts as 'real' Hebrew competence, drawing funding and institutional authority from the criterion's dominance.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, vernacular_hebrew_pedagogy_institutions, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, vernacular_hebrew_pedagogy_institutions, agenda_setter).

% Centuries of unbroken sacred use — prayer, Torah study, halakhic discourse, piyyut — that sustained Hebrew as a living textual and liturgical register across the diaspora without native daily speakers. Under this reading, that entire tradition is redefined as mere preservation, not life: its custodians must now defer to or be measured against the vernacular standard, and their own continuous practice is reclassified as inert. They cannot exit the redefinition; it operates as a claim about what counts as vitality, not a policy they can decline to be governed by.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_hebrew_tradition, payer,
    moderate, civilizational, constrained, global).

% Living Jewish vernaculars that were the actual mother tongues of most diaspora Jews prior to and during the revival period. The native-generation criterion for Hebrew was pursued partly through active suppression of Yiddish and Ladino in Zionist settlement institutions (language wars, school policy, press restrictions) — their speakers had no meaningful exit from a nation-building program that treated their vernaculars as competitors to be displaced rather than parallel living languages.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_yiddish_and_ladino_vernaculars, payer,
    powerless, generational, trapped, continental).

% Heder and yeshiva teachers whose pedagogical authority rested on textual and liturgical Hebrew competence. The vernacular-vitality standard devalues their expertise relative to secular Hebrew educators who teach the spoken register; some adapt by teaching both, but the prestige hierarchy shifts decisively away from their traditional domain.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, traditional_religious_educators, payer,
    moderate, generational, constrained, regional).

% Study the Hebrew case as the paradigm instance of language revival, debating whether native intergenerational transmission is definitionally necessary for 'revival' or whether sustained liturgical/literary vitality without native speakers should also count. Their conclusions feed back into how other endangered-language revitalization movements set their own goals.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, comparative_linguists_and_revival_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, vernacular_hebrew_pedagogy_institutions).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, teachable, testable criterion — native intergenerational transmission — around which an entire national education, military, and civil-administration apparatus could coordinate a shared vernacular, solving the real problem that a nation-state needs one working everyday language rather than a diglossic patchwork.
% TRANSFER_FUNCTION: Moves linguistic prestige, pedagogical authority, and institutional funding from liturgical/textual Hebrew custodians and diaspora vernacular speakers toward vernacular Hebrew pedagogy institutions and the generation of native speakers they produce; it also transfers legitimacy to the state-building project, which can point to a 'living' national language as evidence of national rebirth.
% ABSENT_VOICES: Diaspora Yiddish and Ladino speakers, and religious communities for whom Hebrew's holiness depended precisely on its NOT being profaned by everyday use, are structurally absent from the criterion-setting process — the standard was set by secular nationalist educators and philologists, not negotiated with the communities whose living languages or theological commitments it overrode.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion vanished as the definition of vitality, Modern Hebrew as a spoken vernacular would not disappear — it is now self-sustaining across millions of native speakers. But the CLAIM itself (that only native generation counts as life) does normative and institutional work: without it, liturgical continuity and diaspora vernaculars could be re-admitted as co-equal forms of vitality, changing how heritage-language funding, religious education, and revival movements elsewhere are evaluated. Zionist historiography and Hebrew-language pedagogy would need to recharacterize what the revival actually accomplished.
% FOUNDING_PROBLEM: Diaspora Jewish communities used Hebrew extensively for prayer, study, and correspondence, but lacked a single shared national vernacular; the Zionist project needed a mother tongue capable of unifying immigrants from dozens of countries and mutually unintelligible vernaculars into one functioning national society.
% FOUNDING_PROBLEM_CORROBORATION: The pedagogy institutions and state-building project attest the founding problem (linguistic fragmentation of the Yishuv) was real and is now resolved by a thriving native-speaker population. Independent sociolinguists studying language revival (e.g., scholarship on the Hebrew case as a comparative model) corroborate that native transmission occurred, but historians of Yiddish and religious studies scholars outside the Zionist institutional apparatus contest the FRAMING that liturgical Hebrew was therefore not 'alive' — that framing is corroborated mainly by the beneficiaries who administered the vernacularization program, not by neutral outside observers of the liturgical tradition itself.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.52 by 1980) because the reading does real coordination work — it genuinely produced a functioning national vernacular where none existed — but it also extracts prestige and institutional authority away from liturgical and diaspora-vernacular custodians who did not consent to being redefined as merely preservationist. Suppression peaks in the 1900-1920 window (0.68) during the language wars (Yiddish press restrictions, the 1913 'War of the Languages' over instruction medium at the Technion), then eases somewhat as vernacular Hebrew became self-sustaining and no longer needed active suppression of rivals to maintain dominance — though it never falls to zero because the definitional claim (only native generation = life) continues to structurally deprioritize liturgical and heritage-language funding. Theater ratio stays low throughout (peaking at 0.22): the pedagogical apparatus that grew up around this criterion is mostly functionally real (actual children were actually taught actual spoken Hebrew), not performative.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project and the pedagogy institutions that administer the criterion sit at the beneficiary end: they set the definition, collect legitimacy from it, and are not measured against it themselves. The sabra generation benefits directly and has mobile exit — nothing traps them in the standard, since they simply are what the standard describes. Liturgical Hebrew tradition, diaspora vernaculars, and traditional religious educators sit at the target end: they bear the redefinition without having authored it, and their exit options are constrained-to-trapped because the claim about what 'vitality' means is not something they can individually opt out of — it operates at the level of whose language use gets counted, funded, and prestige-ranked, which is a structural fact about the field, not a policy any one victim can decline.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (linguistic fragmentation among Jewish immigrants needing a shared vernacular) was genuinely live in 1880-1920 and is now largely resolved — Modern Hebrew is a stable, self-sustaining native language. But the DEFINITIONAL CLAIM that outlived the founding problem (only native generation counts as vitality) continues to do work beyond what the founding problem required: it retroactively delegitimizes forms of Hebrew vitality (liturgical, textual, diasporic) that never needed vernacularization to be alive, and it exports this criterion to other revival movements as the paradigm test. This is why the classification is tangled_rope rather than pure rope: the genuine coordination achievement (a shared national vernacular) is real, but it rides alongside an asymmetric extraction of definitional authority that persists past the point the coordination problem was solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definitional_contest_location,
    'Is the boundary between ''vitality'' and ''preservation'' a linguistic fact discoverable by sociolinguistic criteria (e.g. presence of native L1 acquisition), or is it a contested normative/political claim that different communities are entitled to answer differently for their own language?',
    'Compare how the same definitional question is resolved in other revival/maintenance cases (Irish, Māori, Cornish, Coptic) where native-generation and liturgical-continuity readings diverge, and check whether sociolinguistic consensus or political power predicts which reading becomes dominant.',
    'If the boundary is a discoverable linguistic fact, the native_daily_reading''s claim is closer to a mountain (a real threshold) with beneficiaries who happen to sit on the correct side; if it is a contested normative claim, the reading is better understood as an act of definitional extraction that happened to align with the winning nation-building project.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_definitional_contest_location, conceptual, 'Whether the native-generation vitality criterion is a discovered linguistic fact or a contested political framing.').

omega_variable(
    yiddish_suppression_causal_weight,
    'How much of the historical suppression of Yiddish and Ladino in Mandate Palestine was causally necessary to establish Hebrew as the shared vernacular, versus how much was surplus suppression driven by ideological hostility to diaspora culture independent of the coordination need?',
    'Historical analysis of Yishuv language policy debates (e.g. Gruzenberg-era Hebrew Language Council records, kibbutz language enforcement records, Va''ad HaLashon correspondence) separating arguments made on coordination grounds from arguments made on ''negation of the diaspora'' (shlilat ha-golah) ideological grounds.',
    'If suppression was mostly coordination-necessary, the victim classification of diaspora vernaculars weakens toward incidental cost; if mostly ideologically surplus, it strengthens the tangled_rope reading toward a more extractive one, and would push the suppression metric higher relative to what pure coordination required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yiddish_suppression_causal_weight, empirical, 'Whether Yiddish/Ladino suppression was coordination-necessary or ideologically surplus.').

omega_variable(
    sibling_reading_kernel_location,
    'Given three declared readings of the hebrew_vitality kernel, is there a fact of the matter about which reading correctly describes what happened historically, or do all three remain simultaneously defensible because ''vitality'' itself is not a natural kind but a term of art each tradition defines for its own purposes?',
    'This is inherently a committer-frame question, not resolvable by additional data within this story alone; cross-reference with the liturgical_reading and hybrid_continuity_reading stories'' own omega variables to see whether they converge on a shared underlying empirical claim or diverge on values.',
    'If a shared empirical core exists (e.g., all three would agree native speakers emerged by 1948 but disagree only on whether liturgical use ALSO counted as alive), the kernel is a definitional/values dispute; if the readings disagree about what actually happened, at least one reading is empirically wrong.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_kernel_location, conceptual, 'Whether the three kernel readings disagree about values or about historical fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1880, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_vitality__native_daily_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__native_daily_reading, theater_ratio, 1900, 0.13).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.16).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_vitality__native_daily_reading, theater_ratio, 1940, 0.19).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_vitality__native_daily_reading, theater_ratio, 1960, 0.21).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_vitality__native_daily_reading, theater_ratio, 1980, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_vitality__native_daily_reading, base_extractiveness, 1880, 0.28).
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__native_daily_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.46).
narrative_ontology:measurement(hebr_be_t1940, hebrew_vitality__native_daily_reading, base_extractiveness, 1940, 0.5).
narrative_ontology:measurement(hebr_be_t1960, hebrew_vitality__native_daily_reading, base_extractiveness, 1960, 0.51).
narrative_ontology:measurement(hebr_be_t1980, hebrew_vitality__native_daily_reading, base_extractiveness, 1980, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_vitality__native_daily_reading, suppression_requirement, 1880, 0.35).
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__native_daily_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(hebr_su_t1940, hebrew_vitality__native_daily_reading, suppression_requirement, 1940, 0.63).
narrative_ontology:measurement(hebr_su_t1960, hebrew_vitality__native_daily_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(hebr_su_t1980, hebrew_vitality__native_daily_reading, suppression_requirement, 1980, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__native_daily_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the colloquial concept 'Hebrew language revival' along the hebrew_vitality kernel: this story (native_daily_reading) claims only intergenerational native acquisition constitutes vitality; liturgical_reading claims unbroken ritual use already constitutes vitality; hybrid_continuity_reading claims liturgical continuity was necessary but insufficient. Each carries its own epsilon, beneficiary/victim structure, and classification (this one: tangled_rope, moderate epsilon ~0.52) rather than averaging across the contest. Linked via affects_constraints in both directions per family convention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
