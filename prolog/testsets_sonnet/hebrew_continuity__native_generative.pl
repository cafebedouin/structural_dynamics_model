% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity via Native-Generative Vitality Standard
 *   domain: sociolinguistic/cultural/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the native_generative reading of the
 *   contested 'Hebrew continuity' kernel: the claim that Hebrew lives ONLY
 *   through native speaker intuition and daily generative use, not through
 *   preserved liturgical recitation or diaspora contact-language function.
 *   Historically, this reading was forged during the Yishuv-era revival, when
 *   Eliezer Ben-Yehuda's project and successor institutions needed to prove
 *   Hebrew could function as a full national vernacular, not merely a sacred
 *   text-language. It succeeded spectacularly as a coordination project —
 *   producing a mutually intelligible modern vernacular for millions — but
 *   the same kernel reading that made this possible now also functions to
 *   delegitimize other continuous forms of Hebrew transmission (liturgical,
 *   textual, diaspora-contact) as merely 'preserved' rather than 'alive.'
 *   This is a distinct constraint from the liturgical_preservation and
 *   bridge_pidginized readings of the same kernel: each reading has its own
 *   beneficiary/victim structure and its own epsilon, and they are linked
 *   here only through network.affects_constraints, not merged.
 *
 * KEY AGENTS:
 *   - israeli_hebrew_language_academy: agenda_setter (institutional/analytical) — administers the native-generative standard
 *   - sabra_native_speaker_generation: beneficiary (organized/arbitrage) — automatic linguistic authority by birth
 *   - israeli_state_education_system: beneficiary/agenda_setter (institutional/analytical) — built curriculum around the standard
 *   - diaspora_liturgical_only_communities: payer (moderate/trapped) — centuries of transmission reclassified as 'dead'
 *   - yeshiva_textual_hebrew_traditionalists: payer (moderate/constrained) — textual fluency devalued relative to native colloquial fluency
 *   - elderly_revival_era_immigrants_with_accented_hebrew: payer (powerless/trapped) — their own generation's Hebrew located as transitional, not authoritative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.58).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.62).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity via Native-Generative Vitality Standard").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistic/cultural/institutional").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, 'bc96260f-e278-4fa5-a06c-70c2f0b7f84d').
narrative_ontology:cs_kernel_codification('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', distributed).
narrative_ontology:cs_authority_grounding('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', practice).
narrative_ontology:cs_interpretation_layer_present('bc96260f-e278-4fa5-a06c-70c2f0b7f84d').
narrative_ontology:cs_reading_relation('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', hebrew_continuity__bridge_pidginized, forecloses).
narrative_ontology:cs_axiom('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', foundational, native_child_acquisition_is_necessary_for_linguistic_life).
narrative_ontology:cs_axiom_status(native_child_acquisition_is_necessary_for_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', native_child_acquisition_is_necessary_for_linguistic_life, conventional).
narrative_ontology:cs_axiom('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', secondary, generative_daily_use_supersedes_textual_fidelity_as_vitality_criterion).
narrative_ontology:cs_axiom_status(generative_daily_use_supersedes_textual_fidelity_as_vitality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', generative_daily_use_supersedes_textual_fidelity_as_vitality_criterion, instrumental).
narrative_ontology:cs_reference_frame('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', yishuv_era_vernacularization_project).
narrative_ontology:cs_drift_state('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', contemporary_multilingual_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bc96260f-e278-4fa5-a06c-70c2f0b7f84d', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, sabra_native_speaker_generation).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_state_education_system).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_liturgical_only_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, yeshiva_textual_hebrew_traditionalists).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, elderly_revival_era_immigrants_with_accented_hebrew).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, vernacularization_as_national_revival_doctrine).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, living_language_requires_native_transmission_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the official standard for what counts as legitimate, living Hebrew — lexical expansion committees, phonological standardization, grammar rulings. Its authority rests on the claim that Hebrew's continuity runs through the intuitions of native child speakers, not through preserved liturgical recitation. It adjudicates which usages are 'organic Hebrew' versus 'foreign interference,' and its rulings feed directly into school curricula and state media style guides.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_hebrew_language_academy, agenda_setter,
    institutional, generational, analytical, national).

% Native-born Hebrew speakers whose childhood-acquired intuition is treated as the ultimate arbiter of correct, living Hebrew. Their fluency confers social, professional, and cultural capital automatically; they need not study liturgical Hebrew or historical textual forms to be recognized as authoritative speakers of 'real' Hebrew. Their linguistic judgments outrank textual scholarship in disputes over usage.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sabra_native_speaker_generation, beneficiary,
    organized, generational, arbitrage, national).

% Built its entire pedagogical apparatus around producing native-generative Hebrew speakers from early childhood — immersion, ulpan programs, national curricula. It benefits from the native-generative standard because it validates decades of institutional investment in a monolingual national vernacular and legitimizes the state's founding revival narrative.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_state_education_system, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, israeli_state_education_system, agenda_setter).

% Communities across the diaspora who have transmitted Hebrew for centuries through prayer, textual study, and ritual recitation without ever developing native child speakers. Under the native-generative standard their Hebrew is classified as functionally 'dead' — preserved but not living — which delegitimizes their claim to be genuine bearers of the language's continuity. They cannot exit the judgment; their entire transmission mode is structurally disqualified by the kernel reading itself.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_liturgical_only_communities, payer,
    moderate, civilizational, trapped, global).

% Scholars and institutions whose relationship to Hebrew is deep textual and exegetical fluency without native colloquial generativity. They can continue their practice on their own terms, but within the dominant national narrative their expertise is treated as antiquarian rather than as evidence of a living language, costing them institutional prestige and access to state-linguistic authority.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, yeshiva_textual_hebrew_traditionalists, payer,
    moderate, civilizational, constrained, regional).

% First-generation adult immigrants who acquired Hebrew as a second language during the Yishuv and early state period, speaking it fluently but with non-native phonology and calques from their birth languages. Their Hebrew is treated as a stepping-stone generation rather than the standard itself; the native-generative kernel formally locates linguistic authority one generation downstream of them, in their own children.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, elderly_revival_era_immigrants_with_accented_hebrew, payer,
    powerless, biographical, trapped, national).

% Teach Hebrew as a heritage or liturgical language abroad without access to a native-speaking child population. They would argue that vitality can be sustained through textual and communal transmission without native generativity, but they are not represented on the standard-setting bodies that decide what counts as 'living' Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_hebraist_educators, excluded,
    moderate, generational, constrained, global).

% Study Hebrew's revival as a rare case of large-scale vernacularization and compare it to other revitalization efforts (Irish, Māori, Hawaiian). They can trace how the native-generative standard was constructed, whom it privileges, and how it interacts with rival continuity claims, without having a stake in which reading of Hebrew continuity prevails.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, comparative_sociolinguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, teachable, generationally self-reproducing standard for spoken Hebrew so that a national population can acquire one mutually intelligible vernacular rather than fragmenting into regional or communal dialects and calques inherited from diaspora contact languages.
% TRANSFER_FUNCTION: Moves linguistic authority, institutional legitimacy, and access to state cultural infrastructure from textual/liturgical Hebrew communities and accented adult-immigrant speakers toward native-born child speakers and the pedagogical institutions that produce them.
% ABSENT_VOICES: Diaspora Hebraist educators and liturgical communities who sustain Hebrew transmission without native child speakers are not seated on the language-standardizing bodies; they would argue that a language can be 'alive' through unbroken textual and ritual use, but that argument has no institutional channel within the native-generative kernel reading.
% DISAPPEARANCE_RATIONALE: If the native-generative standard were withdrawn as the governing continuity claim, the Israeli Hebrew Language Academy's authority over 'correct' usage would lose its foundational justification, school curricula built on immersion-native fluency would need re-justification, and liturgical/textual Hebrew communities could reassert equal or superior claims to being authentic bearers of the language's continuity — a real institutional and status reallocation, not a cosmetic one.
% FOUNDING_PROBLEM: In the late 19th and early 20th centuries, Hebrew existed almost exclusively as a liturgical and literary language with no native speakers; the founding problem was to transform it into a full vernacular capable of serving a modern national community's daily, familial, and generational life.
% FOUNDING_PROBLEM_CORROBORATION: The Hebrew Language Academy and the state education system attest the founding problem remains partially live (maintaining vernacular robustness against English/Arabic contact influence, generational lexical gaps). Independent sociolinguists studying revitalization outside Israel's own institutions note the original crisis — Hebrew having zero native speakers — was resolved by the mid-20th century; persistence of the native-generative gatekeeping standard past that point functions increasingly to allocate status and resources rather than to solve an ongoing survival problem, a reading corroborated by comparative revitalization scholars with no stake in Israeli Hebrew's internal politics.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.22 to 0.58) as the coordination achievement of vernacularization (early, low extraction — genuinely solving a real problem of a language with zero speakers) gradually calcifies into a status-and-resource-allocation mechanism that continues to strip legitimacy from non-native-generative Hebrew traditions long after the original survival crisis passed. Suppression is high throughout but declines slightly (0.75 to 0.62) as diaspora and liturgical communities gain alternative validation channels (academic linguistics, heritage-language pedagogy) outside the Israeli state apparatus, though it remains substantial because state cultural authority, media, and educational credentialing still route through the native-generative standard. Theater ratio rises modestly (0.08 to 0.28) reflecting growing performative elements — Academy rulings on 'purity' of usage that function more to assert institutional relevance than to solve an actual coordination problem, since the vernacular is now robustly self-sustaining and no longer needs active defense against extinction.
 *
 * PERSPECTIVAL GAP:
 *   From the Academy's seat, the native-generative requirement is a coordination necessity: without it, 'living Hebrew' dissolves into contested regional and denominational variants. From a diaspora liturgical community's seat, the same requirement is an act of erasure — their centuries-unbroken transmission is retroactively declared inadequate by a standard invented to solve a problem (zero native speakers) that never applied to them. The engine should compute these as structurally different experiences of one constraint, not reconcile them into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The Academy and state education system sit at the beneficiary end: they administer and are validated by the standard. Native-born speakers benefit from an ascribed, low-effort authority (d near beneficiary) with arbitrage-level exit — they can code-switch into other registers freely without status loss. Diaspora liturgical communities and yeshiva traditionalists sit near the target end: trapped or constrained exit, their mode of transmission is precisely what the kernel reading declares insufficient for 'life.' Elderly first-generation immigrants are a distinctive case — they DID the labor of vernacularization but are structurally located as a transitional generation rather than the standard itself, which is why they are named as victims despite being foundational to the revival's success.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Hebrew having no native speakers at all — was substantially solved by roughly the mid-20th century. The native-generative standard's continued operation as the SOLE arbiter of linguistic 'life,' decades after the survival crisis ended, is a candidate mandatrophy: an apparatus built to solve an acute crisis (extinction risk) now primarily allocates ongoing status and institutional resources among Hebrew-adjacent communities. Classifying this as tangled_rope rather than snare preserves the genuine, non-fabricated coordination achievement (a mutually intelligible national vernacular genuinely exists and genuinely required this kind of standardization to emerge) while still registering the asymmetric, continuing cost imposed on communities whose transmission mode was never native-generative and cannot become so.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reconstructed_vs_natural_kernel,
    'Is the native-generative Hebrew standard a natural continuation of an organic vernacular, or was it a deliberately engineered kernel (lexical committees, phonological standardization boards) that only later naturalized into ''how Hebrew simply is''?',
    'Historical-linguistic analysis of the Hebrew Language Committee/Academy''s early 20th-century interventions versus organic usage drift in the Yishuv population; comparison with unplanned vernacularization cases.',
    'If substantially engineered, the native-generative standard is better modeled as a constructed institutional kernel with identifiable authors and beneficiaries rather than an emergent natural-language fact — reinforcing the tangled_rope rather than any mountain-adjacent reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstructed_vs_natural_kernel, empirical, 'Whether the native-generative kernel was engineered or organically emergent.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the native_generative, liturgical_preservation, and bridge_pidginized readings of Hebrew continuity genuinely incommensurable claims about what makes a language ''alive,'' or do they secretly share an underlying continuity concept that could in principle be reconciled into one measure?',
    'Comparative sociolinguistic theory of language vitality (e.g. Fishman''s GIDS scale, EGIDS) applied to see whether a single vitality metric could subsume all three readings without loss, or whether they measure structurally distinct phenomena (intergenerational transmission vs. functional domain coverage vs. textual continuity).',
    'If genuinely incommensurable, the three-story decomposition is structurally necessary (per the epsilon-invariance principle) and this story''s isolation of native-generative extraction is correct. If reconcilable, the family may need a fourth ''umbrella'' story documenting the reconciliation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the sibling kernel readings are structurally distinct or convergent under a shared vitality measure.').

omega_variable(
    mandatrophy_resolution_timing,
    'At what point, if any, did the native-generative standard''s function shift from solving an acute extinction-prevention problem to primarily allocating institutional status among Hebrew-adjacent communities?',
    'Track Israeli Hebrew Language Academy ruling activity and public discourse volume against independent measures of vernacular robustness (native-speaker population growth, domain coverage) to find the inflection point where standardization effort decoupled from marginal vitality gain.',
    'Pinpointing the transition would sharpen the temporal measurements and could shift the classification toward a more clearly mandatrophy-resolved framing, or alternatively show the founding problem remains partially live (e.g. contact-language erosion pressure) longer than assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_resolution_timing, empirical, 'When the standard''s dominant function shifted from survival to status allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__native_generative, theater_ratio, 20, 0.12).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__native_generative, theater_ratio, 40, 0.17).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__native_generative, theater_ratio, 60, 0.21).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__native_generative, theater_ratio, 80, 0.25).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__native_generative, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__native_generative, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__native_generative, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__native_generative, base_extractiveness, 60, 0.49).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__native_generative, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__native_generative, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__native_generative, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__native_generative, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__native_generative, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(hebr_su_t80, hebrew_continuity__native_generative, suppression_requirement, 80, 0.63).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__native_generative, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__native_generative, 0.1).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the colloquial label 'Hebrew continuity' per the epsilon-invariance principle: native_generative (this story, tangled_rope — genuine vernacularization coordination plus asymmetric delegitimization of non-native-generative transmission), liturgical_preservation (sibling — continuity through ritual/textual transmission, different beneficiary/victim structure), and bridge_pidginized (sibling — continuity through diaspora contact-language function). Each has a distinct epsilon and stakeholder set. The native_generative reading historically emerged first as an institutional project and its success created downstream legitimacy pressure on the other two readings, which is why this story is authored as upstream in the affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__native_generative, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
