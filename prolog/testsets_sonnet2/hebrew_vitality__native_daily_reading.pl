% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint captures one specific reading within the contested
 *   'Hebrew vitality' kernel: the claim, central to the ideological
 *   self-understanding of the Zionist linguistic revival, that ONLY
 *   spontaneous native daily generation — children acquiring Hebrew as a
 *   mother tongue and using it unreflectively for the full range of daily
 *   life, including profane and technical registers — constitutes genuine
 *   linguistic 'life.' Under this reading, the continuous liturgical, legal,
 *   and literary use of Hebrew across nearly two millennia of Jewish diaspora
 *   life is reclassified as mere 'preservation': a freezer, not a heartbeat.
 *   This reading required real institutional enforcement (Hebrew-only
 *   schooling, lexical modernization committees, suppression of rival
 *   vernaculars, especially Yiddish) to manufacture the native-generation
 *   condition it then treats as proof of vitality — a coordination function
 *   (a shared national vernacular) riding alongside asymmetric extraction
 *   (delegitimizing liturgical and diaspora-vernacular custodians' own claims
 *   to living tradition).
 *
 * KEY AGENTS:
 *   - zionist_state_building_project: agenda_setter/beneficiary (institutional/arbitrage) — defines and enforces the vitality criterion
 *   - ivrit_pedagogical_establishment: agenda_setter/beneficiary (organized/arbitrage) — administers Hebrew-only enforcement
 *   - sabra_native_speaker_generation: beneficiary (moderate/constrained) — the living proof-object of the claim
 *   - liturgical_tradition_custodians: payer (organized/constrained) — demoted from 'living tradition' to 'preservation'
 *   - diaspora_yiddish_speakers: payer (powerless/trapped) — actual native vernacular suppressed to install Hebrew's
 *   - non_revivalist_religious_communities: payer (organized/constrained) — theologically opposed to vernacularization, reclassified as non-vital
 *   - historical_linguists: observer (analytical/analytical) — evaluate the criterion's defensibility
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
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '4ef45ce3-fc46-4530-9179-5154a2c3ca96').
narrative_ontology:cs_kernel_codification('4ef45ce3-fc46-4530-9179-5154a2c3ca96', distributed).
narrative_ontology:cs_authority_grounding('4ef45ce3-fc46-4530-9179-5154a2c3ca96', extraction).
narrative_ontology:cs_interpretation_layer_present('4ef45ce3-fc46-4530-9179-5154a2c3ca96').
narrative_ontology:cs_reading_relation('4ef45ce3-fc46-4530-9179-5154a2c3ca96', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('4ef45ce3-fc46-4530-9179-5154a2c3ca96', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('4ef45ce3-fc46-4530-9179-5154a2c3ca96', foundational, spontaneous_native_acquisition_is_necessary_for_vitality).
narrative_ontology:cs_axiom_status(spontaneous_native_acquisition_is_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('4ef45ce3-fc46-4530-9179-5154a2c3ca96', spontaneous_native_acquisition_is_necessary_for_vitality, conventional).
narrative_ontology:cs_axiom('4ef45ce3-fc46-4530-9179-5154a2c3ca96', secondary, liturgical_use_alone_constitutes_non_life).
narrative_ontology:cs_axiom_status(liturgical_use_alone_constitutes_non_life, holdable).
narrative_ontology:cs_axiom_grounding('4ef45ce3-fc46-4530-9179-5154a2c3ca96', liturgical_use_alone_constitutes_non_life, instrumental).
narrative_ontology:cs_reference_frame('4ef45ce3-fc46-4530-9179-5154a2c3ca96', pre_revival_diaspora_liturgical_hebrew).
narrative_ontology:cs_drift_state('4ef45ce3-fc46-4530-9179-5154a2c3ca96', contemporary_israeli_national_narrative, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ef45ce3-fc46-4530-9179-5154a2c3ca96', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, ivrit_pedagogical_establishment).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, sabra_native_speaker_generation).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition_custodians).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, non_revivalist_religious_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, language_death_and_rebirth_thesis).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, vernacularization_as_national_project).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the ideological and institutional criterion that only spontaneous native daily generation of Hebrew counts as 'living language,' and builds schools, the Hebrew Language Committee, and later the Academy of the Hebrew Language to enforce this standard against rival vernaculars (chiefly Yiddish) and against purely liturgical use. Collects legitimacy and territorial-national coherence from the claim that Hebrew was 'reborn,' not merely maintained.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, zionist_state_building_project, beneficiary).

% Teachers, lexicographers, and the Hebrew-only immersion (Ivrit b'Ivrit) movement administer the enforcement machinery: Hebrew-only schooling, suppression of Yiddish and Arabic in Yishuv public life, coining of new vocabulary for daily/technical domains liturgical Hebrew never needed. Their professional and institutional standing depends on native generation being the measure of success, not on liturgical continuity being sufficient.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, ivrit_pedagogical_establishment, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, ivrit_pedagogical_establishment, beneficiary).

% The first generation of children raised with Hebrew as a mother tongue in Ottoman/Mandate Palestine. They receive full linguistic capital and social belonging from being living proof of the vitality claim, but their exit from the imposed monolingual norm was limited — the household and street environment was actively engineered around them.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, sabra_native_speaker_generation, beneficiary,
    moderate, biographical, constrained, national).

% Rabbinic authorities and communities for whom Hebrew's sanctity rested precisely in its reserved, non-vernacular liturgical use (loshn koydesh). Under this reading their millennia-long practice of continuous prayer, study, and textual transmission is redefined as mere 'preservation,' not 'life' — a demotion that delegitimizes their claim to be custodians of a living tradition and cedes cultural authority to the secular revival project. They cannot exit the reframing; the vitality criterion is imposed on the language they still use daily in a different register.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition_custodians, payer,
    organized, civilizational, constrained, global).

% Millions of Ashkenazi Jews for whom Yiddish, not Hebrew, was the actual vernacular of daily native generation. The native-generation criterion applied to Hebrew required actively suppressing Yiddish's own claim to vitality within the Zionist linguistic hierarchy (the 'Language War' of the 1910s-1930s); many had no institutional power to resist the reclassification of their mother tongue as an obstacle to Hebrew's revival, and post-Holocaust demographic collapse foreclosed most exit options entirely.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_yiddish_speakers, payer,
    powerless, biographical, trapped, regional).

% Haredi and other communities that maintained Hebrew/Aramaic as sacred-register languages while speaking Yiddish or other vernaculars at home, explicitly rejecting vernacular Hebrew revival on theological grounds (some viewing it as blasphemous appropriation of the holy tongue for profane use). The native-generation criterion frames their continued practice as backward-looking non-vitality, pressuring them either to assimilate to secular Ivrit norms or remain permanently classified as outside 'living' Hebrew culture.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, non_revivalist_religious_communities, payer,
    organized, generational, constrained, national).

% Scholars of language death and revival who evaluate whether the native-generation criterion is a defensible linguistic standard (distinguishing Hebrew from cases with no continuous use at all) or an ideologically motivated redefinition that erases the linguistic work liturgical, legal, and literary Hebrew performed continuously for centuries.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, teachable, enforceable standard for what counts as a 'living' national language, enabling mass Hebrew-medium schooling, army, bureaucracy, and print culture to coordinate around one vernacular rather than fragmenting across Yiddish, Ladino, Arabic, and diaspora languages brought by immigrants.
% TRANSFER_FUNCTION: Moves cultural legitimacy and the status of 'authentic living tradition' away from liturgical and diaspora-vernacular custodians and toward the secular Zionist revival project and its native-speaker beneficiaries; moves linguistic capital from Yiddish speakers to Hebrew speakers via institutional suppression of the former.
% ABSENT_VOICES: Yiddishist cultural nationalists (the Czernowitz conference tradition) and Haredi anti-revivalist rabbis both had substantial things to say about why vernacular Hebrew revival was either linguistically unnecessary or theologically illegitimate; both were structurally marginalized from the Yishuv's language-policy institutions that decided the vitality criterion.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion were dropped in favor of treating liturgical continuity as sufficient vitality, the entire ideological architecture crediting Zionism with a unique 'linguistic miracle' would lose its distinguishing claim — Hebrew would be understood as one of several continuously-used sacred/literary languages (like Sanskrit or Church Latin) rather than a uniquely 'resurrected' vernacular, undercutting a founding narrative of Israeli national identity and the pedagogical institutions built on it.
% FOUNDING_PROBLEM: Jewish immigrants to late Ottoman/Mandate Palestine arrived speaking dozens of different vernaculars with no shared spoken language; a criterion was needed to justify and drive the costly, coercive project of making Hebrew everyone's actual mother tongue rather than settling for a shared vernacular of convenience (Yiddish, Arabic, or German were all live candidates).
% FOUNDING_PROBLEM_CORROBORATION: Contemporary sociolinguists (e.g., scholarship on language revival cases outside the Zionist tradition) and non-Zionist Hebrew liturgical communities attest that shared vernacular coordination has long since been achieved and does not require continued denial of liturgical vitality; this corroboration comes from scholars and communities outside the beneficiary set (the Zionist pedagogical and state-building institutions), who have an interest in the native-generation criterion's continued ideological force precisely because it is now doing legitimation work rather than coordination work.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness sits at a moderate 0.52 (rising through the peak coercive period 1900-1935, then relaxing somewhat post-statehood as the criterion's ideological work was largely complete and enforcement pressure could ease) because the native-generation criterion did real coordination work (a shared national vernacular was a genuine collective-action solution to Yishuv linguistic fragmentation) while simultaneously performing an asymmetric reclassification that stripped liturgical and Yiddish-speaking communities of their own claims to linguistic vitality — without their consent and largely without compensating benefit to them. Suppression is higher (0.61, peaking at 0.72 during the interwar 'Language War') because the criterion's persistence depended on actively suppressing Yiddish in Yishuv public life and marginalizing anti-revivalist religious voices, not merely on the criterion's inherent explanatory appeal. Theater ratio is low-moderate (0.22): the coordination function was substantially real (Hebrew genuinely became a functioning national vernacular), so this is not primarily a performative constraint, though the continued ideological insistence on the native-generation criterion after statehood (once the coordination problem was solved) shows a rising theatrical component.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project and the pedagogical establishment sit near the full-beneficiary end: they set the criterion, administer its enforcement, and collect the legitimacy payoff of the 'linguistic miracle' narrative. The sabra generation benefits (full native fluency, social centrality) but had constrained exit from the monolingual environment engineered around them as children. Liturgical custodians, Yiddish speakers, and non-revivalist religious communities sit near the full-target end: the criterion is imposed on their linguistic practice from outside, redefines their tradition's status without their participation, and — for Yiddish speakers particularly — was actively enforced against their actual native vernacular through suppression in Yishuv schools and public life.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (a shared vernacular for a linguistically fragmented immigrant population) was substantially solved by the 1930s-40s and fully solved by statehood in 1948. The native-generation criterion nonetheless persists as an ideological load-bearing element of the state's founding narrative well past the point where it was needed to solve the coordination problem — it now does legitimation work (distinguishing the 'Hebrew miracle' from ordinary continuous-use language maintenance) rather than coordination work. This is exactly the tangled_rope signature: real coordination function at founding, riding alongside continued asymmetric extraction (denial of liturgical vitality) that persists after the coordination need has been met.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_naturalness_vs_construction,
    'Is ''only native generation constitutes vitality'' a linguistically defensible criterion (distinguishing genuinely dead languages requiring artificial reconstruction from continuously-used ones) or an ideologically motivated redefinition serving Zionist state-building legitimacy needs?',
    'Comparative sociolinguistic analysis against other continuously-used liturgical/literary languages (Sanskrit, Church Latin, Classical Arabic) that are NOT claimed to have ''died'' despite lacking native daily generation for centuries — if the criterion is applied inconsistently (denied to those languages, asserted for pre-revival Hebrew), that supports the constructed-for-legitimacy reading.',
    'If the criterion is shown to be applied inconsistently across comparable cases, the claimed_type moves further toward extraction (snare-adjacent) rather than genuine coordination; if the criterion tracks a real linguistic distinction (e.g. absence of native speakers changes phonology, spontaneous lexical innovation, and register range in linguistically measurable ways), the coordination function is stronger than the tangled_rope classification suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_naturalness_vs_construction, conceptual, 'Whether the native-generation vitality criterion is a real linguistic distinction or a legitimation device for the Zionist revival narrative.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three hebrew_vitality kernel readings (native_daily_reading, liturgical_reading, hybrid_continuity_reading) locate their disagreement — is it about what ''vitality'' MEANS (a conceptual/definitional dispute) or about what HAPPENED historically (an empirical dispute about whether liturgical Hebrew ever functioned as a full vernacular)?',
    'Textual and sociolinguistic evidence on the actual functional range of Hebrew in pre-revival diaspora communities (was it ever used for the full range of daily/technical/emotional registers in any sustained community, e.g. some Yemenite or Sephardic contexts) would bear on the empirical question; the definitional question is not resolvable by evidence alone and remains a matter of contested framing.',
    'If the disagreement is purely definitional, all three readings are equally ''true'' by their own lights and the family is genuinely a case of multiple non-adjudicable framings; if there is a live empirical dispute about historical register range, one reading''s factual premises could be shown weaker than the others'', which would not foreclose the reading (definitional claims survive factual correction) but would weaken its evidentiary support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel''s sibling readings disagree about the meaning of vitality or about historical linguistic facts.').

omega_variable(
    post_statehood_persistence_function,
    'Does the native-generation criterion continue to serve any live coordination function in contemporary Israeli society, or has it become purely a legitimating narrative element with no operational role?',
    'Examine contemporary Israeli linguistic policy and education: does the native-generation framing still drive any active institutional decisions (e.g. immigrant absorption ulpan policy, Arabic-Hebrew bilingual education debates), or does it appear only in historical/national-narrative contexts?',
    'If purely narrative with no operational role, this strengthens the mandatrophy reading (founding_problem_status: dead) and would support reclassifying the constraint toward piton in its post-1960 operation even though the earlier interval shows genuine tangled_rope dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_statehood_persistence_function, empirical, 'Whether the vitality criterion still performs coordination work in contemporary Israel or has become purely narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1880, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_vitality__native_daily_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__native_daily_reading, theater_ratio, 1900, 0.14).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(hebr_tr_t1935, hebrew_vitality__native_daily_reading, theater_ratio, 1935, 0.2).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_vitality__native_daily_reading, theater_ratio, 1948, 0.24).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_vitality__native_daily_reading, theater_ratio, 1960, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_vitality__native_daily_reading, base_extractiveness, 1880, 0.3).
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__native_daily_reading, base_extractiveness, 1900, 0.42).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(hebr_be_t1935, hebrew_vitality__native_daily_reading, base_extractiveness, 1935, 0.58).
narrative_ontology:measurement(hebr_be_t1948, hebrew_vitality__native_daily_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(hebr_be_t1960, hebrew_vitality__native_daily_reading, base_extractiveness, 1960, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_vitality__native_daily_reading, suppression_requirement, 1880, 0.35).
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__native_daily_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(hebr_su_t1935, hebrew_vitality__native_daily_reading, suppression_requirement, 1935, 0.72).
narrative_ontology:measurement(hebr_su_t1948, hebrew_vitality__native_daily_reading, suppression_requirement, 1948, 0.63).
narrative_ontology:measurement(hebr_su_t1960, hebrew_vitality__native_daily_reading, suppression_requirement, 1960, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__native_daily_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language 'Hebrew vitality' claim per the ε-invariance principle. native_daily_reading (this file) authors moderate ε reflecting institutional-enforcement extraction against liturgical/diaspora-vernacular custodians; liturgical_reading (sibling) authors a different beneficiary/victim structure entirely (liturgical continuity itself as the kernel, likely low ε as pure preservation coordination); hybrid_continuity_reading (sibling) treats liturgical substrate as necessary-but-insufficient, likely intermediate ε. Each reading is evaluated by its own lights per the ε-referent rule for kernel readings; they are not averaged or reconciled into a single value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
