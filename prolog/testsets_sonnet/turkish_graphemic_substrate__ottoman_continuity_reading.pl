% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__ottoman_continuity_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__ottoman_continuity_reading
 *   human_readable: Arabic Script as Legitimate Graphemic Substrate of Turkish-Ottoman Continuity
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   This story instantiates the ottoman_continuity_reading of the contested
 *   turkish_graphemic_substrate kernel: the claim that Turkish linguistic
 *   identity is structurally continuous with Ottoman-Islamic civilization,
 *   and that Arabic script is therefore the legitimate graphemic substrate
 *   for that identity — not merely a practical or transitional choice but a
 *   civilizational commitment. This reading is held by the ulema, the Ottoman
 *   bureaucratic-literary establishment, sufi orders, and the sultan-caliph's
 *   court, who each derive institutional legitimacy or material advantage
 *   from the continuity claim. It is a distinct constraint from the
 *   secular_nationalist_reading (which asserts Turkish identity as
 *   discontinuous from the Ottoman-Islamic past and Latin script as the
 *   legitimate substrate) and from the gradual_transition_reading (which
 *   brackets the legitimacy question entirely in favor of a managed
 *   coexistence period). The three readings are not measured on a shared ε
 *   scale by design — each has its own beneficiary/victim structure and its
 *   own persistence logic; only this one is authored here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, 0.42).
domain_priors:suppression_score(turkish_graphemic_substrate__ottoman_continuity_reading, 0.35).
domain_priors:theater_ratio(turkish_graphemic_substrate__ottoman_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__ottoman_continuity_reading, "Arabic Script as Legitimate Graphemic Substrate of Turkish-Ottoman Continuity").
narrative_ontology:topic_domain(turkish_graphemic_substrate__ottoman_continuity_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__ottoman_continuity_reading, 'bc3643cc-54df-43a9-86f7-c1191c8cb904').
narrative_ontology:cs_kernel_codification('bc3643cc-54df-43a9-86f7-c1191c8cb904', distributed).
narrative_ontology:cs_authority_grounding('bc3643cc-54df-43a9-86f7-c1191c8cb904', lineage).
narrative_ontology:cs_interpretation_layer_present('bc3643cc-54df-43a9-86f7-c1191c8cb904').
narrative_ontology:cs_reading_relation('bc3643cc-54df-43a9-86f7-c1191c8cb904', turkish_graphemic_substrate__secular_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('bc3643cc-54df-43a9-86f7-c1191c8cb904', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('bc3643cc-54df-43a9-86f7-c1191c8cb904', foundational, turkish_identity_constituted_by_ottoman_islamic_lineage).
narrative_ontology:cs_axiom_status(turkish_identity_constituted_by_ottoman_islamic_lineage, holdable).
narrative_ontology:cs_axiom_grounding('bc3643cc-54df-43a9-86f7-c1191c8cb904', turkish_identity_constituted_by_ottoman_islamic_lineage, conventional).
narrative_ontology:cs_axiom('bc3643cc-54df-43a9-86f7-c1191c8cb904', foundational, arabic_script_is_necessary_vessel_of_sacred_and_civilizational_text).
narrative_ontology:cs_axiom_status(arabic_script_is_necessary_vessel_of_sacred_and_civilizational_text, overridden).
narrative_ontology:cs_axiom_grounding('bc3643cc-54df-43a9-86f7-c1191c8cb904', arabic_script_is_necessary_vessel_of_sacred_and_civilizational_text, theological).
narrative_ontology:cs_reference_frame('bc3643cc-54df-43a9-86f7-c1191c8cb904', ottoman_islamic_caliphal_continuity).
narrative_ontology:cs_drift_state('bc3643cc-54df-43a9-86f7-c1191c8cb904', post_1928_alphabet_reform, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('bc3643cc-54df-43a9-86f7-c1191c8cb904', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_and_religious_scholars).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_bureaucratic_literati).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, sufi_orders).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, traditional_madrasa_educators).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, rural_turkish_speaking_peasantry).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, non_muslim_minority_populations).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, women_excluded_from_religious_education).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, aspiring_secular_professional_classes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control Quranic exegesis, fiqh instruction, and the interpretive apparatus that only trained Arabic-script readers can access. Their social authority and material livelihood depend on the script remaining the exclusive gateway to sacred and legal text. They actively lobby the state and issue fatwas defending the script's religious necessity.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_and_religious_scholars, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_and_religious_scholars, agenda_setter).

% Career and prestige rest on mastery of the elaborate Ottoman chancery register, laden with Arabic and Persian vocabulary rendered in Arabic script. A script change devalues decades of accumulated literacy capital and threatens their monopoly on administrative and diplomatic correspondence.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_bureaucratic_literati, beneficiary,
    powerful, generational, constrained, national).

% Transmit devotional literature, hagiography, and mystical poetry through manuscript traditions bound to the Arabic script. Their lodges function as informal literacy networks; the script is inseparable from their ritual and pedagogical continuity across generations.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, sufi_orders, beneficiary,
    organized, civilizational, constrained, regional).

% Teach reading through memorization of religious texts in Arabic script; their curriculum, credentialing, and small-scale income depend on the script's continued institutional legitimacy. A script shift renders their pedagogical training obsolete almost overnight.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, traditional_madrasa_educators, beneficiary,
    moderate, generational, constrained, local).

% Face a script whose orthography poorly maps Turkish vowel harmony, producing chronically low literacy rates even where schooling exists. They bear the mismatch between the graphemic system and their spoken language without any voice in the debate over which script should be used.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, rural_turkish_speaking_peasantry, payer,
    powerless, biographical, trapped, local).

% Their own liturgical and communal scripts (Armenian, Greek, Hebrew) are treated as parallel but subordinate; participation in the imperial bureaucratic and legal apparatus requires functional literacy in the state's Arabic-script Ottoman Turkish, a system they did not choose and gain no religious benefit from.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, non_muslim_minority_populations, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__ottoman_continuity_reading, non_muslim_minority_populations, excluded).

% Formal madrasa instruction in Arabic-script literacy is overwhelmingly reserved for men; women's exclusion from the primary literacy-transmission institutions compounds under a system where literacy itself is gated by religious-institutional access.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, women_excluded_from_religious_education, payer,
    powerless, biographical, trapped, local).

% Military officers, engineers, and administrators trained in European technical curricula find the script a persistent barrier to printing efficiency, mass education, and interoperability with European scientific and administrative texts. They pay in slowed institutional modernization and constrained career mobility.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, aspiring_secular_professional_classes, payer,
    moderate, biographical, constrained, national).

% Holds the dual religious-political office whose legitimacy is partly constituted by continuity with the Islamic caliphate; sponsors religious-educational infrastructure and derives legitimating authority from being seen as steward of an Ottoman-Islamic civilizational inheritance encoded in Arabic script.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_sultan_caliph, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Study literacy statistics, publishing output, and comparative script-reform outcomes across the late Ottoman and early Republican periods to assess whether the script itself, or the institutions built around it, drove observed literacy and modernization patterns.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, later_historians_and_linguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The shared Arabic-script literary and religious canon allows a multi-ethnic, multi-lingual Islamic polity to coordinate legal, theological, and scholarly activity through a single interpretive tradition, preserving intelligibility across centuries of accumulated Ottoman administrative and devotional text.
% TRANSFER_FUNCTION: Moves interpretive authority, administrative gatekeeping power, and educational credentialing value toward those already trained in Arabic-script literacy (ulema, bureaucratic literati, sufi orders, madrasa educators), while imposing literacy-acquisition costs, career ceilings, and exclusion from modern technical and administrative advancement on rural populations, religious minorities, women, and secularizing professionals.
% ABSENT_VOICES: Rural Turkish-speaking peasants whose vernacular is poorly served by Arabic orthography, women barred from the primary literacy-transmission institutions, and non-Muslim minorities who bear the state's script choice without religious stake in it are structurally absent from the theological and bureaucratic debate over script legitimacy — the debate is conducted almost entirely among the beneficiary institutions themselves.
% DISAPPEARANCE_RATIONALE: If the claim of Ottoman-Islamic civilizational continuity via Arabic script lost its authority overnight, the ulema's interpretive monopoly, the bureaucratic literati's credentialing advantage, and the madrasa system's pedagogical relevance would all lose their legitimating grounding simultaneously — precisely what did happen, rapidly and by state fiat, in the 1928 Turkish alphabet reform, which triggered mass re-credentialing, a print-industry restructuring, and a generational literacy rupture.
% FOUNDING_PROBLEM: How to preserve a multi-century administrative, legal, and religious textual tradition binding a multi-ethnic Islamic empire together as a legible, coordinated civilizational unit, while legitimating dynastic-religious authority through demonstrated continuity with the Islamic scholarly and caliphal inheritance.
% FOUNDING_PROBLEM_CORROBORATION: The ulema and Ottoman court chroniclers attest the continuity problem remains fully live and constitutive of legitimate governance. Independent linguists and literacy historians (e.g. comparative studies of Ottoman-era literacy rates versus post-1928 rates) attest that the specific graphemic-continuity problem was substantially a construction of institutional interest rather than a linguistic necessity, since Turkish vowel harmony is demonstrably better served by an adapted Latin alphabet — this corroboration comes from outside the beneficiary institutions and is contested by them.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).
:- end_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) and suppression (0.35) are moderate-to-substantial rather than extreme: the coordination function is real (a multi-ethnic empire needs a shared administrative-religious textual tradition) but the arrangement channels literacy-transmission control toward existing religious-bureaucratic elites at the direct expense of populations whose spoken Turkish is poorly served by Arabic orthography. Theater ratio rises modestly over the interval (0.15→0.28) as the reading's defenders increasingly invoke civilizational-continuity rhetoric defensively, in response to growing secularizing pressure, rather than as a description of settled practice. Accessibility collapse (0.4) is only moderate because alternative orthographic solutions (Latin transliteration schemes, simplified Arabic orthographies) were demonstrably available and discussed by contemporaries — the collapse is institutional and political, not epistemic or technical.
 *
 * PERSPECTIVAL GAP:
 *   From the ulema's and bureaucratic literati's seats, the arrangement is a coherent, continuous civilizational inheritance whose defense is a religious-scholarly and administrative duty. From the rural peasantry's and excluded women's seats, the same arrangement is an accident of institutional power that happens to gate literacy and legal standing behind training they cannot readily obtain. The engine computes these as different seat-level classifications from the same structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The ulema, bureaucratic literati, sufi orders, and madrasa educators are structural beneficiaries: their authority, livelihood, and pedagogical relevance are constituted by the script remaining the exclusive substrate of sacred and administrative text (d near the beneficiary end). Rural peasants, non-Muslim minorities, excluded women, and secularizing professionals are structural targets: they bear either literacy-acquisition costs mismatched to their spoken language, exclusion from primary literacy-transmission institutions, or blocked modernization, without corresponding benefit (d near the target end). The sultan-caliph sits with the beneficiary coalition because dynastic-religious legitimacy is itself partly constituted by the continuity claim, even though he is nominally the arrangement's steward rather than a rent-collector in the narrow sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating a multi-ethnic Islamic polity's legal, religious, and administrative activity through one shared textual tradition — was genuinely live for centuries. By the late Ottoman period the corroboration is contested: outside linguists and reformers argue the coordination problem could be solved (indeed was later solved) by an alternative script better matched to spoken Turkish, while the continuity claim increasingly served to defend institutional position rather than to solve a live coordination problem. Classifying this as tangled_rope rather than snare or mountain preserves that ambiguity: the coordination function was real at founding and is not fabricated, but by the reading's later life the extraction component (credentialing monopoly, literacy exclusion) is doing more structural work than the coordination component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civilizational_continuity_vs_institutional_interest,
    'Is the claim that Turkish identity is continuous with Ottoman-Islamic civilization (and that Arabic script is its legitimate substrate) a genuine claim about cultural-historical continuity, or is it substantially a legitimating construction serving the material and status interests of the ulema, bureaucratic literati, and dynastic court?',
    'Comparative historical analysis of literacy rates, print output, and educational access before and after the 1928 script reform, cross-referenced against contemporaneous arguments made by reform advocates and opponents, and against comparable script-continuity claims in other post-imperial contexts (e.g. post-Soviet Central Asian scripts).',
    'If predominantly institutional interest, the coordination-function claim underlying the tangled_rope classification weakens and the constraint drifts toward snare; if predominantly genuine continuity concern, the coordination component is more substantial and the classification is more solidly tangled_rope rather than pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilizational_continuity_vs_institutional_interest, conceptual, 'Whether the continuity claim is authentic cultural argument or institutional self-interest dressed as civilizational necessity.').

omega_variable(
    kernel_reading_incommensurability,
    'Can the ottoman_continuity_reading, secular_nationalist_reading, and gradual_transition_reading be evaluated on any shared empirical basis, or do they rest on incommensurable premises about what constitutes ''legitimate'' linguistic identity that no data can adjudicate?',
    'This is inherently a conceptual/framing question rather than one resolvable by additional historical data; it can only be documented, not settled, by comparing the axiomatic commitments each reading treats as foundational.',
    'If incommensurable, the three sibling constraints are properly treated as permanently coexisting readings (coexists_with relations) rather than as competing hypotheses awaiting evidence; if a shared empirical basis exists (e.g. comparative literacy outcomes), one reading''s practical claims could be falsified even while its identity claims remain contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel''s sibling readings are empirically adjudicable or normatively incommensurable.').

omega_variable(
    religious_education_infrastructure_dependency,
    'How much of the pre-reform religious education infrastructure (madrasas, sufi lodge pedagogy) could have been preserved and adapted to a non-Arabic script, versus how much was intrinsically and irreducibly bound to Arabic-script literacy specifically?',
    'Study of contemporary and later attempts to render Islamic religious texts and instruction in transliterated or Latin-script Turkish, and of comparable Muslim-majority states that did or did not change scripts, to assess whether religious infrastructure preservation required this specific script or merely required continuity in access to the canon.',
    'If the infrastructure could largely transfer to another script, the expected structural delta (preserved religious education infrastructure) attributed to this reading is weaker than claimed and partly a matter of institutional inertia rather than genuine substrate-dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_education_infrastructure_dependency, empirical, 'Whether religious-education continuity genuinely requires Arabic script or merely requires uninterrupted institutional control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__ottoman_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(turk_tr_t8, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(turk_tr_t16, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(turk_tr_t24, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(turk_tr_t32, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(turk_tr_t40, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(turk_be_t8, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(turk_be_t16, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(turk_be_t24, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(turk_be_t32, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(turk_be_t40, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(turk_su_t8, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(turk_su_t16, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(turk_su_t24, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 24, 0.31).
narrative_ontology:measurement(turk_su_t32, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 32, 0.33).
narrative_ontology:measurement(turk_su_t40, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__ottoman_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, gradual_transition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'the Turkish script question' per the ε-invariance principle. Each reading of the turkish_graphemic_substrate kernel is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification: ottoman_continuity_reading (this story, tangled_rope), secular_nationalist_reading, and gradual_transition_reading. They are linked bidirectionally via affects_constraints because each reading's political success or failure structurally changes the resource availability and legitimacy conditions available to the others (e.g. the eventual dominance of the secular_nationalist_reading directly displaced this reading's institutional base).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
