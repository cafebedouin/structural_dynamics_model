% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: 1928 Turkish Script Reform Read as Deliberate Civilizational Rupture
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   In 1928 the Turkish Republic replaced the Ottoman Arabic-based script
 *   with a Latin alphabet within a matter of months, banning the old script
 *   from official and eventually print use and launching mass literacy
 *   campaigns in the new alphabet. This story reads that reform through its
 *   rupture function: the deliberate severance of the population's everyday
 *   textual continuity with the Ottoman and Islamic past, as a load-bearing
 *   part of Kemalist nation-building rather than an incidental side effect of
 *   technical modernization. Under DP-001 ε-invariance, this reading is
 *   authored as its own constraint with its own stable, very-high
 *   extractiveness value — the sibling readings (continuity_reading, which
 *   reads the same event as preserving adapted Ottoman-Islamic continuity,
 *   and modernization_reading, which reads it as technical/scientific
 *   enablement with Turkish identity preserved) are separate constraints in
 *   the orthographic_kernel family, not blended into this one.
 *
 * KEY AGENTS:
 *   - kemalist_state_apparatus: agenda-setter, designs and enforces the rupture (institutional/arbitrage)
 *   - secularizing_elite: primary beneficiary, converts existing advantage into discourse monopoly (powerful/mobile)
 *   - pre_reform_literate_population: primary victim, rendered functionally illiterate overnight (moderate/trapped)
 *   - ulema_and_religious_scholars: targeted victim, transmission chain of religious authority is the explicit rupture target (organized/trapped)
 *   - historians_of_turkish_language_policy: analytical observer, corroborates founding-problem claim from archival record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.88).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.9).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "1928 Turkish Script Reform Read as Deliberate Civilizational Rupture").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, 'd6245322-39a5-4e86-8fb9-9cbd190b2db4').
narrative_ontology:cs_kernel_codification('d6245322-39a5-4e86-8fb9-9cbd190b2db4', formalized).
narrative_ontology:cs_authority_grounding('d6245322-39a5-4e86-8fb9-9cbd190b2db4', extraction).
narrative_ontology:cs_interpretation_layer_present('d6245322-39a5-4e86-8fb9-9cbd190b2db4').
narrative_ontology:cs_reading_relation('d6245322-39a5-4e86-8fb9-9cbd190b2db4', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('d6245322-39a5-4e86-8fb9-9cbd190b2db4', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('d6245322-39a5-4e86-8fb9-9cbd190b2db4', foundational, deliberate_civilizational_severance_is_legitimate_founding_act).
narrative_ontology:cs_axiom_status(deliberate_civilizational_severance_is_legitimate_founding_act, holdable).
narrative_ontology:cs_axiom_grounding('d6245322-39a5-4e86-8fb9-9cbd190b2db4', deliberate_civilizational_severance_is_legitimate_founding_act, conventional).
narrative_ontology:cs_axiom('d6245322-39a5-4e86-8fb9-9cbd190b2db4', secondary, ottoman_islamic_textual_authority_must_not_transmit_to_new_generation).
narrative_ontology:cs_axiom_status(ottoman_islamic_textual_authority_must_not_transmit_to_new_generation, holdable).
narrative_ontology:cs_axiom_grounding('d6245322-39a5-4e86-8fb9-9cbd190b2db4', ottoman_islamic_textual_authority_must_not_transmit_to_new_generation, instrumental).
narrative_ontology:cs_reference_frame('d6245322-39a5-4e86-8fb9-9cbd190b2db4', ottoman_arabic_script_administrative_continuity).
narrative_ontology:cs_drift_state('d6245322-39a5-4e86-8fb9-9cbd190b2db4', post_1928_legislative_enactment, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('d6245322-39a5-4e86-8fb9-9cbd190b2db4', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, new_literacy_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, secularizing_elite).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, pre_reform_literate_population).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ulema_and_religious_scholars).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_bureaucratic_class).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, elderly_and_rural_populations).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, national_rupture_necessity_doctrine).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, civilizational_reorientation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, legislates, and enforces the 1928 Latin alphabet law within months, closing Arabic-script publishing, retraining teachers, and criminalizing continued official use of the old script. Frames the change as severing ties to an Ottoman-Islamic past it wants ended, not merely modernizing orthography. Collects the political capital of a decisive founding rupture and the administrative leverage of a citizenry newly dependent on state-run literacy campaigns.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Urban, Western-educated reformist class whose social and political capital rises as the new script devalues classical Ottoman education and religious credentialing. They already possess or can rapidly acquire Latin literacy and European-language fluency, so the rupture converts their existing advantages into a durable monopoly on legitimate public discourse.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, secularizing_elite, beneficiary,
    powerful, generational, mobile, national).

% The Millet Mektepleri (Nation's Schools) apparatus and associated printing/education ministries expand enormously to teach the new script nationwide, gaining budget, staff, and permanent institutional relevance from a population rendered newly illiterate by state decree and then re-taught by the same state.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, new_literacy_bureaucracy, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, new_literacy_bureaucracy, agenda_setter).

% Millions of adults who were literate in Ottoman Arabic script became functionally illiterate overnight when the state banned the script in official and print use. Cannot read their own accumulated correspondence, contracts, or religious texts without state-sponsored retraining; no meaningful exit exists inside Turkey's borders since publishing, schooling, and administration all shift at once.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, pre_reform_literate_population, payer,
    moderate, biographical, trapped, national).

% Religious scholarly authority rested on direct fluency with Arabic-script Quranic and juridical texts; the rupture is designed to sever that authority's transmission chain to the new generation. Their institutional base (medreses) is dismantled in the same reform wave, leaving them with no lawful public platform from which to contest the change.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ulema_and_religious_scholars, payer,
    organized, civilizational, trapped, national).

% Career civil servants trained under the old administrative and legal script find their professional credentials devalued within a single legislative session; those who cannot rapidly retrain lose employment or are displaced by younger Latin-script-trained hires favored by the new state apparatus.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_bureaucratic_class, payer,
    moderate, biographical, constrained, national).

% Rural and older populations, least able to attend new-script literacy campaigns due to distance, labor demands, or age, are effectively permanently cut off from participation in print culture, official documents, and eventually intergenerational transmission of family and religious texts.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, elderly_and_rural_populations, payer,
    powerless, biographical, trapped, regional).

% Those who would frame the same script change as either preserving Ottoman-Islamic continuity in adapted form, or as pure technical modernization without civilizational rupture intent, are not part of this reading's account — they populate the sibling constraints (continuity_reading, modernization_reading), which this story does not narrate or average against.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, continuity_and_modernization_reading_advocates, excluded,
    organized, generational, constrained, national).

% Assess archival evidence, parliamentary debate records, and Ataturk's own stated rationale to determine whether the reform's dominant intent was rupture, modernization, or some blend — producing the corroborating (and sometimes disputing) record this reading's founding-problem claim is checked against.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, historians_of_turkish_language_policy, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes a single national script for administration, education, and print, replacing a script the reformist state judged inadequate for phonetic Turkish and for the nation-building project it wanted to run — a genuine technical coordination problem (script-phoneme fit, print standardization) exists underneath the rupture framing.
% TRANSFER_FUNCTION: Moves cultural, religious, and administrative authority from those literate in and credentialed by the Ottoman-Arabic script tradition to those who can be rapidly certified in the new Latin script by the state's own institutions — a transfer of legitimacy and employability from an old elite and mass literate public to a new elite and a state literacy apparatus.
% ABSENT_VOICES: The ulema and much of the pre-reform literate public had no forum to contest the reform's speed or its explicit anti-Ottoman, anti-Islamic framing; parliamentary debate was tightly managed and the reform was implemented as a fait accompli within months, foreclosing gradualist alternatives before they could be argued.
% DISAPPEARANCE_RATIONALE: Had the script change (specifically its rupture-intent framing and enforcement) not occurred, Ottoman-Arabic-literate networks of religious, legal, and bureaucratic authority would likely have persisted alongside gradual orthographic reform, print culture would look different, and the state's claim to civilizational discontinuity from the Ottoman-Islamic past would lack its most visible instrument.
% FOUNDING_PROBLEM: The reformist state sought to permanently and visibly break the population's everyday connection to Ottoman and Islamic textual tradition, using script as the most totalizing available lever — every literate act would now require the new alphabet, making the rupture unavoidable and irreversible in daily life, not merely declared in law.
% FOUNDING_PROBLEM_CORROBORATION: Ataturk's own 1928 speeches and Kemalist party documents explicitly frame the reform in civilizational-rupture terms ('Turkey will free itself from the fetters that have kept it from the civilized world'), corroborating this reading from within the reforming coalition itself. Independent linguists and historians outside the Kemalist tradition (e.g., Geoffrey Lewis's account of the reform as a 'catastrophic success') corroborate that severance of textual continuity, not merely phonetic efficiency, was a deliberate and understood consequence, not an accidental byproduct.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.88, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very high (0.72 rising to 0.88) because the reading's victim set is the entire pre-reform literate population and the beneficiary set is the new state apparatus and secularizing elite who convert the disruption into durable advantage — this is the expected structural delta for the rupture reading specifically, distinct from the lower-ε modernization reading's technical framing. Suppression starts near-maximal (0.95) reflecting the abrupt legal ban on the old script and criminalization of its official use, then eases modestly as the new generation is raised entirely inside the new alphabet and active suppression becomes less necessary (0.75 by 1960) — enforcement intensity naturally declines once the cohort that needed coercing has been replaced by one raised inside the new system. Theater ratio stays low throughout (0.10-0.20): the literacy campaigns were substantially functional, not merely performative, even though their coordination function rode alongside a genuinely extractive rupture agenda. Accessibility collapse is very high (0.93) because within a single generation the old script became functionally unreadable to the general population.
 *
 * PERSPECTIVAL GAP:
 *   From the kemalist_state_apparatus seat, this reform is a decisive, necessary founding act that the engine may compute as coordination-forward; from the ulema and pre_reform_literate_population seats, the identical structural event computes as extraction-forward — loss of authority, loss of functional literacy, imposed at speed with no exit. The engine derives this divergence from the declared power/exit/beneficiary-victim structure; this reading does not adjudicate which seat is 'correct' about the reform's overall value, only what the rupture-intent reading's own structural data implies.
 *
 * DIRECTIONALITY LOGIC:
 *   The kemalist_state_apparatus and secularizing_elite sit at the beneficiary end: the state gains a durable instrument of civilizational reorientation and a permanently dependent literacy-training relationship with its population; the secularizing elite converts pre-existing Western-language fluency into discourse monopoly at the very moment their competitors' credentials are devalued. The pre_reform_literate_population, ulema, ottoman_bureaucratic_class, and elderly_and_rural_populations sit at the target end with trapped or constrained exit: there was no national territory within which the old script retained official standing, so exit required either successful retraining (itself state-administered) or acceptance of functional illiteracy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview resolves as contested rather than dead or fully live: the coordination problem (a phonetically adequate national script) was genuinely solved and remains solved, but the deliberate rupture component of the reform's original intent has no ongoing 'problem' status independent of the political project that motivated it in 1928 - it was a one-time civilizational severance, not a persisting coordination need. This is precisely the case classification exists to distinguish: treating the entire reform as pure coordination (rope) would erase the documented deliberate-rupture intent and its victim set; treating it as pure extraction (snare) would erase the genuine phonetic/administrative coordination the new script does provide. Tangled rope captures both: real coordination function, real and asymmetric extraction, riding the same structure, requiring active enforcement to hold in its early years.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_intent_vs_side_effect,
    'Was the severance of Ottoman-Islamic textual continuity a deliberate, load-bearing goal of the 1928 reform''s designers, or a foreseeable but secondary consequence of a primarily technical/modernizing script choice?',
    'Close reading of Kemalist parliamentary debate transcripts, Ataturk''s contemporaneous speeches, and internal party correspondence around the Alphabet Commission''s deliberations, cross-checked against independent historiography (e.g., Lewis, Zurcher) written outside the Kemalist tradition.',
    'If deliberate rupture is corroborated as primary intent, this reading''s very-high ε and tangled_rope classification stand as the dominant structural account. If historiography instead supports the modernization framing as primary and rupture as unintended fallout, resource weight should shift toward the modernization_reading sibling constraint and this reading''s confidence should be downgraded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_intent_vs_side_effect, empirical, 'Whether civilizational rupture was designed intent or foreseeable side effect of the script reform.').

omega_variable(
    generational_suppression_decay_mechanism,
    'Does the declining suppression_requirement trajectory (0.95 to 0.75) reflect genuine internalization/normalization of the new script by a new generation, or continued but less visible coercive enforcement (e.g., exclusion of Arabic-script materials from libraries and curricula) that the suppression metric under-measures over time?',
    'Track post-1928 archival policy on Arabic-script material access (library holdings, religious text availability, legal recognition of old documents) rather than relying solely on legal-ban intensity as a proxy for suppression.',
    'If suppression persisted in less visible institutional forms, the true suppression trajectory is flatter than authored and the constraint''s classification-relevant enforcement burden is understated at later time points.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_suppression_decay_mechanism, empirical, 'Whether declining suppression reflects normalization or merely less visible enforcement.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the rupture_reading, continuity_reading, and modernization_reading decomposition itself exhaustive, or does a fourth framing exist (e.g., a purely pragmatic literacy-rate framing indifferent to civilizational content) that would further split this kernel?',
    'Survey comparative script-reform literature (e.g., Vietnamese romanization, Soviet Central Asian Latinization waves) for additional structurally distinct framings applied to comparable reforms, and test whether any produces a materially different ε or victim/beneficiary set from the three declared readings.',
    'If a fourth framing is structurally distinct (different ε, different victim set), the kernel should be expanded with an additional sibling constraint rather than folding the new framing into one of the existing three.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three declared kernel readings exhaust the structurally distinct framings of the script reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 1928, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_kernel__rupture_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement(orth_tr_t1932, orthographic_kernel__rupture_reading, theater_ratio, 1932, 0.14).
narrative_ontology:measurement(orth_tr_t1938, orthographic_kernel__rupture_reading, theater_ratio, 1938, 0.17).
narrative_ontology:measurement(orth_tr_t1945, orthographic_kernel__rupture_reading, theater_ratio, 1945, 0.19).
narrative_ontology:measurement(orth_tr_t1952, orthographic_kernel__rupture_reading, theater_ratio, 1952, 0.2).
narrative_ontology:measurement(orth_tr_t1960, orthographic_kernel__rupture_reading, theater_ratio, 1960, 0.2).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_kernel__rupture_reading, base_extractiveness, 1928, 0.72).
narrative_ontology:measurement(orth_be_t1932, orthographic_kernel__rupture_reading, base_extractiveness, 1932, 0.8).
narrative_ontology:measurement(orth_be_t1938, orthographic_kernel__rupture_reading, base_extractiveness, 1938, 0.85).
narrative_ontology:measurement(orth_be_t1945, orthographic_kernel__rupture_reading, base_extractiveness, 1945, 0.87).
narrative_ontology:measurement(orth_be_t1952, orthographic_kernel__rupture_reading, base_extractiveness, 1952, 0.88).
narrative_ontology:measurement(orth_be_t1960, orthographic_kernel__rupture_reading, base_extractiveness, 1960, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_kernel__rupture_reading, suppression_requirement, 1928, 0.95).
narrative_ontology:measurement(orth_su_t1932, orthographic_kernel__rupture_reading, suppression_requirement, 1932, 0.92).
narrative_ontology:measurement(orth_su_t1938, orthographic_kernel__rupture_reading, suppression_requirement, 1938, 0.88).
narrative_ontology:measurement(orth_su_t1945, orthographic_kernel__rupture_reading, suppression_requirement, 1945, 0.82).
narrative_ontology:measurement(orth_su_t1952, orthographic_kernel__rupture_reading, suppression_requirement, 1952, 0.78).
narrative_ontology:measurement(orth_su_t1960, orthographic_kernel__rupture_reading, suppression_requirement, 1960, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the orthographic_kernel (the 1928 Turkish script reform), decomposed per DP-001 ε-invariance because the natural-language label 'the script reform' covers structurally distinct claims with materially different ε values: rupture_reading (this story, ε=0.88, tangled_rope, victim set = entire pre-reform literate population) is the highest-extraction reading; modernization_reading (ε moderate, rope-leaning, framing the change as technical/scientific enablement) is the lowest-extraction reading; continuity_reading (ε very low, framing the reform as continuity-preserving adaptation) sits between them in narrative content but is structurally closer to a mountain/rope framing. All three share the same historical event and text corpus but author different beneficiary/victim structures and different ε. They are linked via affects_constraints rather than merged, per the BGS decomposition pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
