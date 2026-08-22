% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: 1928 Turkish Alphabet Reform as Kemalist Civilizational Rupture
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   This story instantiates the Kemalist rupture reading of the
 *   script-as-identity kernel: the claim that adopting the Latin alphabet in
 *   1928 was not merely a technical orthographic improvement but the
 *   deliberate severing mechanism that made secular republican modernization
 *   possible, by cutting the population's living access to the
 *   Ottoman-Islamic Arabic-script textual and legal corpus. Under this
 *   reading the rupture is the feature, not an unfortunate cost of an
 *   otherwise neutral technical choice. The state faced zero transition cost
 *   in the sense that mattered to it — it had no entrenched incumbent
 *   institution it needed to preserve, since the whole point was to displace
 *   the incumbent Ottoman-Islamic textual authority structure. The state's
 *   decree-and-schools apparatus (Millet Mektepleri) gave it a literacy
 *   monopoly: it alone defined, at speed, who would count as literate under
 *   the new order. This is a DIFFERENT constraint, with a different ε, from
 *   the phonetic_instrumentalism_reading (which holds script choice was a
 *   neutral technical decision, near-zero identity extraction) and from the
 *   ottoman_continuity_reading (which holds Arabic script was constitutive of
 *   Turkish-Islamic identity and treats the reform as pure civilizational
 *   extraction against the ulema and Ottoman-literate public, with a
 *   correspondingly different beneficiary/victim map centered on defending
 *   continuity rather than measuring rupture-as-tool).
 *
 * KEY AGENTS:
 *   - kemalist_republican_state: agenda-setter, decrees the rupture and builds the enforcement and retraining apparatus
 *   - secular_urban_elite: primary beneficiary, already aligned with the new script's cultural orientation
 *   - ottoman_arabic_literate_generation: primary payer, loses functional literacy overnight
 *   - religious_scholars_ulema: organized payer, loses the textual monopoly underwriting their authority
 *   - kurdish_and_minority_script_traditions: powerless payer, collateral casualty of Turkish national homogenization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.62).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.78).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "1928 Turkish Alphabet Reform as Kemalist Civilizational Rupture").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, '29a492b4-19eb-422a-b9a2-7c385d1e9f2e').
narrative_ontology:cs_kernel_codification('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', distributed).
narrative_ontology:cs_authority_grounding('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', extraction).
narrative_ontology:cs_interpretation_layer_present('29a492b4-19eb-422a-b9a2-7c385d1e9f2e').
narrative_ontology:cs_reading_relation('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', foundational, textual_rupture_is_necessary_precondition_for_secular_modernization).
narrative_ontology:cs_axiom_status(textual_rupture_is_necessary_precondition_for_secular_modernization, holdable).
narrative_ontology:cs_axiom_grounding('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', textual_rupture_is_necessary_precondition_for_secular_modernization, instrumental).
narrative_ontology:cs_axiom('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', secondary, state_monopoly_on_literacy_definition_is_legitimate_transition_tool).
narrative_ontology:cs_axiom_status(state_monopoly_on_literacy_definition_is_legitimate_transition_tool, holdable).
narrative_ontology:cs_axiom_grounding('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', state_monopoly_on_literacy_definition_is_legitimate_transition_tool, conventional).
narrative_ontology:cs_reference_frame('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', ottoman_arabic_script_administrative_continuity).
narrative_ontology:cs_drift_state('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', post_1928_decree_enforcement, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('29a492b4-19eb-422a-b9a2-7c385d1e9f2e', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_republican_state).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_urban_elite).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, post_reform_literacy_cohorts).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_arabic_literate_generation).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_scholars_ulema).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, arabic_script_publishing_trades).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, kurdish_and_minority_script_traditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the Latin alphabet by decree in 1928, establishes Millet Mektepleri (Nation's Schools) to compel adult literacy retraining, bans Arabic-script printing of new material within a compressed window, and monopolizes the apparatus that defines who counts as literate going forward. Frames the rupture as the mechanism of secular modernization itself, not a side effect of it.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_republican_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Already oriented toward European institutions and print culture, this class absorbs the new alphabet quickly, staffs the new bureaucracy and press, and converts the reform into durable political and cultural capital. Its members had the least invested in Ottoman-Arabic textual authority and the most to gain from a state apparatus reorganized around their preexisting orientation.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_urban_elite, beneficiary,
    powerful, generational, mobile, national).

% Children and young adults who learn only the new script gain rapid, low-friction literacy in a genuinely more phonetically transparent system for Turkish vowel harmony, and inherit unmediated access to the state's institutions of employment, law, and press. Their gain is real, but it exists only because an entire prior corpus was placed out of their reach.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, post_reform_literacy_cohorts, beneficiary,
    moderate, generational, constrained, national).

% Adults who spent years achieving Arabic-script literacy become functionally illiterate overnight relative to the new administrative and print apparatus. Retraining is offered but is costly in time and dignity, especially for older or rural populations; many never fully transition and are permanently disadvantaged in state employment, correspondence, and civic participation.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_arabic_literate_generation, payer,
    powerless, biographical, trapped, national).

% Their authority rested substantially on exclusive command of Arabic-script religious and legal texts and on their role as interpreters of that textual tradition for the wider public. The script change, combined with the earlier abolition of the Caliphate and religious courts, severs the reproductive pipeline of their institutional authority; the new generation cannot read what makes them authoritative.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_scholars_ulema, payer,
    organized, generational, constrained, national).

% Typesetters, calligraphers, and printers whose entire craft and capital investment is in Arabic-script production are rendered obsolete within a few years by decree, with no transition subsidy comparable to the new script's promotional apparatus. Their skills do not transfer, and the market for their output is legislated out of existence.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, arabic_script_publishing_trades, payer,
    moderate, biographical, trapped, national).

% Kurdish and other minority-language communities that had used Arabic-derived scripts for their own languages lose that infrastructure as part of a state project explicitly aimed at Turkish national homogenization, with no equivalent institutional investment made in developing or legitimizing a parallel minority-language Latin orthography.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kurdish_and_minority_script_traditions, payer,
    powerless, generational, trapped, regional).

% Study the reform as a case of engineered discontinuity used to consolidate a new political order, comparing it to other 20th-century script and calendar reforms undertaken explicitly to break generational transmission of a prior authority structure.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, comparative_historians_of_nation_building, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__kemalist_rupture_reading, kemalist_republican_state).
narrative_ontology:fixing_cost_class(script_as_identity__kemalist_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, phonetically simpler national script that lowers the cost of mass literacy campaigns and standardizes state administration, education, and print around one system instead of a contested plural one.
% TRANSFER_FUNCTION: Moves cultural and institutional authority from Ottoman-Arabic textual gatekeepers (ulema, court scribes, Arabic-script publishers) to the secular republican state and the urban elite already aligned with it; moves literacy capital from the older Arabic-literate generation to newly schooled cohorts; moves minority-language script infrastructure out of existence in favor of Turkish-Latin uniformity.
% ABSENT_VOICES: The Ottoman-Arabic-literate generation, the ulema, and Kurdish/minority script communities had essentially no seat in the 1928 decision process, which was executed by decree through a small reforming circle around Mustafa Kemal; their objections surface mainly in later historical and diasporic accounts, not in contemporaneous institutional channels.
% DISAPPEARANCE_RATIONALE: If the Latin-script mandate and its enforcement apparatus vanished, Arabic-script literacy and publishing would not simply resume by default — but the state's monopoly on defining literacy would be broken, opening space for contested reintroduction of religious-textual authority structures and for minority scripts; the rupture is precisely what would need to be undone, and undoing it would require a comparable act of state will.
% FOUNDING_PROBLEM: Ottoman administrative and religious authority was seen by reformers as inseparable from Arabic-script textual transmission; a genuinely secular, rapidly literate, Western-oriented republic was judged impossible while that transmission chain remained intact, so severing it was treated as a precondition for state modernization rather than a byproduct.
% FOUNDING_PROBLEM_CORROBORATION: The Kemalist state and its historiographic successors attest the problem (Ottoman-Islamic institutional inertia blocking modernization) was real and that the rupture solved it. Independent linguists and comparative historians outside the Kemalist tradition corroborate the phonetic-transparency claim as partially real but contest that severance from the Ottoman corpus was necessary to achieve it, noting Latinization proposals had circulated since the 19th century within an Ottoman-continuity frame; minority-script historians attest the problem as framed erased their own populations' needs rather than solving a problem for them.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is substantial but not maximal: the phonetic-transparency gain for future Turkish speakers is real and not fabricated, so this reading does not collapse the constraint to pure extraction the way the ottoman_continuity_reading would. Suppression is high (0.78-0.90, declining slightly as the transition normalizes) because the reform's persistence depended on decree-backed bans on Arabic-script printing and compelled retraining, not on voluntary uptake alone. Accessibility collapse is high (0.72) because within roughly a generation, Arabic-script literacy in Turkish ceased to be a viable path to state participation. Resistance (0.55) reflects real but eventually overwhelmed opposition from religious authorities and the older literate cohort, who lacked the organizational capacity to reverse a state-enforced fait accompli.
 *
 * DIRECTIONALITY LOGIC:
 *   The state and the secular urban elite sit at the beneficiary end: the state collects legitimacy and administrative control, the elite converts pre-existing orientation into institutional capital. Post-reform literacy cohorts benefit as well, but derivatively — their gain is structurally downstream of the state's monopoly move rather than an independent coordination surplus. The Ottoman-Arabic literate generation, the ulema, the publishing trades, and minority script communities sit at the target end: each loses a form of capital (literacy, doctrinal authority, craft investment, linguistic infrastructure) that does not transfer to the new order, and each has constrained-to-trapped exit because reversing the decree was not an option available to them individually.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman-Islamic institutional structures blocking secular modernization, as the reformers framed it) is authored as contested rather than flatly dead or live: the modernization function largely completed within a generation, meaning the ongoing suppression apparatus (script laws, print controls) increasingly persisted past the point its original justification was doing real work, which is the mandatrophy signal this reading should surface rather than obscure. Classifying this as tangled_rope rather than snare preserves the fact that a genuine coordination function (mass phonetic literacy) was actually delivered to a real beneficiary population, while still naming the asymmetric extraction from those who paid the rupture's cost — collapsing it to pure snare would erase the reading's own claim that the rupture had a genuine modernizing payoff, and collapsing it to rope would erase the coercion and the losers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_as_feature_vs_incidental_cost,
    'Was the severing of Ottoman-Arabic textual access a deliberately instrumentalized mechanism of the reform''s political function, or an incidental (even regretted) cost of an otherwise phonetically motivated technical change?',
    'Archival analysis of the reforming circle''s internal deliberations, contemporaneous speeches, and the design of accompanying institutions (e.g. whether retraining apparatus was designed to erase old literacy versus merely to build new literacy) would distinguish deliberate rupture from incidental effect.',
    'If rupture was incidental, this story collapses toward the phonetic_instrumentalism_reading and ε should be much lower; if rupture was instrumentalized as this reading claims, the tangled_rope classification with substantial extraction is the accurate structural read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_as_feature_vs_incidental_cost, conceptual, 'Whether this reading''s core premise (rupture as deliberate mechanism) is historically well-founded versus a retrospective narrative imposed on a more incidental process.').

omega_variable(
    location_of_kernel_disagreement,
    'Where exactly do the three readings of the script-as-identity kernel diverge structurally — is it in the beneficiary/victim assignment, in whether script carries constitutive versus instrumental identity content, or in the empirical claim about transition necessity?',
    'Cross-reading structural comparison: hold the empirical event (1928 decree) fixed and vary only the interpretive premise about what script IS (constitutive identity marker vs. neutral technology vs. instrumentalized rupture device) to see which premise shift changes the beneficiary/victim map versus which only changes the evaluative gloss.',
    'Locating the disagreement in beneficiary/victim assignment (as this reading does, by naming the ulema and Ottoman-literate generation as victims of an instrumentalized rupture) versus locating it purely in evaluative framing determines whether the three readings are genuinely different constraints (as this framework requires) or one constraint under three moral glosses — this story is authored on the premise that they are structurally different, per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(location_of_kernel_disagreement, conceptual, 'Documents where, structurally, the kemalist_rupture, ottoman_continuity, and phonetic_instrumentalism readings actually part ways.').

omega_variable(
    minority_script_erasure_intentionality,
    'Was the erasure of Kurdish and other minority Arabic-derived script traditions a foreseen and accepted cost of Turkish national homogenization, or an unintended byproduct of a policy aimed solely at the Ottoman-Turkish Arabic script?',
    'Comparison with contemporaneous minority-language policy (e.g. Kurdish-language publishing bans of the same period) would indicate whether the state treated minority script loss as a bonus outcome of the reform or as unrelated collateral damage.',
    'If intentional, the tangled_rope classification understates the extraction directed at minority communities specifically, which may warrant treating that sub-population''s experience as closer to snare within this same reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_script_erasure_intentionality, empirical, 'Whether minority script loss was a targeted or incidental component of the reform under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 1928, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__kemalist_rupture_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement(scri_tr_t1933, script_as_identity__kemalist_rupture_reading, theater_ratio, 1933, 0.15).
narrative_ontology:measurement(scri_tr_t1938, script_as_identity__kemalist_rupture_reading, theater_ratio, 1938, 0.2).
narrative_ontology:measurement(scri_tr_t1945, script_as_identity__kemalist_rupture_reading, theater_ratio, 1945, 0.24).
narrative_ontology:measurement(scri_tr_t1952, script_as_identity__kemalist_rupture_reading, theater_ratio, 1952, 0.26).
narrative_ontology:measurement(scri_tr_t1960, script_as_identity__kemalist_rupture_reading, theater_ratio, 1960, 0.28).

% Extraction over time
narrative_ontology:measurement(scri_be_t1928, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1928, 0.55).
narrative_ontology:measurement(scri_be_t1933, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1933, 0.6).
narrative_ontology:measurement(scri_be_t1938, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1938, 0.62).
narrative_ontology:measurement(scri_be_t1945, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(scri_be_t1952, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1952, 0.61).
narrative_ontology:measurement(scri_be_t1960, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1960, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1928, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1928, 0.9).
narrative_ontology:measurement(scri_su_t1933, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1933, 0.85).
narrative_ontology:measurement(scri_su_t1938, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1938, 0.82).
narrative_ontology:measurement(scri_su_t1945, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1945, 0.79).
narrative_ontology:measurement(scri_su_t1952, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1952, 0.78).
narrative_ontology:measurement(scri_su_t1960, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1960, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__kemalist_rupture_reading, 0.08).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the script_as_identity kernel, decomposed per the epsilon-invariance principle rather than represented as a single constraint with a measurement parameter. kemalist_rupture_reading (this file, tangled_rope, epsilon=0.62) claims the 1928 reform delivered genuine phonetic-literacy coordination while instrumentalizing textual rupture as a tool of secular state-building, extracting from Ottoman-Arabic-literate populations. ottoman_continuity_reading treats the same event as extraction against a constitutive identity with no offsetting coordination function (expected higher epsilon, snare-leaning). phonetic_instrumentalism_reading treats script choice as neutral technology with negligible identity extraction (expected much lower epsilon, rope-leaning). All three share the same underlying historical event but diverge on what script IS to Turkish national identity, producing genuinely different beneficiary/victim structures and different epsilon values, not merely different evaluative glosses on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
