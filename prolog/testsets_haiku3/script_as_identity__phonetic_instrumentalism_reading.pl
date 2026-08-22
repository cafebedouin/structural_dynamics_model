% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__phonetic_instrumentalism_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: script_as_identity__phonetic_instrumentalism_reading
 *   human_readable: Script Neutrality: Latin as Phonetic Optimization
 *   domain: linguistic/state-building/political-authority
 *
 * SUMMARY:
 *   Between 1926 and 1928, the Turkish state adopted Latin script to replace
 *   Ottoman Arabic script. The phonetic_instrumentalism_reading frames this
 *   change as a technical choice: Latin script better represents Turkish
 *   phonology, particularly its vowel harmony system and consonant
 *   distinctions. On this reading, the script choice is depoliticized—it is
 *   about efficient writing, not identity rupture. This reading obscures the
 *   deeper political and identity functions: the script change severs
 *   Ottoman-Islamic continuity and transfers epistemic authority from
 *   religious scholars to secular linguists. The constraint's claimed type
 *   (rope—coordination toward a single standard) and its authored metrics
 *   (low extractiveness, high theater) diverge deliberately: the high theater
 *   ratio signals that a large share of the constraint's activity is
 *   performative justification, not functional coordination. The engine
 *   computes this divergence and surfaces the claim/metric gap as a
 *   measurement of how effectively the constraint's identity-bearing function
 *   is concealed by its technical framing.
 *
 * KEY AGENTS:
 *   - technical_linguists: institutional beneficiaries—gain credibility and authority by providing technical justification
 *   - modernization_advocates: powerful beneficiaries—gain the ability to depoliticize an identity-severing choice
 *   - ottoman_continuity_advocates: organized payers—lose institutional authority and cultural legitimacy; identity_locked exit
 *   - literate_general_population: moderate payers and incidental beneficiaries—relearn script; gain or lose textual access depending on stance
 *   - textual_heritage_keepers: moderate payers—face erosion of access to Ottoman-era written heritage
 *   - competing_script_frameworks: excluded—their technical arguments are ruled out by the assertion that Latin is objectively superior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.15).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.22).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Script Neutrality: Latin as Phonetic Optimization").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "linguistic/state-building/political-authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, '48b3ed87-d4d1-429a-a03a-cb202f54dce0').
narrative_ontology:cs_kernel_codification('48b3ed87-d4d1-429a-a03a-cb202f54dce0', formalized).
narrative_ontology:cs_authority_grounding('48b3ed87-d4d1-429a-a03a-cb202f54dce0', extraction).
narrative_ontology:cs_interpretation_layer_present('48b3ed87-d4d1-429a-a03a-cb202f54dce0').
narrative_ontology:cs_reading_relation('48b3ed87-d4d1-429a-a03a-cb202f54dce0', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('48b3ed87-d4d1-429a-a03a-cb202f54dce0', script_as_identity__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('48b3ed87-d4d1-429a-a03a-cb202f54dce0', foundational, script_is_neutral_technology).
narrative_ontology:cs_axiom_status(script_is_neutral_technology, holdable).
narrative_ontology:cs_axiom_grounding('48b3ed87-d4d1-429a-a03a-cb202f54dce0', script_is_neutral_technology, empirically_contingent).
narrative_ontology:cs_axiom('48b3ed87-d4d1-429a-a03a-cb202f54dce0', foundational, phonetic_transparency_grounds_legitimacy).
narrative_ontology:cs_axiom_status(phonetic_transparency_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('48b3ed87-d4d1-429a-a03a-cb202f54dce0', phonetic_transparency_grounds_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('48b3ed87-d4d1-429a-a03a-cb202f54dce0', script_as_technical_optimization).
narrative_ontology:cs_drift_state('48b3ed87-d4d1-429a-a03a-cb202f54dce0', post_institutionalization_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48b3ed87-d4d1-429a-a03a-cb202f54dce0', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, technical_linguists).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, modernization_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, literate_general_population).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, ottoman_continuity_advocates).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, literate_general_population).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, textual_heritage_keepers).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, phonetic_transparency_principle).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, script_technical_neutrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Linguists and script reformers who advocate for Latin script based on phonetic and typological grounds. They frame the choice as a technical matter: Latin's alphabet better represents Turkish vowel harmony and consonant distinctions than Arabic script does. Their authority derives from expertise in phonology and writing systems. They benefit by gaining institutional credibility for a script reform framed as scientifically justified rather than politically motivated.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, technical_linguists, beneficiary,
    institutional, generational, arbitrage, national).

% State apparatus and secular political elites who support script reform as part of modernization programs. They adopt the phonetic neutrality framing because it depoliticizes the decision—making it appear to be about technical optimization rather than severing ties with Ottoman-Islamic heritage. They benefit from the framing's capacity to obscure their deeper agenda (rupture with the past) under a technical justification.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, modernization_advocates, beneficiary,
    powerful, biographical, mobile, national).

% Religious scholars, traditional elites, and cultural conservatives who see Arabic script as inseparable from Turkish-Islamic identity and historical legitimacy. They pay a cultural and institutional cost when the phonetic neutrality framing succeeds in depoliticizing what they understand as an identity-bearing decision. Their exit from this constraint is identity-fused: abandoning Arabic script means accepting a break with Ottoman continuity they regard as foundational to Turkish identity.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_continuity_advocates, payer,
    organized, generational, identity_locked, national).

% Ordinary citizens who must learn whichever script is established as official. They benefit from phonetic clarity if the script genuinely represents the language better. They pay a cost in relearning, in loss of access to Ottoman-era texts, and in the severing of textual continuity. The script choice is presented to them as technical, but the constraints on their exit are real: they cannot maintain the prior script and participate fully in the modernized state.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, literate_general_population, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__phonetic_instrumentalism_reading, literate_general_population, beneficiary).

% Proponents of reformed Arabic script (Jawi, hybrid systems) or other scripts that might have served Ottoman Turkish are structurally excluded from the decision space. The phonetic neutrality framing rules them out by asserting that Latin is objectively superior on technical grounds, leaving no room for competing technical or cultural arguments. They would argue that script is always identity-bearing and that alternatives should be evaluated on both phonetic and cultural grounds.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, competing_script_frameworks, excluded,
    powerful, generational, trapped, national).

% Scholars, librarians, and cultural institutions responsible for preserving Ottoman-era written heritage in Arabic script. They pay the cost of script transition in terms of access barriers: as the population's literacy in Arabic script atrophies, the ability to read and transmit Ottoman texts decays. The phonetic neutrality framing obscures the loss of textual heritage as a side effect of a technical choice rather than recognizing it as a cost of the constraint.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, textual_heritage_keepers, payer,
    moderate, generational, constrained, national).

% Comparative linguists and international standards bodies that evaluate script choices on phonetic and typological merit. They provide external validation for the phonetic neutrality framing by confirming that Latin script does represent Turkish phonetics efficiently. They are observational because their validation is cited to justify the constraint but they do not bear its identity or cultural costs.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, international_technical_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__phonetic_instrumentalism_reading, technical_linguists).
narrative_ontology:fixing_cost_class(script_as_identity__phonetic_instrumentalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, standardized script for writing Turkish that is transparent to its phonological structure. Solves the problem of script heterogeneity (Ottoman vs. Persian vs. Arabic variants coexisting) by settling on one standard that represents all Turkish sounds unambiguously.
% TRANSFER_FUNCTION: Moves epistemic authority from religious scholars and traditional elites (who legitimized Arabic script through Islamic heritage) to technical linguists and secular modernizers (who justify Latin script through phonetic science). Transfers the cost of script relearning and textual heritage loss to the general population and cultural conservatives.
% ABSENT_VOICES: Proponents of reformed Arabic script or hybrid systems are excluded from technical debate; their argument would be that script choice is never purely technical and that cultural identity should weight equally with phonetic clarity. Ottoman continuity advocates are marginalized because the phonetic neutrality framing leaves no room for identity-based arguments.
% DISAPPEARANCE_RATIONALE: If the phonetic neutrality constraint and its underlying framing disappeared, the script choice would become explicitly political again. Arabic script would likely persist in religious and cultural domains; state institutions might revert to Arabic or adopt a hybrid system; the decision would be contested openly rather than settled by technical expertise.
% FOUNDING_PROBLEM: Ottoman Turkish was written in multiple incompatible scripts (Arabic, Persian, Ottoman variants) causing administrative confusion and limiting literacy standardization. A single, standardized script was needed to enable modern state administration and universal education.
% FOUNDING_PROBLEM_CORROBORATION: Administrative records from the late Ottoman period confirm script heterogeneity posed real standardization problems. Modernization advocates attest the problem is still live (citing necessity of administrative uniformity). Ottoman continuity advocates attest the standardization problem could have been solved by reforming Arabic script rather than replacing it, and that the founding problem was an administrative pretext for a political choice.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).
:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15 at interval end) because the reading attributes the script choice to phonetic optimization, not to power asymmetry. From the technical reading's standpoint, no one is being extracted from; a better tool is being adopted. Theater is high (0.68) because the constraint's observable activity is dominated by justification and validation of the choice (academic papers, pedagogical design, international technical correspondence) rather than by the coordination of script standardization itself, which is a relatively simple administrative task. Suppression is moderate (0.22) because the constraint does not require active coercion in the strong sense—literacy in the new script eventually becomes the default—but it does require institutional suppression of competing framings (the exclusion of identity-based script arguments from official debate, the marginalization of Ottoman continuity advocates). The measurement series shows theater_ratio rising then stabilizing: as the script becomes institutionalized and the question recedes from active debate, the ratio of performative activity to functional coordination remains high because the original technical justification must be continuously reaffirmed to prevent the underlying political question from resurfacing. Extractiveness remains low because the technical reading never acknowledges any asymmetry of power or benefit; suppression remains stable because the exclusion of alternative framings is maintained institutionally, not through increasing force.
 *
 * PERSPECTIVAL GAP:
 *   A technical_linguist seat and a modernization_advocate seat would perceive this constraint very differently from an ottoman_continuity_advocate seat or a textual_heritage_keeper seat. From the technical seat, the constraint solves a real coordination problem (script standardization) with good phonetic grounds; extraction is near zero because the technical reading acknowledges no asymmetry of benefit. From the heritage-keeper seat, the same constraint is a depoliticized identity rupture that obscures the cost of losing Ottoman textual continuity; extraction is perceived as much higher because the cost is real but conceptually hidden. The engine computes directionality from the structural data (beneficiary/victim + exit options): technical_linguists have institutional power and mobile exit (they can relocate expertise), generating low d (beneficiary end); ottoman_continuity_advocates have organized power but identity_locked exit (they cannot abandon the cultural frame the constraint severs), generating high d (target end). This structural divergence is the perspectival gap: the same constraint computes to different types at different seats precisely because the technical reading obscures the political function from beneficiaries while making it inescapable for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   technical_linguists (beneficiary, institutional power, arbitrage exit): d ≈ 0.2. They gain institutional authority by providing technical grounds; they can relocate this expertise to other domains if the political environment shifts. modernization_advocates (beneficiary, powerful, mobile exit): d ≈ 0.25. They benefit from depoliticization; they can shift to other modernization projects if this one concludes. ottoman_continuity_advocates (payer, organized power, identity_locked exit): d ≈ 0.78. They pay in institutional authority and cultural legitimacy; they cannot exit without abandoning the identity frame they regard as foundational. literate_general_population (payer + incidental beneficiary, moderate power, constrained exit): d ≈ 0.55. They pay relearning costs and heritage-access loss; they benefit from eventual phonetic clarity; they cannot exit without forgoing participation in the modernized state. textual_heritage_keepers (payer, moderate power, constrained exit): d ≈ 0.68. They pay in atrophying access to Ottoman texts; they cannot exit without abandoning their institutional function of heritage preservation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (script heterogeneity) remains partially live: Ottoman-era texts in various scripts are still extant and still require standardized reading; the general population still reads in the established Latin script. However, the problem has substantially resolved itself through generational replacement. No one now disputes that Turkish should be written in one standard; the technical debate about which script is optimal has been conclusively settled in the institutional view. The constraint persists not because the founding problem remains urgent but because the epistemic authority of the technical reading has been institutionalized—disrupting it would now require disrupting the authority of technical linguists themselves, not merely the script choice. This is a piton-adjacent reading: the core coordination function (establish one standard script) is secure and no longer needs active enforcement; what persists is the theater of technical justification, maintaining the depoliticized framing against any resurgence of identity-based alternative framings. The mandatrophy signal is present but subdued: the founding problem is not dead (script standardization is still relevant), and the constraint is not pure extraction (real coordination benefit exists), but the theatrical maintenance of the technical framing is substantial relative to the functional need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    script_technical_neutrality_premise,
    'Is script choice genuinely neutral technology, or is script always identity-bearing even when technical arguments are available?',
    'Comparative historical analysis: examine other script choices in multilingual states and ask whether technical superiority arguments alone explain the outcomes, or whether identity and power relations always inflect the decision. Test whether communities ever maintain phonetically suboptimal scripts when identity attachment is strong.',
    'If script is always identity-bearing, the phonetic_instrumentalism reading obscures a political choice through technical framing—extractiveness rises and classification shifts toward snare or tangled_rope. If script can be genuinely neutral under the right conditions, the reading''s low extractiveness is justified and classification stands as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_technical_neutrality_premise, conceptual, 'Whether script is neutral technology or always identity-encoding.').

omega_variable(
    phonetic_transparency_claim_accuracy,
    'Does Latin script actually represent Turkish phonology more transparently than reformed or hybrid Arabic scripts would?',
    'Formal phonetic analysis comparing Ottoman Arabic script, Latin script, and possible reformed Arabic variants on their ability to represent Turkish vowels, consonants, and suprasegmental features. Independent linguists outside the Turkish technical establishment evaluate the phonetic claims.',
    'If Latin is genuinely phonetically superior, the technical reading''s core premise holds and theater_ratio represents legitimate justification of a real technical choice. If alternatives are equally transparent, the technical advantage claim is false and the theater ratio signals that justification is performing work beyond technical optimization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonetic_transparency_claim_accuracy, empirical, 'Whether Latin script is phonetically superior or the claim is overstated.').

omega_variable(
    depoliticization_effectiveness_over_time,
    'Does the phonetic neutrality framing successfully suppress the emergence of identity-based script alternatives, or do identity-based arguments periodically resurface despite technical framing?',
    'Longitudinal analysis of public discourse, educational materials, and policy debates about script over the 100-year interval: track how often identity-based arguments appear and how they are marginalized or accommodated by the technical framing.',
    'If the framing successfully suppresses identity-based alternatives, suppression and theater_ratio remain stable—the depoliticization is effective. If identity-based arguments periodically resurface and require active marginalization, suppression and theater increase over time as more effort is needed to maintain the technical framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depoliticization_effectiveness_over_time, empirical, 'Whether phonetic neutrality framing maintains suppression of political readings.').

omega_variable(
    reading_foreclosure_and_coexistence,
    'Do the three readings (phonetic_instrumentalism, kemalist_rupture, ottoman_continuity) genuinely coexist as live options held by different parties, or does the institutionalization of phonetic_instrumentalism functionally foreclose the others by making it the only legitimate framing?',
    'Track institutional capture: measure the extent to which the technical reading dominates official discourse, education curricula, and state policy. Measure the organizational capacity and institutional access of advocates for the other two readings. If the technical reading has achieved hegemonic status while the others are marginalized but still held by organized constituencies, they coexist; if one reading has been completely eliminated from possibility space, foreclosure has occurred.',
    'Coexistence_with is the declared relation; if empirically foreclosure has occurred, the reading_relations need revision and the constraint''s mandatrophy status may need reassessment (a foreclosed reading is no longer a live constraint—it becomes historical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_and_coexistence, empirical, 'Whether the three script-choice readings remain live alternatives or one has foreclosed others.').

omega_variable(
    beneficiary_structure_concealment,
    'To what extent does the phonetic neutrality framing successfully conceal from the beneficiary seats (modernizers, technical linguists) that they are beneficiaries at all?',
    'Survey and interview data from beneficiary seats: ask how they perceive the script choice (as technical optimization vs. politically motivated change). Measure the degree of belief in their own disinterestedness. Compare with interview data from victim seats: do they perceive beneficiaries as having been conscious of political motivation?',
    'If beneficiaries genuinely believe the choice is technical (high concealment), they may be unconscious agents of political change rather than strategic extractors—this affects interpretation of the constraint as snare vs. rope vs. tangled_rope. If beneficiaries are conscious but the framing makes it costly for them to acknowledge the political dimension, the concealment is strategic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_concealment, empirical, 'Whether beneficiaries are conscious or unconscious agents of political change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(scri_tr_t0, observed).
narrative_ontology:measurement(scri_tr_t14, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 14, 0.61).
narrative_ontology:measurement_basis(scri_tr_t14, observed).
narrative_ontology:measurement(scri_tr_t28, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 28, 0.64).
narrative_ontology:measurement_basis(scri_tr_t28, observed).
narrative_ontology:measurement(scri_tr_t42, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 42, 0.67).
narrative_ontology:measurement_basis(scri_tr_t42, observed).
narrative_ontology:measurement(scri_tr_t70, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 70, 0.69).
narrative_ontology:measurement_basis(scri_tr_t70, observed).
narrative_ontology:measurement(scri_tr_t100, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 100, 0.68).
narrative_ontology:measurement_basis(scri_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(scri_be_t0, observed).
narrative_ontology:measurement(scri_be_t14, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 14, 0.12).
narrative_ontology:measurement_basis(scri_be_t14, observed).
narrative_ontology:measurement(scri_be_t28, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 28, 0.14).
narrative_ontology:measurement_basis(scri_be_t28, observed).
narrative_ontology:measurement(scri_be_t42, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 42, 0.15).
narrative_ontology:measurement_basis(scri_be_t42, observed).
narrative_ontology:measurement(scri_be_t70, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 70, 0.16).
narrative_ontology:measurement_basis(scri_be_t70, observed).
narrative_ontology:measurement(scri_be_t100, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement_basis(scri_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(scri_su_t0, observed).
narrative_ontology:measurement(scri_su_t14, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 14, 0.19).
narrative_ontology:measurement_basis(scri_su_t14, observed).
narrative_ontology:measurement(scri_su_t28, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 28, 0.21).
narrative_ontology:measurement_basis(scri_su_t28, observed).
narrative_ontology:measurement(scri_su_t42, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 42, 0.22).
narrative_ontology:measurement_basis(scri_su_t42, observed).
narrative_ontology:measurement(scri_su_t70, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 70, 0.22).
narrative_ontology:measurement_basis(scri_su_t70, observed).
narrative_ontology:measurement(scri_su_t100, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement_basis(scri_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(script_as_identity__phonetic_instrumentalism_reading, 0.05).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__ottoman_continuity_reading).

% DUAL FORMULATION NOTE:
% The script_as_identity kernel decomposes into three constraint stories representing three distinct readings held by different institutional seats. Each reading instantiates different ε, beneficiary/victim structure, and type. (1) phonetic_instrumentalism_reading (this story): low ε (technical optimization frame); beneficiaries are technical experts and modernizers; victims are continuity advocates and heritage keepers. (2) kemalist_rupture_reading: moderate-high ε (political modernization); beneficiaries are secular elites and state apparatus; victims are religious scholars and traditionalists. (3) ottoman_continuity_reading: high ε (identity rupture); beneficiaries are none (or foreign observers); victims are Ottoman cultural continuity advocates. The three readings coexist as live positions (relation: coexists_with, not forecloses) because they are held by different factions. None logically eliminates the others; rather, institutionalization of one (phonetic_instrumentalism) creates pressure on the others by changing what counts as legitimate grounds for debate. The ε-invariance principle requires three separate constraint files because the three readings make fundamentally different claims about what the constraint is: technical optimization vs. political rupture vs. identity preservation. A single file attempting to hold all three would have indeterminate ε and would violate the invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__phonetic_instrumentalism_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
