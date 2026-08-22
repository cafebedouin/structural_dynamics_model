% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__secular_nationalist_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: 1928 Turkish Alphabet Reform — Secular-Nationalist Reading
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   In 1928 the Turkish Republic under Mustafa Kemal Atatürk replaced the
 *   Arabic-script Ottoman Turkish orthography with a new Latin-based
 *   alphabet, banning the old script from official use within a matter of
 *   months and building a nationwide adult-education campaign (Millet
 *   Mektepleri) to force rapid conversion. Framed by the state as a purely
 *   technical literacy improvement, the reform simultaneously severed the
 *   reading public from the accumulated Ottoman-Islamic textual tradition and
 *   re-anchored Turkish national identity toward Europe. This story authors
 *   the secular-nationalist justification for that rupture as the standing
 *   arrangement under contest, evaluated by its own internal logic — not the
 *   arrangement the sibling readings would prefer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.68).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.81).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "1928 Turkish Alphabet Reform — Secular-Nationalist Reading").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, '13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d').
narrative_ontology:cs_kernel_codification('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', formalized).
narrative_ontology:cs_authority_grounding('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', extraction).
narrative_ontology:cs_interpretation_layer_present('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d').
narrative_ontology:cs_reading_relation('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', foundational, turkish_identity_discontinuous_with_ottoman_islamic_past).
narrative_ontology:cs_axiom_status(turkish_identity_discontinuous_with_ottoman_islamic_past, holdable).
narrative_ontology:cs_axiom_grounding('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', turkish_identity_discontinuous_with_ottoman_islamic_past, conventional).
narrative_ontology:cs_axiom('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', foundational, latin_script_as_civilizational_alignment_marker).
narrative_ontology:cs_axiom_status(latin_script_as_civilizational_alignment_marker, holdable).
narrative_ontology:cs_axiom_grounding('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', latin_script_as_civilizational_alignment_marker, instrumental).
narrative_ontology:cs_reference_frame('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', kemalist_rupture_founding_moment).
narrative_ontology:cs_drift_state('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', contemporary_turkish_identity_politics, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('13d4feb9-ac68-4f90-9cd7-820c9cdbaf5d', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, urban_secular_elite).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, new_literacy_cohorts).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, european_aligned_technocrats).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_generation).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_ulema).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, provincial_arabic_literate_clergy).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_and_minority_script_traditions).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, turkish_national_distinctiveness_doctrine).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, european_modernity_alignment_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates the 1928 script reform, establishes the Language Commission, criminalizes continued use of Arabic script in official and eventually much public contexts within a compressed timeline, and builds Millet Mektepleri (Nation's Schools) to force-march adult literacy conversion. Administers the enforcement machinery and defines what counts as legitimate Turkish identity going forward.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Already oriented toward European institutions, languages, and print culture; converts quickly and gains outsized access to new administrative, educational, and press positions that the reform newly privileges. Their existing cultural capital is revalued upward by the substrate change.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, urban_secular_elite, beneficiary,
    organized, generational, arbitrage, national).

% Children and young adults entering the school system after 1928 learn only the Latin alphabet and gain a genuinely simpler, higher-fidelity phonetic writing system for Turkish; national literacy rates rise substantially over subsequent decades. They inherit the new substrate as unmarked normal rather than as rupture.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, new_literacy_cohorts, beneficiary,
    moderate, generational, constrained, national).

% Diplomats, engineers, and modernizing bureaucrats who benefit from Turkey's legibility to European institutions, trade partners, and technical standards bodies. The script change is read abroad as a costly, credible signal of civilizational realignment, improving their negotiating position.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, european_aligned_technocrats, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Adults who spent decades achieving literacy in Arabic script are rendered functionally illiterate overnight in the new official order. Government documents, newspapers, and street signage in the new script are inaccessible to them; many never fully convert and lose administrative and economic standing they previously held. There is no exit — the state does not recognize the old script for official purposes.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_generation, payer,
    powerless, biographical, trapped, national).

% Their authority rested on textual mastery of Arabic-script Ottoman, Quranic, and juristic corpora. The reform, paired with the abolition of religious courts and the caliphate, severs the young from the script that carries their tradition, cutting the pipeline of students able to read primary religious and legal texts without specialized retraining. Resistance is possible but increasingly marginalized from state institutions.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_ulema, payer,
    organized, generational, constrained, national).

% Local imams and scribes in rural Anatolia whose social function depended on being the literate intermediary between villagers and Arabic/Ottoman-script documents. The reform strips this intermediary role without offering them a comparable place in the new secular-literate order; many are left with a skill the state no longer recognizes.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, provincial_arabic_literate_clergy, payer,
    powerless, biographical, trapped, regional).

% Non-Turkish linguistic communities whose own script traditions and identity claims are entirely absent from the reform's framing, which treats the nation as linguistically and ethnically homogeneous. The Latin-script reform is bundled with broader Turkification policy; minority script and language practices are further marginalized as a side effect of the same nation-building logic.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_and_minority_script_traditions, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_and_minority_script_traditions, excluded).

% Intellectuals and clerics who hold that Turkish identity is continuous with Ottoman-Islamic civilization and that Arabic script is the legitimate substrate are excluded from the post-1928 institutional conversation entirely; their position is treated by the state not as a rival policy option but as the superseded past.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_continuity_advocates, excluded,
    organized, generational, trapped, national).

% Study the reform's speed, enforcement mechanisms, and literacy outcomes, and compare Turkey's rupture strategy to script reforms elsewhere (e.g. Vietnamese romanization, Soviet Central Asian Latinization/Cyrillization). Take testimony from displaced Ottoman-literate populations, state archives, and comparative linguistic data.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, historians_of_turkish_modernization, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_state_apparatus).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__secular_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, phonetically transparent national writing system that dramatically lowers the cost of mass literacy acquisition and standardizes an administrative, educational, and print infrastructure across a large and diverse territory under one legible script.
% TRANSFER_FUNCTION: Moves cultural, administrative, and interpretive authority from Arabic-script-literate religious and Ottoman-bureaucratic elites to Latin-script-literate secular, state-aligned, and rising professional classes; moves generational literacy capital from the old to the young; moves legibility and legitimacy from Ottoman-Islamic civilizational reference toward European civilizational reference.
% ABSENT_VOICES: Ottoman-continuity advocates, religious scholars invested in Arabic-script textual authority, and non-Turkish minority communities with their own script and language claims are not represented in the reform's design process; the Language Commission and legislative apparatus are staffed by figures already committed to the secular-nationalist premise before deliberation begins.
% DISAPPEARANCE_RATIONALE: Were the Latin-script mandate and its enforcement to vanish, Ottoman Turkish in Arabic script would likely re-enter administration, education, and religious life within a generation; the accumulated post-1928 print corpus, state archives, and pedagogical infrastructure built entirely around Latin orthography would become a stranded asset, and the civilizational self-positioning toward Europe that the script change encodes would lose its most visible daily marker.
% FOUNDING_PROBLEM: The Ottoman Arabic-script orthography poorly represented Turkish vowel phonology, producing high illiteracy and slow print-culture development; simultaneously, the new Republic sought to sever institutional and symbolic continuity with the caliphate, the ulema's textual authority, and the multi-ethnic Ottoman order in favor of a homogeneous, European-aligned Turkish national identity.
% FOUNDING_PROBLEM_CORROBORATION: State historiography and Kemalist-aligned linguists attest the literacy problem was real and substantially solved by the reform, citing rising national literacy statistics. Independent comparative linguists and historians outside the Turkish state (e.g. scholars of Ottoman studies and comparative script-reform literature) corroborate the phonetic-mismatch problem as real but contest whether rupture rather than gradual orthographic modification was structurally necessary to solve it — this is precisely the disagreement the gradual_transition_reading formalizes as a separate constraint.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply in the reform's first decade (0.42 to 0.61 over the first 8 years) as the practical costs of the switch land on the Ottoman-literate adult population and the ulema's textual authority is severed, then plateaus around 0.68 as the new order stabilizes into a durable generational transfer of literacy capital rather than an active daily extraction event. Suppression is highest at the outset (0.90) when Arabic-script use was actively criminalized and enforced through school, press, and administrative mandates, then declines as the new script becomes unmarked default among successive cohorts and active coercion is no longer needed to maintain it. Theater ratio stays low throughout (0.08 to 0.22) because the coordination function — mass literacy in a phonetically superior script — is genuinely substantial and not merely performed; the reform's extractive core is the civilizational-rupture bundling, not the literacy technology itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The Kemalist state apparatus sits at the pure-beneficiary end: it authored the reform, controls its enforcement, and captures the legitimacy dividend of European alignment. Urban secular elites, new literacy cohorts, and internationally-facing technocrats are structural beneficiaries whose existing or future capital is revalued upward. Ottoman-literate adults, the ulema, provincial Arabic-literate clergy, and minority-script communities are targets: trapped or constrained exit, no institutional recognition of their prior literacy or textual authority under the new order. Directionality here is not symmetric even among payers — the ulema retain organized power and some capacity to resist through religious institutions (until those are also curtailed), while ordinary Ottoman-literate adults and provincial clergy have no comparable leverage at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding literacy problem (poor phonetic fit of Arabic-script orthography to Turkish vowels) was real and is substantially solved; treating the reform purely as solved-and-therefore-benign would miss that the same act consolidated a civilizational-rupture and homogenization agenda that has no comparable technical justification and continues to structure Turkish identity politics. Classifying this as tangled_rope rather than a pure rope keeps the genuine coordination gain (mass literacy) analytically visible without letting it launder the asymmetric extraction from Ottoman-literate populations, religious scholars, and minority communities who bore the rupture's costs without comparable say in its design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_versus_gradualism_necessity,
    'Was the compressed, punitive rupture strategy (criminalizing Arabic script within months) structurally necessary to achieve the literacy and modernization gains, or would the gradual_transition_reading''s managed coexistence have achieved comparable outcomes at lower cost to Ottoman-literate and religious populations?',
    'Comparative analysis against other 20th-century script reforms (Vietnamese romanization''s multi-decade transition, Soviet Central Asian Latinization phases, Malaysian/Indonesian orthographic standardization) measuring literacy-gain trajectories against social-cost trajectories under different transition speeds.',
    'If gradualism achieves comparable literacy outcomes, the extraction measured here (0.68) is substantially attributable to the rupture''s speed and enforcement rather than to the coordination function itself, sharpening the case that this reading''s extractive component is severable from its genuine literacy benefit. If rupture speed was load-bearing for the civilizational-realignment goal specifically, the extraction is intrinsic to this reading''s own stated aim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_versus_gradualism_necessity, empirical, 'Whether the reform''s speed was necessary for its stated literacy goal or served the separate rupture goal.').

omega_variable(
    civilizational_framing_naturalness,
    'Is ''European modernity alignment'' a natural, self-evident developmental trajectory (as this reading''s own framing asserts) or a constructed civilizational hierarchy that retroactively justifies the state''s preference for rupture over continuity?',
    'Examine whether comparable literacy and administrative modernization was achievable in other 20th-century states without a Latin-script/European-alignment framing (e.g. Japan, which modernized administratively while retaining its script), which would indicate the European-alignment claim is a legitimating narrative rather than a technical necessity.',
    'If European alignment is not a necessary condition for modernization, the vindicated_propositions here (national distinctiveness, European alignment thesis) function partly as ideological cover for the extraction from Ottoman-Islamic-affiliated populations rather than as an independently justified developmental claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilizational_framing_naturalness, conceptual, 'Whether the European-modernity framing is a technical necessity or a legitimating ideological construct.').

omega_variable(
    kernel_reading_incommensurability,
    'Given that this reading, the ottoman_continuity_reading, and the gradual_transition_reading each authorize a different graphemic substrate as legitimate, is there any shared empirical standard (literacy outcomes, textual continuity, minority accommodation) by which one reading could be shown structurally superior, or are the three readings resting on incommensurable value premises about what Turkish national identity IS?',
    'This is the committer-structure question the kernel itself poses; it is not resolvable by additional data internal to this reading alone. Cross-reading comparison of the three sibling constraint files'' beneficiary/victim sets and cs_structure.axioms is the intended analytical route.',
    'If incommensurable, the three readings remain permanently coexisting positions (as declared in reading_relations) rather than converging on a single ''correct'' account of the 1928 reform, and the extraction measured in each reading is properly indexed to that reading''s own premises rather than to a neutral external standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings can be adjudicated by shared standards or rest on incommensurable identity premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(turk_tr_t8, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(turk_tr_t16, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(turk_tr_t24, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(turk_tr_t32, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement(turk_tr_t40, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(turk_be_t8, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(turk_be_t16, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(turk_be_t24, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(turk_be_t32, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(turk_be_t40, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(turk_su_t8, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 8, 0.86).
narrative_ontology:measurement(turk_su_t16, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(turk_su_t24, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(turk_su_t32, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(turk_su_t40, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__secular_nationalist_reading, 0.08).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, gradual_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the turkish_graphemic_substrate kernel, each authored as a separate ε-invariant story per the ε-invariance principle: secular_nationalist_reading (this file, tangled_rope, ε=0.68 — genuine mass-literacy coordination bundled with asymmetric civilizational-rupture extraction), ottoman_continuity_reading (Arabic script as legitimate substrate grounded in civilizational continuity, authored separately with its own beneficiary/victim structure and ε), and gradual_transition_reading (managed multi-script coexistence, authored separately with its own, lower ε reflecting reduced rupture cost). All three share the same underlying historical event (the 1928 alphabet reform) but instantiate structurally distinct constraints because each reading's core normative premise about Turkish identity's relationship to the Ottoman past differs, producing different victim sets and different extraction profiles. Do not average across these files; each stands as its own reading's account.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
