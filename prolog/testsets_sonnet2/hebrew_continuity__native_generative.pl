% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Native-Generative Standard of Hebrew Vitality (Revival Ideology Reading)
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the native-generative reading of the
 *   contested 'Hebrew continuity' kernel: the claim that Hebrew lives ONLY
 *   through native child acquisition and daily generative use, as embodied in
 *   the Israeli Hebrew revival project led by Eliezer Ben-Yehuda and
 *   institutionalized by the Hebrew Language Academy and the state education
 *   system. This reading treats the achievement of a native-speaking,
 *   generatively productive Israeli Hebrew vernacular as the sole legitimate
 *   marker of the language's life, and by that criterion classifies Hebrew
 *   maintained solely through liturgical recitation or hybrid diaspora
 *   vernaculars as effectively 'dead' or pre-vital. Early in the revival
 *   period (T=0), the standard required intense enforcement — active
 *   suppression of Yiddish, coercive Hebrew-only schooling, social stigma
 *   against 'jargon' languages — to displace competing linguistic
 *   continuities and establish native transmission at scale. As the
 *   native-speaking population became self-sustaining, direct suppression
 *   eased (a native speech community reproduces itself without policing) even
 *   as the standard's exclusionary force against liturgical and adult-learner
 *   Hebrew rose in relative and institutional terms (rising extractiveness,
 *   rising theater as academy activity increasingly performs modernization
 *   rather than solves urgent coordination gaps).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.62).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.58).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Native-Generative Standard of Hebrew Vitality (Revival Ideology Reading)").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, 'd6d02832-12e9-44dc-969e-83ed0533b2ac').
narrative_ontology:cs_kernel_codification('d6d02832-12e9-44dc-969e-83ed0533b2ac', distributed).
narrative_ontology:cs_authority_grounding('d6d02832-12e9-44dc-969e-83ed0533b2ac', extraction).
narrative_ontology:cs_interpretation_layer_present('d6d02832-12e9-44dc-969e-83ed0533b2ac').
narrative_ontology:cs_reading_relation('d6d02832-12e9-44dc-969e-83ed0533b2ac', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('d6d02832-12e9-44dc-969e-83ed0533b2ac', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('d6d02832-12e9-44dc-969e-83ed0533b2ac', foundational, native_child_acquisition_is_necessary_for_life).
narrative_ontology:cs_axiom_status(native_child_acquisition_is_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('d6d02832-12e9-44dc-969e-83ed0533b2ac', native_child_acquisition_is_necessary_for_life, empirically_contingent).
narrative_ontology:cs_axiom('d6d02832-12e9-44dc-969e-83ed0533b2ac', secondary, generative_daily_use_supersedes_ritual_recitation).
narrative_ontology:cs_axiom_status(generative_daily_use_supersedes_ritual_recitation, holdable).
narrative_ontology:cs_axiom_grounding('d6d02832-12e9-44dc-969e-83ed0533b2ac', generative_daily_use_supersedes_ritual_recitation, conventional).
narrative_ontology:cs_reference_frame('d6d02832-12e9-44dc-969e-83ed0533b2ac', ben_yehuda_revival_founding_moment).
narrative_ontology:cs_drift_state('d6d02832-12e9-44dc-969e-83ed0533b2ac', contemporary_israeli_hebrew, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d6d02832-12e9-44dc-969e-83ed0533b2ac', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_hebrew_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, sabra_native_speaker_generation).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, state_education_ministry).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_only_diaspora_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, yiddish_and_ladino_heritage_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, elderly_immigrant_hebrew_learners).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, ben_yehuda_revival_doctrine).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, linguistic_normalization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the standard of what counts as 'living' Hebrew, adjudicates lexical expansion, and certifies phonological norms taught in schools. Administers the criterion that native child acquisition is the only legitimate vitality test, and controls curriculum and broadcast-standard Hebrew that operationalizes this criterion.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_hebrew_academy, agenda_setter,
    institutional, generational, arbitrage, national).

% Acquired Hebrew as a first language through the revival project; their intuitive command of the language becomes the arbiter of correctness and cultural authenticity. They gain social, institutional, and cultural capital simply by being the population the standard was built around.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sabra_native_speaker_generation, beneficiary,
    organized, biographical, mobile, national).

% Enforces native-generative Hebrew as the sole medium of instruction and civic life, building the nation-building project on the premise that a living vernacular (not a preserved liturgical register) is required for a functioning modern state. Benefits from a unified national language that legitimizes state institutions.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, state_education_ministry, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, state_education_ministry, beneficiary).

% Maintain Hebrew solely through prayer, textual study, and ritual recitation without generative daily use or native child transmission. Under this reading's criterion, their Hebrew is classified as 'dead' or non-vital, delegitimizing centuries of unbroken textual transmission and denying it standing as a living form of the language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_only_diaspora_communities, payer,
    powerless, generational, trapped, global).

% Carried Hebrew-inflected vernacular and liturgical fluency embedded within Yiddish/Ladino diaspora culture. The native-generative standard treats their hybrid linguistic inheritance as pre-modern or transitional, accelerating the marginalization of these vernaculars in favor of standardized Israeli Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, yiddish_and_ladino_heritage_speakers, payer,
    powerless, generational, constrained, global).

% Adult immigrants (olim) who learned Hebrew as a second language through ulpan programs never achieve the intuitive native command the standard requires. They are structurally excluded from full linguistic authority no matter their fluency, since the criterion privileges childhood acquisition they cannot retroactively obtain.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, elderly_immigrant_hebrew_learners, payer,
    powerless, biographical, trapped, national).

% Study the Hebrew revival as a unique case of large-scale planned vernacularization, documenting both its remarkable success and the marginalization of alternative continuities (liturgical, diasporic, hybrid) that the native-generative narrative renders invisible.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sociolinguists_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, israeli_hebrew_academy).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, mutually intelligible, generative vernacular that allows a demographically diverse immigrant population to function as one civic and cultural community — solving the real problem that a purely liturgical or fragmented multilingual Jewish population could not build shared modern institutions.
% TRANSFER_FUNCTION: Moves linguistic legitimacy and cultural authority from liturgical, textual, and diasporic-vernacular forms of Hebrew continuity toward the native-speaking Israeli population and the state institutions that certify their speech as the only 'living' Hebrew.
% ABSENT_VOICES: Diaspora liturgical communities and elderly immigrant learners who maintain deep textual or devotional relationships with Hebrew are not represented in the academy or ministry that set the vitality standard; their objection — that unbroken transmission through recitation and study is itself a form of life — is structurally outside the room that defines what counts as living language.
% DISAPPEARANCE_RATIONALE: If the native-generative standard were abandoned overnight, Hebrew's legitimacy would no longer hinge on child first-language acquisition; liturgical, diasporic, and adult-acquired forms of Hebrew would regain parity as valid continuities, altering immigration absorption policy, school curricula, and the cultural status hierarchy that currently privileges sabra speech.
% FOUNDING_PROBLEM: Diaspora Jewish communities lacked a shared vernacular; Hebrew existed mainly as a liturgical and literary register with no native speakers, and Zionist nation-building required a living common language to unify immigrants from dozens of linguistic backgrounds into one functioning society.
% FOUNDING_PROBLEM_CORROBORATION: The Hebrew Academy and generations of native speakers attest the founding problem remains live — ongoing lexical modernization and standardization work continues. Independent historians and sociolinguists outside the beneficiary population note the core nation-building problem was substantially solved by the mid-20th century, and that continued insistence on native-generative exclusivity now functions to marginalize alternative Hebrew continuities rather than to solve an unsolved coordination problem.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.38 to 0.62) as the coordination problem the standard was built to solve (a functioning shared vernacular for a heterogeneous immigrant society) becomes progressively more solved, while the standard's exclusionary force against liturgical and diasporic forms persists and intensifies institutionally (academy certification, school curricula, media norms). Suppression falls over time (0.70 to 0.58) because active coercive enforcement (Yiddish suppression campaigns, corporal and social punishment for 'jargon' use) was heaviest during the founding decades and eased once a self-reproducing native-speaker population existed; what remains is softer institutional gatekeeping rather than coercive enforcement. Theater ratio rises modestly (0.12 to 0.28) as academy activity increasingly performs linguistic modernization (new coinages, phonological rulings) as institutional ritual rather than urgent nation-building necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hebrew Academy and Ministry of Education are agenda-setters who administer and benefit from the native-generative criterion — it legitimizes their institutional authority and the national language project they built. Native speakers (sabras) are direct beneficiaries: their intuitive command becomes the automatic standard of correctness, requiring no additional labor or certification. Liturgical-only communities, diaspora heritage-vernacular speakers, and elderly immigrant learners are structural targets: the criterion is defined in a way that permanently excludes what they have (textual fluency, hybrid vernacular competence, adult-acquired fluency) from counting as 'living' Hebrew, regardless of their actual linguistic practice or devotion to the language.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the absence of a shared vernacular for nation-building — was substantially solved by the mid-20th century once a self-reproducing native-speaker population existed. The native-generative standard's continued strict exclusivity (denying liturgical and diasporic Hebrew any status as 'living') no longer serves an urgent coordination function; it now functions primarily to police cultural and religious authority within Israeli and diaspora Jewish life. Classifying this as tangled_rope rather than pure snare recognizes that the standard DID solve (and residually still supports) a genuine coordination problem — Israeli civic and cultural life requires the shared generative vernacular — while ALSO now serving as a mechanism of exclusion against communities whose Hebrew survived through different, equally legitimate means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is ''Hebrew lives only through native generative use'' a defensible linguistic-vitality criterion, or is it one contested reading among several (liturgical preservation, diaspora bridge-pidgin) that happens to have won institutional backing through the Zionist state-building project?',
    'Comparative sociolinguistic analysis of language-vitality frameworks (e.g., UNESCO''s language endangerment criteria, which recognize multiple modes of transmission) applied neutrally across all three readings without privileging the reading that happens to hold state power.',
    'If native-generative use is accepted as the sole legitimate criterion, liturgical and diaspora-vernacular Hebrew are correctly classified as non-living, and this reading''s beneficiary/victim structure is descriptively accurate. If vitality is polythetic (multiple valid modes of continuity), this reading is itself a constructed, institutionally-backed narrowing that manufactures victims out of communities practicing a different but equally real form of linguistic continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether native-generative use is a natural linguistic-vitality criterion or one institutionally-selected reading among several defensible ones.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding coordination problem (lack of shared vernacular for nation-building) been so thoroughly solved that continued strict enforcement of native-generative exclusivity now serves primarily to maintain institutional and cultural authority rather than solve any live problem?',
    'Track institutional Hebrew Academy activity and school curricular emphasis on native-only legitimacy against measurable outcomes in civic language coordination; if academy rulings increasingly concern maintaining boundaries against Yiddish/liturgical/Arabic-Hebrew hybrid forms rather than solving new coordination gaps, mandatrophy is corroborated.',
    'If mandatrophy is confirmed, the tangled_rope classification should shift toward snare as the coordination function becomes vestigial and the exclusionary function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the coordination rationale for strict native-generative exclusivity has become obsolete relative to its continuing exclusionary force.').

omega_variable(
    cs_framing_alternative,
    'Could this constraint alternatively be framed around the legitimacy claim of ''linguistic authenticity'' layered above the Hebrew Academy''s institutional authority, rather than the institution itself — and would that framing change the cs_pattern classification?',
    'Compare a framing centered on the Academy-as-institution (authority_grounding: extraction/practice) against a framing centered on the narrative of ''unbroken native transmission as authenticity'' (which might ground authority differently, e.g. in a more diffuse cultural consensus not reducible to one institution).',
    'If the narrative-of-authenticity framing is adopted, authority_grounding might shift toward ''distributed'' or ''diffuse_epistemic'', weakening the case for treating the Academy as the sole interpretive layer and potentially altering whether interpretation_layer_present should be declared true.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Alternative framing of authority: the institution (Academy) versus the diffuse cultural narrative of authenticity it administers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__native_generative, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__native_generative, theater_ratio, 40, 0.18).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__native_generative, theater_ratio, 60, 0.22).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__native_generative, theater_ratio, 80, 0.25).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__native_generative, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__native_generative, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__native_generative, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__native_generative, base_extractiveness, 60, 0.57).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__native_generative, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__native_generative, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__native_generative, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__native_generative, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__native_generative, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(hebr_su_t80, hebrew_continuity__native_generative, suppression_requirement, 80, 0.59).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__native_generative, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__native_generative, 0.1).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the 'hebrew_continuity' kernel, each authored as a separate ε-invariant story per the ε-invariance principle. 'native_generative' (this file) claims tangled_rope with substantial and rising extraction against liturgical and diaspora-vernacular communities. 'liturgical_preservation' and 'bridge_pidginized' author their own ε, beneficiaries, and victims from their own reading's premises. The three files are linked bidirectionally via affects_constraints to preserve the kernel-family structure for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
