% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim as Substitution Archive — Prayer and Study Replace Sacrifice
 *   domain: religious/rabbinic_judaism/commitment_system
 *
 * SUMMARY:
 *   After the Second Temple's destruction (70 CE), the rabbinic movement
 *   faced an existential crisis: the Torah's central worship system — animal
 *   sacrifice administered by priests in Jerusalem — was physically
 *   impossible. Their solution was the substitution doctrine: prayer
 *   (tefillah) replaces the daily tamid offerings, and Torah study (talmud
 *   torah) replaces the sacrificial order itself. The Kodashim corpus (the
 *   Mishnah/Talmud tractates on sacrifices) became the textual archive of
 *   this substitution — studied intensively, but framed as preserving the
 *   memory of what was lost rather than preparing for its return. This
 *   reading (substitution_archive) claims the replacement is COMPLETE: the
 *   kernel is not occupied by ongoing practice nor awaiting messianic
 *   restoration; it is a memorial archive documenting a superseded system.
 *   The constraint extracts by declaring the replacement permanent while
 *   suppressing restorationist claims, benefiting the rabbinic institutions
 *   that administer the substitute system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.58).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.62).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim as Substitution Archive — Prayer and Study Replace Sacrifice").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/rabbinic_judaism/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '8ee316b0-5830-4835-9062-7ab0c6026cae').
narrative_ontology:cs_kernel_codification('8ee316b0-5830-4835-9062-7ab0c6026cae', formalized).
narrative_ontology:cs_authority_grounding('8ee316b0-5830-4835-9062-7ab0c6026cae', lineage).
narrative_ontology:cs_interpretation_layer_present('8ee316b0-5830-4835-9062-7ab0c6026cae').
narrative_ontology:cs_reading_relation('8ee316b0-5830-4835-9062-7ab0c6026cae', kodashim_corpus__performance_only, forecloses).
narrative_ontology:cs_reading_relation('8ee316b0-5830-4835-9062-7ab0c6026cae', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_axiom('8ee316b0-5830-4835-9062-7ab0c6026cae', foundational, prayer_and_study_permanently_replace_sacrifice).
narrative_ontology:cs_axiom_status(prayer_and_study_permanently_replace_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('8ee316b0-5830-4835-9062-7ab0c6026cae', prayer_and_study_permanently_replace_sacrifice, deontological).
narrative_ontology:cs_axiom('8ee316b0-5830-4835-9062-7ab0c6026cae', foundational, kodashim_is_memorial_not_occupied_kernel).
narrative_ontology:cs_axiom_status(kodashim_is_memorial_not_occupied_kernel, holdable).
narrative_ontology:cs_axiom_grounding('8ee316b0-5830-4835-9062-7ab0c6026cae', kodashim_is_memorial_not_occupied_kernel, conventional).
narrative_ontology:cs_axiom('8ee316b0-5830-4835-9062-7ab0c6026cae', secondary, restoration_is_not_current_obligation).
narrative_ontology:cs_axiom_status(restoration_is_not_current_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8ee316b0-5830-4835-9062-7ab0c6026cae', restoration_is_not_current_obligation, conventional).
narrative_ontology:cs_reference_frame('8ee316b0-5830-4835-9062-7ab0c6026cae', rabbinic_substitution_authority_post_70ce).
narrative_ontology:cs_drift_state('8ee316b0-5830-4835-9062-7ab0c6026cae', post_1967_temple_mount_control, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8ee316b0-5830-4835-9062-7ab0c6026cae', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, liturgical_leadership).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, halakhic_authorities).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, sacrificial_restoration_advocates).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, temple_mount_activists).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, third_temple_movements).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, prayer_substitutes_sacrifice).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, torah_study_as_avodah).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, rabbinic_continuity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the study of Kodashim as central curriculum in yeshivas and kollels; define the substitution narrative that Torah study IS the replacement for Temple service; collect institutional legitimacy, funding, and authority from sustaining the textual tradition while physical sacrifice remains impossible.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter).

% Lead prayer services structured as explicit substitutes for daily and festival sacrifices (Shacharit, Mincha, Mussaf); claim continuity with Temple practice through the siddur's sacrificial architecture; derive authority and communal role from administering the substitute worship system.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, liturgical_leadership, beneficiary,
    organized, biographical, constrained, global).

% Adjudicate the halakhic status of Kodashim study, rule on whether sacrifice law remains practically applicable or is purely theoretical, determine the boundaries of the substitution doctrine; their rulings sustain the archive-as-continuity framework and suppress restorationist challenges.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Seek living sacrificial practice (korbanot) as binding obligation, not historical memory; told by mainstream authorities that prayer and study have permanently replaced sacrifice; bear the cost of being labeled messianic extremists or halakhically confused while their desired practice is declared obsolete by the substitution archive.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, sacrificial_restoration_advocates, payer,
    moderate, biographical, identity_locked, national).

% Advocate for renewed sacrifice on the Temple Mount; face legal prohibition, religious opposition from the substitution archive's guardians, and physical exclusion; their exclusion is structurally necessary to the archive's claim that the kernel is occupied by study rather than awaiting restoration.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, temple_mount_activists, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, temple_mount_activists, excluded).

% Organize around the expectation of literal Third Temple and restored sacrificial cult; the substitution archive's narrative directly forecloses their eschatology by declaring the replacement complete; they are kept out of the halakhic conversation because their presence would falsify the claim that Kodashim is merely a memorial.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, third_temple_movements, excluded,
    powerful, generational, trapped, national).

% Study Kodashim as historical literature documenting a superseded practice; analyze the rabbinic substitution doctrine as a mechanism for survival after 70 CE; neither participate in nor adjudicate the living halakhic dispute, but their historical-critical reading undermines the archive's claim of seamless continuity.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, academic_talmud_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a survival mechanism for Jewish practice after the Temple's destruction by redirecting sacrificial obligation into prayer and Torah study, preserving communal coherence when the central cult was physically impossible.
% TRANSFER_FUNCTION: Moves sacrificial obligation from the physical domain (animals, altar, priesthood) to the textual-liturgical domain (prayer text, study curriculum, rabbinic authority); transfers legitimacy and institutional control from a Temple-based system to a text-based system.
% ABSENT_VOICES: The priestly lineages (kohanim) who would have performed the actual sacrifices are structurally absent — their hereditary role was dissolved by the substitution; Second Temple sects (Sadducees, Essenes) who contested sacrificial practice are historically absent; contemporary kohanim who might claim restoration rights are excluded from the halakhic conversation.
% DISAPPEARANCE_RATIONALE: If the substitution archive vanished overnight, the halakhic framework declaring prayer and study as full replacements for sacrifice would collapse; restorationist movements would gain immediate halakhic legitimacy; rabbinic institutions would lose their central curricular justification; the liturgy would lose its sacrificial architecture; the entire post-70 CE survival framework would require reconstruction.
% FOUNDING_PROBLEM: How to maintain covenantal continuity and communal survival when the Temple — the divinely mandated center of sacrificial worship — was destroyed by Rome in 70 CE, making the core mitzvot of the Torah physically impossible to perform.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Temple destruction ending sacrificial practice) is historically corroborated by Josephus, Roman sources, and archaeology. The STATUS is contested: rabbinic authorities attest the problem remains live (exile continues, Temple not rebuilt); restorationists attest the problem is dead (political sovereignty restored, Temple Mount accessible, only rabbinic doctrine prevents renewal); academic scholars attest the problem was solved by the substitution itself (a successful adaptation, not a persistent crisis).
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the transfer of sacrificial obligation from a physical system (with material costs borne by all Israel) to a textual system (where study institutions capture the value of 'avodah' through curriculum control and authority). Suppression (0.62) is significant: the constraint actively excludes restorationist voices from halakhic legitimacy, legally and socially marginalizes Temple Mount activism, and enforces the substitution narrative through educational and liturgical channels. Theater ratio (0.41) captures that Kodashim study performs the MEMORY of sacrifice while the actual practice is declared obsolete — the study is real but its claimed continuity with living sacrificial practice is theatrical. Accessibility collapse (0.55) and resistance (0.48) reflect that alternatives (restoration) are partially but not totally foreclosed — the Temple Mount's physical existence and Jewish sovereignty over Jerusalem keep the restoration option structurally alive despite doctrinal suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic institution seat, the constraint is genuine coordination: a brilliant survival adaptation that preserved Judaism for two millennia. From the restorationist seat, the same structure is enforced extraction: a substitution doctrine that became a permanent displacement, suppressing the very practice it claimed to temporarily replace. The engine computes this divergence from the structural data — the declared beneficiaries and victims, their exit options, and the active enforcement required to maintain the substitution narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rabbinic institutions, liturgical leadership, halakhic authorities) sit at low directionality — they control the substitution narrative, collect institutional authority from administering the archive, and face arbitrage-grade exit (they can always study something else). Victims (restoration advocates, Temple Mount activists, Third Temple movements) sit at high directionality — they bear the cost of being told their desired practice is obsolete, face identity-locked exit (their religious identity is constituted by the restoration claim), and are structurally excluded from the halakhic conversation. The excluded seats (Third Temple movements especially) are trapped — their exclusion is the enforcement mechanism that maintains the archive's claim of completion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction ending sacrifice) was real and acute in 70 CE. The substitution was a genuine coordination response. But as centuries passed and especially after 1948/1967 restored Jewish sovereignty over the Temple Mount's geography, the coordination function atrophied while the extraction function persisted: the archive now serves primarily to legitimize the institutions that administer it, while actively suppressing the restoration that the founding crisis assumed would eventually occur. This is classic mandatrophy — the mandate (preserve continuity until restoration) outlived its function, but the constraint (substitution archive) persists through institutional inertia and active suppression of the restoration it was meant to await.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_permanence_vs_temporariness,
    'Was the rabbinic substitution of prayer and study for sacrifice intended as permanent replacement or temporary survival measure pending restoration?',
    'Close analysis of early rabbinic sources (Mishnah, Tosefta, Yerushalmi) for explicit statements on whether the substitution is conditional on exile vs. unconditional; historical tracking of when the ''temporary'' framing shifted to ''permanent'' in halakhic discourse.',
    'If temporary, the current archive''s claim of completion is a later doctrinal innovation that extracts by freezing a survival measure into permanent doctrine; if permanent, the restorationists are the innovators and the archive''s suppression is coordination maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_permanence_vs_temporariness, conceptual, 'Whether the substitution doctrine''s permanence is original or accrued.').

omega_variable(
    extraction_from_restoration_suppression,
    'Does the substitution archive actively extract value FROM suppressing restoration, or does it merely coordinate around an objectively impossible practice?',
    'Counterfactual analysis: if restoration became physically and politically feasible tomorrow, would rabbinic institutions facilitate or obstruct it? Track institutional responses to Temple Mount activism, halakhic rulings on korbanot today, and resource allocation to study vs. restoration preparation.',
    'If institutions would obstruct feasible restoration, the archive extracts by foreclosing the very outcome it claims to await; if they would facilitate, the suppression is coordination around genuine impossibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_from_restoration_suppression, empirical, 'Whether the constraint''s suppression of restoration is extractive or coordinative.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Is the Kodashim corpus itself a single kernel with three readings, or are these three distinct constraints that happen to share a textual corpus?',
    'Apply the ε-invariance test: if measuring the constraint via ''halakhic obligation to study Kodashim'' yields different ε than ''liturgical substitution of prayer for sacrifice'' or ''institutional curriculum control over sacrificial law'', then they are distinct constraints requiring separate stories.',
    'If multiple constraints, each reading gets its own ε and classification; if single kernel, the readings are perspectival variants on one constraint and the engine must compute per-seat types from one structural dataset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the kernel decomposition into three readings satisfies ε-invariance or masks multiple constraints.').

omega_variable(
    identity_lock_mechanism_restorationists,
    'What specific identity-fusion mechanism binds restoration advocates to the constraint such that exit is identity_locked rather than merely constrained?',
    'Interview/survey data from Temple Mount activists and Third Temple movements: is exit blocked by theological conviction (sacrifice is non-negotiable divine command), communal identity (restorationism defines their community), eschatological commitment (redemption requires Temple), or halakhic self-concept (they see themselves as the only faithful observers)?',
    'If identity_locked is theological/eschatological, the constraint''s suppression operates on conscience — the target carries the suppression internally. If communal, exit is structurally possible but socially catastrophic. The mechanism changes the engine''s effective extraction computation for the victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_restorationists, empirical, 'The specific identity-fusion mechanism creating identity_locked exit for restorationist victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_corpus__substitution_archive, theater_ratio, 70, 0.12).
narrative_ontology:measurement(koda_tr_t300, kodashim_corpus__substitution_archive, theater_ratio, 300, 0.22).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__substitution_archive, theater_ratio, 800, 0.31).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__substitution_archive, theater_ratio, 1200, 0.36).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(koda_tr_t1800, kodashim_corpus__substitution_archive, theater_ratio, 1800, 0.4).
narrative_ontology:measurement(koda_tr_t1948, kodashim_corpus__substitution_archive, theater_ratio, 1948, 0.41).
narrative_ontology:measurement(koda_tr_t1967, kodashim_corpus__substitution_archive, theater_ratio, 1967, 0.41).
narrative_ontology:measurement(koda_tr_t2024, kodashim_corpus__substitution_archive, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_corpus__substitution_archive, base_extractiveness, 70, 0.15).
narrative_ontology:measurement(koda_be_t300, kodashim_corpus__substitution_archive, base_extractiveness, 300, 0.28).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__substitution_archive, base_extractiveness, 800, 0.42).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__substitution_archive, base_extractiveness, 1200, 0.48).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.52).
narrative_ontology:measurement(koda_be_t1800, kodashim_corpus__substitution_archive, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(koda_be_t1948, kodashim_corpus__substitution_archive, base_extractiveness, 1948, 0.56).
narrative_ontology:measurement(koda_be_t1967, kodashim_corpus__substitution_archive, base_extractiveness, 1967, 0.57).
narrative_ontology:measurement(koda_be_t2024, kodashim_corpus__substitution_archive, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_corpus__substitution_archive, suppression_requirement, 70, 0.35).
narrative_ontology:measurement(koda_su_t300, kodashim_corpus__substitution_archive, suppression_requirement, 300, 0.45).
narrative_ontology:measurement(koda_su_t800, kodashim_corpus__substitution_archive, suppression_requirement, 800, 0.52).
narrative_ontology:measurement(koda_su_t1200, kodashim_corpus__substitution_archive, suppression_requirement, 1200, 0.58).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(koda_su_t1800, kodashim_corpus__substitution_archive, suppression_requirement, 1800, 0.61).
narrative_ontology:measurement(koda_su_t1948, kodashim_corpus__substitution_archive, suppression_requirement, 1948, 0.62).
narrative_ontology:measurement(koda_su_t1967, kodashim_corpus__substitution_archive, suppression_requirement, 1967, 0.62).
narrative_ontology:measurement(koda_su_t2024, kodashim_corpus__substitution_archive, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__substitution_archive, 0.1).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, temple_mount_access_regime).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, rabbinic_curriculum_authority).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, liturgical_substitution_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one member of the kodashim_corpus constraint family (kernel_id: kodashim_corpus). The three readings instantiate three distinct constraints with different ε values and beneficiary/victim structures: performance_only (low ε, Mountain-adjacent — archive as passive blueprint), study_as_exercise (moderate ε, Rope — study as genuine coordination), substitution_archive (moderate-high ε, Tangled Rope — substitution as extraction-suppression hybrid). All three are linked via affects_constraints. The decomposition follows the ε-invariance principle: the label 'Kodashim' conflates structurally distinct claims about the corpus's function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, institutional, 0.15).
constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, organized, 0.25).
constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, moderate, 0.8).
constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
