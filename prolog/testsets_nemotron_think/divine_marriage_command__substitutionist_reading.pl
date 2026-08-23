% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Substitutionist Reading: Manifesto as New Revelation Requiring Monogamy
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   The 1890 Woodruff Manifesto declared the end of plural marriage in the
 *   LDS Church. The substitutionist reading holds this as new revelation
 *   superseding the prior divine command (D&C 132), making monogamy the
 *   current doctrinal requirement. This reading forecloses the
 *   continuationist position (polygamy remains valid, Manifesto was
 *   prudential) and coexists with the coercion-visibility reading (Manifesto
 *   as acknowledged response to federal pressure). The constraint operates as
 *   a tangled rope: genuine coordination function (institutional survival,
 *   theological continuity, federal reconciliation) coupled with asymmetric
 *   extraction (fundamentalists bear excommunication, legal vulnerability,
 *   and identity dissolution while mainstream members and leadership gain
 *   legitimacy and stability). Active enforcement persists through
 *   disciplinary councils, temple recommend standards, and public
 *   excommunications of fundamentalist groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.65).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.8).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Substitutionist Reading: Manifesto as New Revelation Requiring Monogamy").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '996b77ac-7bc9-40eb-adbb-6491f49eebd1').
narrative_ontology:cs_kernel_codification('996b77ac-7bc9-40eb-adbb-6491f49eebd1', formalized).
narrative_ontology:cs_authority_grounding('996b77ac-7bc9-40eb-adbb-6491f49eebd1', lineage).
narrative_ontology:cs_interpretation_layer_present('996b77ac-7bc9-40eb-adbb-6491f49eebd1').
narrative_ontology:cs_reading_relation('996b77ac-7bc9-40eb-adbb-6491f49eebd1', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('996b77ac-7bc9-40eb-adbb-6491f49eebd1', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('996b77ac-7bc9-40eb-adbb-6491f49eebd1', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('996b77ac-7bc9-40eb-adbb-6491f49eebd1', foundational, manifesto_as_new_revelation).
narrative_ontology:cs_axiom_status(manifesto_as_new_revelation, holdable).
narrative_ontology:cs_axiom_grounding('996b77ac-7bc9-40eb-adbb-6491f49eebd1', manifesto_as_new_revelation, deontological).
narrative_ontology:cs_axiom('996b77ac-7bc9-40eb-adbb-6491f49eebd1', foundational, polygamy_as_apostasy_post_manifesto).
narrative_ontology:cs_axiom_status(polygamy_as_apostasy_post_manifesto, holdable).
narrative_ontology:cs_axiom_grounding('996b77ac-7bc9-40eb-adbb-6491f49eebd1', polygamy_as_apostasy_post_manifesto, deontological).
narrative_ontology:cs_reference_frame('996b77ac-7bc9-40eb-adbb-6491f49eebd1', prophetic_continuity_framework).
narrative_ontology:cs_drift_state('996b77ac-7bc9-40eb-adbb-6491f49eebd1', contemporary_fundamentalist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('996b77ac-7bc9-40eb-adbb-6491f49eebd1', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, lds_church_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, mainstream_lds_members).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_mormons).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, prophetic_infallibility_doctrine).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, continuing_revelation_principle).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, institutional_survival_as_divine_will).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto as prophetic revelation ending plural marriage; administers enforcement through excommunication, temple recommend denial, and disciplinary councils. Collects institutional legitimacy, statehood, federal recognition, and theological coherence. Exit is arbitrage-grade: the leadership defines the constraint and its interpretation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, lds_church_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain institutional stability, social acceptance, political integration, and theological certainty from the Manifesto's revelation-status. Bear diffuse costs: cognitive dissonance from doctrinal reversal, loss of distinctive identity, complicity in fundamentalist marginalization. Exit is constrained by community ties, family, and belief investment.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, mainstream_lds_members, beneficiary,
    organized, biographical, constrained, global).

% Continue plural marriage as doctrinal obligation; bear excommunication, legal prosecution, social exclusion, economic marginalization, and loss of temple access. Their identity is fused to the rejected practice — exit requires abandoning self-concept as true believers. No institutional recourse; the constraint declares them apostate.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_mormons, payer,
    powerless, generational, identity_locked, regional).

% Applied coercive pressure (Edmunds Act 1882, Edmunds-Tucker Act 1887) that precipitated the Manifesto. Historical actor whose enforcement created the existential threat the revelation resolved. Now analytical observer of church-state dynamics.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_government_historical, observer,
    institutional, immediate, analytical, national).

% Analyze the Manifesto as revelation, accommodation, or coercion-response. No stake in outcome; provide external corroboration for founding problem status and structural dynamics.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, religious_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the existential conflict between divine command for plural marriage and federal suppression threatening institutional survival; provides theological mechanism for doctrinal continuity through prophetic revelation rather than institutional surrender.
% TRANSFER_FUNCTION: Moves theological legitimacy and institutional survival from the plural marriage regime to the monogamy regime; moves the cost of federal non-compliance from the institution to the fundamentalist dissidents who bear excommunication, marginalization, and loss of communal standing.
% ABSENT_VOICES: Polygamous wives and children of the 1890 period whose lived arrangements were dissolved without their consent; their perspectives were not recorded in the revelation narrative. Contemporary fundamentalist women who experience the constraint as patriarchal control rather than divine will.
% DISAPPEARANCE_RATIONALE: If the Manifesto's revelation-status were retracted, the LDS Church would face immediate theological crisis: either reinstate plural marriage (triggering renewed federal conflict) or admit prophetic fallibility (undermining the authority structure that grounds all subsequent revelation). The institutional identity, legal standing, and theological coherence of the mainstream church depend on this constraint's validity.
% FOUNDING_PROBLEM: The 1887 Edmunds-Tucker Act disincorporated the Church, seized its assets, dissolved its perpetual succession, and imprisoned leadership; plural marriage had become an existential threat to institutional survival and the prophetic office itself.
% FOUNDING_PROBLEM_CORROBORATION: Non-Mormon historians (Sarah Barringer Gordon 'The Mormon Question', Kathleen Flake 'The Politics of American Religious Identity') and federal congressional records confirm the existential legal threat. The Church's own institutional history (Journal of Discourses, General Conference Reports, Woodruff's journals) corroborates the survival pressure. No corroboration exists from within the fundamentalist community for the 'dead' status — they contest it.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the concentrated costs on fundamentalists — loss of religious community, legal exposure, economic marginalization — while benefits diffuse across the mainstream institution. Suppression (0.8) is high because the constraint's persistence depends on active exclusion: excommunication machinery, legal cooperation with anti-polygamy prosecutions, and social shunning. Theater ratio (0.3) is low-moderate because the revelation framing is genuinely believed by leadership and mainstream members; the performative element grows over time as the founding crisis recedes but enforcement continues. Accessibility collapse (0.75) is high because once the Manifesto is accepted as revelation, the theological space for plural marriage closes — alternatives collapse into apostasy. Resistance (0.5) is moderate: fundamentalist communities persist and resist, but lack institutional power to challenge the constraint's dominance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (leadership) computes this as genuine revelation — a mountain of divine will. The payer seat (fundamentalists) computes it as snare — coercive extraction dressed in revelation language. The beneficiary seat (mainstream) computes it as rope — coordination for survival. The engine will compute these divergences from the structural data; the authored claim (tangled_rope) reflects the analyst's synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership sits at d ≈ 0.1 (full beneficiary: defines the constraint, collects legitimacy, faces no enforcement). Mainstream members sit at d ≈ 0.4 (net beneficiary with diffuse costs: gain stability but carry cognitive dissonance). Fundamentalists sit at d ≈ 0.95 (full target: identity-locked, bear concentrated extraction, no exit). Federal government (historical) sits at d ≈ 0.0 as the coercive force that shaped the constraint's emergence. Scholars sit at d = 0.5 (analytical). The identity-locked exit of fundamentalists is critical: their self-concept is constituted through the rejected practice, making exit structurally unavailable without identity dissolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead — statehood achieved, legal pressure resolved. Yet the constraint persists and intensifies enforcement against fundamentalists. This is mandatrophy: the arrangement has outlived its founding function and now serves to police the boundary between orthodoxy and schism. The substitutionist reading prevents mislabeling this as pure coordination (rope) by exposing the asymmetric extraction on fundamentalists, and prevents mislabeling as pure extraction (snare) by acknowledging the genuine survival coordination that was the Manifesto's genesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_accommodation_ambiguity,
    'Is the Manifesto a genuine new revelation superseding D&C 132, or a pragmatic accommodation to federal coercion framed as revelation?',
    'Comparative analysis of Woodruff''s private writings, contemporary apostolic diaries, and the Manifesto''s textual evolution (1890 Manifesto vs 1904 Second Manifesto vs 1910 doctrinal expositions). If private records reveal strategic calculation absent from public revelation rhetoric, the accommodation reading gains weight.',
    'If accommodation, the constraint is a snare from inception — extraction disguised as revelation. If genuine revelation, the constraint is a tangled rope with authentic coordination function. The substitutionist reading''s legitimacy as a distinct structural position depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_accommodation_ambiguity, conceptual, 'Whether the Manifesto''s revelation-status is epistemically authentic or strategically constructed.').

omega_variable(
    fundamentalist_legitimacy_contestation,
    'Do fundamentalist Mormons represent authentic continuation of the restored gospel, or schismatic innovation that the substitutionist reading correctly identifies as apostasy?',
    'Sociological analysis of fundamentalist communities'' claims to priesthood lineage, succession narratives, and doctrinal fidelity. Theological analysis of whether ''new revelation superseding prior command'' is a coherent hermeneutic within Mormonism''s continuing revelation framework.',
    'If fundamentalists are authentic continuators, the substitutionist reading''s extraction is illegitimate — it suppresses a valid interpretive tradition. If schismatic, the extraction is the cost of boundary maintenance. This determines whether the constraint''s victim structure is structural injustice or institutional self-definition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fundamentalist_legitimacy_contestation, conceptual, 'The legitimacy status of the fundamentalist position relative to the substitutionist reading.').

omega_variable(
    coercion_mechanism_internalization,
    'Is fundamentalist resistance to the Manifesto sustained by structural barriers (excommunication, legal threat) or by internalized theological conviction that the substitutionist reading is false?',
    'Longitudinal study of fundamentalist community formation: do defectors from mainstream LDS adopt fundamentalism primarily through theological persuasion or through social networks that provide exit scaffolding? Post-exit interviews assessing whether suppression persists after institutional barriers are removed.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — fundamentalists carry the suppression with them as theological conviction. If structural, suppression metrics accurately capture the coercive apparatus. This affects the omega-adjusted suppression value for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_internalization, empirical, 'Whether suppression of fundamentalist practice is externally enforced or internally reproduced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 134).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_tr_t60, divine_marriage_command__substitutionist_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_tr_t80, divine_marriage_command__substitutionist_reading, theater_ratio, 80, 0.29).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_tr_t100, divine_marriage_command__substitutionist_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_tr_t134, divine_marriage_command__substitutionist_reading, theater_ratio, 134, 0.3).

% Extraction over time
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_be_t60, divine_marriage_command__substitutionist_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_be_t80, divine_marriage_command__substitutionist_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_be_t100, divine_marriage_command__substitutionist_reading, base_extractiveness, 100, 0.64).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_be_t134, divine_marriage_command__substitutionist_reading, base_extractiveness, 134, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_su_t60, divine_marriage_command__substitutionist_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_su_t80, divine_marriage_command__substitutionist_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_su_t100, divine_marriage_command__substitutionist_reading, suppression_requirement, 100, 0.8).
narrative_ontology:measurement(divine_marriage_command__substitutionist_reading_su_t134, divine_marriage_command__substitutionist_reading, suppression_requirement, 134, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__substitutionist_reading, 0.08).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the divine_marriage_command kernel into three readings with distinct ε values and victim/beneficiary structures. The substitutionist reading (this story) has ε=0.65 with fundamentalists as victims. The continuationist reading would have near-zero ε for fundamentalists (they are beneficiaries of doctrinal continuity) but high ε for mainstream members who lose theological coherence. The coercion_visibility reading would have ε driven by federal coercion as the extractive force, with the institution as partial victim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, powerless, 0.95).
constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, organized, 0.4).
constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
