% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Verse 9:5 as Universal Abrogating Offensive Jihad Mandate
 *   domain: islamic_jurisprudence/political_theology/hermeneutics
 *
 * SUMMARY:
 *   This constraint story captures the 'abrogating universal' reading of
 *   Quran 9:5 ('When the sacred months have passed, slay the polytheists
 *   wherever you find them...'). This reading holds that 9:5 abrogates
 *   (nasikh) all prior verses commanding patience, peaceful invitation, or
 *   defensive-only warfare, establishing offensive jihad against all
 *   polytheists (mushrikun) as a perpetual legal obligation until they
 *   convert or submit to Islamic rule. The constraint operates as a snare: it
 *   extracts theological legitimacy, human lives, political autonomy, and
 *   material resources from vast populations while suppressing alternative
 *   readings through takfir, institutional control, and violence.
 *   Beneficiaries are expansionist movements and regimes that capture the
 *   extraction; victims are non-Muslims, moderate scholars, minorities, and
 *   coexistence advocates. The historical measurements show the reading's
 *   extractiveness and suppression increasing as it was doctrinally hardened
 *   from a contextual wartime verse into a universal legal principle across
 *   the classical period, then reactivated in modern revivalist movements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.92).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.88).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.92).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Verse 9:5 as Universal Abrogating Offensive Jihad Mandate").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "islamic_jurisprudence/political_theology/hermeneutics").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2').
narrative_ontology:cs_kernel_codification('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', fixed_text).
narrative_ontology:cs_authority_grounding('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', lineage).
narrative_ontology:cs_interpretation_layer_present('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2').
narrative_ontology:cs_reading_relation('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', foundational, verse_9_5_abrogates_all_peaceful_verses).
narrative_ontology:cs_axiom_status(verse_9_5_abrogates_all_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', verse_9_5_abrogates_all_peaceful_verses, deontological).
narrative_ontology:cs_axiom('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', foundational, offensive_jihad_is_perpetual_obligation_until_universal_submission).
narrative_ontology:cs_axiom_status(offensive_jihad_is_perpetual_obligation_until_universal_submission, holdable).
narrative_ontology:cs_axiom_grounding('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', offensive_jihad_is_perpetual_obligation_until_universal_submission, deontological).
narrative_ontology:cs_axiom('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', secondary, mushrikun_category_includes_all_non_muslims_absent_submission).
narrative_ontology:cs_axiom_status(mushrikun_category_includes_all_non_muslims_absent_submission, holdable).
narrative_ontology:cs_axiom_grounding('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', mushrikun_category_includes_all_non_muslims_absent_submission, conventional).
narrative_ontology:cs_reference_frame('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', classical_medinan_wartime_revelation).
narrative_ontology:cs_drift_state('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', classical_fiqh_codification_9th_century, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2cd3eb87-47a7-484f-b1a0-25d37c9e0dc2', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadi_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, caliphate_revivalist_groups).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, authoritarian_islamist_regimes).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations_under_jurisdiction).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, moderate_muslim_scholars).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, coexistence_advocacy_networks).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, religious_minorities_in_islamic_polities).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, nasikh_doctrine_universal_abrogation).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, offensive_jihad_as_perpetual_obligation).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, polytheist_submission_or_conversion_binary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim divine mandate for territorial expansion and governance based on this reading; recruit using the verse as proof-text for legitimacy; the reading constitutes their primary theological authorization for offensive operations.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihadi_movements, beneficiary,
    organized, generational, identity_locked, global).

% Use this reading to justify state-building projects requiring universal jurisdiction; the abrogation thesis makes coexistence frameworks legally impossible, which is structurally necessary for their political program.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, caliphate_revivalist_groups, beneficiary,
    organized, civilizational, identity_locked, global).

% Incorporate the reading into state legal codes to legitimize suppression of dissent and minority communities; control official religious institutions that promulgate this interpretation; benefit from the constraint's suppression of alternative theological frameworks.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, authoritarian_islamist_regimes, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, authoritarian_islamist_regimes, agenda_setter).

% Face the binary choice of conversion, submission (dhimma with discriminatory restrictions), or death; no legal exit from the constraint's jurisdiction; bear the material costs of discriminatory taxation, legal disabilities, and periodic violence.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_populations_under_jurisdiction, payer,
    powerless, biographical, trapped, local).

% Oppose the reading on textual, historical, and ethical grounds but face takfir (excommunication) threats, institutional marginalization, and physical danger; their exit from the constraint's discursive field requires abandoning scholarly authority or accepting exile.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, moderate_muslim_scholars, payer,
    moderate, biographical, constrained, global).

% Build interfaith and intra-Muslim peace initiatives that this reading declares legally impossible; their work is structurally suppressed by the constraint's denial of legitimacy to pluralistic frameworks.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, coexistence_advocacy_networks, payer,
    moderate, generational, constrained, global).

% Subject to the constraint's legal architecture (dhimma restrictions, blasphemy laws, apostasy penalties) that derive authority from the abrogation thesis; no political exit without regime change or migration.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, religious_minorities_in_islamic_polities, payer,
    powerless, biographical, trapped, local).

% Analyze the verse's historical context, textual variants, and reception history; their work is cited by all sides but does not determine which reading becomes legally operative.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, quranic_textual_scholars, observer,
    analytical, civilizational, analytical, universal).

% Document and condemn human rights violations justified by this reading; lack enforcement power against sovereign states that adopt it as constitutional principle.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates expansionist violence by providing a divine legal framework that overrides tribal, ethnic, and political fragmentation — converts contested territorial claims into religious obligation.
% TRANSFER_FUNCTION: Transfers the right to life, property, and religious autonomy from non-Muslim populations and dissenting Muslims to expansionist movements and regimes claiming the mandate; moves theological authority from pluralistic interpretive traditions to literalist monopolists.
% ABSENT_VOICES: Pre-modern jurists who limited 9:5 to specific historical contexts (e.g., Shafi'i conditionalists, Hanafi treaty-prioritizers); contemporary Muslim communities living peacefully under non-Muslim rule who are rendered theologically illegitimate by this reading; victims of jihadi violence who cannot testify in the hermeneutical arena.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the primary theological authorization for offensive jihad as universal obligation would collapse; jihadi movements would lose their central proof-text; regimes would lose legal basis for discriminatory codes; coexistence frameworks would regain theological legitimacy; the global security architecture would fundamentally reorganize.
% FOUNDING_PROBLEM: Early Muslim community in Medina faced existential threat from polytheist tribes that repeatedly broke treaties; the verse addressed this specific military-political crisis by authorizing decisive action against identified treaty-breakers.
% FOUNDING_PROBLEM_CORROBORATION: Classical tafsir literature (Tabari, Ibn Kathir, Qurtubi) documents the specific historical occasion (asbab al-nuzul) — the verse addresses the polytheists of Mecca and allied tribes who violated the Treaty of Hudaybiyyah. Modern historians (Watt, Crone, Donner) corroborate the 7th-century Medinan political context. The expansionist universalization is a later doctrinal development, attested by the evolution of fiqh al-jihad from defensive to offensive categories across the 8th-10th centuries.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.92) reflects the total transfer of life/property/autonomy rights from victims to claimants of the mandate. Suppression (0.88) reflects active enforcement through takfir, blasphemy laws, apostasy penalties, and violence against dissenters. Theater ratio (0.25) is moderate-low: the constraint performs some coordination (unifying fractious groups under single banner) but this is overwhelmed by extractive function. Accessibility collapse (0.78) is high but not total — alternative readings exist and persist (contextual_defensive, progressive_synthesis) but are structurally marginalized. Resistance (0.42) is moderate: significant scholarly and popular opposition exists but is suppressed. The claimed type 'snare' reflects pure extraction masked by divine mandate cover story; coordination function is instrumental to extraction, not independent.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seat (authoritarian regimes, jihadi movements), the constraint appears as divine law — coordination of the umma against corruption, realization of God's sovereignty. From the payer seats (non-Muslims, moderate scholars, minorities), it appears as theological totalitarianism — a legal architecture for permanent subjugation. The engine computes this divergence from the structural data; the claimed type 'snare' reflects the payer-seat reality while the beneficiary seat would claim 'mountain' (divine law). This perspectival gap IS the constraint's operational mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (expansionist movements, revivalist groups, authoritarian regimes) sit at d ≈ 0.05–0.15: they capture theological authority, recruitment streams, legal legitimacy, and material resources. Victims (non-Muslim populations, moderate scholars, minorities, coexistence advocates) sit at d ≈ 0.85–0.95: they bear the full cost of the constraint's operation with trapped or constrained exit. Observers (textual scholars, human rights bodies) sit at d ≈ 0.5: they analyze but do not structurally benefit or pay. The identity_locked exit for beneficiary groups reflects theological fusion — their self-concept and legitimacy are constituted through this reading; abandoning it would dissolve their identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (7th-century Medinan existential threat from treaty-breaking tribes) is dead — the specific historical context has not obtained for 14 centuries. Yet the arrangement persists and has intensified. The mandate has atrophied into pure extraction: the verse's original defensive/contextual function has been replaced by universal offensive obligation. The constraint now serves as a legitimation engine for movements and regimes that extract power, territory, and resources. Mandatrophy is resolved: this is a snare, not a degraded scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nasikh_scope_ambiguity,
    'Does the nasikh (abrogation) doctrine, as classically formulated, actually support universal abrogation of ALL peaceful verses by 9:5, or is this a later doctrinal expansion?',
    'Comparative analysis of classical usul al-fiqh texts on nasikh conditions (majority require explicit textual indication or consensus; 9:5 lacks both for universal scope) versus later revivalist reinterpretations.',
    'If universal abrogation lacks classical usul support, the reading is a constructed constraint masquerading as divine law — strengthening snare classification. If classically grounded, the constraint''s legitimacy claims gain structural weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nasikh_scope_ambiguity, conceptual, 'Whether the universal abrogation claim has genuine classical jurisprudential warrant or is a later doctrinal innovation.').

omega_variable(
    mushrikun_definition_instability,
    'Who counts as ''mushrikun'' (polytheists) subject to the verse''s command — only 7th-century Arabian pagans, or all non-Muslims including People of the Book?',
    'Classical tafsir survey on whether ''mushrikun'' in 9:5 includes Ahl al-Kitab; Hanafi/Shafi''i divergence on whether 9:29 (jizya verse) limits 9:5''s scope; modern revivalist expansion of the category.',
    'If mushrikun is historically restricted, the victim set is far smaller than the reading claims — the universal victim set is a constructed expansion. If universally extended, the constraint''s extraction scope matches its claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mushrikun_definition_instability, empirical, 'The definitional boundary of the victim set — historically contested, strategically expanded by beneficiaries.').

omega_variable(
    coercion_mechanism_suppression,
    'Is the constraint''s suppression primarily structural (state violence, legal penalties) or internalized (theological terror, identity fusion making exit unthinkable)?',
    'Post-exit trajectory study: track individuals who leave jihadi movements or authoritarian Islamic polities — does suppression persist as internalized fear/guilt, or does it dissipate with physical exit?',
    'If substantially internalized, effective suppression exceeds structural measures; victims carry the constraint with them. If primarily structural, exit (though dangerous) genuinely reduces suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_suppression, empirical, 'Structural vs. internalized suppression mechanism in theological totalitarian constraints.').

omega_variable(
    reading_foreclosure_structure,
    'Does the abrogating_universal reading logically foreclose the contextual_defensive reading within a single coherent framework, or do they operate as competing but non-contradictory hermeneutical options?',
    'Formal analysis of the logical relations between: (a) ''9:5 abrogates all peaceful verses universally'' and (b) ''9:5 is contextual, peaceful verses remain operative''. Do they share a common usul framework that could adjudicate between them, or are they incommensurable?',
    'If forecloses, the kernel has genuine logical schism; if coexists_with, the dispute is political/theological, not logical. This determines cs_structure.reading_relations accuracy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Logical relationship between sibling readings of the quran_9_5_scope kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 632, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_9_5_abrogating_tr_t632, quran_9_5_scope__abrogating_universal, theater_ratio, 632, 0.05).
narrative_ontology:measurement(quran_9_5_abrogating_tr_t750, quran_9_5_scope__abrogating_universal, theater_ratio, 750, 0.12).
narrative_ontology:measurement(quran_9_5_abrogating_tr_t900, quran_9_5_scope__abrogating_universal, theater_ratio, 900, 0.18).
narrative_ontology:measurement(quran_9_5_abrogating_tr_t1258, quran_9_5_scope__abrogating_universal, theater_ratio, 1258, 0.22).
narrative_ontology:measurement(quran_9_5_abrogating_tr_t1798, quran_9_5_scope__abrogating_universal, theater_ratio, 1798, 0.2).
narrative_ontology:measurement(quran_9_5_abrogating_tr_t1928, quran_9_5_scope__abrogating_universal, theater_ratio, 1928, 0.23).
narrative_ontology:measurement(quran_9_5_abrogating_tr_t1979, quran_9_5_scope__abrogating_universal, theater_ratio, 1979, 0.25).
narrative_ontology:measurement(quran_9_5_abrogating_tr_t2001, quran_9_5_scope__abrogating_universal, theater_ratio, 2001, 0.27).
narrative_ontology:measurement(quran_9_5_abrogating_tr_t2014, quran_9_5_scope__abrogating_universal, theater_ratio, 2014, 0.28).
narrative_ontology:measurement(quran_9_5_abrogating_tr_t2024, quran_9_5_scope__abrogating_universal, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(quran_9_5_abrogating_be_t632, quran_9_5_scope__abrogating_universal, base_extractiveness, 632, 0.35).
narrative_ontology:measurement(quran_9_5_abrogating_be_t750, quran_9_5_scope__abrogating_universal, base_extractiveness, 750, 0.62).
narrative_ontology:measurement(quran_9_5_abrogating_be_t900, quran_9_5_scope__abrogating_universal, base_extractiveness, 900, 0.78).
narrative_ontology:measurement(quran_9_5_abrogating_be_t1258, quran_9_5_scope__abrogating_universal, base_extractiveness, 1258, 0.85).
narrative_ontology:measurement(quran_9_5_abrogating_be_t1798, quran_9_5_scope__abrogating_universal, base_extractiveness, 1798, 0.82).
narrative_ontology:measurement(quran_9_5_abrogating_be_t1928, quran_9_5_scope__abrogating_universal, base_extractiveness, 1928, 0.88).
narrative_ontology:measurement(quran_9_5_abrogating_be_t1979, quran_9_5_scope__abrogating_universal, base_extractiveness, 1979, 0.92).
narrative_ontology:measurement(quran_9_5_abrogating_be_t2001, quran_9_5_scope__abrogating_universal, base_extractiveness, 2001, 0.94).
narrative_ontology:measurement(quran_9_5_abrogating_be_t2014, quran_9_5_scope__abrogating_universal, base_extractiveness, 2014, 0.93).
narrative_ontology:measurement(quran_9_5_abrogating_be_t2024, quran_9_5_scope__abrogating_universal, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(quran_9_5_abrogating_su_t632, quran_9_5_scope__abrogating_universal, suppression_requirement, 632, 0.4).
narrative_ontology:measurement(quran_9_5_abrogating_su_t750, quran_9_5_scope__abrogating_universal, suppression_requirement, 750, 0.65).
narrative_ontology:measurement(quran_9_5_abrogating_su_t900, quran_9_5_scope__abrogating_universal, suppression_requirement, 900, 0.75).
narrative_ontology:measurement(quran_9_5_abrogating_su_t1258, quran_9_5_scope__abrogating_universal, suppression_requirement, 1258, 0.82).
narrative_ontology:measurement(quran_9_5_abrogating_su_t1798, quran_9_5_scope__abrogating_universal, suppression_requirement, 1798, 0.78).
narrative_ontology:measurement(quran_9_5_abrogating_su_t1928, quran_9_5_scope__abrogating_universal, suppression_requirement, 1928, 0.82).
narrative_ontology:measurement(quran_9_5_abrogating_su_t1979, quran_9_5_scope__abrogating_universal, suppression_requirement, 1979, 0.88).
narrative_ontology:measurement(quran_9_5_abrogating_su_t2001, quran_9_5_scope__abrogating_universal, suppression_requirement, 2001, 0.9).
narrative_ontology:measurement(quran_9_5_abrogating_su_t2014, quran_9_5_scope__abrogating_universal, suppression_requirement, 2014, 0.89).
narrative_ontology:measurement(quran_9_5_abrogating_su_t2024, quran_9_5_scope__abrogating_universal, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__abrogating_universal, 0.08).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_29_jizya_obligation).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_8_39_fitna_elimination).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, classical_fiqh_jihad_offensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, modern_jihadi_recruitment_narrative).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_state_legal_architecture).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, apostasy_blasphemy_law_complex).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the quran_9_5_scope kernel. The contextual_defensive reading (low extractiveness, mountain/rope) and progressive_synthesis reading (scaffold, low extractiveness) share the same verse but instantiate structurally distinct constraints with different beneficiary/victim structures and different ε values. The abrogating_universal reading is the high-extraction instantiation that captures the doctrinal development from contextual verse to universal legal principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, institutional, 0.1).
constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, organized, 0.15).
constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, powerless, 0.95).
constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
