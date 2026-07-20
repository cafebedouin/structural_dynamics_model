% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Quran 9:5 Universal Abrogation Reading â Offensive Jihad as Standing Obligation
 *   domain: religious/political/jurisprudential
 *
 * SUMMARY:
 *   This constraint is the abrogating_universal reading of the contested
 *   kernel quran_9_5_scope. The kernel is the scope and legal status of Quran
 *   9:5 (the 'Verse of the Sword'). This reading holds that 9:5 abrogates all
 *   prior verses commanding peace or restraint with polytheists, establishing
 *   a universal, timeless command to offensive jihad until submission or
 *   conversion. Sibling readings are contextual_defensive (9:5 addressed
 *   specific treaty-breaking tribes, does not abrogate peaceful verses) and
 *   progressive_synthesis (9:5 is time-bound, superseded by Quranic ethical
 *   trajectory). The structural elements that differ across readings are: (1)
 *   the scope of naskh (abrogation), (2) the temporal validity of the
 *   command, and (3) the identity of the victim set (universal non-Muslims
 *   vs. specific historical actors vs. none).
 *
 * KEY AGENTS:
 *   - classical_jurists_abrogation_school: Agenda setter (institutional/civilizational) â administers the abrogation framework and determines which verses are abrogated.
 *   - expansionist_movements: Primary beneficiary (organized/mobile) â collect divine mandate legitimacy and territorial gains without administering the legal framework.
 *   - non_muslim_populations: Primary target/payer (powerless/trapped) â bear the cost of subjugation, conversion pressure, or violence authorized by the reading.
 *   - dissenting_muslims: Secondary payer (moderate/identity_locked) â bear suppression costs for rejecting the reading within their own tradition.
 *   - coexistence_theologians: Excluded voice (moderate/constrained) â hold alternative readings but are structurally excluded from authoritative interpretation.
 *   - comparative_theologian: Analytical observer (analytical/analytical) â maps the structural relationship between the reading and its political deployment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.88).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.92).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.88).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Quran 9:5 Universal Abrogation Reading â Offensive Jihad as Standing Obligation").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/political/jurisprudential").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '2669291b-5c80-493e-8eec-eac4a1ec90f5').
narrative_ontology:cs_kernel_codification('2669291b-5c80-493e-8eec-eac4a1ec90f5', fixed_text).
narrative_ontology:cs_authority_grounding('2669291b-5c80-493e-8eec-eac4a1ec90f5', lineage).
narrative_ontology:cs_interpretation_layer_present('2669291b-5c80-493e-8eec-eac4a1ec90f5').
narrative_ontology:cs_reading_relation('2669291b-5c80-493e-8eec-eac4a1ec90f5', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('2669291b-5c80-493e-8eec-eac4a1ec90f5', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('2669291b-5c80-493e-8eec-eac4a1ec90f5', foundational, universal_abrogation_of_peaceful_verses).
narrative_ontology:cs_axiom_status(universal_abrogation_of_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('2669291b-5c80-493e-8eec-eac4a1ec90f5', universal_abrogation_of_peaceful_verses, theological).
narrative_ontology:cs_axiom('2669291b-5c80-493e-8eec-eac4a1ec90f5', foundational, eternal_offensive_jihad_obligation).
narrative_ontology:cs_axiom_status(eternal_offensive_jihad_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2669291b-5c80-493e-8eec-eac4a1ec90f5', eternal_offensive_jihad_obligation, theological).
narrative_ontology:cs_reference_frame('2669291b-5c80-493e-8eec-eac4a1ec90f5', classical_usul_unified_legal_code).
narrative_ontology:cs_drift_state('2669291b-5c80-493e-8eec-eac4a1ec90f5', contemporary_international_norms_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2669291b-5c80-493e-8eec-eac4a1ec90f5', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_movements).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, dissenting_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the classical naskh framework, classify verses as abrogated or abrogating, and produce legal opinions that bind the community to the universal offensive jihad reading. Their authority derives from continuity with early jurists; they are constrained by the weight of their own tradition but set its boundaries.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, classical_jurists_abrogation_school, agenda_setter,
    institutional, civilizational, constrained, global).

% Claim divine mandate through this reading to authorize territorial expansion and political domination. They collect legitimacy and material gains without being the juridical authors of the framework. They can exit by shifting to alternative ideological justifications.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_movements, beneficiary,
    organized, biographical, mobile, global).

% Are categorized by this reading as legitimate objects of offensive jihad until they submit, convert, or enter a formal treaty of subjugation. They cannot exit the constraint except through identity change or submission; the reading places them in a permanent target category absent protective treaty.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_populations, payer,
    powerless, immediate, trapped, global).

% Reject the universal abrogation reading on contextual or ethical grounds. They face social exclusion, accusations of apostasy, or political suppression within communities where the reading is dominant. Their religious identity locks them into the discourse but not into acceptance of the reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, dissenting_muslims, payer,
    moderate, biographical, identity_locked, global).

% Advance contextual or progressive readings that reject universal abrogation. They are structurally excluded from authoritative juristic bodies and state-backed religious institutions that enforce the abrogating framework.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, coexistence_theologians, excluded,
    moderate, generational, constrained, global).

% Analyzes the constraint from outside the Islamic juridical framework, tracking how the reading functions as a legal-political mechanism and how its structural relationships differ from sibling readings of the same verse.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, comparative_theologian, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, expansionist_movements).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Muslim community under a unified legal-theological framework regarding relations with non-Muslims, resolving the apparent contradiction between Quranic verses of peace and verses of warfare through the classical naskh (abrogation) mechanism.
% TRANSFER_FUNCTION: Moves divine sanction and territorial expansion authorization from the Quranic text to expansionist political movements; moves the costs of subjugation, conversion pressure, and violence to non-Muslim populations and to dissenting Muslims who reject the reading.
% ABSENT_VOICES: Contextualist jurists, progressive Quranic scholars, and non-Muslim interlocutors are structurally excluded; their objections are ruled out by the hermeneutic premise that later verses universally abrogate earlier peaceful ones.
% DISAPPEARANCE_RATIONALE: If this reading vanished as an operative legal constraint, classical jurisprudence would lose its primary apparatus for authorizing offensive jihad; expansionist movements would lose their central theological warrant; peaceful Quranic verses would regain legal force in schools that suppressed them; and non-Muslim populations would be removed from the category of permanent legitimate targets.
% FOUNDING_PROBLEM: Reconciling contradictory Quranic revelations on warfare and establishing a definitive legal rule for Muslim relations with polytheist tribes in early Medina, particularly after treaty violations.
% FOUNDING_PROBLEM_CORROBORATION: Critical historians and Quranic studies scholars outside the beneficiary set attest the verse addressed specific 7th-century Arabian treaty-breakers. Modern international law scholars and human rights institutions attest that the founding geopolitical problem no longer exists. The beneficiary set (expansionist movements) claims the problem is eternally live.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.88, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is very high (0.88) because the reading authorizes first-strike violence, subjugation, and conversion pressure against an entire category of people, extracting life, liberty, and autonomy. Suppression is higher still (0.92) because the constraint's persistence depends on actively suppressing alternative Quranic readings (peaceful verses, contextual interpretations) through the abrogation mechanism and enforcing theological conformity. Theater ratio is moderate (0.40): the extraction is materially real across history, but a substantial share of activity is performative maintenance of juristic legitimacy and ritual citation of the abrogation framework to foreclose debate. Accessibility collapse is high (0.85) because once the abrogation framework is accepted as divine hermeneutic, peaceful alternatives collapse entirely within that framework. Resistance is moderate-high (0.68) because dissenting Muslims, excluded theologians, and targeted populations have continuously resisted, though often at severe cost.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (expansionist movements) experiences the constraint as a source of divine legitimacy and political authorization; the agenda setter seat (classical jurists) experiences it as a coherent legal solution to revelatory contradiction. The payer seats (non-Muslim populations, dissenting Muslims) experience the identical structure as an existential threat and a mechanism of suppression. The engine computes this divergence from the structural data: identical constraint, radically different directionality and effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist movements are structural beneficiaries (collect divine mandate and territorial gains, mobile exit â d near the beneficiary end). Non-Muslim populations are structural targets (trapped, powerless, global scope â d near the full-target end). Dissenting Muslims are secondary targets (identity_locked, moderate power â d elevated by identity fusion but damped slightly relative to the powerless). Classical jurists are agenda setters with constrained exit; they are not declared beneficiaries but their institutional power gives them a fallback directionality nearer the beneficiary end than the targets. Coexistence theologians are excluded; their directionality is not computed because they are outside the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was localized 7th-century Arabian treaty conflict. That problem is dead. The arrangement persists because it serves the extraction function for expansionist movements and the institutional authority function for the classical juristic school. Without the mandate, the constraint would be a piton; because active beneficiaries continue to capture real gains from its enforcement, it remains a snare. The R5 genealogy mismatch (founding_problem_status=dead, disappearance_verdict=world_rearranges) flags it as mandate-persistent extraction rather than living coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the abrogating_universal reading of kernel quran_9_5_scope; siblings (contextual_defensive, progressive_synthesis) would eliminate the universal victim set and dissolve the extractive transfer to expansionist_movements. Where is the structural disagreement located?',
    'Comparative juristic analysis of naskh application to 9:5, historical-critical study of the verse''s Sitz im Leben, and cross-reading structural comparison to identify which reading the empirical evidence supports.',
    'If the contextual or progressive reading is correct, the victim set collapses and the constraint''s type shifts from snare to rope or scaffold; if this reading is correct, the high extraction and suppression are intrinsic to the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural location of disagreement between kernel readings').

omega_variable(
    abrogation_mechanism_scope,
    'Does the classical naskh mechanism invoked by this reading operate universally across all prior peaceful verses, or does it apply narrowly to specific treaty clauses in a localized historical context?',
    'Textual and historical analysis of abrogation claims in classical tafsir and usul al-fiqh, combined with statistical analysis of naskh ascriptions across the corpus.',
    'A narrow mechanism would remove the universal victim set and likely reclassify the constraint as a historically bounded rope or scaffold; a universal mechanism sustains the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_mechanism_scope, empirical, 'Scope of the abrogation mechanism').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative readings maintained by institutional enforcement (state or polity coercion) or by hermeneutic foreclosure internal to classical juristic methodology (where alternatives are ruled impossible by the system''s premises)?',
    'Compare suppression trajectories across political regimes: where the reading lacks state backing, does suppression persist through scholarly exclusion and social sanction alone? If so, internalized/hermeneutic foreclosure dominates.',
    'If suppression is primarily internalized, the constraint''s effective suppressive force exceeds the institutional measure because the target population carries the suppression across political transitions. If institutional, suppression may decay when state enforcement weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95_ab_univ_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.3).
narrative_ontology:measurement(q95_ab_univ_tr_t200, quran_9_5_scope__abrogating_universal, theater_ratio, 200, 0.25).
narrative_ontology:measurement(q95_ab_univ_tr_t400, quran_9_5_scope__abrogating_universal, theater_ratio, 400, 0.3).
narrative_ontology:measurement(q95_ab_univ_tr_t600, quran_9_5_scope__abrogating_universal, theater_ratio, 600, 0.38).
narrative_ontology:measurement(q95_ab_univ_tr_t800, quran_9_5_scope__abrogating_universal, theater_ratio, 800, 0.48).
narrative_ontology:measurement(q95_ab_univ_tr_t1000, quran_9_5_scope__abrogating_universal, theater_ratio, 1000, 0.55).
narrative_ontology:measurement(q95_ab_univ_tr_t1200, quran_9_5_scope__abrogating_universal, theater_ratio, 1200, 0.5).
narrative_ontology:measurement(q95_ab_univ_tr_t1400, quran_9_5_scope__abrogating_universal, theater_ratio, 1400, 0.4).

% Extraction over time
narrative_ontology:measurement(q95_ab_univ_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(q95_ab_univ_be_t200, quran_9_5_scope__abrogating_universal, base_extractiveness, 200, 0.85).
narrative_ontology:measurement(q95_ab_univ_be_t400, quran_9_5_scope__abrogating_universal, base_extractiveness, 400, 0.82).
narrative_ontology:measurement(q95_ab_univ_be_t600, quran_9_5_scope__abrogating_universal, base_extractiveness, 600, 0.75).
narrative_ontology:measurement(q95_ab_univ_be_t800, quran_9_5_scope__abrogating_universal, base_extractiveness, 800, 0.68).
narrative_ontology:measurement(q95_ab_univ_be_t1000, quran_9_5_scope__abrogating_universal, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement(q95_ab_univ_be_t1200, quran_9_5_scope__abrogating_universal, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement(q95_ab_univ_be_t1400, quran_9_5_scope__abrogating_universal, base_extractiveness, 1400, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(q95_ab_univ_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(q95_ab_univ_su_t200, quran_9_5_scope__abrogating_universal, suppression_requirement, 200, 0.92).
narrative_ontology:measurement(q95_ab_univ_su_t400, quran_9_5_scope__abrogating_universal, suppression_requirement, 400, 0.9).
narrative_ontology:measurement(q95_ab_univ_su_t600, quran_9_5_scope__abrogating_universal, suppression_requirement, 600, 0.82).
narrative_ontology:measurement(q95_ab_univ_su_t800, quran_9_5_scope__abrogating_universal, suppression_requirement, 800, 0.75).
narrative_ontology:measurement(q95_ab_univ_su_t1000, quran_9_5_scope__abrogating_universal, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(q95_ab_univ_su_t1200, quran_9_5_scope__abrogating_universal, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement(q95_ab_univ_su_t1400, quran_9_5_scope__abrogating_universal, suppression_requirement, 1400, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the quran_9_5_scope kernel. The natural-language label 'scope of Quran 9:5' conflates three structurally distinct claims with different epsilon values, victim sets, and coordination/extraction profiles. Each reading instantiates a different constraint and must be evaluated separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
