% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Reading of Usul al-Fiqh: Expansive Qiyas, Ra'y, and Istihsan
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This story is the Hanafi reading of the contested usul al-fiqh kernel:
 *   the question of what sources and methods are legitimate for deriving
 *   Islamic legal rulings when scripture does not directly speak. The Hanafi
 *   reading resolves the kernel toward maximal jurist discretion — qiyas is
 *   presumptively applicable wherever text is silent, ra'y supplements where
 *   analogy runs out, and istihsan permits an explicit departure from strict
 *   analogical reasoning where the analogically 'correct' result would
 *   produce hardship or violate public interest. This reading is authored on
 *   its own terms; the Maliki, Shafi'i, and Hanbali readings are separate
 *   constraints (linked via network.affects_constraints), each with their own
 *   ε and structural relationships, not observable variants of this one.
 *
 * KEY AGENTS:
 *   - rationalist_trained_jurist_class: primary beneficiary and agenda-setter — trained skill directly rewarded by the method's breadth
 *   - hanafi_court_administrators: institutional beneficiary — flexibility serves state administrative needs
 *   - textualist_hadith_scholars: primary victim of the reading's threshold-setting power — their evidentiary standard is structurally subordinated
 *   - litigants_favoring_predictable_textual_rulings: diffuse victim — bears unpredictability cost
 *   - comparative_legal_historians: analytical observer across the kernel's readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.42).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.38).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Reading of Usul al-Fiqh: Expansive Qiyas, Ra'y, and Istihsan").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "religious/legal").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '2f7a1220-e8cb-458c-9054-1fa8ee5385c9').
narrative_ontology:cs_kernel_codification('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', distributed).
narrative_ontology:cs_authority_grounding('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', practice).
narrative_ontology:cs_interpretation_layer_present('2f7a1220-e8cb-458c-9054-1fa8ee5385c9').
narrative_ontology:cs_reading_relation('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', foundational, qiyas_presumptively_valid_absent_direct_text).
narrative_ontology:cs_axiom_status(qiyas_presumptively_valid_absent_direct_text, holdable).
narrative_ontology:cs_axiom_grounding('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', qiyas_presumptively_valid_absent_direct_text, conventional).
narrative_ontology:cs_axiom('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', foundational, istihsan_overrides_analogy_for_public_interest).
narrative_ontology:cs_axiom_status(istihsan_overrides_analogy_for_public_interest, holdable).
narrative_ontology:cs_axiom_grounding('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', istihsan_overrides_analogy_for_public_interest, instrumental).
narrative_ontology:cs_reference_frame('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', kufan_rationalist_derivation_tradition).
narrative_ontology:cs_drift_state('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', post_classical_madhhab_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f7a1220-e8cb-458c-9054-1fa8ee5385c9', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_court_administrators).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, commercial_litigants_seeking_flexible_rulings).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_hadith_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, litigants_favoring_predictable_textual_rulings).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, minority_traditions_excluded_from_juristic_discretion).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, juristic_reasoning_as_legitimate_source_of_law).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, public_interest_as_valid_ground_for_departure_from_analogy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained extensively in dialectical reasoning and case analogy, this class administers courts and issues fatwas under a method that rewards exactly their trained skill set. They set the boundaries of when qiyas reaches its 'limit' and ra'y or istihsan may be invoked, giving them wide interpretive discretion that less rationally-trained textualist scholars cannot exercise as credibly.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class, agenda_setter).

% Operating under Ottoman and earlier Islamic state patronage, administrators favor the Hanafi method's flexibility because it allows rulings adaptable to changing commercial and administrative needs without waiting for new textual authority. Their institutional position is reinforced by the method's own self-legitimation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_court_administrators, beneficiary,
    institutional, generational, arbitrage, continental).

% Merchants and property holders whose disputes involve novel commercial arrangements benefit when istihsan permits departure from a rigid analogy that would otherwise produce an inconvenient or commercially destructive result. They cannot choose their legal school easily once embedded in a Hanafi-administered jurisdiction, but they benefit from its outcomes.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, commercial_litigants_seeking_flexible_rulings, beneficiary,
    moderate, biographical, constrained, regional).

% Scholars committed to hadith-based derivation see their evidentiary standard displaced whenever qiyas or istihsan is invoked in preference to a weaker or less-authenticated hadith. Their claim that textual sources should maximally restrict juristic discretion is structurally overridden by the Hanafi method's threshold for declaring 'textual silence.' They can argue and write refutations but cannot force adoption of their standard within Hanafi courts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_hadith_scholars, payer,
    organized, civilizational, constrained, continental).

% Ordinary litigants who expected a ruling to track a known hadith or clear textual precedent instead find outcomes shaped by a judge's exercise of ra'y or istihsan, an outcome harder to predict or contest since it rests on discretionary reasoning rather than a citable text. Geographic and jurisdictional constraints trap them within the prevailing school's court.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, litigants_favoring_predictable_textual_rulings, payer,
    powerless, biographical, trapped, local).

% Local custom-bearers, minority legal traditions, and lay practitioners whose own interpretive practices are not recognized as valid sources have no voice in whether a given istihsan ruling reflects genuine public interest or simply the trained class's preference; they are subject to outcomes without participating in generating them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, minority_traditions_excluded_from_juristic_discretion, excluded,
    powerless, generational, trapped, regional).

% Study how the four classical schools diverged on source hierarchy and discretion, tracing how the Hanafi method's breadth of jurist discretion shaped Ottoman legal administration relative to the narrower hadith-first methods of rival schools.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, rationalist_trained_jurist_class).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable method for deriving rulings in the vast space where the Quran and hadith are silent or ambiguous, allowing a functioning legal system to answer novel questions (commercial disputes, administrative matters, cases arising in newly conquered or converted territories) without waiting indefinitely for textual clarification.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to determine legal outcomes from a fixed, checkable textual corpus to a trained juristic class exercising analogy, reasoned opinion, and preference; moves predictability and contestability away from litigants and toward the discretion of judges credentialed in this method.
% ABSENT_VOICES: Textualist hadith scholars and minority local traditions are structurally present as antagonists in inter-school polemic but are excluded from actually setting the threshold for when qiyas is 'exhausted' and istihsan may be invoked — that threshold-setting power sits entirely within the trained jurist class this reading empowers.
% DISAPPEARANCE_RATIONALE: If the Hanafi method's expansive use of qiyas, ra'y, and istihsan were withdrawn, courts operating under it would have to fall back to a narrower textualist derivation (closer to the Hanbali or Shafi'i reading), producing different outcomes in commercial law, administrative rulings, and novel cases; historically, entire administrative traditions (Ottoman qanun alongside sharia) were built assuming this interpretive latitude existed.
% FOUNDING_PROBLEM: Early Kufan jurists faced a rapidly expanding Islamic polity encountering novel commercial, administrative, and social situations for which explicit Quranic or hadith guidance was often absent or contested, and needed a systematic method to derive workable rulings rather than leaving vast areas of practice unregulated.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists themselves attest the problem remains live: new circumstances continually arise requiring analogical extension. Historians of comparative Islamic law (writing from outside the Hanafi school, including Shafi'i-tradition scholars like al-Shafi'i himself in Kitab al-Umm) attest that much of what the Hanafi method treats as 'textual silence' is itself a contestable judgment, and that the school's discretion has at times been used to ratify outcomes convenient to state administrators rather than strictly to fill textual gaps.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).
:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.42 rather than high: the coordination function (answering genuinely silent cases) is real and substantial, and the method is not simply cover for rent-seeking — Ottoman legal administration genuinely required this flexibility to function across a vast, diverse empire. But there is a real transfer: interpretive authority and the power to determine outcomes shift from a checkable text to a credentialed class whose training happens to match exactly what the method rewards, which is the asymmetric extraction component that makes this tangled_rope rather than rope. Suppression (0.38) reflects that dissenting textualist positions are not banned, but the threshold for declaring 'qiyas exhausted' is set entirely by the empowered class, making genuine contestation structurally difficult rather than legally forbidden.
 *
 * DIRECTIONALITY LOGIC:
 *   The rationalist-trained jurist class sits near the full-beneficiary end: they collect interpretive authority and social/institutional standing directly from the method's breadth, and their exit options are effectively arbitrage-grade (they can move between administrative and judicial roles freely within the system they define). Textualist hadith scholars sit near the target end: the method's threshold-setting displaces their evidentiary standard whenever invoked, and their only recourse is polemical argument, not structural override — hence 'constrained' rather than 'trapped' exit (they can still write, teach, and found rival schools, which historically did happen). Ordinary litigants expecting textual predictability are the least powerful and most trapped, since local court jurisdiction is not something they control.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (silence of text on novel cases in an expanding polity) remains genuinely live in some sense — new circumstances keep arising — which prevents a simple 'dead mandate' verdict. But the founding_problem_status is authored as contested because outside corroboration (including from within the rival Shafi'i tradition) suggests the boundary of 'textual silence' has at times been stretched to ratify administratively convenient outcomes rather than strictly to fill genuine gaps. This is precisely the tangled_rope signature: a real coordination function (deriving rulings for silent cases) persists alongside an asymmetric extraction (discretion concentrated in a trained class whose judgment cannot be checked against a fixed textual standard) that requires active enforcement (court authority, credentialing, patronage) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_silence_vs_constructed_silence,
    'When a Hanafi jurist declares that textual sources are ''silent'' on a matter and therefore qiyas or istihsan may be invoked, is that silence a genuine feature of the textual corpus, or a constructed judgment that expands the space in which the trained jurist class''s discretion operates?',
    'Comparative analysis of specific rulings across schools: cases where Hanafi jurists declared silence and invoked qiyas/istihsan versus cases where Shafi''i or Hanbali jurists located an applicable hadith the Hanafi ruling did not cite or weighted differently.',
    'If silence is frequently constructed rather than genuine, the extraction component of this reading is understated; if silence is generally a faithful description of the textual gap, the coordination function dominates and extraction should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_silence_vs_constructed_silence, conceptual, 'Whether declared textual silence is a faithful description or a discretion-expanding judgment call.').

omega_variable(
    istihsan_as_principled_exception_or_unconstrained_override,
    'Is istihsan a principled, rule-governed exception to qiyas (bounded by identifiable criteria for public interest) or an effectively unconstrained juristic override that can be invoked whenever the analogically correct answer is inconvenient?',
    'Systematic review of istihsan rulings across centuries of Hanafi fiqh literature for consistency of the criteria invoked, cross-checked against contemporaneous critiques (including the Shafi''i critique that istihsan amounts to ''legislating by mere personal preference'').',
    'If istihsan is tightly rule-governed, the constraint tilts toward rope (genuine coordination refinement); if it functions as unconstrained override, it tilts more heavily toward the extraction pole already present in the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(istihsan_as_principled_exception_or_unconstrained_override, conceptual, 'Whether istihsan is principled exception-making or unconstrained discretionary override.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Across the four classical readings of usul al-fiqh, is the core disagreement located in the ranking of sources (text vs. practice vs. reason) or in the threshold for when a source is exhausted and another may be consulted?',
    'Structural comparison of the four readings'' own stated methodological texts (al-Shafi''i''s Risala, Hanafi usul texts, Maliki treatments of ''amal, Hanbali sadd al-dhara''i literature) to locate where the axioms actually diverge.',
    'If the disagreement is chiefly about thresholds rather than source ranking, the four readings are closer to variants of a shared method than genuinely distinct kernels, which would argue for tighter network coupling between the sibling constraint stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Where exactly the four sibling readings'' core disagreement is structurally located.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanafi_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanafi_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__hanafi_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__hanafi_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__hanafi_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement(usul_su_t80, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 80, 0.36).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'usul al-fiqh method' into structurally distinct readings of a single contested kernel. Each reading (hanafi_reading, maliki_reading, shafii_reading, hanbali_reading) has its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle — they are not the same constraint measured differently. The Hanafi reading is authored here as the most rationalist/discretion-expansive of the four; the Hanbali reading anchors the opposite (textually maximal-restrictive) pole; Maliki and Shafi'i occupy structurally distinct intermediate positions (community-practice-grounded and hadith-authentication-gated, respectively). All four link to each other via affects_constraints to preserve the kernel-family structure for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
