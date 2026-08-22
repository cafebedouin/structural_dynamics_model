% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Usul al-Fiqh Methodology â Maximal Textual Restrictiveness
 *   domain: legal/religious/comparative_law
 *
 * SUMMARY:
 *   This constraint instantiates the Hanbali reading of the usul al-fiqh
 *   method kernel, characterized by maximal textual restrictiveness: the
 *   Quran and authenticated hadith are treated as maximally restrictive,
 *   qiyas is minimized to cases of clear textual silence, weak hadith is
 *   preferred over qiyas, and sadd al-dhara'i blocks innovations to preserve
 *   textual fidelity. It is one of four canonical madhhab-specific readings
 *   of the same kernel; sibling readings (Hanafi, Maliki, Shafi'i)
 *   instantiate greater openness to analogical reasoning, customary practice,
 *   or systematic meta-jurisprudence. The Hanbali reading functions as a
 *   methodological constraint that coordinates legal derivation around a
 *   fixed textual kernel while extracting scope from rationalist and
 *   customary legal development.
 *
 * KEY AGENTS:
 *   - hanbali_textualist_jurists: Agenda-setter and primary beneficiary (institutional/generational/identity_locked) â administer the textualist method and collect interpretive authority and institutional prestige.
 *   - hadith_scholars: Secondary beneficiary (institutional/generational/identity_locked) â gain methodological priority over rationalist jurists through the weak-hadith-over-qiyas rule.
 *   - rationalist_jurists: Primary payer (moderate/biographical/constrained) â bear suppression of qiyas, ra'y, and independent legal reasoning.
 *   - customary_law_practitioners: Secondary payer (moderate/biographical/constrained) â bear denial of independent evidentiary weight for regional custom and local consensus.
 *   - reformist_jurists: Excluded voice (moderate/biographical/constrained) â would advocate for modern legal adaptation but are kept out by innovation-blocking doctrine.
 *   - comparative_legal_historians: Analytical observer (analytical/generational/analytical) â sees cross-madhhab structural effects from outside theological commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.62).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Usul al-Fiqh Methodology â Maximal Textual Restrictiveness").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "legal/religious/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, 'e092589e-c459-4e42-adb6-2e3b37d94b6e').
narrative_ontology:cs_kernel_codification('e092589e-c459-4e42-adb6-2e3b37d94b6e', fixed_text).
narrative_ontology:cs_authority_grounding('e092589e-c459-4e42-adb6-2e3b37d94b6e', lineage).
narrative_ontology:cs_interpretation_layer_present('e092589e-c459-4e42-adb6-2e3b37d94b6e').
narrative_ontology:cs_reading_relation('e092589e-c459-4e42-adb6-2e3b37d94b6e', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('e092589e-c459-4e42-adb6-2e3b37d94b6e', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('e092589e-c459-4e42-adb6-2e3b37d94b6e', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('e092589e-c459-4e42-adb6-2e3b37d94b6e', foundational, textual_sources_maximally_restrictive).
narrative_ontology:cs_axiom_status(textual_sources_maximally_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('e092589e-c459-4e42-adb6-2e3b37d94b6e', textual_sources_maximally_restrictive, theological).
narrative_ontology:cs_axiom('e092589e-c459-4e42-adb6-2e3b37d94b6e', foundational, qiyas_subordinate_to_weak_hadith).
narrative_ontology:cs_axiom_status(qiyas_subordinate_to_weak_hadith, holdable).
narrative_ontology:cs_axiom_grounding('e092589e-c459-4e42-adb6-2e3b37d94b6e', qiyas_subordinate_to_weak_hadith, theological).
narrative_ontology:cs_reference_frame('e092589e-c459-4e42-adb6-2e3b37d94b6e', prophetic_textual_sufficiency).
narrative_ontology:cs_drift_state('e092589e-c459-4e42-adb6-2e3b37d94b6e', contemporary_nation_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e092589e-c459-4e42-adb6-2e3b37d94b6e', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hadith_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_sufficiency_thesis).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, sadd_al_dhara_i_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the Hanbali usul al-fiqh methodology through teaching, ifta, and institutional gatekeeping. Derive legitimacy from maximal adherence to Quran and authenticated hadith. Block qiyas and customary expansion unless textual silence is absolutely clear. Their scholarly and religious authority is fused with the textualist method; abandoning it would mean abandoning their identity and community standing.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_jurists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_jurists, beneficiary).

% Benefit from the methodological elevation of hadith science over analogical reasoning. In this reading, even weak hadith takes precedence over qiyas, expanding the jurisdiction and prestige of hadith-authentication institutions relative to rationalist jurists.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hadith_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% Bear the cost of methodological suppression: qiyas is minimized, ra'y is delegitimized, and weak hadith overrides their analogical arguments. Their capacity for independent legal development is constrained to narrow textual gaps. Exit means leaving the Hanbali scholarly community or adopting taqlid of textualist positions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, global).

% Local customary legal arrangements and regional norms ('urf, 'amal ahl al-Madina) are subordinated to textual sources. Their practices survive only when anchored to hadith, denying independent evidentiary weight to customary coherence or local consensus.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_law_practitioners, payer,
    moderate, biographical, constrained, regional).

% Advocate for legal adaptation to modern contexts through unrestricted maslaha or renewed ijtihad beyond textual bounds. Structurally excluded because sadd al-dhara'i blocks innovations and the maximal textual restrictiveness leaves no authorized gap for reformist expansion.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, reformist_jurists, excluded,
    moderate, biographical, constrained, global).

% Study the divergence between madhhab usul systems from outside the theological commitments of any single reading. Document how the Hanbali reading's restrictiveness concentrates authority and limits legal adaptability relative to other readings.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_legal_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_jurists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves unity of legal derivation across dispersed Muslim communities by anchaling rulings to a fixed textual kernel (Quran and authenticated hadith), preventing fragmentation through unbounded rationalism, local custom, or speculative innovation.
% TRANSFER_FUNCTION: Moves interpretive authority and methodological scope from rationalist jurists and customary practitioners to textualist jurists and hadith scholars; transfers legal-development priority from analogy and public interest to textual authentication and innovation-blocking.
% ABSENT_VOICES: Rationalist jurists advocating expansive qiyas and ra'y, reformist jurists seeking modern legal adaptation through unrestricted maslaha, and customary-law communities whose local norms are denied independent evidentiary weight. They are structurally excluded by the methodological priority of textual sources and the doctrine of sadd al-dhara'i.
% DISAPPEARANCE_RATIONALE: If maximal textual restrictiveness vanished overnight, Hanbali jurisprudence would reorganize around expanded qiyas, integrated custom, and state or public-interest reasoning, resembling Hanafi or Maliki configurations; hadith scholars' authority would diminish relative to rationalist jurists, and the innovation-blocking function would collapse.
% FOUNDING_PROBLEM: The early Muslim community needed to derive legal rulings for novel cases after the Prophet's death while preserving connection to revelatory sources and preventing arbitrary innovation (bid'a) that could fragment the community's normative coherence.
% FOUNDING_PROBLEM_CORROBORATION: Western Islamic legal historians and comparative jurists attest the founding problem of post-Prophetic legal derivation was genuine in the formative period, but contest whether maximal textual restrictiveness remains the appropriate response in the modern era. Nation-state legal systems across the Muslim world largely operate outside classical madhhab usul, corroborating a shifted-function reading from outside the textualist beneficiary set.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the reading blocks entire avenues of legal development (qiyas, maslaha mursala, independent custom) and concentrates interpretive authority in textualist institutions. Suppression (0.62) reflects the institutional gatekeeping, scholarly credentialing, and theological sanction required to maintain the weak-hadith-over-qiyas hierarchy against persistent rationalist challenge. Theater ratio (0.32) captures the mixture of genuine textual scholarship and performative piety through textualist display. Accessibility collapse (0.58) is moderate-to-high: within the Hanbali framework alternatives nearly collapse, though they persist in sibling madhhabs offering constrained exit. Resistance (0.48) is moderate, driven by rationalist jurists, reformists, and state codification movements that bypass madhhab constraints. The temporal series share one grid and show extraction and suppression dipping during the colonial and early nation-state periods when madhhab authority was displaced, then rising again in the contemporary period with identity-based revival movements.
 *
 * PERSPECTIVAL GAP:
 *   From the textualist jurist's seat, the constraint is necessary coordination that prevents the fragmentation of divine law into arbitrary human opinion. From the rationalist or customary practitioner's seat, the same structure operates as extraction of interpretive authority and suppression of context-sensitive legal development. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The hanbali_textualist_jurists and hadith_scholars are structural beneficiaries: their authority and institutional scope are subsidized by the constraint (low d). Rationalist_jurists and customary_law_practitioners are structural targets: their methodologies are actively suppressed and their exit is constrained by identity and community lock-in (high d). The reformist_jurists are excluded rather than coordinated â their absence is a structural feature of the constraint's innovation-blocking function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â deriving law after the Prophet's death while preserving revelatory connection â was genuine and live in the formative period. However, the specific solution of maximal textual restrictiveness has become contested: nation-state legal systems, colonial courts, and modernist reform movements have largely bypassed or superseded madhhab usul, suggesting the arrangement persists partly through institutional inertia and identity lock-in even where its original coordination function has been displaced by statutory codification. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags potential mandatrophy: the world has rearranged around state law, yet the textualist method persists as a tangled rope of coordination and extraction within traditional scholarly communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'Does the Hanbali reading''s core premise that weak hadith outweighs qiyas foreclose the Hanafi reading''s expansive qiyas, or do the readings merely coexist as parallel institutional traditions?',
    'Examine historical cases of jurists holding cross-madhhab affiliations or modern fiqh councils attempting unified rulings; if no single jurist can simultaneously apply expansive qiyas and the weak-hadith-over-qiyas rule in one legal act, the relation is coexistent; if the premises logically contradict at the axiomatic level, foreclosure applies.',
    'If foreclosed, the Hanbali reading exhibits stronger cross-index coupling and potential snare-like suppression of rationalist methods within a unified framework; if coexistent, the constraint functions as tangled rope within a plural legal ecosystem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Structural relation of Hanbali reading to Hanafi sibling').

omega_variable(
    coordination_extraction_boundary,
    'Is the restriction of qiyas and maslaha a necessary coordination mechanism to preserve revelatory coherence, or primarily an extraction mechanism concentrating interpretive authority in textualist institutions?',
    'Comparative legal-certainty analysis across madhhabs and measurement of interpretive-authority concentration in hadith-authentication institutions versus rationalist-jurist institutions.',
    'Would shift the balance between coordination and extraction metrics and affect theater_ratio assessment of textualist argumentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Coordination versus extraction in textualist legal method').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of rationalist and customary legal methods enforced through institutional gatekeeping external to the rationalist jurist, or internalized through theological commitment to textual sufficiency?',
    'Trajectory analysis of rationalist jurists post-exit: do they continue to self-censor rationalist arguments after leaving Hanbali institutions, or do they freely adopt expansive methods?',
    'If internalized, effective suppression exceeds structural measure and the constraint''s extractiveness for rationalist jurists is higher than the institutional metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'External versus internalized suppression of rationalist methods').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_hanbali_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(usul_hanbali_tr_t200, usul_al_fiqh_method__hanbali_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement(usul_hanbali_tr_t400, usul_al_fiqh_method__hanbali_reading, theater_ratio, 400, 0.28).
narrative_ontology:measurement(usul_hanbali_tr_t600, usul_al_fiqh_method__hanbali_reading, theater_ratio, 600, 0.32).
narrative_ontology:measurement(usul_hanbali_tr_t800, usul_al_fiqh_method__hanbali_reading, theater_ratio, 800, 0.38).
narrative_ontology:measurement(usul_hanbali_tr_t1000, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1000, 0.42).
narrative_ontology:measurement(usul_hanbali_tr_t1200, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1200, 0.32).

% Extraction over time
narrative_ontology:measurement(usul_hanbali_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usul_hanbali_be_t200, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(usul_hanbali_be_t400, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 400, 0.65).
narrative_ontology:measurement(usul_hanbali_be_t600, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 600, 0.62).
narrative_ontology:measurement(usul_hanbali_be_t800, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 800, 0.52).
narrative_ontology:measurement(usul_hanbali_be_t1000, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1000, 0.5).
narrative_ontology:measurement(usul_hanbali_be_t1200, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1200, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_hanbali_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(usul_hanbali_su_t200, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 200, 0.45).
narrative_ontology:measurement(usul_hanbali_su_t400, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 400, 0.58).
narrative_ontology:measurement(usul_hanbali_su_t600, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 600, 0.55).
narrative_ontology:measurement(usul_hanbali_su_t800, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 800, 0.45).
narrative_ontology:measurement(usul_hanbali_su_t1000, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1000, 0.4).
narrative_ontology:measurement(usul_hanbali_su_t1200, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the usul_al_fiqh_method kernel, decomposed per the Îµ-invariance principle because the Hanbali reading's structural parameters (maximal textual restrictiveness, weak hadith preference, sadd al-dhara'i) produce a distinct Îµ from sibling readings. The four madhhab-specific usul readings form a constraint family linked by shared kernel but divergent beneficiary/victim structures and source hierarchies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
