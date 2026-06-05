% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu Coexistence as Incoherent Bundle (Meiji Reading)
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo (Shinto-Buddhist syncretism) dominated Japanese religious
 *   practice from the Heian period through the Edo period (roughly 8th–19th
 *   centuries). Practitioners made simultaneous devotional commitments to
 *   Shinto kami and Buddhist deities; institutional arrangements allowed a
 *   single priesthood to serve both functions; cosmological narratives
 *   attempted to explain the relationship between kami and Buddhas. In 1868,
 *   the Meiji government implemented shinbutsu-bunri (Shinto-Buddhist
 *   separation), treating Shinto as indigenous Japanese religion and Buddhism
 *   as foreign import. The reading presented here claims that shinbutsu-shugo
 *   never achieved coherence, and Meiji bunri was successful precisely
 *   because it revealed an underlying incoherence that institutional power
 *   had suppressed. The constraint operates by preventing practitioners from
 *   asking categorical questions: Are kami and Buddhas the same? If not, what
 *   is their relationship? Are they compatible? These questions were
 *   systematically foreclosed through institutional suppression (economic
 *   penalty, social stigma, intellectual marginalization) and through what
 *   might be called 'coherence-work avoidance' — institutional actors
 *   actively prevented the synthesis that practitioners might have achieved.
 *   Extractiveness increased over the constraint's lifecycle (0.42 → 0.58) as
 *   late-Edo institutional actors became more sophisticated at maintaining
 *   ambiguity-through-silence. Theater ratio increased (0.62 → 0.81) as the
 *   ritual apparatus became more elaborate and the performative content of
 *   ambiguity-maintenance grew.
 *
 * KEY AGENTS:
 *   - Institutional Clergy Networks (Buddhist temples + Shinto shrines): Primary beneficiary (institutional/arbitrage) — extract patronage and donations from both religious constituencies; have capacity to switch identity when Meiji incentives shift
 *   - Political Authority (Tokugawa regime): Primary beneficiary (institutional/arbitrage) — benefits from religious incoherence that prevents either Shinto or Buddhism from consolidating power to challenge political authority
 *   - Theological Coherence Seekers: Primary victim (powerless/trapped) — any attempt to formulate coherent theological position triggers suppression; institutional foreclosure of coherence-work
 *   - Village Practitioners: Secondary victim (moderate/constrained) — forced to maintain incoherent devotional practice; bear cognitive load and social conformity pressure
 *   - Institutional Inertia Carriers: Institutional actor (institutional/constrained) — late-Edo actors maintaining the constraint partly through enforcement, partly through path-dependence and normalized practice
 *   - Meiji Political Authority: Analytical actor — reveals incoherence by removing institutional suppression; bunri functions as diagnostic instrument
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.58).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.72).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu Coexistence as Incoherent Bundle (Meiji Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '0b3535ef-1ff7-48d0-8b35-6e986895148c').
narrative_ontology:cs_kernel_codification('0b3535ef-1ff7-48d0-8b35-6e986895148c', distributed).
narrative_ontology:cs_authority_grounding('0b3535ef-1ff7-48d0-8b35-6e986895148c', extraction).
narrative_ontology:cs_reading_relation('0b3535ef-1ff7-48d0-8b35-6e986895148c', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('0b3535ef-1ff7-48d0-8b35-6e986895148c', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('0b3535ef-1ff7-48d0-8b35-6e986895148c', foundational, categorical_incoherence_not_rationalizable).
narrative_ontology:cs_axiom_status(categorical_incoherence_not_rationalizable, holdable).
narrative_ontology:cs_axiom_grounding('0b3535ef-1ff7-48d0-8b35-6e986895148c', categorical_incoherence_not_rationalizable, deontological).
narrative_ontology:cs_axiom('0b3535ef-1ff7-48d0-8b35-6e986895148c', foundational, institutional_suppression_enabled_apparent_coherence).
narrative_ontology:cs_axiom_status(institutional_suppression_enabled_apparent_coherence, holdable).
narrative_ontology:cs_axiom_grounding('0b3535ef-1ff7-48d0-8b35-6e986895148c', institutional_suppression_enabled_apparent_coherence, empirically_contingent).
narrative_ontology:cs_reference_frame('0b3535ef-1ff7-48d0-8b35-6e986895148c', undisturbed_ambiguity_maintenance).
narrative_ontology:cs_drift_state('0b3535ef-1ff7-48d0-8b35-6e986895148c', meiji_bunri_enforcement, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('0b3535ef-1ff7-48d0-8b35-6e986895148c', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, institutional_clergy_networks).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, political_authority).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theological_coherence).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, subordinated_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THEOLOGICAL COHERENCE (SNARE) — Cannot exit the incoherence trap. Any practitioner or scholar attempting to formulate a coherent theological account of Shinto-Buddhist relationship faces institutional suppression. The coherence victim bears full cost of maintained ambiguity: theoretical work is foreclosed, systematic understanding is penalized, categorical clarity triggers institutional backlash.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__incoherent_bundle_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VILLAGE PRACTITIONER (SNARE) — Constrained by local institutional power and absence of exit alternatives within community. Forced to maintain incoherent devotion: simultaneously Shinto and Buddhist observances without integrated framework. Suppression manifests as social exclusion if coherence-seeking questions are raised. Non-trivial cost to exit (social/economic/spiritual isolation) with minimal coordination benefit.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__incoherent_bundle_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL CLERGY NETWORK (ROPE) — Beneficiary position (monasteries, temples, shrines all extracting patronage from maintained ambiguity). Experiences constraint as coordination: the incoherence allows a single institution to collect both Shinto and Buddhist revenues without categorical commitment. Arbitrage exit available (Meiji clergy successfully switch to either pure-Shinto or pure-Buddhist institutional identity). Low effective extraction experienced because these actors have agency and can exit when incentives shift.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL AUTHORITY (ROPE) — Beneficiary position. Tokugawa regime experiences incoherence maintenance as coordination solution: neither Shinto nor Buddhism can challenge political authority when both are locked in categorical ambiguity. Arbitrage available — when Meiji power shifts, political authority immediately enforces bunri (separation). Extraction experienced as solving a political problem, not oppressing a constituency.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL INERTIA (PITON) — The constraint persists partly through the active enforcement of ambiguity maintenance (theater=0.81) and partly through institutional inertia. By the Edo period (late stage), many local traditions no longer understand the Shinto-Buddhist distinction as meaningful — the incoherence has been naturalized into local practice. Institutional actors maintain the ambiguity partly because alternatives require new frameworks, partly out of path-dependence.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__incoherent_bundle_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, Shinbutsu-shugo contains genuine coordination function (religious syncretism solves integration problems for practitioners with diverse devotional needs) alongside extraction function (institutional clergy and political authority benefit from maintained ambiguity that prevents theological challenge). The constraint is not a pure snare because syncretism provides real coordination value. It is tangled because that coordination value is inseparable from the extraction mechanism — the benefit to practitioners exists only because institutional actors suppress theoretical coherence.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__incoherent_bundle_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__incoherent_bundle_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, TR),
    TR >= 0.70.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from powerless theological coherence-seekers and moderate village practitioners. The extraction manifests as foreclosure of intellectual work (powerless) and forced participation in incoherent ritual (moderate). It does not rise higher than 0.58 because the institutional beneficiaries do not extract through direct oppression but through suppression of alternatives — the extraction mechanism is 'prevent coherence from forming' rather than 'actively extract resources.' Suppression (0.72): High. Significant suppressive infrastructure maintained categorical confusion: absence of coherent theological literature, institutional penalties for coherence-seeking, social stigma, linguistic foreclosure of categorical distinctions. The suppression is not totalitarian (people do practice, do seek coherence privately) but is substantial and sustained. Theater ratio (0.81): High. By late Edo, the ritual apparatus was elaborate and performative content was dominant. Priests maintained elaborate ceremonies, doctrinal pronouncements, and institutional structures that performed coherence while actual coherence was absent. The theater increased over time as institutional actors became more sophisticated at maintaining ambiguity through ritualization rather than through explicit suppression.
 *
 * PERSPECTIVAL GAP:
 *   The maximum gap occurs between the institutional beneficiaries (who experience Rope or low-chi coordination) and the powerless coherence-seekers (who experience Snare). The beneficiaries see a functional system that solves political and economic problems; the coherence-seekers see intellectual foreclosure and suppression. The analytical observer's Tangled Rope is intermediate — acknowledging that genuine coordination value exists (syncretism does allow practitioners with diverse needs to participate in single religious framework) while recognizing that this coordination value is inseparable from the extraction mechanism (institutional suppression of coherence is what allows the coordination to persist without being destabilized by contradictions). The piton perspective identifies late-stage institutional inertia — the constraint persisting partly through enforcement, partly through naturalization and path-dependence. The gap between piton and snare is significant: one emphasizes inertia and degradation, the other emphasizes active suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality (d value) is determined by each agent's structural position relative to the extraction flow. Institutional beneficiaries (clergy, political authority) have arbitrage exit options and benefit from ambiguity — they experience low d, negative f(d), minimal experienced extraction. The powerless theological coherence-seeker has trapped exit and bears full cost of foreclosure — high d, high f(d), maximum chi. The village practitioner has constrained exit (social pressure but not absolute foreclosure) and bears mixed costs/benefits — moderate d, moderate f(d), moderate chi. The analytical observer derives d from the analytical canonical (0.73) and sees the constraint from civilizational scope, producing the tangled_rope classification that synthesizes beneficiary and victim perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this reading's structure is NOT 'incoherent bundle maintained through suppression' vs. 'coherent bundle.' Rather, the reading instantiates a specific epistemic position on what the kernel IS. If the kernel is fundamentally incoherent, then other readings (syncretic, partition) are engaging in post-hoc rationalization and false coherence-making. If the kernel is fundamentally coherent, then this reading mischaracterizes it by focusing on institutional suppression rather than theological sophistication. The mandatrophy is not resolved here — it is preserved in omega variables. The reading is analytically valid only if the incoherence question is actually unsettled at the kernel level. The Meiji bunri data is diagnostic: if practitioners had integrated theology waiting to replace ambiguity, bunri would have encountered resistance; the speed of bunri's success and the rapid reversion to separate Shinto/Buddhist frameworks post-bunri suggests no integrated theology existed — which supports the incoherent_bundle reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_suppression_mechanism,
    'What was the actual institutional mechanism by which theological coherence-seeking was suppressed — doctrinal orthodoxy enforcement, economic penalty, social stigma, or intellectual/linguistic foreclosure?',
    'Archival analysis of suppressed coherence-seeking attempts; examination of Edo-period intellectual production and theological questions raised vs. foreclosed; local record evidence of penalties for coherence questions',
    'If suppression was primarily doctrinal/epistemic: snare classification strengthened (active intellectual coercion). If primarily economic/social: classification potentially shifts toward constrained-exit rather than trapped-exit (agents face high costs but visible alternatives). If multilayered: confirms snare with multiple suppression modalities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_suppression_mechanism, empirical, 'What institutional mechanism suppressed theological coherence-seeking').

omega_variable(
    sincere_syncretism_vs_institutional_ambiguity,
    'Did the incoherence reflect genuine theological syncretism (practitioners synthesizing Shinto and Buddhist concepts) or institutional ambiguity-maintenance (deliberate institutional preservation of categorical confusion)?',
    'Analysis of household ritual manuals, local theology texts, practitioner correspondence; comparison of Edo-period Shinto-Buddhist integration attempts vs. post-bunri rationalization claims; linguistic evidence of categorical boundaries or their absence',
    'If sincere syncretism: reading underestimates coordination function; constraint is less extractive, more rope-like. If institutional ambiguity: reading correctly characterizes incoherence as extraction mechanism. If both (layered): snare classification is robust — institutional actors deliberately prevented the synthesis that practitioners might have achieved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_syncretism_vs_institutional_ambiguity, conceptual, 'Whether incoherence was sincere syncretism or institutional ambiguity-maintenance').

omega_variable(
    meiji_bunri_causality,
    'Did Meiji bunri create the incoherence by imposing categorical separation (as other readings claim) or reveal pre-existing structural incoherence by removing the institutional pressure that maintained ambiguity?',
    'Comparative analysis of Edo-period late-stage coherence-seeking attempts vs. pre-Edo foundational synthesis works; examination of whether bunri met resistance from practitioners holding integrated theology or was accepted because no integrated theology existed; post-bunri theological production speed and confidence vs. groping for new frameworks',
    'If bunri created incoherence: incoherent_bundle_reading is wrong; sibling domain_partition_reading is correct; constraint should be reclassified. If bunri revealed incoherence: reading confirmed; extractiveness and snare classification hold; Meiji pressure is diagnostic instrument, not cause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_bunri_causality, empirical, 'Whether Meiji bunri created or revealed structural incoherence').

omega_variable(
    practitioner_cognitive_load,
    'What was the psychological/cognitive cost to practitioners of maintaining incoherent devotion — was it suppressed cognitive dissonance, successful compartmentalization, or unrecognized incoherence?',
    'Analysis of personal devotional writings, confessional records, ritual practice variation; examination of whether practitioners expressed awareness of contradiction vs. treating observations as complementary; comparison of psychological burden narratives Edo vs. post-bunri periods',
    'If high cognitive load with awareness: suppression was active and victims suffered. If successful compartmentalization: snare classification weaker — practitioners were not trapped, they were cognitively resourceful. If incoherence unrecognized: snare classification shifts to piton (theater gate) — no active suppression, just institutional inertia.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practitioner_cognitive_load, empirical, 'What was the psychological cost of maintaining incoherent devotion').

omega_variable(
    reading_kernel_ambiguity,
    'Is the kernel itself (shinbutsu_coexistence_commitment) best understood as a coherent theological/institutional commitment that this reading mischaracterizes, or as an inherently incoherent commitment that other readings rationalize?',
    'Genealogical analysis of shinbutsu-shugo foundational texts and institutional arrangements; examination of whether a stable coherent position exists in foundational literature or whether all coherence is post-hoc rationalization by specific readings; assessment of whether sibling readings suppress or acknowledge the incoherence question',
    'If kernel is coherent: this reading is wrong about the fundamental structure. If kernel is incoherent: this reading correctly identifies it; sibling readings are engaging in false coherence-making (oracle gap). If kernel-level coherence is ambiguous: the reading accurately captures the committer-axis dispute as itself about kernel ontology, not just about interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the kernel itself is coherent or inherently incoherent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(shinbutsu_tr_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 200, 0.75).
narrative_ontology:measurement(shinbutsu_tr_t400, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 400, 0.81).

% Extraction over time
narrative_ontology:measurement(shinbutsu_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shinbutsu_be_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(shinbutsu_be_t400, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 400, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_su_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(shinbutsu_su_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement(shinbutsu_su_t400, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 400, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_bunri_institutional_pressure).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, edo_period_theological_production).

% DUAL FORMULATION NOTE:
% The shinbutsu_coexistence_commitment kernel generates three separate constraint stories corresponding to three irreconcilable readings of what the kernel actually is. The incoherent_bundle_reading is upstream (epistemically) of the other two in that it claims their reading strategies are rationalizations post-facto. However, structurally, all three are siblings operating on the same kernel with different classification outcomes. Network links should be bidirectional — each reading affects the others by offering alternative interpretations that may foreclosure or influence sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
