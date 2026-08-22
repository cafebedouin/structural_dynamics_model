% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Study Obligation under Messianic Deferral
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint instantiates the messianic deferral reading of the
 *   kodashim (sacrifice law) kernel: sacrifice commandments remain
 *   normatively operative despite Temple destruction and practical
 *   impossibility, justified by the claim that study maintains readiness for
 *   messianic restoration and Temple rebuilding. The reading treats
 *   suspension as temporary and contingent on a future event that the
 *   tradition holds as a live possibility. Present-generation practitioners
 *   are obligated to master sacrifice law even though enactment is
 *   impossible; the obligation is justified by preparation for a contingency
 *   rather than by present-day function. The constraint extracts opportunity
 *   cost from present-generation practitioners (whose time and intellectual
 *   resources are committed to an apparatus that cannot be enacted) and
 *   subordinates other active commandment practice. The rabbinic interpretive
 *   authority benefits by maintaining hermeneutical jurisdiction over the
 *   deferral framework. The reading coexists with two sibling
 *   interpretations: performance_only (suspension without study obligation,
 *   the husk doctrine) and study_as_performance (study itself fulfills the
 *   commandment, no deferral required). The messianic deferral reading
 *   occupies the middle ground: study is obligatory and fills the suspension
 *   gap, but the obligation is justified by contingent future restoration
 *   rather than present fulfillment.
 *
 * KEY AGENTS:
 *   - interpretive_authority_rabbinic: Sustains the deferral framework; adjudicates suspension-legitimacy; benefits from maintaining hermeneutical jurisdiction over the apparatus
 *   - present_generation_practitioners: Bear the study obligation; pay the opportunity cost; identity-locked exit (renouncing the obligation ruptures their relationship to the tradition)
 *   - practical_alternative_commandments: Displaced in priority and resource allocation by the kodashim study obligation
 *   - messianic_contingency: The future state treated as live possibility that justifies present study obligation
 *   - non_observant_judaism: Excluded from the interpretive framework; would dispute that readiness-for-contingency justifies subordinating present needs
 *   - analytical_observer: Measures the structural divergence between claimed function and measured cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.58).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.41).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, scaffold).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Study Obligation under Messianic Deferral").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).
narrative_ontology:has_sunset_clause(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'd6155c61-3b14-4389-bde1-35abd9467a77').
narrative_ontology:cs_kernel_codification('d6155c61-3b14-4389-bde1-35abd9467a77', fixed_text).
narrative_ontology:cs_authority_grounding('d6155c61-3b14-4389-bde1-35abd9467a77', lineage).
narrative_ontology:cs_interpretation_layer_present('d6155c61-3b14-4389-bde1-35abd9467a77').
narrative_ontology:cs_reading_relation('d6155c61-3b14-4389-bde1-35abd9467a77', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('d6155c61-3b14-4389-bde1-35abd9467a77', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('d6155c61-3b14-4389-bde1-35abd9467a77', foundational, suspension_implies_readiness_obligation).
narrative_ontology:cs_axiom_status(suspension_implies_readiness_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d6155c61-3b14-4389-bde1-35abd9467a77', suspension_implies_readiness_obligation, deontological).
narrative_ontology:cs_axiom('d6155c61-3b14-4389-bde1-35abd9467a77', foundational, messianic_restoration_contingency_live).
narrative_ontology:cs_axiom_status(messianic_restoration_contingency_live, holdable).
narrative_ontology:cs_axiom_grounding('d6155c61-3b14-4389-bde1-35abd9467a77', messianic_restoration_contingency_live, empirically_contingent).
narrative_ontology:cs_reference_frame('d6155c61-3b14-4389-bde1-35abd9467a77', suspension_without_obsolescence_prepared_for_restoration).
narrative_ontology:cs_drift_state('d6155c61-3b14-4389-bde1-35abd9467a77', contemporary_post_enlightenment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6155c61-3b14-4389-bde1-35abd9467a77', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, interpretive_authority_rabbinic).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_practitioners).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, practical_alternative_commandments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rabbinical interpretive authority that sustains and adjudicates the deferral framework. Controls what counts as proper engagement with sacrifice law, maintains the hermeneutical apparatus, and derives jurisdiction from being the seat that determines whether suspension-without-obsolescence is legitimate. Benefits by preserving a vast apparatus that requires specialist mastery and by maintaining institutional authority over present-generation obligation architecture.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, interpretive_authority_rabbinic, agenda_setter,
    institutional, civilizational, analytical, global).

% Observant Jews committed to the tradition who are obligated by the deferral reading to master sacrifice law even though enactment is impossible. Pay through time investment, intellectual labor, and displaced opportunity (other learning, commandment practice, secular pursuits). The constraint is enforced through hermeneutical authority and internalized identity-fusion: practitioners' self-understanding as part of the Jewish tradition is constituted through the interpretive framework that imposes the obligation. Exit is possible but is experienced as identity-rupturing.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_practitioners, payer,
    organized, biographical, identity_locked, global).

% Other active commandments (charity, ethical living, prayer, study of non-sacrifice law, observance of dietary rules, etc.) are subordinated in priority and temporal allocation by the study obligation. The constraint is not a zero-sum resource war but functions as a hermeneutical prioritization: the tradition's structural emphasis places readiness-for-restoration above other present-day commandment fulfillment, even though the other commandments remain formally active.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, practical_alternative_commandments, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, practical_alternative_commandments).

% The hypothetical future state in which the Temple is restored and sacrifice can be enacted. The constraint treats present study as maintenance-of-readiness for this state, such that if restoration occurs, the present-generation's sacrifice-law mastery will enable rapid reinstatement. The beneficiary is not an actor but a contingency that the constraint is structured to serve.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_restoration_state, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, messianic_restoration_state).

% Jewish movements and individuals who have substantially abandoned or reinterpreted the deferral obligation (Conservative, Reform, Reconstructionist Judaism; secular Jewish thought). They are structurally excluded from the interpretive framework that sustains the deferral reading. They would argue the constraint subordinates present-generation needs to a remote contingency and that the hermeneutical apparatus is a cover for maintaining institutional authority rather than a necessary response to post-Temple reality. Their voices are not seated at the rabbinical table where deferral-legitimacy is adjudicated.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, non_observant_judaism, excluded,
    moderate, generational, mobile, global).

% External scholarly perspective on the constraint's structural relationship between claimed function (readiness for restoration), actual operation (study obligation under interpretive authority), and measured cost (opportunity cost to present generation, theater rise over time, stability of institutional control). The observer seat reports on whether the constraint's justification tracks its structural persistence and whether the messianic deferral reading remains empirically coherent as a primary justification or has become increasingly theatrical.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, interpretive_authority_rabbinic).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains hermeneutical and textual continuity of the Jewish legal tradition across the dispersion period when sacrifice-law enactment is impossible; preserves the conceptual apparatus and institutional knowledge required for Temple service in case restoration occurs; coordinates the Jewish community around the interpretive framework that makes sense of suspension-without-obsolescence. Provides a unifying narrative (preparation for restoration) that explains why ancient law remains normatively binding despite practical impossibility.
% TRANSFER_FUNCTION: Transfers present-generation time, intellectual resources, and opportunity cost to an apparatus justified by future restoration; transfers hermeneutical authority and jurisdictional control from practitioners to the rabbinic interpretive authority that adjudicates suspension-legitimacy; transfers present attention and community commitment away from other active commandments and toward the preservation of a non-enactable apparatus.
% ABSENT_VOICES: Non-observant Jews and Jewish movements that have abandoned or substantially reframed the deferral obligation. Secular scholars who would argue the constraint is a constructed institutional choice rather than a natural response to destruction. Practitioners who would prioritize immediate ethical commandment fulfillment over preparation for a contingent future. Voices questioning whether readiness-for-restoration justifies opportunity cost in a world where restoration has become increasingly unlikely and indefinitely deferred. These parties would object that the constraint subordinates present needs to a remote contingency and that the hermeneutical framework is a mechanism for maintaining institutional authority rather than a necessary response to structural reality.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared (the deferral obligation were dropped), the tradition would reallocate time and community attention toward: (1) other active commandments and their deepening; (2) secular and intellectual pursuits currently displaced; (3) present-day ethical and communal needs; (4) the possibility of studying sacrifice law as history and literature rather than as binding legal apparatus; (5) honest reckoning with whether restoration is a live belief or a narrative useful for other purposes. The interpretive authority that sustains the deferral framework would lose a major domain of jurisdiction. Observant practitioners would experience discontinuity in their hermeneutical relationship to the tradition (the claim that present engagement IS obligation-fulfillment would rupture, requiring new framing). The tradition's public narrative about readiness-for-restoration would face forced clarification: either practitioners believe restoration is possible (and the constraint remains justified) or they don't (and the constraint becomes pure theater). Conservative and Reform movements have already made this move (treating the constraint as optional or superseded), showing the world can reorganize around alternative readings.
% FOUNDING_PROBLEM: After the Second Temple's destruction in 70 CE, sacrifice law became impossible to enact but remained canonical in the Jewish legal tradition. The hermeneutical problem: if commandments are binding on the Jewish people, what is the status of a commandment that cannot be performed? The deferral reading's answer: the commandment remains binding but is temporally suspended; study maintains readiness for restoration, which the tradition holds as a future possibility. This frames the dispersion period not as permanent rupture but as temporary suspension contingent on messianic restoration.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic tradition from the Talmudic period onward (particularly Maimonides, and subsequent halakhic authorities) attests that suspension-without-obsolescence is the legitimate response and that readiness-for-restoration justifies study obligation. Contemporary rabbinical authority across Orthodox Judaism affirms this position. HOWEVER: no corroborating source outside the benefiting interpretive authority — outside those who maintain hermeneutical jurisdiction — affirms that readiness-for-restoration is the proper solution to the founding problem. Historical scholarship and comparative-religion analysis suggest the deferral reading was a choice among alternatives (performance-only, study-as-performance, supersession, metaphorization); it was not a natural or inevitable response. Conservative and Reform movements have explicitly rejected it. Secular scholarship on Jewish law treats the deferral reading as a constructed narrative rather than a natural law. Non-observant Jews argue the founding problem is no longer live — Judaism adapted to dispersion through other mechanisms (prayer, ethics, study for its own sake), making the sacrifice-law apparatus optional. The restoration narrative itself has shifted: early rabbinics treated restoration as possibly imminent; medieval thought delayed it indefinitely; modern Jewish thought increasingly treats it as a meta-commandment (a hope or aspiration) rather than a live political possibility. The corroboration gap is acute: within the interpretive authority that sustains the deferral reading, no dissenting voice is credentialed to speak. Outside that authority, substantial dissent exists.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint creates genuine opportunity cost: practitioners must master an apparatus they cannot enact, displacing other learning and commandment practice, justified by a future event that may not occur and that the tradition increasingly treats as remote. The extraction is not coercive in the classical snare sense (practitioners are not trapped by legal force) but is structured into the hermeneutical framework that controls what counts as obligation-fulfillment. Suppression is lower (0.41) because practitioners maintain genuine choice to leave or reinterpret the constraint (the non-observant do), though for identity-locked practitioners the cost of exit is prohibitive. Theater is moderate-to-high (0.62) because the actual function of the constraint has drifted: over two millennia, the justification 'readiness for restoration' became increasingly detached from empirical likelihood of restoration, yet the apparatus and its authority structures persist. The measurement series shows extractiveness rising from the medieval period through early modernity, then stabilizing as alternative interpretations (Haskalah, modern Jewish thought) offered competing frames. The theater ratio also rises, marking the increasing gap between the stated justification (readiness) and the structural function (maintaining interpretive authority and hermeneutical coherence). Suppression requirement rises slightly, tracking the interpretive work needed to defend the deferral claim against challenges from within and outside the tradition. The time points are authored from historical Jewish legal development: t0 = immediate post-Temple period, t500 ≈ Talmudic codification era, t1000 ≈ medieval Ashkenazi / Sephardic flowering, t1500 ≈ early modernity / Haskalah challenge, t2000 ≈ contemporary moment where the constraint persists but its justification is openly contested.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (rabbinic authority) computes the constraint as legitimate coordination: hermeneutical coherence, tradition maintenance, readiness for restoration. The payer seat (present practitioners) computes it as extraction: opportunity cost, obligation justified by remote contingency, hermeneutical authority that controls the interpretive frame. The engine should compute these divergently from the structural data. The rabbinic seat has institutional power, analytical exit options, civilizational time horizon (can take a long view on messianic restoration). Practitioners have organized power but identity-locked exit, biographical time horizon (the contingency may never arrive in their lifetime). The spatial scope is global (the deferral reading is rabbinically authoritative across diaspora communities) but the extraction is distributed unevenly by degree of observance and hermeneutical access.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic interpretive authority is the structural beneficiary: it maintains jurisdiction over the deferral framework, controls what counts as proper engagement with the apparatus, and derives authority from being the seat that adjudicates suspension-legitimacy (d ≈ 0.15). Present-generation practitioners are the primary targets: they pay the opportunity cost (time, intellectual labor, displacement of other commandment practice) under an obligation justified by a contingency they did not choose and whose likelihood they may doubt (d ≈ 0.82, high target position modulated only slightly by identity-lock, since the identity itself is constituted through the tradition that imposes the obligation). The messianic contingency is treated as a beneficiary in the structure, though it is not an actor and collects nothing; instead, it serves the function of moving the justification away from present-generation benefit toward a future-conditional legitimacy that insulates the obligation from present-cost-benefit scrutiny. Non-observant Judaism sits at the excluded boundary: they would have strong reasons to object (the constraint subordinates present needs to contingent-future preparation) but are not seated at the interpretive table where the deferral's legitimacy is adjudicated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Temple law coherence) remains live in one reading but has been substantially superseded in others. The deferral reading itself contains an internal mandatrophy: the justification shifts from 'restoration is imminent' (early rabbinic era) to 'restoration is a live possibility' (medieval) to 'restoration is a commanded hope' (modern) while the apparatus and obligation persist unchanged. The theater rise (0.45 → 0.62) tracks this shift: the constraint becomes increasingly theatrical as the justification becomes increasingly detached from empirical contingency. The interpretive authority sustains the obligation not because practitioners believe restoration is imminent (few do in the modern period) but because the hermeneutical apparatus and the authority that controls it have become self-perpetuating. The constraint should be classified as tangled_rope in the agenda-setter seat (genuine coordination value in tradition maintenance, but extraction via authority maintenance) and as snare or piton in the payer seat (extraction sustained by identity-lock and hermeneutical suppression, with persistent theater masking the shift in justification). The claim/metric independence rule applies here: the story is CLAIMED as scaffold (sunset implicit in the contingency of restoration) but the authored metrics (high theater, stable suppression, rising extractiveness over time) describe something closer to piton with an unfulfilled eschatological premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_contingency_probability,
    'How should the tradition treat the empirical likelihood of messianic restoration? Is the constraint''s justification contingent on restored belief in imminent restoration, or does it survive even if restoration is treated as an extremely remote possibility?',
    'Survey of contemporary rabbinical authority on messianic timeline and the status of readiness-for-restoration as a justification when the contingency is no longer empirically live. Comparison of medieval Maimonidean universalism (restoration as mandatory belief) with contemporary pluralism (restoration as optional meta-commandment).',
    'If restoration probability is treated as effectively zero in modern thought, the constraint becomes pure theater: readiness for something treated as impossible to occur. If restoration remains normatively live (commanded hope), the constraint remains legitimately deferral-justified. The measurement series suggests the empirical belief shifted while the apparatus persisted, marking the theater rise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_contingency_probability, conceptual, 'Whether the messianic restoration remains empirically live or has become theoretical.').

omega_variable(
    study_fulfillment_boundary,
    'Does study of sacrifice law constitute ANY degree of commandment-fulfillment (study_as_performance reading), or is it purely preparation-for-future-enactment (messianic_deferral reading)? Where is the line between engagement that satisfies and engagement that merely preserves readiness?',
    'Close reading of halakhic authorities on the status of study: does it substitute for enactment (Maimonides'' position in some contexts) or supplement it? Does the dispute between deferral and performance readings reflect different hermeneutical foundations or a scalar disagreement about the degree of fulfillment study provides?',
    'If study IS partial fulfillment, the constraint becomes less extractive (present-generation engagement has present-day meaning). If study is purely future-oriented, the extraction of opportunity cost from present generation becomes clearer. The boundary between the two readings is here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_fulfillment_boundary, empirical, 'Whether study constitutes fulfillment or merely preparation.').

omega_variable(
    suppression_mechanism_identity_vs_institutional,
    'Is the suppression of alternatives (performance_only, study_as_performance readings, non-observant exit) institutional (rabbinical authority excludes competitors from the interpretive table) or internalized (practitioners'' identity-fusion with the deferral framework makes alternatives unthinkable)? Are these mechanisms separable?',
    'Post-exit ethnography: practitioners who have left the constraint report on whether suppression persisted after institutional authority ceased (revealing internalization). Comparison of historically Jewish communities with varying institutional authority structures but similar cultural cohesion.',
    'If suppression is institutional, weakening authority structures should weaken the constraint. If suppression is internalized (identity-lock), the constraint persists even after institutional authority decays. The measurement series stability suggests strong internalization. If both are present, the true suppression effectiveness is higher than the scalar measure, and the constraint is more entrenched than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_vs_institutional, empirical, 'Whether suppression of alternatives is institutional or internalized.').

omega_variable(
    kernel_forecast_restoration_timing,
    'What does the constraint presuppose about the timing of restoration? Early rabbinic thought treated it as possibly imminent; medieval thought pushed it further; modern thought increasingly treats it as indefinitely deferred or unspecified. Does the constraint''s structure change as the presupposed timing changes?',
    'Temporal analysis of rabbinical writings on restoration probability across epochs. Measurement of whether extractiveness and theater_ratio correlate with stated restoration timelines.',
    'If the constraint structure depends on restoration being temporally proximate (imminent), then the shift to indefinite deferral should trigger reclassification. If the constraint is agnostic about timing, it survives temporal shifts. The measurement series suggests the constraint absorbed the timing shift without reclassification, marking increasing theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_forecast_restoration_timing, empirical, 'How the constraint''s structure maps onto changing beliefs about restoration timing.').

omega_variable(
    false_summit_natural_law_vs_constructed,
    'Is this constraint grounded in natural law (sacrifice is an inherent part of Jewish covenant; deferral is the logically necessary response to Temple destruction) or is it constructed (a hermeneutical choice made by rabbinical authority to preserve an apparatus and maintain jurisdiction)? The framing as ''suspension without obsolescence'' may be a narrative choice rather than a structural necessity.',
    'Comparative study of other religious traditions'' responses to the loss of enactable commandments: do they uniformly adopt deferral, or do alternative responses (supersession, reinterpretation as metaphor, abandonment) suggest the choice is constructed? Analysis of whether the deferral reading was contested when first proposed, indicating contemporaries saw it as one choice among others rather than a natural law.',
    'If constructed, the constraint is a false summit: natural law framing masking institutional extraction via interpretive authority maintenance. If natural law (logically necessary), the extraction is the price of preserving coherence, not institutional rent. The measurement series and theater rise suggest construction, not natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_constructed, conceptual, 'Whether the deferral framework is natural necessity or constructed choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.45).
narrative_ontology:measurement(koda_tr_t500, kodashim_commandment_status__messianic_deferral, theater_ratio, 500, 0.52).
narrative_ontology:measurement(koda_tr_t1000, kodashim_commandment_status__messianic_deferral, theater_ratio, 1000, 0.58).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__messianic_deferral, theater_ratio, 1500, 0.64).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__messianic_deferral, theater_ratio, 2000, 0.62).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(koda_be_t500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 500, 0.48).
narrative_ontology:measurement(koda_be_t1000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1500, 0.62).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 2000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(koda_su_t500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 500, 0.35).
narrative_ontology:measurement(koda_su_t1000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1000, 0.41).
narrative_ontology:measurement(koda_su_t1500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1500, 0.43).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 2000, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.18).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% The kodashim commandment status kernel decomposes into three reading-specific constraint stories with distinct ε values and structural relationships. This story (messianic_deferral, ε ≈ 0.58) treats suspension as temporary and study as obligation-justified by future restoration. The performance_only reading (ε ≈ 0.15, lower extraction because it requires no present-generation obligation) treats suspension as complete until restoration, with study as optional piety. The study_as_performance reading (ε ≈ 0.35, moderate extraction from hermeneutical authority but less opportunity cost) treats present study as fulfillment, dissolving the deferral. All three readings share the kernel (sacrifice law status post-Temple) but diverge radically in their prescriptions for present practitioners and in who benefits from the interpretive framework. The three stories form a constraint family linked by network.affects_constraints; each reading's structural data shows how the same kernel produces different extraction profiles depending on which reading one adopts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
