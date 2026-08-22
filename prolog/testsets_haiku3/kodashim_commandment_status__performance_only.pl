% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Commandment Status: Performance-Only Reading
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The performance-only reading of the kodashim (sacrifice laws) commandment
 *   status claims that the obligation to sacrifice is contingent on Temple
 *   existence; without the altar, the commandment is suspended — a husk
 *   awaiting either permanent obsolescence or restoration. This reading
 *   originated post-70 CE as a halakhic response to the Temple's destruction
 *   but has become institutionalized as the canonical framework across most
 *   diaspora communities and scholarly establishments. The performance-only
 *   reading extracts substantial resources from temple-restoration movements
 *   (which could leverage arguments for readiness and preparation) and from
 *   alternative interpretive frameworks (study-as-performance, which claims
 *   intellectual engagement fulfills the commandment). The constraint's
 *   theater ratio is elevated (0.68 at interval end) because much of the
 *   institutional enforcement machinery defends the reading's primacy rather
 *   than resolving the underlying theological tension — the reading is
 *   sustained theatrically through interpretive repetition and institutional
 *   authority assertion rather than through discovery of new evidence. This
 *   story instantiates ONE reading of a contested kernel; the sibling
 *   readings (messianic-deferral, study-as-performance) are separate
 *   constraints with their own ε values and stakeholder structures.
 *
 * KEY AGENTS:
 *   - interpretive_authorities: maintain and enforce the performance-only reading's canonical status through textual interpretation and institutional authority
 *   - scholarly_institutions: benefit from the indefinite curriculum justification and legitimacy framework the reading provides
 *   - temple_restoration_constituencies: bear the cost; the reading delegitimizes their foundational project and frames restoration as reopening a dead commandment
 *   - alternative_reading_adherents: excluded from institutional authority structures; their interpretations marginalized as heterodox
 *   - observant_laity: benefit from clear framework that avoids perpetual commandment-breach framing; bear cost of foreclosed alternative meanings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.72).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.58).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.72).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, piton).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Commandment Status: Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '19872a73-c1df-4209-ac56-01831ed95f45').
narrative_ontology:cs_kernel_codification('19872a73-c1df-4209-ac56-01831ed95f45', distributed).
narrative_ontology:cs_authority_grounding('19872a73-c1df-4209-ac56-01831ed95f45', lineage).
narrative_ontology:cs_interpretation_layer_present('19872a73-c1df-4209-ac56-01831ed95f45').
narrative_ontology:cs_reading_relation('19872a73-c1df-4209-ac56-01831ed95f45', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_reading_relation('19872a73-c1df-4209-ac56-01831ed95f45', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('19872a73-c1df-4209-ac56-01831ed95f45', foundational, commandment_contingency_on_temple).
narrative_ontology:cs_axiom_status(commandment_contingency_on_temple, holdable).
narrative_ontology:cs_axiom_grounding('19872a73-c1df-4209-ac56-01831ed95f45', commandment_contingency_on_temple, deontological).
narrative_ontology:cs_axiom('19872a73-c1df-4209-ac56-01831ed95f45', foundational, temple_absence_suspends_obligation).
narrative_ontology:cs_axiom_status(temple_absence_suspends_obligation, holdable).
narrative_ontology:cs_axiom_grounding('19872a73-c1df-4209-ac56-01831ed95f45', temple_absence_suspends_obligation, deontological).
narrative_ontology:cs_reference_frame('19872a73-c1df-4209-ac56-01831ed95f45', temple_destruction_permanent_suspension).
narrative_ontology:cs_drift_state('19872a73-c1df-4209-ac56-01831ed95f45', modern_institutional_saturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19872a73-c1df-4209-ac56-01831ed95f45', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, interpretive_authorities).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, scholarly_institutions).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, temple_restoration_constituencies).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, redirectable_scholarship_opportunity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, observant_laity).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, observant_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Halakhic scholars and institutional bodies (chief rabbinates, yeshiva heads, legal committees) that maintain and enforce the performance-only reading as the canonical determination of the commandment's status. They interpret texts, issue rulings, teach the framework in academies, and determine which applications bind the community. They benefit from the reading's canonical authority and the institutional legitimacy it provides. They have exit options: they could change the reading, but doing so would require reinterpreting foundational texts and overriding centuries of precedent — costly but possible.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, interpretive_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Yeshivas, seminaries, research centers, and publishing venues dedicated to talmudic and halakhic scholarship. They derive curriculum justification, publication outlets, and institutional prestige from sustained engagement with sacrifice law study. The performance-only reading — which frames the commandment as suspended but intellectually significant — justifies indefinite scholarly investment in a domain whose practical application is not expected to resume. Careers, tenure lines, and publication venues are organized around this interpretive framework. Exit would require redirecting institutional purpose and dismantling established programs.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, scholarly_institutions, beneficiary,
    institutional, generational, constrained, global).

% Communities, movements, and individuals oriented toward actual Temple restoration and the resumption of sacrificial practice. For them, the performance-only reading is a barrier and delegitimizer: it reframes their restoration project as reopening a dead commandment rather than restoring a suspended one. It diverts scholarly resources from preparation and readiness (messianic-deferral framing) toward pure study of a framework that declares the commandment permanently inert. Their identity is constituted through the commitment to restoration; their self-concept depends on the belief that restoration is a live commandment obligation, not an obsolete husk. Exit would mean abandoning this identity and spiritual project.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, temple_restoration_constituencies, payer,
    moderate, generational, identity_locked, global).

% Scholars and communities who hold the study-as-performance reading (study itself fulfills the commandment) or the messianic-deferral reading (the commandment is suspended but awaiting restoration). These groups are systematically excluded from the institutional authority structures that enforce the performance-only framing. Their interpretations are labeled heterodox, their institutional platforms are restricted, their publications are marginalized in mainstream halakhic discourse. They are constrained because exiting the observant framework entirely is more costly than remaining and accepting marginalization.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, alternative_reading_adherents, excluded,
    moderate, biographical, constrained, global).

% Members of observant communities who accept the interpretive authorities' determination that sacrifice laws are suspended. They benefit from a clear, binding framework: the performance-only reading resolves the dissonance of holding a perpetual commandment with no means of observance. It allows them to be observant Jews without perpetually violating an obligation. They also bear costs: the reading forecloses alternative frameworks that might provide spiritual meaning (study-as-performance) or activist hope (restoration readiness). Their exit is constrained because leaving observance entirely is a high-cost option; remaining in observance while holding an alternative reading is intellectually difficult within the institutional mainstream.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, observant_laity, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, observant_laity, payer).

% Academic historians, comparativists, and theoretical scholars studying the evolution of Jewish law, religious interpretation, and institutional dynamics across the diaspora. They analyze how the performance-only reading emerged as a response to the Temple destruction, how it became institutionally dominant, how alternative readings arose and persist, and how the three readings compete for authority. They observe the constraint without being bound by it — their analytical seat is outside the communities constituted by observance of the commandment.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, historical_continuity_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, interpretive_authorities).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the logical and spiritual tension created by a perpetual commandment and a permanent material impossibility: if the commandment is contingent on Temple existence and the Temple is destroyed, the obligation is suspended rather than eternally violated. Provides a framework that preserves the commandment's meaningfulness (it will reactivate upon restoration) while removing the dissonance of perpetual non-observance.
% TRANSFER_FUNCTION: Transfers interpretive authority to the institutional bodies that maintain the performance-only reading and enforce it as binding. Transfers scholarly attention and institutional resources to the indefinite study of sacrifice laws despite their practical inapplicability. Transfers delegitimacy from restoration movements (whose project is reframed as reopening a dead commandment) to alternative readings (marginalized as heterodox).
% ABSENT_VOICES: Temple-restoration movements and communities holding the messianic-deferral or study-as-performance readings are excluded from the institutional bodies that determine the binding interpretation. They would argue for readings that maintain the commandment as either temporally suspended (deferral) or perpetually fulfilled (study); their voices are systematically marginalized in mainstream halakhic discourse. Communities in traditional Judaism that sustain alternative readings exist but have minimal institutional platform.
% DISAPPEARANCE_RATIONALE: If the performance-only reading and its institutional enforcement disappeared, the interpretive field would restructure dramatically. Temple-restoration constituencies would no longer be marginalized; the messianic-deferral reading would gain legitimacy as a framework for preparation and readiness; the study-as-performance reading would offer an alternative path to fulfillment. Scholarly resources might redirect toward restoration preparation or study-as-practice rather than indefinite pure study of a suspended domain. The laity's spiritual options would expand. The institutional order defending the performance-only reading would collapse.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE created a permanent halakhic crisis: Jews are obligated to sacrificial commandments, but the only permitted venue for sacrifice (the Temple altar) no longer exists. How can the Jewish people be perpetually commanded to an act they cannot perform? The performance-only reading answers: the commandment is contingent on Temple existence. Without the Temple, the obligation is suspended, not perpetually violated.
% FOUNDING_PROBLEM_CORROBORATION: The Temple destruction is historical fact. The halakhic consequences are deeply disputed. Interpretive authorities endorse the performance-only reading as the established determination, claiming it is the correct reading of the texts and precedents. Temple-restoration movements attest that the founding problem remains unresolved: either the commandment is still live (messianic-deferral) and awaits restoration, or it is fulfilled through study (study-as-performance) and requires no physical altar. Historical-critical scholarship confirms the performance-only reading emerged post-70 CE as one response among several and remains contested across different Jewish communities and interpretive traditions. No corroboration exists from outside the disputing parties themselves; the kernel is internally contested.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 at t0 to 0.72 at t26 because institutional investment in the performance-only reading accumulates over time — more yeshivas teach it, more publications defend it, more careers depend on it. Theater ratio is elevated throughout (0.55 to 0.68) because the reading is maintained through interpretive repetition and institutional assertion rather than new evidence or resolved theological tension. Suppression requirement is moderate-to-low (0.48 to 0.58) because the reading operates largely through authority and institutional coherence rather than coercive force — alternative readings persist, temples are not physically prevented from being rebuilt, people are not forbidden to study-as-performance privately. However, suppression IS required to maintain the reading's canonical status against alternatives: institutional review bodies suppress heterodox interpretations, publishing venues restrict heterodox voices, educational curricula standardize the performance-only framing. The measurement series track the reading's institutional entrenchment over the interval; the theater ratio plateau at t20-t26 reflects the reading reaching stable institutional saturation.
 *
 * PERSPECTIVAL GAP:
 *   From the interpretive authorities' seat, the performance-only reading is a genuine halakhic solution: it resolves the logical problem, provides a clear binding framework, and maintains continuity with tradition. From the temple-restoration constituencies' seat, the same structure is extractive: it uses institutional authority to suppress their foundational project, reframe restoration as heterodox, and divert resources from messianic preparation. From the scholarly institutions' seat, the reading is coordinating: it provides institutional identity and curriculum structure. From the alternative-reading adherents' seat, it is suppressive: their interpretations are systematically excluded from mainstream discourse. The engine computes these divergences from the structural data (power, exit_options, directionality atoms) — the authored claim (piton) does not adjudicate across seats. The piton classification emerges from the constellation: the agenda-setter (authorities) could easily change the reading but does not (it benefits them); the payees (restoration constituencies) cannot exit (identity-locked) but cannot fix it (powerless); the theater ratio is high because much enforcement activity defends the reading's primacy rather than resolving the underlying question.
 *
 * DIRECTIONALITY LOGIC:
 *   Interpretive authorities are beneficiaries (d near 0.0-0.2): they set the agenda, maintain canonical authority, and collect institutional legitimacy. Scholarly institutions are beneficiaries (d near 0.2-0.3): they depend on the reading for curriculum justification and institutional purpose. Temple-restoration constituencies are targets (d near 0.8-0.9): they are identity-locked (restoration is their foundational project; exit would dissolve their communities) and powerless relative to institutional authorities. The reading's persistence depends on suppressing their alternative interpretation. Alternative-reading adherents are also targets (d near 0.7-0.85): their readings are marginalized, their institutional platforms restricted. Observant laity sit near symmetric (d near 0.4-0.6): they benefit from a clear binding framework but bear the cost of foreclosed alternative meanings and the delegitimization of restoration hope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction creates a permanent halakhic crisis) is live at t0 but the performance-only reading has effectively declared it dead — the commandment is not perpetually violated, merely suspended. This reading persists not because it solved the founding problem but because institutional authorities maintain it as the canonical framework. Mandatrophy is detected at the intersection: the world-would-rearrange verdict (if the reading disappeared, temple-restoration movements would surge) combined with the theater_ratio plateau at 0.68+ indicates the reading is sustained by interpretive repetition and institutional assertion rather than by discovery or new evidence. The reading does not appear to be in active mandatrophy resolution (no evidence of weakening or systematic revision); instead, it is in stable attrition — held in place by institutional inertia and the cost to authorities of changing it, not by any property of the founding problem itself. The alternative readings (messianic-deferral, study-as-performance) offer resolution paths the performance-only reading forecloses; the engine will detect cross-kernel piton dynamics when all three readings are compiled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of alternative readings primarily structural (institutional gatekeeping, publishing barriers, curriculum control) or internalized (observant Jews have accepted the performance-only reading as inevitable and unthinkable to challenge)?',
    'Post-exit suppression trajectory: interview former members of the institutional structure who have moved to communities holding alternative readings; measure persistence of the performance-only reading''s epistemic authority after institutional barriers are removed. If the reading remains compelling and dominant even outside the institutional structure, suppression is substantially internalized.',
    'If suppression is primarily structural, remedies (institutional pluralism, alternative-reading platforms) could shift the reading''s authority. If substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests — the reading extracts continued intellectual conformity even after institutional pressure is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternative kodashim readings is institutional or internalized.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Is the performance-only reading logically incompatible with messianic-deferral and study-as-performance, or do the three readings coexist as live interpretive options held by different parties?',
    'Textual and theological analysis: do the three readings offer contradictory claims about the ontological status of the commandment (one claims it is perpetually dead, one claims it is temporally suspended but live, one claims it is perpetually fulfilled through study)? Or do they represent different pragmatic stances on the same contested kernel?',
    'If the readings logically foreclose each other, only one can be true within any single framework, and institutional enforcement of the performance-only reading represents truth-seeking. If they coexist, the reading is an interpretive choice rather than a logical necessity, and the theater_ratio elevation reflects maintenance of one choice against alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether performance-only forecloses sibling readings or coexists with them.').

omega_variable(
    restoration_movement_power_dynamics,
    'If temple-restoration movements had equal institutional resources and authority as the performance-only reading defenders, would the messianic-deferral or restoration-focused reading dominate, or would all readings coexist?',
    'Historical analysis of periods when restoration movements had greater influence (early Second Temple period, Bar Kokhba era, medieval redemption movements); measure the prevalence of alternative readings in those periods. Counterfactual: model resource allocation if contemporary funding and institutional support were equally distributed.',
    'If restoration-focused readings would dominate with equal resources, the performance-only reading is maintained through power asymmetry (institutional capture). If alternatives would coexist, the reading reflects genuine interpretive equilibrium. This affects whether the constraint is better classified as snare (dominant through power) or tangled-rope (coordination that happens to favor one reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_movement_power_dynamics, empirical, 'Whether performance-only dominance reflects interpretive equilibrium or power asymmetry.').

omega_variable(
    kernel_reading_count_and_boundary,
    'Are there more than three readings of this kernel, or are performance-only, messianic-deferral, and study-as-performance the exhaustive logical alternatives?',
    'Comprehensive review of halakhic literature across traditions (Ashkenazi, Sephardi, Karaite, Mystical, Hasidic, Contemporary liberal Judaism); identify all distinct claims about the commandment''s status and grouping into maximal coherent families.',
    'If three readings are exhaustive and mutually exclusive, the kernel is well-bounded and the engine''s pairwise reading_relations analysis is complete. If more readings exist or they overlap, the kernel is under-decomposed and needs refactoring into finer-grained constraint stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_count_and_boundary, conceptual, 'Whether the kodashim kernel has been fully decomposed into distinct readings.').

omega_variable(
    study_fulfillment_empirical_claim,
    'The study-as-performance reading claims intellectual engagement fulfills the commandment. Is this a factual claim about commandment semantics, a pragmatic concession, or a normative reframing?',
    'Textual analysis of study-as-performance authorities: do they argue (a) the commandment''s original meaning includes intellectual study; (b) study substitutes practically for physical sacrifice and thus fulfills it; or (c) we should reframe what fulfillment means to include study? Different groundings produce different foreclosure relationships.',
    'If (a), study-as-performance and performance-only contradict on empirical grounds (what the commandment means). If (b), they differ on practical equivalence. If (c), they differ on normative reframing. The relation type (forecloses vs. coexists_with) depends on which grounding is authoritative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_fulfillment_empirical_claim, conceptual, 'The semantic/normative status of study-as-performance fulfillment claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.55).
narrative_ontology:measurement(koda_tr_t4, kodashim_commandment_status__performance_only, theater_ratio, 4, 0.59).
narrative_ontology:measurement(koda_tr_t8, kodashim_commandment_status__performance_only, theater_ratio, 8, 0.62).
narrative_ontology:measurement(koda_tr_t12, kodashim_commandment_status__performance_only, theater_ratio, 12, 0.65).
narrative_ontology:measurement(koda_tr_t16, kodashim_commandment_status__performance_only, theater_ratio, 16, 0.67).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__performance_only, theater_ratio, 20, 0.68).
narrative_ontology:measurement(koda_tr_t26, kodashim_commandment_status__performance_only, theater_ratio, 26, 0.68).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(koda_be_t4, kodashim_commandment_status__performance_only, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(koda_be_t8, kodashim_commandment_status__performance_only, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(koda_be_t12, kodashim_commandment_status__performance_only, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(koda_be_t16, kodashim_commandment_status__performance_only, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__performance_only, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(koda_be_t26, kodashim_commandment_status__performance_only, base_extractiveness, 26, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__performance_only, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(koda_su_t4, kodashim_commandment_status__performance_only, suppression_requirement, 4, 0.51).
narrative_ontology:measurement(koda_su_t8, kodashim_commandment_status__performance_only, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(koda_su_t12, kodashim_commandment_status__performance_only, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(koda_su_t16, kodashim_commandment_status__performance_only, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__performance_only, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(koda_su_t26, kodashim_commandment_status__performance_only, suppression_requirement, 26, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__performance_only, 0.1).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% The kodashim_commandment_status kernel decomposes into three structurally distinct constraints corresponding to three halakhic readings. The performance-only reading (this constraint) claims the sacrifice commandment is suspended without Temple; messianic-deferral claims it is temporally suspended but maintains readiness for future restoration; study-as-performance claims intellectual study fulfills the commandment. Each reading instantiates a different constraint with distinct beneficiary/victim structures and ε values. The performance-only reading produces high extractiveness (0.72) because institutional investment in the reading accumulates without resolving the founding theological problem; the sibling readings offer alternative solutions the performance-only framework forecloses. All three are linked via network.affects_constraints; the engine will detect cross-kernel piton dynamics when compiled together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
