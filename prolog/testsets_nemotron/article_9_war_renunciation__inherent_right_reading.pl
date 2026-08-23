% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 Inherent Right Reading — Minimum Necessary Self-Defense Threshold
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   Article 9 of Japan's Constitution renounces war and the maintenance of
 *   war potential. The 'inherent right reading' (固有の自衛権解釈) holds that
 *   sovereign states retain an inherent right to individual self-defense
 *   under international law, and Article 9 only renounces 'war' as aggressive
 *   action — not the minimum necessary capacity for territorial defense. This
 *   reading, adopted by the Japanese government in 1954 and sustained through
 *   cabinet legislation, enables the Self-Defense Forces (SDF) as a
 *   constitutionally legitimate organization operating within a 'minimum
 *   necessary' threshold. It functions as a tangled rope: it coordinates
 *   genuine security needs (deterrence, alliance credibility, disaster
 *   response) while extracting legitimacy and burden from strict pacifist
 *   constituencies and base-hosting communities. The coordination function is
 *   real (no amendment needed, alliance works), but the extraction is
 *   asymmetric — pacifists lose textual fidelity, Okinawans bear
 *   externalities, textualists lose professional standing — and active
 *   enforcement (cabinet legislation, judicial avoidance, police power
 *   against protesters) sustains it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.42).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.38).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 Inherent Right Reading — Minimum Necessary Self-Defense Threshold").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, 'b69501bb-4cc0-4e12-b4c3-db6091352952').
narrative_ontology:cs_kernel_codification('b69501bb-4cc0-4e12-b4c3-db6091352952', fixed_text).
narrative_ontology:cs_authority_grounding('b69501bb-4cc0-4e12-b4c3-db6091352952', extraction).
narrative_ontology:cs_interpretation_layer_present('b69501bb-4cc0-4e12-b4c3-db6091352952').
narrative_ontology:cs_reading_relation('b69501bb-4cc0-4e12-b4c3-db6091352952', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b69501bb-4cc0-4e12-b4c3-db6091352952', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('b69501bb-4cc0-4e12-b4c3-db6091352952', foundational, sovereign_inherent_right_to_self_defense).
narrative_ontology:cs_axiom_status(sovereign_inherent_right_to_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('b69501bb-4cc0-4e12-b4c3-db6091352952', sovereign_inherent_right_to_self_defense, deontological).
narrative_ontology:cs_axiom('b69501bb-4cc0-4e12-b4c3-db6091352952', foundational, article_9_renounces_aggressive_war_only).
narrative_ontology:cs_axiom_status(article_9_renounces_aggressive_war_only, holdable).
narrative_ontology:cs_axiom_grounding('b69501bb-4cc0-4e12-b4c3-db6091352952', article_9_renounces_aggressive_war_only, conventional).
narrative_ontology:cs_axiom('b69501bb-4cc0-4e12-b4c3-db6091352952', secondary, minimum_necessary_defensive_capacity_threshold).
narrative_ontology:cs_axiom_status(minimum_necessary_defensive_capacity_threshold, holdable).
narrative_ontology:cs_axiom_grounding('b69501bb-4cc0-4e12-b4c3-db6091352952', minimum_necessary_defensive_capacity_threshold, instrumental).
narrative_ontology:cs_reference_frame('b69501bb-4cc0-4e12-b4c3-db6091352952', postwar_constitutional_pacifism).
narrative_ontology:cs_drift_state('b69501bb-4cc0-4e12-b4c3-db6091352952', contemporary_security_environment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b69501bb-4cc0-4e12-b4c3-db6091352952', '2026-08-22T14:30:00Z').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japan_self_defense_forces).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, us_japan_alliance_architects).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, constitutional_revisionists).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, strict_pacifist_constituency).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, okinawa_base_communities).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, constitutional_textualists).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, proportionality_doctrine_in_constitutional_interpretation).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, sovereign_equality_under_international_law).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, minimum_necessary_defensive_capacity_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and operates the Self-Defense Forces under the inherent right interpretation. The SDF's institutional identity, budget, and legal mandate are constituted by this reading — exit would mean dissolving the organization or accepting a radical reinterpretation that denies its constitutional legitimacy. Benefits from organizational recognition and resource allocation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japan_self_defense_forces, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, japan_self_defense_forces, beneficiary).

% US defense planners and Japanese foreign policy elites who designed and maintain the alliance structure. The inherent right reading legitimizes SDF capabilities that enable alliance interoperability, collective exercises, and burden-sharing. They have exit options through alternative alliance frameworks but gain strategically from this reading's validation of Japanese military capacity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, us_japan_alliance_architects, beneficiary,
    institutional, generational, arbitrage, global).

% Political actors (LDP revisionist wing, Nippon Kaigi, etc.) who seek explicit constitutional amendment but accept the inherent right reading as a functional substitute. They benefit from the SDF's legitimacy and operational scope without needing formal amendment. Exit is politically mobile — they can push for revision or accept status quo.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_revisionists, beneficiary,
    organized, biographical, mobile, national).

% Citizens, legal scholars (Article 9 Association), and opposition parties who read Article 9 as a categorical prohibition. They bear the democratic cost of a constitutional order that operates contrary to the text they regard as binding — their constitutional commitment is overridden by interpretive practice. Exit is trapped: emigration is the only full exit; domestic political contestation has failed for decades.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, strict_pacifist_constituency, payer,
    organized, biographical, trapped, national).

% Okinawan residents who disproportionately host US bases and SDF facilities enabled by the inherent right reading. They bear noise, environmental, accident, and crime externalities from the military footprint this reading legitimizes. Exit is constrained — internal migration possible but economically and culturally costly; political voice diluted by national security imperatives.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, okinawa_base_communities, payer,
    powerless, generational, constrained, local).

% Legal scholars and jurists who maintain that 'never be maintained' (戦力は、これを保持しない) categorically prohibits armed forces. They bear professional and intellectual costs: their interpretive position is systematically excluded from official constitutional practice despite textual fidelity. Identity-locked — their professional identity is constituted by textual fidelity; abandoning it would dissolve their epistemic stance.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_textualists, payer,
    moderate, biographical, identity_locked, national).

% Japan's highest court, which has consistently avoided ruling on SDF constitutionality (political question doctrine / judicial restraint). Its silence functions as tacit validation of the inherent right reading. As an analytical seat, it sees the full structure but does not collect or pay.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, supreme_court_grand_bench, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional threshold that permits Japan to maintain defensive military capacity (SDF) while formally renouncing aggressive war, enabling alliance interoperability with the US and deterrence against regional threats without formal constitutional amendment.
% TRANSFER_FUNCTION: Moves constitutional legitimacy and resource allocation from strict pacifist constituencies (who lose textual fidelity) and Okinawan communities (who bear base externalities) to the SDF institution and alliance architects (who gain operational legitimacy, budget, and strategic capability). The transfer is legitimacy and burden, not direct money.
% ABSENT_VOICES: Okinawan base communities are structurally excluded from national security decision-making despite bearing disproportionate costs. Would-be constitutional amendment opponents are excluded because the inherent right reading operates as a substitute for amendment — the constitutional moment never occurs. Future generations who would inherit a normalized militarized posture are temporally excluded.
% DISAPPEARANCE_RATIONALE: If the inherent right reading vanished overnight, the SDF would lose its primary constitutional legitimation. Either a strict pacifist reading would force SDF dissolution (massive security rearrangement), a collective self-defense reading would expand missions (different rearrangement), or formal amendment would be forced (constitutional crisis). The current security architecture of Northeast Asia would reorganize.
% FOUNDING_PROBLEM: Post-WWII Japan needed a constitutional framework that renounced aggressive war (satisfying Allied occupation demands and domestic pacifist sentiment) while preserving the state's survival capacity in a hostile Cold War environment. The inherent right reading emerged as the interpretive bridge: renounce 'war' as aggressive action, retain 'minimum necessary' self-defense.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Allied occupation records (GHQ drafts, Japanese government responses), Yoshida Shigeru's statements, and contemporary constitutional scholars (Inoue Kyoshi, Sato Tatsuo). However, the status is contested: revisionists argue the Cold War threat environment persists (China, North Korea); pacifists argue the founding problem was occupation-imposed and the renunciation was genuine; alliance architects say the problem evolved into alliance maintenance.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).
:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the gap between 'minimum necessary' as a constraining principle and the SDF's actual capabilities (power projection, collective exercises, budget growth). The constraint extracts constitutional fidelity from pacifists and physical burden from Okinawans. Suppression (0.38) is moderate: the constraint persists through interpretive monopoly (cabinet legislation), judicial avoidance (political question doctrine), and policing of base opposition — not total censorship. Theater (0.31) is significant: 'exclusively defense-oriented' (専守防衛) and 'minimum necessary' are performative thresholds that expand over time (collective self-defense 2014, strike capabilities 2022). Accessibility collapse (0.58) is partial: the strict pacifist reading remains publicly available and politically represented, but institutional channels block its implementation. Resistance (0.47) is sustained: Article 9 Association, Okinawan protests, opposition parties, and scholarly critique persist but cannot dislodge the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the SDF/alliance seat, the constraint is a genuine coordination mechanism: it solves the post-occupation security dilemma without amendment. From the pacifist/textualist seat, it is a snare: the text says 'never be maintained' but the state maintains forces anyway, enforced by interpretive monopoly. From Okinawa, it is extraction with suppressed exit. The engine computes these per-seat types from the structural data — the claimed type (tangled_rope) acknowledges the coordination function while the metrics capture the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   SDF and alliance architects are structural beneficiaries (d ~0.15-0.25): they collect legitimacy, budget, and strategic capability. Constitutional revisionists are incidental beneficiaries (d ~0.3): they gain functional substitute for amendment. Strict pacifist constituency and constitutional textualists are targets (d ~0.75-0.85): their constitutional commitment is overridden, their interpretive position excluded from official practice. Okinawans are targets (d ~0.8): they bear externalities with constrained exit. Identity-locked exit for SDF (institutional identity constituted by reading) and textualists (professional identity = textual fidelity) amplifies their directionality. The Supreme Court sits at analytical (d=0.5): it sees the structure but neither collects nor pays.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Cold War survival without amendment) has mutated: the Cold War ended, but the reading expanded to cover new threats (North Korea missiles, China rise) and new functions (collective self-defense, overseas deployment). The original coordination problem is contested — revisionists say it persists; pacifists say it was occupation-imposed and the renunciation was genuine. Mandatrophy is unresolved: the reading persists because it serves current alliance architecture, not because the 1947 problem remains unchanged. The 'minimum necessary' threshold has become a ratchet — each expansion redefines the minimum.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_necessary_boundary_indeterminacy,
    'Does ''minimum necessary for self-defense'' (必要最小限度) have a stable semantic boundary, or is it an inherently expansive concept that ratchets upward with threat perception and capability development?',
    'Historical analysis of SDF capability expansions (1954 NPR, 1972 Okinawa reversion, 1991 PKO law, 2014 collective self-defense, 2022 counterstrike capability) correlated with official ''minimum necessary'' justifications. If each expansion redefines the minimum post-hoc, the concept is performative.',
    'If ''minimum necessary'' is inherently expansive, the coordination function is a moving target and the constraint trends toward snare (extraction masked by a threshold that always expands to cover current capabilities). If it has a stable core, the tangled rope classification holds — genuine coordination with bounded extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_necessary_boundary_indeterminacy, conceptual, 'Whether the ''minimum necessary'' threshold is a genuine constraint or a ratcheting cover story.').

omega_variable(
    pacifist_exclusion_mechanism,
    'Is the strict pacifist constituency''s exclusion from constitutional practice structural (institutional design: cabinet legislation monopoly, judicial avoidance) or contingent (political majority could change it)?',
    'Counterfactual: if a pacifist-majority government took power, could it implement strict pacifist reading through cabinet legislation alone, or would it face structural vetoes (US pressure, SDF institutional resistance, Supreme Court)?',
    'If structural, the constraint is a snare for pacifists — their exit is blocked by design. If contingent, it is a tangled rope where coordination currently favors one side but the mechanism is not rigged. Affects suppression scoring and directionality for pacifist seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pacifist_exclusion_mechanism, empirical, 'Whether pacifist exclusion is a feature of the constraint''s architecture or a reversible political outcome.').

omega_variable(
    okinawan_burden_as_extraction_vs_coordination_cost,
    'Are Okinawan base externalities a necessary cost of the coordination function (deterrence requires bases somewhere), or are they extractive burden-shifting enabled by the constraint''s national-security framing?',
    'Comparative analysis: do other prefectures host comparable SDF/US base burdens proportionally? If burden is concentrated in Okinawa without strategic necessity, it is extraction. If distributed by strategic logic, it is coordination cost.',
    'If extractive burden-shifting, the constraint has a clearer snare dimension for the Okinawan seat. If coordination cost, the tangled rope holds with victims bearing distributed but necessary costs. Affects victim classification and extraction allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(okinawan_burden_as_extraction_vs_coordination_cost, empirical, 'Whether Okinawan externalities are distributed coordination cost or targeted extraction.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the article_9_war_renunciation kernel best framed as (a) a constitutional text with competing interpretations, or (b) a settled commitment system where the inherent right reading is the operating kernel and the others are dissent positions?',
    'CS-structure analysis: does the Japanese state treat the inherent right reading as the authoritative kernel (authority_grounding: extraction/lineage) with an interpretation layer (cabinet legislation), or as one reading among equals (authority_grounding: distributed)?',
    'If (b), the kernel itself is extractive — the commitment system''s authority derives from preventing revision. This would reframe the sibling readings as resistance to the kernel rather than alternative readings of it. Changes CS classification and network position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel framing itself prejudices the constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_9_inherent_right_tr_t1947, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(article_9_inherent_right_tr_t1954, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(article_9_inherent_right_tr_t1960, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(article_9_inherent_right_tr_t1972, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1972, 0.22).
narrative_ontology:measurement(article_9_inherent_right_tr_t1991, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1991, 0.25).
narrative_ontology:measurement(article_9_inherent_right_tr_t2001, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(article_9_inherent_right_tr_t2014, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2014, 0.3).
narrative_ontology:measurement(article_9_inherent_right_tr_t2022, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2022, 0.31).

% Extraction over time
narrative_ontology:measurement(article_9_inherent_right_be_t1947, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement(article_9_inherent_right_be_t1954, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1954, 0.28).
narrative_ontology:measurement(article_9_inherent_right_be_t1960, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1960, 0.32).
narrative_ontology:measurement(article_9_inherent_right_be_t1972, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1972, 0.35).
narrative_ontology:measurement(article_9_inherent_right_be_t1991, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1991, 0.38).
narrative_ontology:measurement(article_9_inherent_right_be_t2001, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(article_9_inherent_right_be_t2014, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2014, 0.41).
narrative_ontology:measurement(article_9_inherent_right_be_t2022, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2022, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(article_9_inherent_right_su_t1947, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1947, 0.1).
narrative_ontology:measurement(article_9_inherent_right_su_t1954, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1954, 0.25).
narrative_ontology:measurement(article_9_inherent_right_su_t1960, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(article_9_inherent_right_su_t1972, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1972, 0.32).
narrative_ontology:measurement(article_9_inherent_right_su_t1991, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1991, 0.34).
narrative_ontology:measurement(article_9_inherent_right_su_t2001, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2001, 0.36).
narrative_ontology:measurement(article_9_inherent_right_su_t2014, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2014, 0.37).
narrative_ontology:measurement(article_9_inherent_right_su_t2022, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2022, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__inherent_right_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, us_japan_security_treaty_architecture).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, japan_sdf_legal_basis).

% DUAL FORMULATION NOTE:
% This constraint is one member of the article_9_war_renunciation constraint family (kernel). The strict_pacifist_reading and collective_self_defense_reading are sibling constraints with different ε, beneficiaries, victims, and claimed types. The inherent right reading is the operating interpretation (de facto kernel) since 1954; the collective self-defense reading (2014) extends it; the strict pacifist reading is the textualist counter-reading. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, institutional, 0.2).
constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, organized, 0.8).
constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, powerless, 0.85).
constraint_indexing:directionality_override(article_9_war_renunciation__inherent_right_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
