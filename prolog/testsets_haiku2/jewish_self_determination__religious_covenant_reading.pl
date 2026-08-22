% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Divine Covenant as Religious Obligation to Territorial Sovereignty
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested
 *   'jewish_self_determination' kernel. The religious_covenant_reading holds
 *   that the Jewish claim to the land derives from divine covenant, making
 *   territorial sovereignty a religious obligation independent of secular
 *   political frameworks. Within this reading's internal logic, the
 *   constraint exhibits properties of a natural law (divine command is
 *   immutable, unconditional) — ε approaches zero inside the framework.
 *   However, because the framework itself is contested (four sibling readings
 *   hold different interpretations of the same kernel), and because
 *   institutional enforcement depends on active suppression of alternative
 *   frameworks and territorial negotiation possibilities, the effective
 *   measured extractiveness rises substantially (0.82 at interval end). The
 *   claim/metric gap is intentional: the reading claims MOUNTAIN (divine
 *   covenant as natural immutable law), while the metrics describe a
 *   constraint that requires active institutional enforcement and generates
 *   resistance. The engine measures this divergence to detect false summits —
 *   claims of natural law that operate as enforced extraction.
 *
 * KEY AGENTS:
 *   - religious_zionist_movement: Agenda-setter holding the covenant reading; identity-locked to the framework; beneficiary because the reading forecloses compromise.
 *   - settlement_enterprise: Institutional beneficiary operating within the covenant reading's theological justification; receives state support and territorial expansion authorization.
 *   - israeli_state_apparatus: Enforces the reading through law, settlement policy, military doctrine; constrained exit because coalition dependence on religious parties.
 *   - orthodox_rabbinical_authority: Controls interpretive authority; transmits reading through education and halakhic tradition; identity-locked.
 *   - palestinian_territorial_claimants: Payer; trapped; territorial loss and negotiation foreclosure are the operative costs.
 *   - secular_territorial_negotiation_framework: Non-agent; the constraint's primary victim; frameworks like two-state solutions are rendered theologically illegitimate.
 *   - secular_jewish_diaspora: Excluded; would argue alternative Jewish self-determination frameworks; marginalized in Orthodox institutional authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.82).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.71).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Divine Covenant as Religious Obligation to Territorial Sovereignty").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '19bd40fa-d439-4dc7-abe1-926e39335c81').
narrative_ontology:cs_kernel_codification('19bd40fa-d439-4dc7-abe1-926e39335c81', fixed_text).
narrative_ontology:cs_authority_grounding('19bd40fa-d439-4dc7-abe1-926e39335c81', lineage).
narrative_ontology:cs_interpretation_layer_present('19bd40fa-d439-4dc7-abe1-926e39335c81').
narrative_ontology:cs_reading_relation('19bd40fa-d439-4dc7-abe1-926e39335c81', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_reading_relation('19bd40fa-d439-4dc7-abe1-926e39335c81', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('19bd40fa-d439-4dc7-abe1-926e39335c81', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('19bd40fa-d439-4dc7-abe1-926e39335c81', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('19bd40fa-d439-4dc7-abe1-926e39335c81', foundational, divine_covenant_immutable).
narrative_ontology:cs_axiom_status(divine_covenant_immutable, holdable).
narrative_ontology:cs_axiom_grounding('19bd40fa-d439-4dc7-abe1-926e39335c81', divine_covenant_immutable, theological).
narrative_ontology:cs_axiom('19bd40fa-d439-4dc7-abe1-926e39335c81', foundational, territorial_sovereignty_religious_obligation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_religious_obligation, holdable).
narrative_ontology:cs_axiom_grounding('19bd40fa-d439-4dc7-abe1-926e39335c81', territorial_sovereignty_religious_obligation, deontological).
narrative_ontology:cs_reference_frame('19bd40fa-d439-4dc7-abe1-926e39335c81', divine_covenant_framework).
narrative_ontology:cs_drift_state('19bd40fa-d439-4dc7-abe1-926e39335c81', contemporary_post_secular_state_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('19bd40fa-d439-4dc7-abe1-926e39335c81', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_territorial_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds that divine covenant grants Jewish people inalienable right to the land; frames territorial sovereignty as religious obligation, not political negotiation. Sets the interpretive framework through rabbinical authority, educational institutions, and state-aligned institutions. Beneficiaries from the constraint because it forecloses compromise on territorial boundaries — the reading renders negotiation theologically illegitimate. Identity-locked: for adherents, the religious covenant is constitutive of Jewish self-understanding.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, beneficiary).

% Operates within the covenant reading's framework: territorial expansion is divine obligation, not negotiable concession. Receives institutional, legal, and security support from the state apparatus. The constraint's religious framing insulates settlement expansion from international legal objections grounded in territorial compromise frameworks. Constrained exit: institutional dependence on state support and theological justification; abandoning settlements requires rejecting the covenant reading itself.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, settlement_enterprise, agenda_setter).

% The non-agent entity: frameworks grounded in international law, mutual recognition, and territorial compromise (UN resolutions, two-state solutions, land-for-peace models). The constraint FORECLOSES these frameworks by rendering them theologically illegitimate within the reading's logic. The secular framework 'bears costs' in the sense that it is rendered operationally impossible when the covenant reading dominates institutional decision-making.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework, payer,
    powerful, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework).

% Implements and enforces the covenant reading through law, settlement policy, military doctrine, and institutional support for religious Zionist institutions. The state's enforcement machinery is what operationalizes the reading from theological claim into territorial policy. Constrained exit: the state apparatus is structurally dependent on coalition support from religious parties that hold the covenant reading; rejecting the reading risks losing political authority.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, regional).

% Interprets and transmits the covenant reading through halakhic (Jewish legal) tradition and religious instruction. Controls the interpretive frame through control of religious education and institutional religious authority. Identity-locked: the reading is the foundation of Orthodox institutional identity and theological self-understanding.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, orthodox_rabbinical_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Bear the constraint in the form of territorial loss, settlement expansion, and foreclosure of negotiation on land restitution. The covenant reading makes their territorial claims illegitimate within the reading's theological logic (the land is divinely allocated to Jews, rendering Palestinian claims superseded). Trapped: their ability to negotiate depends on frameworks the covenant reading forecloses.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_territorial_claimants, payer,
    powerless, biographical, trapped, regional).

% Observes the conflict and applies frameworks (international humanitarian law, self-determination doctrine, territorial dispute resolution) that the covenant reading implicitly rejects as irrelevant. The observer position is asymmetric: the religious reading does not acknowledge the secular framework's legitimacy as arbiter, while the secular framework treats the religious reading as a contestable claim rather than a binding determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_secular_legal_framework, observer,
    institutional, biographical, analytical, universal).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__religious_covenant_reading, international_secular_legal_framework).

% Would argue (if seated) that Jewish survival is best secured through minority rights, pluralism, and diaspora institutions rather than territorial sovereignty; that the covenant reading conflates ethnic nationalism with religious obligation; that secular frameworks for Jewish self-determination exist and deserve equal standing. Excluded: their voices are marginalized in institutional religious authority structures dominated by the covenant reading, and their alternative frameworks are rendered theologically suspect within the reading's logic.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_jewish_diaspora, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified interpretive frame grounding Jewish collective identity in an immutable territorial claim, resolving the coordination problem of how dispersed Jewish communities maintain continuity across diaspora, displacement, and historical fragmentation. The covenant reading offers a theological answer: all Jews are bound by the same divine obligation to the land, creating a unified telos regardless of physical location.
% TRANSFER_FUNCTION: Moves legitimacy from secular territorial negotiation frameworks to religious obligation frameworks. Transfers decision-making authority from internationally-grounded compromise models to religiously-grounded interpretation of divine will. The constraint channels institutional resources (state support, settlement funding, military doctrine) toward fulfilling the divine obligation, away from accommodation frameworks.
% ABSENT_VOICES: Secular Jewish diaspora (diasporist and liberal nationalist voices are largely excluded from Orthodox institutional authority structures and rabbinical interpretation). Palestinian territorial claimants (their voices are structurally positioned as illegitimate within the reading's theological framework — the land is allocated, their claims are rendered superseded). International secular legal frameworks (observers rather than participants in the interpretive process).
% DISAPPEARANCE_RATIONALE: Adherents to the reading argue: if this constraint disappeared, Jewish claim to the land would collapse, territorial expansion would lose theological justification, and the Jewish state would lose its foundational narrative — a civilizational catastrophe. Critics argue: if the constraint disappeared, territorial negotiation would become possible, and both Jewish and Palestinian self-determination frameworks could operate without the constraint foreclosing compromise. The stakes differ fundamentally by seat.
% FOUNDING_PROBLEM: Maintained Jewish collective identity across diaspora, expulsion, and historical discontinuity. How does a dispersed people without territorial control maintain itself as a coherent entity? The covenant reading provides an answer: the land is permanently allocated by divine covenant, making all Jews stakeholders in a single territory regardless of current location.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox rabbinical tradition and religious Zionist institutions attest the problem is live and the reading solves it. Secular Jewish historians and diaspora communities attest that Jewish identity and continuity have been maintained through non-territorial mechanisms (religious law, communal institutions, cultural memory) and that the founding problem is substantially solved by those mechanisms without requiring territorial return. International scholars of Jewish history (outside the benefiting parties) document both mechanisms for identity continuity and the reading's emergence as a 19th-century innovation (not ancient doctrine) — see works on Zionism's historical origins by Shlomo Avineri, David Myers, and mainstream academic Jewish historiography.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metric trajectory shows extraction rising from 0.38 to 0.82 over the interval, indicating that the constraint's operation has shifted from theological doctrine to enforced institutional policy with real territorial consequences. Theater ratio rises from 0.18 to 0.44, suggesting that performative religious legitimation has grown relative to theological substance — the constraint operates increasingly as justification for territorial policy rather than as pure theological claim. Suppression requirement rises from 0.32 to 0.71, indicating that maintaining the reading's dominance has required growing institutional suppression of alternative frameworks: control of Palestinian movement, delegitimization of secular negotiation proposals, marginalization of diaspora voices. Accessibility_collapse is high (0.78) because once the reading is adopted as institutional framework, alternatives become cognitively inaccessible to adherents — the theological framework is totalizing. Resistance is substantial (0.68) because the reading meets real opposition from Palestinian territorial claims, international secular legal frameworks, and excluded secular Jewish voices.
 *
 * PERSPECTIVAL GAP:
 *   From the religious Zionist seat, the constraint is MOUNTAIN — immutable divine command, not a negotiable policy choice. The measurement of suppression appears low (suppression is just 'defense of truth against falsehood'). From the secular negotiation and Palestinian seats, the constraint operates as TANGLED_ROPE or SNARE — extraction of territorial authority combined with active suppression of alternatives, enforced by state machinery. The engine computes per-seat classifications from the structural data and should produce this divergence: beneficiaries at the mountain/rope end, targets at the snare end. This divergence is the signal the framework exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist movement and settlement enterprise sit at the beneficiary end of the directionality spectrum (d ≈ 0.15–0.25): they benefit from the constraint, hold interpretive authority, control institutional machinery, and have identity-locked commitment that makes exit structurally unthinkable. Palestinian territorial claimants sit at the target end (d ≈ 0.85–0.95): they bear the costs (territorial loss, negotiation foreclosure, suppression of alternative frameworks), have trapped/constrained exit, and operate at the powerless/moderate power level. The state apparatus sits in the middle (d ≈ 0.45–0.55): it benefits from the religious justification for territorial policy and settlement expansion, but is constrained by coalition dependence and international legal objections — it is both beneficiary and enforcer, with partial target characteristics (international pressure). The secular negotiation framework (non-agent) is the primary victim — not individuals but the framework itself is what the constraint forecloses.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintain Jewish collective identity and claim to the land) was live at founding (addressing diaspora fragmentation) but is contested now: secular mechanisms for Jewish identity continuity are demonstrably functional (diaspora institutions, religious law divorced from territorial claim, cultural memory practices). The measurement trajectory shows extraction accumulating (base_extractiveness rising 0.38→0.82) while the founding mandate weakens (status=contested in six_questions). This mandatrophy pattern — rising extraction amid mandate decay — is a canonical piton signature. However, the constraint is not yet purely piton: the constraint still claims to solve an identity coordination problem (unified Jewish telos), and it does coordinate religious institutional behavior. The theater ratio of 0.44 indicates substantial but not dominant performativity. The classification should compute as tangled_rope at most seats, with piton characteristics emergent (theater ratio rising, mandate contested, extraction rising without corresponding expansion of coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_immutability_vs_historical_construction,
    'Is the covenant reading a revelation of immutable divine will, or a historical construction that emerged in 19th-century Zionist ideology?',
    'Textual and historical analysis: scholarly consensus on the reading''s emergence date and continuity with pre-Zionist Jewish theology; attestation from Orthodox tradition authorities on whether they hold the reading as ancient doctrine or modern interpretation.',
    'If immutable (divine will), the constraint is genuinely MOUNTAIN — natural law, not constructed extraction. If historical construction (19th-century innovation), the reading is a false summit: a constructed constraint with identifiable beneficiaries (religious Zionist institutions, settlement enterprise) that invokes naturalness to resist scrutiny. This is the central oracle question for false-summit detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_immutability_vs_historical_construction, empirical, 'Whether the divine covenant reading is ancient theological doctrine or modern historical construction').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression (0.71) primarily structural (military control, legal prohibition, territorial barriers to Palestinian movement) or internalized (religious identity fusion that makes exit unthinkable, ideological commitment to the reading)?',
    'Post-exit trajectory analysis: if suppression persists after structural barriers are removed (e.g., secular Jews in diaspora who have left the framework), the suppression is partially internalized. If suppression declines when structural barriers are removed, suppression is primarily structural.',
    'If primarily structural, removing the constraint is administratively possible. If substantially internalized (identity-locked adherents carrying the constraint psychologically regardless of institutional barriers), the constraint''s effective suppression is higher than the structural measure — institutional removal would not free adherents from their own internalized commitment. Affects recommendations for constraint modification or removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether suppression is structural (external barriers) or internalized (psychological fusion with identity)').

omega_variable(
    foreclosure_vs_coexistence_with_secular_frameworks,
    'Does the religious covenant reading logically foreclose secular frameworks for territorial negotiation (true foreclosure), or do the two frameworks merely compete for institutional authority while both remaining logically possible (coexistence)?',
    'Formal logical analysis: can both ''divine covenant grants immutable territorial right'' AND ''secular negotiation framework for land allocation'' be held true simultaneously within a coherent belief system? Theological analysis from adherents: do they hold the reading as logically incompatible with secular frameworks, or merely as the preferred truth?',
    'True foreclosure: the reading and secular frameworks cannot coexist; institutional adoption of the reading eliminates logical space for compromise. Mere competition: both frameworks remain logically possible; institutional adoption of the reading is a choice (not a logical necessity), and compromise remains theoretically possible if the reading is deprioritized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_with_secular_frameworks, conceptual, 'Whether the covenant reading logically forecloses secular territorial frameworks or merely competes with them').

omega_variable(
    framework_identity_capture,
    'Is the reading''s dominance in Orthodox institutional authority a result of genuine theological consensus, or of institutional capture by religious Zionist factions?',
    'Historical analysis of institutional control: who holds positions of rabbinical authority, who funds institutions, what are the governance structures? Do competing theological interpretations have equal institutional voice, or are they marginalized? Testimonial accounts from alternative voices within Orthodoxy.',
    'If genuine consensus, the reading reflects the tradition''s authentic theological self-understanding. If institutional capture, the reading''s dominance is a power effect rather than theological truth — the constraint operates as extracted from secular frameworks and alternative Orthodox voices through institutional control mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_identity_capture, empirical, 'Whether the reading''s institutional dominance reflects theological consensus or institutional capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(jewi_tr_t8, jewish_self_determination__religious_covenant_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(jewi_tr_t16, jewish_self_determination__religious_covenant_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(jewi_tr_t24, jewish_self_determination__religious_covenant_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(jewi_tr_t32, jewish_self_determination__religious_covenant_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__religious_covenant_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(jewi_be_t8, jewish_self_determination__religious_covenant_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(jewi_be_t16, jewish_self_determination__religious_covenant_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(jewi_be_t24, jewish_self_determination__religious_covenant_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(jewi_be_t32, jewish_self_determination__religious_covenant_reading, base_extractiveness, 32, 0.79).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__religious_covenant_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(jewi_su_t8, jewish_self_determination__religious_covenant_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(jewi_su_t16, jewish_self_determination__religious_covenant_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(jewi_su_t24, jewish_self_determination__religious_covenant_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(jewi_su_t32, jewish_self_determination__religious_covenant_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__religious_covenant_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__religious_covenant_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the jewish_self_determination kernel. Each reading instantiates a structurally distinct constraint with different ε values, beneficiaries/victims, and operative type classifications. The religious_covenant_reading claims MOUNTAIN (immutable divine law) but operates as TANGLED_ROPE (religious authority entangled with state power, generating extraction). The settler_colonial_reading also claims high extraction but via different mechanisms (dispossession, legal exclusion). The liberal_nationalist_reading frames the same territorial claim as ROPE (coordination among equals). The diasporist_reading frames territorial sovereignty itself as a constraint (not a solution), rejecting the entire kernel. The indigenous_return_reading claims the constraint is MOUNTAIN but via different axioms (indigenous title via unbroken presence, not divine covenant). All five readings share the same contested kernel; each decomposes it differently. The network edges (affects_constraints) record how the readings influence and foreclose each other's logical space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
