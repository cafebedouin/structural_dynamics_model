% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__honorific_similarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__honorific_similarity_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: homoousios_nicene__honorific_similarity_reading
 *   human_readable: Honorific Similarity Reading of Nicene Homoousios
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   The honorific similarity reading of the Nicene homoousios construes the
 *   credal term as signifying likeness and honorific unity between Father and
 *   Son rather than strict ontological identity. This reading functions as an
 *   ecclesiastical constraint by defining theological orthodoxy in a way that
 *   includes semi-Arian moderates and apophatic traditions while
 *   marginalizing both strict Nicene metaphysicians (as rigid) and hard
 *   subordinationists (as heretical). Interpretive authority shifts toward
 *   local bishops and pastoral discretion, creating a standing arrangement
 *   that coordinates conciliar communion at the cost of asymmetrically
 *   extracting legitimacy from the two theological flanks.
 *
 * KEY AGENTS:
 *   - semi_arian_moderates: Primary beneficiary (moderate/constrained) â gains conciliar legitimacy from a blurred boundary
 *   - apophatic_traditions: Primary beneficiary (moderate/constrained) â gains protective ambiguity against positive ontological claims
 *   - strict_nicene_enforcers: Primary payer (institutional/constrained) â loses interpretive authority and faces charges of rigidity
 *   - hard_subordinationists: Secondary payer (moderate/trapped) â faces heresy charges under a boundary they cannot reliably predict
 *   - local_bishops: Agenda setter (institutional/constrained) â administers pastoral discretion and sets local doctrinal boundaries
 *   - ecumenical_councils: Agenda setter (institutional/constrained) â enforces the formula and anathematizes deviation
 *   - theological_historian: Analytical observer (analytical/analytical) â tracks how conciliar language is reinterpreted to manage political-theological conflict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, 0.65).
domain_priors:suppression_score(homoousios_nicene__honorific_similarity_reading, 0.71).
domain_priors:theater_ratio(homoousios_nicene__honorific_similarity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__honorific_similarity_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__honorific_similarity_reading, "Honorific Similarity Reading of Nicene Homoousios").
narrative_ontology:topic_domain(homoousios_nicene__honorific_similarity_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__honorific_similarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__honorific_similarity_reading, '2da635ea-f9b1-4347-a33d-a5cb75486283').
narrative_ontology:cs_kernel_codification('2da635ea-f9b1-4347-a33d-a5cb75486283', fixed_text).
narrative_ontology:cs_authority_grounding('2da635ea-f9b1-4347-a33d-a5cb75486283', lineage).
narrative_ontology:cs_interpretation_layer_present('2da635ea-f9b1-4347-a33d-a5cb75486283').
narrative_ontology:cs_reading_relation('2da635ea-f9b1-4347-a33d-a5cb75486283', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('2da635ea-f9b1-4347-a33d-a5cb75486283', homoousios_nicene__subordinationist_reading, influences).
narrative_ontology:cs_axiom('2da635ea-f9b1-4347-a33d-a5cb75486283', foundational, divine_unity_is_honorific_not_ontological).
narrative_ontology:cs_axiom_status(divine_unity_is_honorific_not_ontological, holdable).
narrative_ontology:cs_axiom_grounding('2da635ea-f9b1-4347-a33d-a5cb75486283', divine_unity_is_honorific_not_ontological, theological).
narrative_ontology:cs_axiom('2da635ea-f9b1-4347-a33d-a5cb75486283', foundational, apophatic_reserve_governs_credal_language).
narrative_ontology:cs_axiom_status(apophatic_reserve_governs_credal_language, holdable).
narrative_ontology:cs_axiom_grounding('2da635ea-f9b1-4347-a33d-a5cb75486283', apophatic_reserve_governs_credal_language, theological).
narrative_ontology:cs_reference_frame('2da635ea-f9b1-4347-a33d-a5cb75486283', apophatic_unity_frame).
narrative_ontology:cs_drift_state('2da635ea-f9b1-4347-a33d-a5cb75486283', post_chalcedonian_definitions, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2da635ea-f9b1-4347-a33d-a5cb75486283', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__honorific_similarity_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, apophatic_traditions).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, hard_subordinationists).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, apophatic_reserve_doctrine).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, pastoral_discretion_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theological moderates occupying the space between strict Nicene identity and explicit subordinationism. The honorific similarity reading validates their position by interpreting homoousios as compatible with nuanced distinction, granting them conciliar legitimacy and protection from anathema. Their exit is constrained because abandoning this reading would force them to choose between explicit heresy and rigid orthodoxy, both of which marginalize them.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates, beneficiary,
    moderate, generational, constrained, continental).

% Theological traditions emphasizing the incomprehensibility of the divine essence and the limits of positive ontological language. The reading vindicates their suspicion of strict metaphysical formulas by framing homoousios as honorific rather than definitional, giving them a recognized voice in orthodox discourse. Exit is constrained because alternative readings either demand precise ontological commitments they reject or abandon the credal tradition entirely.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_traditions, beneficiary,
    moderate, civilizational, constrained, universal).

% Theologians and ecclesiastical parties committed to strict ontological identity between Father and Son. They experience the honorific reading as a devaluation of the Nicene achievement, eroding their interpretive authority and exposing them to charges of rigidity or inadequate pastoral sensitivity. Their institutional base is strong but geographically and politically constrained by imperial and conciliar alignment toward the center.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers, payer,
    institutional, generational, constrained, continental).

% Theologians defending the Son's derivation from and subordination to the Father. The honorific similarity reading maintains the anathema against their position but blurs the boundary between acceptable similarity and unacceptable subordination, making it difficult to know which expressions cross the line. They are trapped because open advocacy invites heresy proceedings, while strategic silence still leaves them outside the beneficiary set.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, hard_subordinationists, payer,
    moderate, biographical, trapped, regional).

% Administrators of dioceses who exercise pastoral discretion in teaching and disciplinary application of the creed. The honorific similarity reading expands their interpretive leeway, allowing them to accommodate local theological variation while remaining inside conciliar boundaries. They cannot easily exit because their authority derives from communion with the conciliar network.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, local_bishops, agenda_setter,
    institutional, generational, constrained, regional).

% Conciliar assemblies that ratify credal formulas and anathematize deviations. Under the honorific similarity reading, they must sustain the anathema against explicit subordinationism while permitting blurred language that avoids strict ontological precision. Their enforcement is constrained by the need to maintain imperial and inter-episcopal consensus.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, ecumenical_councils, agenda_setter,
    institutional, civilizational, constrained, universal).

% Analytical observer tracking how a fourth-century credal term is reinterpreted across centuries to manage ecclesiastical politics, imperial unity, and theological diversity. Neither collects legitimacy nor pays costs within the constraint.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, theological_historian, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves conciliar communion and imperial ecclesiastical unity across diverse metaphysical commitments by interpreting a single credal term as a functional boundary marker rather than a precise ontological equation.
% TRANSFER_FUNCTION: Moves theological legitimacy and conciliar standing from strict ontological interpreters and explicit subordinationists toward semi-Arian moderates and apophatic traditions; moves interpretive authority from centralized conciliar definition toward local episcopal discretion.
% ABSENT_VOICES: Hard subordinationists who cannot predict the blurred boundary; strict Nicene laity who understand the creed as plain metaphysical identity; women and non-ordained believers whose theological experience is shaped but who have no seat at conciliar formulation.
% DISAPPEARANCE_RATIONALE: Without the honorific similarity reading, semi-Arian moderates lose conciliar cover and are pushed toward explicit heresy or strict orthodoxy; apophatic traditions lose their protective ambiguity; local bishops lose expanded pastoral discretion; and the conciliar center likely fragments into schism between identity and subordination factions.
% FOUNDING_PROBLEM: The collapse of theological consensus and imperial political instability following the Arian controversy, where uncompromising ontological claims on both sides threatened to fracture the Church and its relationship with the Roman state.
% FOUNDING_PROBLEM_CORROBORATION: Semi-Arian and apophatic theologians attest the problem persists as the danger of rigid metaphysical speculation dividing the church. Strict Nicene historians attest the problem was solved by the original Nicene formula and that the honorific reading reintroduces the very ambiguity that caused the crisis. Secular historians of late antiquity corroborate the political stakes from outside the theological beneficiary set, documenting imperial concern for ecclesiastical unity as a governance prerequisite.
narrative_ontology:disappearance_verdict(homoousios_nicene__honorific_similarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__honorific_similarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__honorific_similarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__honorific_similarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__honorific_similarity_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__honorific_similarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__honorific_similarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the substantial asymmetry between those who gain conciliar legitimacy under the honorific reading and those who lose itâstrict Nicene enforcers and hard subordinationists are both marginalized. Suppression (0.71) is high because the constraint persists through active conciliar enforcement, anathemas, and episcopal discipline, not through spontaneous consensus. Theater ratio (0.45) captures the heavy ceremonial and discursive maintenance required to sustain a blurred boundary as if it were a precise rule. Accessibility collapse (0.68) indicates that alternative readings, while intellectually available, are effectively closed off by the threat of heresy proceedings and communal exclusion. Resistance (0.58) registers sustained pushback from both theological flanks. The temporal series show extraction and theater rising as the reading is institutionalized, with a slight moderation mid-interval corresponding to temporary conciliar dÃ©tentes.
 *
 * PERSPECTIVAL GAP:
 *   The semi-Arian moderate and apophatic seats experience the constraint as protective coordination that preserves their place in the church; the strict Nicene and hard subordinationist seats experience the same structure as extractive exclusion that costs them legitimacy and safety. The local episcopal seat experiences expanded agency, while the conciliar seat experiences the difficulty of holding a center that is blurred by design. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (semi_arian_moderates, apophatic_traditions) derive low directionality because the constraint subsidizes their theological position and protects them from anathema. Victims (strict_nicene_enforcers, hard_subordinationists) derive high directionality because the constraint extracts legitimacy from them and exposes them to disciplinary action. Local bishops as agenda_setters sit near the middle: they administer the constraint and gain discretion, but are also bound by conciliar communion and cannot exit. The analytical observer carries analytical exit and universal scope, producing neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and asymmetric extraction for tangled rope certification. The honorific reading has a genuine coordination functionâpreserving imperial and ecclesiastical unity across diverse factionsâso it cannot be a pure snare. Yet it also has identifiable victims who bear costs through the same structure, so it cannot be a pure rope. The temporal measurements show rising extraction over time, confirming that the coordination function has become entangled with enforcement rather than evaporating into pure mountain or rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homoousios_kernel_reading_delta,
    'This constraint is the honorific_similarity_reading of kernel homoousios_nicene. What would change structurally if the metaphysical_equality_reading or subordinationist_reading were adopted as the operative interpretation?',
    'Side-by-side comparison of the three constraint stories'' beneficiary/victim sets, directionality maps, and enforcement requirements. The metaphysical_equality_reading would eliminate semi-Arian moderates from the beneficiary set and recast strict Nicene enforcers as orthodox beneficiaries. The subordinationist_reading would collapse the conciliar center and redefine the victim set as all non-subordinationists.',
    'Classification would shift: metaphysical_equality_reading likely computes as rope or mountain depending on suppression; subordinationist_reading likely computes as snare. The disagreement is located in whether the kernel fixes an ontological fact (identity/subordination) or a communal boundary (similarity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(homoousios_kernel_reading_delta, conceptual, 'Sibling reading structural delta for the homoousios kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the exclusion of strict Nicene enforcers and hard subordinationists accomplished through structural conciliar power, or through internalized theological identity that makes alternative readings unthinkable for participants?',
    'Examination of post-conciliar persistence: do excluded theologians continue to hold their views privately (internalized) or are they only silenced under structural threat? Analysis of private correspondence and later recantation patterns.',
    'If internalized, effective suppression is higher than structural measures suggest, and the constraint operates more deeply as identity coordination. If purely structural, removal of conciliar penalties might permit faster pluralism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in conciliar theological constraints.').

omega_variable(
    coordination_extraction_boundary,
    'Does the honorific similarity reading coordinate genuine theological communion, or does it primarily extract legitimacy from the flanks to sustain a fragile center?',
    'Historical comparison of conciliar outcomes under strict identity versus honorific similarity regimes: does the blurred boundary produce durable peace or recurring cycles of renegotiation and enforcement?',
    'If durable peace, the coordination component dominates and extraction is overhead. If recurring cycles, the coordination story is cover for center-periphery extraction, pushing classification toward snare over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable in the ecclesiastical context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__honorific_similarity_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_hs_tr_t0, homoousios_nicene__honorific_similarity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(homoousios_hs_tr_t16, homoousios_nicene__honorific_similarity_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(homoousios_hs_tr_t32, homoousios_nicene__honorific_similarity_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(homoousios_hs_tr_t48, homoousios_nicene__honorific_similarity_reading, theater_ratio, 48, 0.4).
narrative_ontology:measurement(homoousios_hs_tr_t64, homoousios_nicene__honorific_similarity_reading, theater_ratio, 64, 0.42).
narrative_ontology:measurement(homoousios_hs_tr_t80, homoousios_nicene__honorific_similarity_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(homoousios_hs_be_t0, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(homoousios_hs_be_t16, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(homoousios_hs_be_t32, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(homoousios_hs_be_t48, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 48, 0.63).
narrative_ontology:measurement(homoousios_hs_be_t64, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 64, 0.61).
narrative_ontology:measurement(homoousios_hs_be_t80, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 80, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_hs_su_t0, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(homoousios_hs_su_t16, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(homoousios_hs_su_t32, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(homoousios_hs_su_t48, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 48, 0.72).
narrative_ontology:measurement(homoousios_hs_su_t64, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 64, 0.7).
narrative_ontology:measurement(homoousios_hs_su_t80, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 80, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__honorific_similarity_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, subordinationist_reading).

% DUAL FORMULATION NOTE:
% The homoousios_nicene kernel decomposes into three structurally distinct constraints: the honorific_similarity_reading (center), the metaphysical_equality_reading (strict identity), and the subordinationist_reading (hierarchical derivation). Each has a distinct epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by competitive structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
