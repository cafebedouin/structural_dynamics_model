% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority â Rupture Traditionalist Reading
 *   domain: ecclesiological/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the rupture_traditionalist reading of
 *   the contested kernel vatican_ii_doctrinal_authority. From this reading's
 *   perspective, the Second Vatican Council's documents introduced
 *   ambiguities and errors that represent a substantive rupture with
 *   perennial Catholic tradition. The post-conciliar authority structure uses
 *   these texts to coordinate a progressive reform agenda while extracting
 *   doctrinal clarity, traditional liturgy, and missionary zeal from
 *   traditional communities. The constraint is the standing arrangement of
 *   conciliar authority as operationalized by the post-conciliar hierarchy
 *   â not the reading's endorsed alternative (a return to pre-conciliar
 *   magisterial clarity).
 *
 * KEY AGENTS:
 *   - post_conciliar_hierarchy: agenda setter (institutional/constrained) â administers and enforces conciliar implementation
 *   - progressive_clergy: primary beneficiary (moderate/constrained) â collects liturgical and doctrinal flexibility
 *   - ecumenical_reform_advocates: secondary beneficiary (organized/constrained) â collects interfaith and institutional autonomy
 *   - traditional_catholic_communities: primary payer (organized/identity_locked) â bears loss of liturgy and clarity
 *   - missionary_societies: payer (organized/constrained) â bears collapse of propositional confidence
 *   - traditional_monastic_orders: payer (organized/identity_locked) â bears suppression of choral and contemplative patrimony
 *   - traditionalist_intellectuals: analytical observer (moderate/analytical) â documents structural extraction without power to alter it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority â Rupture Traditionalist Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiological/institutional").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '91b05a12-ece6-41bb-abdd-195aab647d31').
narrative_ontology:cs_kernel_codification('91b05a12-ece6-41bb-abdd-195aab647d31', fixed_text).
narrative_ontology:cs_authority_grounding('91b05a12-ece6-41bb-abdd-195aab647d31', lineage).
narrative_ontology:cs_interpretation_layer_present('91b05a12-ece6-41bb-abdd-195aab647d31').
narrative_ontology:cs_reading_relation('91b05a12-ece6-41bb-abdd-195aab647d31', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('91b05a12-ece6-41bb-abdd-195aab647d31', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('91b05a12-ece6-41bb-abdd-195aab647d31', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('91b05a12-ece6-41bb-abdd-195aab647d31', foundational, perennial_magisterial_immutability).
narrative_ontology:cs_axiom_status(perennial_magisterial_immutability, holdable).
narrative_ontology:cs_axiom_grounding('91b05a12-ece6-41bb-abdd-195aab647d31', perennial_magisterial_immutability, theological).
narrative_ontology:cs_axiom('91b05a12-ece6-41bb-abdd-195aab647d31', foundational, conciliar_texts_as_doctrinally_compromised).
narrative_ontology:cs_axiom_status(conciliar_texts_as_doctrinally_compromised, holdable).
narrative_ontology:cs_axiom_grounding('91b05a12-ece6-41bb-abdd-195aab647d31', conciliar_texts_as_doctrinally_compromised, empirically_contingent).
narrative_ontology:cs_reference_frame('91b05a12-ece6-41bb-abdd-195aab647d31', perennial_magisterial_tradition).
narrative_ontology:cs_drift_state('91b05a12-ece6-41bb-abdd-195aab647d31', post_conciliar_implementation_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('91b05a12-ece6-41bb-abdd-195aab647d31', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_reform_advocates).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholic_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_societies).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_monastic_orders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers conciliar implementation through diocesan structures, liturgical commissions, and canonical directives. Controls appointment of bishops and approval of religious orders. Enforces suppression of traditional liturgical practice through administrative measures and doctrinal assessments.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% Utilizes doctrinal and liturgical flexibility enabled by ambiguous conciliar texts to innovate in parish practice and theological expression. Benefits from reduced traditional constraints and increased lay participation models without bearing institutional enforcement costs.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_clergy, beneficiary,
    moderate, biographical, constrained, global).

% Operate institutional offices and dialogues that depend on the conciliar opening toward other Christian bodies and secular modernity. Benefit from textual ambiguity that permits doctrinal accommodations and shared prayer arrangements previously prohibited.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_reform_advocates, beneficiary,
    organized, generational, constrained, global).

% Attached to pre-conciliar liturgical forms, catechetical methods, and devotional practices. Subjected to revised rites and ambiguous teaching. Experience loss of doctrinal clarity and liturgical patrimony. Exit is identity-locked because leaving the sacramental structure of the Church is spiritually unthinkable despite the rupture they perceive.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_catholic_communities, payer,
    organized, biographical, identity_locked, global).

% Dependent on Roman mandate and episcopal funding for missionary jurisdiction. Experience collapse of doctrinal clarity and missionary confidence as ambiguous conciliar formulations replace clear propositional evangelization. Cannot exit without abandoning canonical mission territories.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_societies, payer,
    organized, generational, constrained, global).

% Bound by vows and founding charisms to the Church's liturgical and doctrinal tradition. Compelled to adopt revised rites or operate under restrictive indults. Their contemplative and choral patrimony is treated as obsolete; leaving the order or the Church would violate their consecrated identity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_monastic_orders, payer,
    organized, generational, identity_locked, global).

% Analyze conciliar texts, post-conciliar encyclicals, and canonical developments to document ambiguities and heterodox outcomes. Publish critiques from an analytical seat lacking institutional power to alter enforcement. Their work circulates in marginalized journals and independent publishing houses.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditionalist_intellectuals, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Catholic Church's institutional adaptation to modern secular governance, ecumenical dialogue partners, and post-Enlightenment intellectual frameworks through a common conciliar textual basis.
% TRANSFER_FUNCTION: Moves doctrinal and liturgical control from traditional clergy, monastic orders, and missionary structures to progressive reformers and centralized post-conciliar authorities, while transferring episcopal authority from local tradition-preserving bishops to conciliar-reform-oriented hierarchies.
% ABSENT_VOICES: Pre-conciliar theologians of the Roman School, traditional monastic orders opposed to liturgical experimentation, and missionary bishops who warned against doctrinal accommodation are excluded from post-conciliar magisterial discourse; their objections survive only in marginalized publications, canonical irregularities, and independent apostolates.
% DISAPPEARANCE_RATIONALE: If the conciliar authority framework and its ambiguities vanished, traditional liturgy and doctrine would reassert, progressive reforms would lose institutional backing, and the post-conciliar hierarchy's legitimacy would collapse â the Church would reorganize around pre-conciliar structures or fragment into competing jurisdictions.
% FOUNDING_PROBLEM: The perceived inability of the pre-conciliar Church to engage modern secular states, ecumenical partners, and contemporary intellectual culture without a shared updating of its public posture and institutional presentation.
% FOUNDING_PROBLEM_CORROBORATION: Post-conciliar progressive clergy and ecumenical offices attest the problem was real and required conciliar action. Traditionalist historians and pre-conciliar missionary theologians attest the problem was exaggerated or soluble without doctrinal compromise. Corroboration from outside the benefiting parties: traditionalist intellectuals and some Eastern Orthodox critics of Vatican II provide external attestation that the engagement rationale was cover for Western theological collapse.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.78) because the reading treats the conciliar texts as deliberately or negligently ambiguous mechanisms that transfer doctrinal and liturgical control from tradition to progressive reformers. Suppression (0.72) reflects active episcopal enforcement restricting the traditional Latin Mass, marginalizing traditional orders, and penalizing dissent. Theater ratio (0.60) captures the reading's view that the 'hermeneutic of continuity' is performative cover for substantive rupture. Accessibility collapse (0.70) measures how completely pre-conciliar alternatives have been foreclosed by institutional suppression. Resistance (0.55) acknowledges significant but fragmented traditionalist opposition (SSPX, traditionalist movements, internal episcopal dissent). Temporal measurements show extraction and theater accumulating from 1965 to the present as the post-conciliar regime consolidated.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as necessary reform and legitimate adaptation; the payer seats experience it as rupture and extraction. The engine computes this divergence from the structural asymmetry: beneficiaries have constrained exit within a functioning institutional career, while payers are identity-locked to the sacramental structure they regard as compromised. The divergence is not evaluative noise but the core structural feature the reading identifies.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive clergy and ecumenical reform advocates are declared beneficiaries because they collect flexibility and institutional space from the ambiguous texts without paying enforcement costs. Traditional communities, missionary societies, and monastic orders are declared payers because they bear the loss of liturgical forms, doctrinal certainty, and canonical standing. The post-conciliar hierarchy is agenda-setter rather than beneficiary because its role is administrative and enforcement; it does not personally collect the gains of doctrinal flexibility (though it may benefit from expanded bureaucratic scope). Directionality derives from these declarations: beneficiaries sit near d=0.0, payers near d=1.0, with identity_locked and constrained exit options amplifying effective extraction for the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the arrangement as pure extraction (snare) by requiring a genuine coordination function: even this reading acknowledges that Vatican II coordinates the Church's engagement with modernity, ecumenism, and global institutional governance. The coordination function is real but asymmetrically costly. Conversely, the high epsilon, active enforcement, and victim declarations prevent mislabeling it as mere rope (benign coordination) or mountain (inevitable development). The mandatrophy question â whether the founding problem still exists â is answered as contested, preventing automatic classification as scaffold or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of kernel vatican_ii_doctrinal_authority (rupture_traditionalist). Would a continuity reading of the same kernel reclassify the constraint as a low-extraction coordination mechanism (rope), and does that disagreement turn on empirical facts about conciliar intent or on normative commitments about the immutability of tradition?',
    'Historical demonstration of conciliar commission intent and subsequent magisterial interpretation patterns; also analysis of whether post-conciliar extraction metrics are better explained by textual ambiguity or by external ideological capture.',
    'If the continuity reading is structurally sound, this reading''s high epsilon is an artifact of evaluative framing rather than extraction; if this reading is sound, the continuity reading is a false-summit mountain or snare cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural ambiguity between rupture and continuity readings of the same kernel').

omega_variable(
    sibling_reading_structural_delta,
    'The rupture_progressive reading shares the rupture premise but evaluates it positively. Does the structural identity of the constraint change when the evaluative sign flips, or is epsilon invariant across the sign flip?',
    'Compare the beneficiary/victim structures: both readings identify the same transfer from tradition to reformers, suggesting epsilon is invariant; the difference is purely normative valuation.',
    'If epsilon is invariant, the two readings form a kernel pair with identical structural metrics but opposite beneficiary evaluation, confirming epsilon as a property of the arrangement, not the observer''s valuation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Epsilon invariance across evaluative sign flip with rupture_progressive reading').

omega_variable(
    suppression_mechanism_nature,
    'Is the suppression of traditional practice structural (canonical penalties, liturgical restrictions, episcopal decrees) or internalized (clergy accepting the new paradigm as legitimately Catholic)?',
    'Measure persistence of traditional practice and traditionalist dissent after structural penalties are lifted or relaxed; if suppression persists, the mechanism is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure â the target population carries the suppression even when external barriers are removed, deepening the extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_nature, empirical, 'Structural vs internalized suppression mechanism in ecclesial context').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2dr_tr_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(v2dr_tr_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(v2dr_tr_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(v2dr_tr_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(v2dr_tr_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 40, 0.53).
narrative_ontology:measurement(v2dr_tr_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 50, 0.57).
narrative_ontology:measurement(v2dr_tr_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 60, 0.6).

% Extraction over time
narrative_ontology:measurement(v2dr_be_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(v2dr_be_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(v2dr_be_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(v2dr_be_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(v2dr_be_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(v2dr_be_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 50, 0.73).
narrative_ontology:measurement(v2dr_be_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 60, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(v2dr_su_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(v2dr_su_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(v2dr_su_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(v2dr_su_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(v2dr_su_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(v2dr_su_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(v2dr_su_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 60, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel vatican_ii_doctrinal_authority. The kernel decomposes into structurally distinct claims per the epsilon-invariance principle because different readings produce different epsilon values, beneficiary/victim structures, and directionalities. This reading (rupture_traditionalist) shares the rupture factual premise with rupture_progressive but evaluates it negatively; it forecloses continuity_reading. Sibling constraints handle alternative readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
