% ============================================================================
% CONSTRAINT STORY: ninth_amendment__judicial_unusability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ninth_amendment__judicial_unusability_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ninth_amendment__judicial_unusability_reading
 *   human_readable: Ninth Amendment: Judicial Unusability (Open Texture Suppression)
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   The Ninth Amendment's open texture creates a structural constraint on
 *   judicial action: courts have systematically declined to rest any holding
 *   on the Ninth alone, treating the clause as textually present but
 *   doctrinally unusable. This reading instantiates the Ninth as a constraint
 *   via judicial abstention. The open texture gives judges nothing to apply
 *   (no limiting doctrine, no method for identifying 'unenumerated rights,'
 *   no principle for adjudicating competing claims) and everything to fear
 *   (boundless discretion, loss of institutional legitimacy, incursion into
 *   legislative prerogatives). The beneficiary of this constraint is
 *   doctrinal predictability and institutional judicial stability — courts
 *   coordinate around a shared understanding that the Ninth is a rule of
 *   construction forbidding inference that enumeration denies other rights,
 *   but not an independent source of enforceable claims. The victim set is
 *   unenumerated rights claimants and the clause's unrealized interpretive
 *   potential. The extractiveness value (0.58) reflects that the Ninth's
 *   protective function for substantive rights is substantially suppressed by
 *   judicial abstention, yet the clause retains some coordination role in
 *   interpretive doctrine. This is a tangled rope: genuine coordination
 *   around doctrinal restraint mixed with asymmetric extraction of the
 *   clause's substantive reach.
 *
 * KEY AGENTS:
 *   - Unenumerated Rights Claimants: Primary victim (powerless/trapped) — seek recognition of rights not enumerated in first eight amendments; face judicial abstention grounded in open-texture doctrine
 *   - Institutional Judiciary: Primary beneficiary (institutional/arbitrage) — benefits from doctrinal predictability and interpretive stability achieved by declining to apply Ninth alone
 *   - Academic and Advocacy Community: Secondary agent (moderate/constrained) — can invoke the Ninth as interpretive resource and aspirational doctrine (per Griswold concurrence) but cannot persuade courts to rest holdings solely on it
 *   - Doctrinal Predictability (as abstracted beneficiary): The institutional good produced by shared judicial restraint; the coordination function that justifies extraction
 *   - Constitutional Text/Clause-as-Agent: Victim of unrealized potential — the Ninth's interpretive function is suppressed by the very method (abstention) that enables judicial coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ninth_amendment__judicial_unusability_reading, 0.58).
domain_priors:suppression_score(ninth_amendment__judicial_unusability_reading, 0.72).
domain_priors:theater_ratio(ninth_amendment__judicial_unusability_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ninth_amendment__judicial_unusability_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ninth_amendment__judicial_unusability_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ninth_amendment__judicial_unusability_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ninth_amendment__judicial_unusability_reading, tangled_rope).
narrative_ontology:human_readable(ninth_amendment__judicial_unusability_reading, "Ninth Amendment: Judicial Unusability (Open Texture Suppression)").
narrative_ontology:topic_domain(ninth_amendment__judicial_unusability_reading, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(ninth_amendment__judicial_unusability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ninth_amendment__judicial_unusability_reading, 'e0663556-3c99-416b-b287-b8f7ad176322').
narrative_ontology:cs_kernel_codification('e0663556-3c99-416b-b287-b8f7ad176322', fixed_text).
narrative_ontology:cs_authority_grounding('e0663556-3c99-416b-b287-b8f7ad176322', lineage).
narrative_ontology:cs_interpretation_layer_present('e0663556-3c99-416b-b287-b8f7ad176322').
narrative_ontology:cs_reading_relation('e0663556-3c99-416b-b287-b8f7ad176322', ninth_amendment__rights_reservoir_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0663556-3c99-416b-b287-b8f7ad176322', ninth_amendment__rule_of_construction_reading, coexists_with).
narrative_ontology:cs_axiom('e0663556-3c99-416b-b287-b8f7ad176322', foundational, open_texture_justifies_abstention).
narrative_ontology:cs_axiom_status(open_texture_justifies_abstention, holdable).
narrative_ontology:cs_axiom_grounding('e0663556-3c99-416b-b287-b8f7ad176322', open_texture_justifies_abstention, instrumental).
narrative_ontology:cs_axiom('e0663556-3c99-416b-b287-b8f7ad176322', foundational, judicial_restraint_enables_stability).
narrative_ontology:cs_axiom_status(judicial_restraint_enables_stability, holdable).
narrative_ontology:cs_axiom_grounding('e0663556-3c99-416b-b287-b8f7ad176322', judicial_restraint_enables_stability, conventional).
narrative_ontology:cs_reference_frame('e0663556-3c99-416b-b287-b8f7ad176322', protective_enumeration_of_rights).
narrative_ontology:cs_drift_state('e0663556-3c99-416b-b287-b8f7ad176322', contemporary_doctrine, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e0663556-3c99-416b-b287-b8f7ad176322', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(ninth_amendment__judicial_unusability_reading, ninth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ninth_amendment__judicial_unusability_reading, doctrinal_predictability).
narrative_ontology:constraint_beneficiary(ninth_amendment__judicial_unusability_reading, institutional_judicial_stability).
narrative_ontology:constraint_victim(ninth_amendment__judicial_unusability_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(ninth_amendment__judicial_unusability_reading, clause_interpretive_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNENUMERATED RIGHTS CLAIMANT (SNARE) — An individual asserting a right not enumerated in the first eight amendments faces judicial abstention grounded in open-texture fear. The Ninth offers textual hook but no usable doctrine. The claimant has no exit: either the right is recognized (requiring judges to overcome abstention), or it is lost. Judges explicitly cite the Ninth's open texture as reason to decline application. This is suppression enforced by doctrinal paralysis.
constraint_indexing:constraint_classification(ninth_amendment__judicial_unusability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ACADEMIC/ADVOCATE THEORIST (TANGLED ROPE) — Scholars and advocates benefit from the Ninth's availability as a textual resource and aspirational doctrine (coordination: they can invoke it, cite Griswold, build theory). But they face real constraints: courts will not ride on the Ninth alone, requiring additional constitutional hooks (Fourteenth Amendment liberty, privacy doctrine, etc.). The clause provides interpretive material but no independent decisional mechanism. Mixed: genuine doctrinal function + asymmetric extraction of that function's productive potential.
constraint_indexing:constraint_classification(ninth_amendment__judicial_unusability_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL JUDICIARY / PREDICTABILITY BENEFICIARY (ROPE) — Courts experience the open texture of the Ninth as a coordination problem solved by abstention. By declining to rest holdings on the Ninth alone, courts coordinate around a narrow doctrine that preserves institutional stability and judicial restraint. The beneficiary is doctrinal predictability: judges know what they must not do (ground decisions solely on the Ninth), creating clear coordination. Extraction runs toward the institution, not away. This is pure coordination of judicial behavior.
constraint_indexing:constraint_classification(ninth_amendment__judicial_unusability_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the Ninth exhibits genuine coordination (the 'rule of construction' function: forbidding the inference that enumeration denies other rights) alongside systematic suppression of its independent applicative force. Judges coordinate around abstention to avoid facing the open-texture problem. This creates a hybrid: real doctrinal function (the inference rule) + real extraction (the clause's potential as an independent source of rights is unrealized). The coordination solves a problem (interpretive restraint) by extracting the solution's cost (unenumerated rights claimants bear doctrinal invisibility).
constraint_indexing:constraint_classification(ninth_amendment__judicial_unusability_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ninth_amendment__judicial_unusability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ninth_amendment__judicial_unusability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ninth_amendment__judicial_unusability_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ninth_amendment__judicial_unusability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ninth_amendment__judicial_unusability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The Ninth Amendment's protective force for unenumerated rights is substantially suppressed by judicial doctrine requiring that no holding rest on it alone. Claimants asserting unenumerated rights must anchor their claims in enumerated rights or other constitutional provisions, extracting from the Ninth only its negative function (forbidding inference that enumeration is exclusive) rather than its potential positive function (substantive grounding of unenumerated rights). The extractiveness increases over the interval (0.35 → 0.58) because the doctrine of abstention has hardened: early cases (1960s–1980s) showed some willingness to cite the Ninth as supporting reasoning (even if not as holding); contemporary doctrine (2000s–2020s) rarely invokes it except as historical curiosity. Suppression (0.72): High. Judicial abstention enforced by fear of open texture constitutes active suppression of the clause's independent applicative force. Courts explicitly cite the Ninth's vagueness and open texture as justification for declining to apply it. Claimants have no exit — the Ninth offers no usable method. Theater ratio (0.65): Moderate-high. The invocation of 'open texture' and 'judicial restraint' as justification for abstention contains performative elements. The Ninth is textually present and functionally active (as a rule of construction), but its invocation in most opinions is rhetorical — cited as the reason courts will not apply it, not as the ground for what they will apply. The performativity has increased over time (rising to 0.65) as doctrinal commentary has become more sophisticated about the Ninth's non-applicability, treating it as a settled feature of constitutional doctrine rather than an open question.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The powerless claimant sees a snare — textual promise with no method of enforcement, enforced by judicial fear. The moderate advocate sees a tangled rope — genuine access to the clause as interpretive material, but severe constraints on its independent force. The institutional judiciary sees pure coordination (rope) — the shared understanding that the Ninth is not a standalone source of rights solves the coordination problem of doctrinal predictability. The analytical observer recognizes the hybrid: the clause coordinates judicial behavior (the 'rule of construction') while extracting the cost of that coordination (unenumerated rights go unheard). The readings of the Ninth's kernel (judicial_unusability vs. rights_reservoir vs. rule_of_construction) map onto these perspectives — which reading you adopt determines which classification you produce.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial institution occupies the beneficiary role (arbitrage exit) because abstention solves a genuine coordination problem and the institution captures the value of doctrinal stability. Unenumerated rights claimants occupy the victim role (trapped exit) because they can neither overcome the abstention nor exit the legal system. The suppression is asymmetric: it falls entirely on the victim class. The beneficiary's directionality is low (d ≈ 0.12) because they are arbitrage-positioned and gaining from the constraint. The victim's directionality is high (d ≈ 0.88) because they are trapped and bearing full extraction cost. The moderate advocate occupies a middle position: they can use the Ninth as interpretive argument (partial exit via secondary constitutional hooks), but cannot persuade courts to apply it independently (high constraint cost, constrained exit). This produces a perspectival gap: what the beneficiary sees as solving a coordination problem (how to restrain judges), the victim sees as suppressing their potential rights.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through explicit differentiation of readings. The judicial_unusability_reading (this one) models the Ninth as suppressed by judicial abstention — a tangled rope where the clause's coordination function (rule of construction) coexists with extraction (suppression of substantive rights). The rights_reservoir_reading (sibling) models the Ninth as a genuine spring of unenumerated rights — a rope or snare depending on enforceability. The rule_of_construction_reading (sibling) models the Ninth as a pure interpretive rule — likely a rope (coordination function with no independent extraction). Each reading is coherent under a different set of epistemic commitments about what 'rights' are and how constitutional text operates. The mandatrophy is not resolved by choosing one reading as correct, but by recognizing that the constraint instantiates different types under different readings because the Ninth's kernel is contested — its meaning depends on which interpretive theory grounds the authority structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_texture_interpretation_method,
    'Is the Ninth''s open texture a feature (permitting flexible application) or a bug (requiring judges to invent doctrine wholesale)?',
    'Comparative analysis of how other open-textured constitutional clauses (Fourteenth Amendment due process, First Amendment speech) are applied vs. how courts actually handle Ninth Amendment claims. Do courts apply parallel interpretive methods, or do they treat the Ninth as categorically different?',
    'If feature: suppression is judicial choice (tangled rope). If bug: suppression is structural necessity (snare). The reading''s core claim rests on whether judicial abstention is extractive withholding or legitimate restraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_texture_interpretation_method, conceptual, 'Whether open texture is inherent doctrinal feature or disqualifying structural defect').

omega_variable(
    rights_reservoir_vs_construction_rule_empirical_test,
    'What would it look like if the Ninth were interpretable as a genuine rights-reservoir (sibling: rights_reservoir_reading) vs. a mere rule of construction (sibling: rule_of_construction_reading)?',
    'Examine a concrete claim (e.g., right to bodily autonomy, right to family structure). Under the rights_reservoir reading, the Ninth provides independent textual grounding. Under the rule_of_construction reading, the Ninth forbids the inference that listed rights are exclusive, but contributes nothing substantive itself. Under the judicial_unusability reading, both framings are foreclosed by judicial abstention. Test: can any modern doctrine show the Ninth doing independent work grounding a right?',
    'If yes: the judicial_unusability reading is inaccurate — the Ninth has found application and this reading mischaracterizes the constraint. If no: the judicial_unusability reading''s core characterization (suppression via abstention) is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rights_reservoir_vs_construction_rule_empirical_test, empirical, 'Empirical test of whether Ninth provides independent doctrinal work').

omega_variable(
    doctrinal_predictability_as_beneficiary,
    'Do institutional judges genuinely benefit from abstention on the Ninth, or is this post-hoc rationalization for a constraint driven by other factors (federalism, judicial modesty, separation of powers)?',
    'Historical analysis: compare judicial citation patterns for the Ninth pre- and post-Griswold; examine whether doctrinal predictability metrics (citation consistency, reversal rates, opinion stability) improve when judges avoid the Ninth. Alternative: examine counterfactual — what would doctrinal predictability look like if courts applied the Ninth as they apply the Fourteenth Amendment liberty?',
    'If predictability is genuine beneficiary: tangled rope classification holds. If predictability is false cover story: the constraint is snare (pure extraction with no real coordination function). The beneficiary declaration rests on whether abstention solves a real coordination problem or merely rationalizes extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_predictability_as_beneficiary, empirical, 'Whether doctrinal predictability is genuine coordination benefit or rationalization').

omega_variable(
    judicial_fear_as_suppression_mechanism,
    'Does the suppression metric (0.72) accurately capture how judicial fear of open texture operates, or does it underestimate the psychological/institutional binding?',
    'Doctrinal history analysis: examine how judges explicitly cite the Ninth''s open texture and vagueness as reasons for abstention. Measure frequency of ''fear'' language (uncertainty, unmoored discretion, boundless inference) in opinions declining to apply the Ninth. Compare with other ''feared'' doctrines (vagueness, overbreadth in First Amendment). Does the fear operate symmetrically?',
    'If suppression (0.72) is accurate: the constraint is tangled rope. If the fear is deeper (stronger suppression): the constraint approaches snare. If the fear is surface rationalization: the suppression value is overstated and the constraint may be rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_fear_as_suppression_mechanism, empirical, 'Magnitude and mechanism of judicial fear as suppression force').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ninth_amendment__judicial_unusability_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ninth_jud_theater_t0, ninth_amendment__judicial_unusability_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ninth_jud_theater_t40, ninth_amendment__judicial_unusability_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement(ninth_jud_theater_t240, ninth_amendment__judicial_unusability_reading, theater_ratio, 240, 0.65).

% Extraction over time
narrative_ontology:measurement(ninth_jud_extract_t0, ninth_amendment__judicial_unusability_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ninth_jud_extract_t40, ninth_amendment__judicial_unusability_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(ninth_jud_extract_t240, ninth_amendment__judicial_unusability_reading, base_extractiveness, 240, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ninth_jud_suppress_t0, ninth_amendment__judicial_unusability_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ninth_jud_suppress_t40, ninth_amendment__judicial_unusability_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(ninth_jud_suppress_t240, ninth_amendment__judicial_unusability_reading, suppression_requirement, 240, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ninth_amendment__judicial_unusability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ninth_amendment__judicial_unusability_reading, ninth_amendment__rights_reservoir_reading).
narrative_ontology:affects_constraint(ninth_amendment__judicial_unusability_reading, ninth_amendment__rule_of_construction_reading).

% DUAL FORMULATION NOTE:
% The Ninth Amendment kernel decomposes into three structurally distinct constraints, one for each reading. The judicial_unusability_reading (this one, ε=0.58, tangled_rope) models suppression via abstention. The rights_reservoir_reading (sibling, expected ε>0.40, likely rope or tangled_rope) models the Ninth as a genuine spring of rights. The rule_of_construction_reading (sibling, expected ε<0.30, likely rope) models the Ninth as a rule of inference. All three operate on the same constitutional text, but they instantiate different constraints because they commit to different interpretations of what the text does. The network captures the dependency: each reading's viability rests on whether the other readings are held or rejected. This is the primary instance of kernel decomposition in the corpus — a single constitutional clause that grounds three incommensurate structural constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ninth_amendment__judicial_unusability_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
