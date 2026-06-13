% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Constitutional Validity via Formal Enactment (Positivist Reading)
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   The positivist reading of the U.S. Constitution holds that constitutional
 *   validity derives from formal enactment procedures (the text itself, as
 *   amended through Article V), not from moral content, historical original
 *   meaning (beyond what the formal text captures), or evolving social
 *   consensus. Judges operate under this constraint by treating the
 *   Constitution as a fixed legal document whose meaning is determined by its
 *   formal text and amendment procedures, not by judicial philosophy or
 *   substantive outcomes. This is ONE READING of the contested kernel
 *   'us_constitution_text'. Sibling readings (originalist_reading,
 *   living_constitutionalist_reading) share the same constitutional document
 *   but derive different constraints from competing interpretive premises.
 *   The claim/metric gap is structural: the constraint is CLAIMED as a
 *   tangled_rope (genuine coordination of judicial behavior toward
 *   predictability, plus active suppression of substantive-justice
 *   interpretation) and the metrics describe substantial extraction (0.62 at
 *   interval end) because substantive justice claims not formally authorized
 *   are categorically excluded. The engine measures this divergence; the
 *   story does not reconcile claim to metrics.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: Institutional agenda-setter. Binds itself to formal procedural constraint; enforces it against substantive-outcome arguments.
 *   - institutional_stability_apparatus: Institutional beneficiary. Executive, legislative, state institutions benefit from predictable constitutional baseline.
 *   - substantive_justice_claimants: Organized payers. Must pursue Article V amendment path rather than judicial sympathy.
 *   - marginalized_constituencies_lacking_formal_voice: Powerless, trapped payers. Excluded by the formal text itself; have no recourse unless text is amended.
 *   - originalist_interpreters: Institutional observer, partial beneficiary. Overlap with positivism on text-boundedness; diverge on historical reconstruction.
 *   - living_constitutionalist_interpreters: Institutional excluded. Structurally locked out by the constraint itself.
 *   - amendment_proponents: Organized payers. Face the Article V super-majority gauntlet as the only legitimate path to change.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.62).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Constitutional Validity via Formal Enactment (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, '99a114be-08c6-4c83-9ae1-10267551b4e9').
narrative_ontology:cs_kernel_codification('99a114be-08c6-4c83-9ae1-10267551b4e9', fixed_text).
narrative_ontology:cs_authority_grounding('99a114be-08c6-4c83-9ae1-10267551b4e9', lineage).
narrative_ontology:cs_interpretation_layer_present('99a114be-08c6-4c83-9ae1-10267551b4e9').
narrative_ontology:cs_reading_relation('99a114be-08c6-4c83-9ae1-10267551b4e9', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('99a114be-08c6-4c83-9ae1-10267551b4e9', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('99a114be-08c6-4c83-9ae1-10267551b4e9', foundational, formal_procedure_validity_exhaustive).
narrative_ontology:cs_axiom_status(formal_procedure_validity_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('99a114be-08c6-4c83-9ae1-10267551b4e9', formal_procedure_validity_exhaustive, conventional).
narrative_ontology:cs_axiom('99a114be-08c6-4c83-9ae1-10267551b4e9', foundational, moral_content_irrelevant_to_constitutional_validity).
narrative_ontology:cs_axiom_status(moral_content_irrelevant_to_constitutional_validity, holdable).
narrative_ontology:cs_axiom_grounding('99a114be-08c6-4c83-9ae1-10267551b4e9', moral_content_irrelevant_to_constitutional_validity, deontological).
narrative_ontology:cs_reference_frame('99a114be-08c6-4c83-9ae1-10267551b4e9', constitutional_validity_from_formal_enactment).
narrative_ontology:cs_drift_state('99a114be-08c6-4c83-9ae1-10267551b4e9', contemporary_judicial_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99a114be-08c6-4c83-9ae1-10267551b4e9', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, institutional_stability_apparatus).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, appellate_judiciary).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, marginalized_constituencies_lacking_formal_voice).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 (interval start) to 0.62 (plateau at t=32+) because the constraint's operation increasingly locks out justice claims: as society evolves and new injustices emerge, the formal text's silence on them becomes more obviously a cost borne by substantive claimants. The plateau reflects institutional stability: once positivist textualism is entrenched in the judiciary, the extraction level stabilizes — judges are bound by oath and doctrine. Suppression starts lower (0.48) but also rises and plateaus (0.58) because enforcement requires active rejection of living-constitutionalist and moral-philosophy arguments; judges must actively police the boundary between formal authorization and substantive equity. Theater rises from 0.28 to 0.41 and plateaus, reflecting increasing performative activity: judges write opinions explaining why formal text does not authorize what substantive justice would require, performing the constraint even when the outcome is obviously unjust from outside the positivist frame. The measurement series share one time grid at seven points, ensuring every metric is authored at every examined time point. The plateau at t=32+ reflects institutional lock-in: positivism, once the reigning judicial orthodoxy, becomes self-reinforcing and stable.
 *
 * PERSPECTIVAL GAP:
 *   The appellate_judiciary seat and the substantive_justice_claimants seat experience radically different constraint types. From the judiciary's position (particularly lower courts bound by precedent), the arrangement is genuine coordination: all judges applying the same fixed-text rule creates predictability and rule of law. From the substantive claimant's position, the same structure is a mechanism of foreclosure: their claims are categorically barred from judicial remedy, and they have no exit except through the (nearly impossible) Article V amendment process. The engine computes these divergent classifications from the structural data: institutional beneficiaries with arbitrage exit will see coordination; powerless, trapped victims will see extraction. The authorized claim reflects the positivist self-understanding; the metrics reflect the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for appellate_judiciary is near-beneficiary (~0.15): they control the constraint, benefit from its stability function, and have constrained but high-status exit (can reinterpret within textualist bounds). Directionality for substantive_justice_claimants is near-target (~0.85): they bear the cost (exclusion from judicial remedy), have no meaningful exit (trapped or identity-locked to the same jurisdiction), and do not control the constraint. Marginalized constituencies are at the far target end (~0.95): powerless, trapped, and bearing the cumulative cost of formal textual exclusion across generations. Originalist interpreters sit near neutral (~0.45): they benefit from text-boundedness but their historical-reconstruction layer sometimes conflicts with pure positivism, creating productive tension. Living constitutionalists are excluded from institutional power but retain scholarly voice, placing them near-target in judicial institutional terms (~0.75) but with higher analytical exit options. Amendment proponents are constrained payers (~0.70): they must expend enormous political capital on Article V even when their substantive claim is widely supported.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist reading must maintain a genuine coordination function to avoid being reclassified as pure snare. The coordination is real: judges coordinating on a fixed textual reference does solve the collective action problem of judicial behavior and establishes predictability. However, the extraction component is also real: substantive justice claims not formally authorized are definitively barred. This is exactly the tangled_rope structure: BOTH genuine coordination (rule-of-law predictability) AND asymmetric extraction (substantive claims locked out) operating through the same mechanism (text-boundedness). If the coordination function atrophied — if judges began applying positivism as pure rent-seeking, with no actual predictability or rule-of-law benefit — the constraint would reclassify as snare (pure extraction). The theater ratio's rise (0.28 to 0.41) is diagnostic: judges are increasingly performing the constraint by writing lengthy opinions about why formal text bars justice, rather than simply applying straightforward textual rules. If theater exceeds 0.65 while suppression remains high, the coordinate function would become suspect. At current levels, the tangled_rope classification holds: the constraint solves a real coordination problem (judicial behavior) while extracting a real cost (foreclosure of substantive claims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalism_vs_realism_in_textual_constraint,
    'Does the positivist reading''s claim that formal text constrains judges actually bind them, or is the constraint performative — judges choosing to appear bound while retaining substantive discretion?',
    'Comparative analysis of judicial outcomes across different constitutional interpretive schools, holding facts constant: if originalists, living constitutionalists, and positivists reach systematically different holdings on identical facts, formalism is performing rather than constraining.',
    'If performative, the constraint reclassifies from tangled_rope (real coordination + real extraction) toward piton (theatrical maintenance of a defunct coordination function). If binding, the tangled_rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalism_vs_realism_in_textual_constraint, empirical, 'Whether formal textual constraint genuinely binds judicial discretion or is theatrical.').

omega_variable(
    coordination_vs_institutional_capture_distinction,
    'Does the positivist reading''s coordination function (predictable judicial behavior) serve rule of law, or does it primarily serve institutional capture by entrenched power — locking in distributions that benefit incumbent institutions?',
    'Historical analysis of whom positivism has benefited: does the constraint''s stability equally protect all stakeholders, or does it disproportionately protect incumbent institutional winners (federal government, property holders, organized interests)? Do marginalized constituencies gain stability that protects their rights, or lose access to courts that might extend protections?',
    'If genuinely neutral coordination, the constraint''s extractiveness should be symmetric across stakeholders. If institutionally captured, the constraint''s extractiveness should concentrate on powerless and trapped victims while beneficiaries with exit options absorb little cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_institutional_capture_distinction, empirical, 'Whether formal procedural constraint serves neutral rule-of-law coordination or entrenches incumbent power.').

omega_variable(
    sibling_reading_foreclosure_vs_coexistence,
    'Does the positivist reading''s core premise — that formal procedure, not moral content or historical intent, determines validity — logically foreclose originalism and living constitutionalism, or can these readings coexist as different institutional interpretive frameworks?',
    'Philosophical analysis: does ''validity derives from formal procedure'' entail ''NOT from original intent'' and ''NOT from evolving meaning''? Or can originalists claim their historical reconstruction is part of the formal text''s interpretation, and living constitutionalists claim textual principles evolve? If the premises can be disambiguated, coexistence is possible; if they directly contradict, foreclosure holds.',
    'If foreclosure: positivism represents a fundamental epistemic break from originalism and living constitutionalism, suggesting the constraint is incommensurable with the other readings. If coexistence: the three readings are different emphases within a shared framework, and the constraint''s classification might depend on institutional context (which reading dominates in which court).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_vs_coexistence, conceptual, 'Whether positivism''s core premise logically excludes sibling readings or permits institutional coexistence.').

omega_variable(
    substantive_justice_exclusion_internalization,
    'Among marginalized constituencies experiencing high d (near-target extraction), how much of their apparent acceptance of the constraint reflects genuine belief in formal proceduralism versus internalized suppression — the belief that justice through courts is impossible, even when formal authorization is technically available?',
    'Post-amendment observational data: if a constituency''s formal access to constitutional remedy improves (e.g., through Fourteenth Amendment ratification, Nineteenth Amendment, recent equal-protection victories), does their resistance to the positivist constraint increase? Rising resistance post-authorization would indicate prior internalization; stable acceptance would indicate genuine proceduralist belief.',
    'If internalized suppression dominates, the measured suppression (0.58) understates the constraint''s effective extraction on these constituencies — they carry the constraint even after formal authorization improves. If genuine proceduralism, the suppression measure is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substantive_justice_exclusion_internalization, empirical, 'Whether marginalized constituencies'' acceptance of formal procedural constraint reflects belief or internalized suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__positivist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(us_c_tr_t8, us_constitution_text__positivist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(us_c_tr_t16, us_constitution_text__positivist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_text__positivist_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(us_c_tr_t32, us_constitution_text__positivist_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_text__positivist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_text__positivist_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__positivist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(us_c_be_t8, us_constitution_text__positivist_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(us_c_be_t16, us_constitution_text__positivist_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(us_c_be_t24, us_constitution_text__positivist_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(us_c_be_t32, us_constitution_text__positivist_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(us_c_be_t40, us_constitution_text__positivist_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(us_c_be_t50, us_constitution_text__positivist_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__positivist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(us_c_su_t8, us_constitution_text__positivist_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(us_c_su_t16, us_constitution_text__positivist_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(us_c_su_t24, us_constitution_text__positivist_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(us_c_su_t32, us_constitution_text__positivist_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(us_c_su_t40, us_constitution_text__positivist_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(us_c_su_t50, us_constitution_text__positivist_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The US Constitution kernel decomposes into three structurally distinct constraints: positivist_reading (validity from formal procedure), originalist_reading (validity from original public meaning), and living_constitutionalist_reading (validity from evolving interpretation). The ε-invariance principle requires separate stories because measuring constitutional validity through the lens of formal procedure yields fundamentally different extraction structures than measuring through historical intent or adaptive meaning. Each reading produces a different beneficiary/victim structure and different ε. Positivism extracts from substantive-justice claimants locked out by textual silence. Originalism extracts from those whose concerns post-date the Founding. Living constitutionalism extracts from those seeking interpretive stability. Network links capture the logical and institutional dependencies: positivism and originalism both resist evolutionary interpretation (but for different reasons); living constitutionalism resists both procedural and historical constraints. All three coexist as competing institutional interpretive schools.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
