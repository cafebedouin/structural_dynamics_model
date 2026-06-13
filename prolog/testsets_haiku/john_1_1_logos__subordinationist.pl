% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-07-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Logos Christology: Created Agent, Not Co-Eternal Divine
 *   domain: theology/hermeneutics/christology
 *
 * SUMMARY:
 *   John 1:1-14 presents the Logos ('Word') in relation to God the Father and
 *   describes its incarnation in Jesus. The Subordinationist reading
 *   interprets Logos as a created being — the first and highest creation of
 *   the Father, but not co-eternal or of identical substance (homoousios).
 *   This reading was live in early Christian debates and was formally
 *   condemned as heresy by the Council of Nicaea (325) and subsequent
 *   councils. It remains a live interpretive option in modern biblical
 *   scholarship and is actively maintained by some non-mainstream Christian
 *   communities and independent readers. The constraint operates by
 *   controlling which readings of John 1:1 are permitted in institutional
 *   worship, theological education, and ecclesiastical authority. The Nicene
 *   councils and their institutional successors enforce the orthodox
 *   (non-subordinationist) reading through anathema, suppression of
 *   subordinationist texts, exclusion from sacramental participation, and
 *   monopoly over authoritative interpretation.
 *
 * KEY AGENTS:
 *   - Monarchian_interpreters: Theological communities reading Logos subordination from John 1:1, avoiding trinitarian complexity; organized but regionally constrained.
 *   - Nicene_orthodox_traditions: Catholic, Orthodox, mainstream Protestant institutional churches whose sacramental authority depends on Logos full divinity; institutional power, identity-locked, civilizational scope.
 *   - High_church_sacramental_authority: Priesthoods and bishops claiming eucharistic efficacy grounded in Christ's divinity; institutional power, identity-locked, defending through enforcement.
 *   - Non-Chalcedonian_traditions: Oriental Orthodox communities using subordinationist exegesis to resist Niceno-Chalcedonian hegemony; organized, regionally distributed.
 *   - Nicene_councils_enforcement: Ecumenical councils and their dogmatic successors setting and enforcing orthodoxy; institutional agenda-setter, civilizational scope.
 *   - Independent_biblical_scholars: Modern Johannine scholars working in secular/ecumenical contexts; powerful, arbitrage exit, global scope; analytical observers.
 *   - Early_Christian_communities: First and second century readers of John; powerless, excluded from modern interpretation debates, historical voice silenced.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.58).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.72).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Logos Christology: Created Agent, Not Co-Eternal Divine").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, '0ddd608e-a3e7-4113-a631-281ada30ce86').
narrative_ontology:cs_kernel_codification('0ddd608e-a3e7-4113-a631-281ada30ce86', fixed_text).
narrative_ontology:cs_authority_grounding('0ddd608e-a3e7-4113-a631-281ada30ce86', lineage).
narrative_ontology:cs_interpretation_layer_present('0ddd608e-a3e7-4113-a631-281ada30ce86').
narrative_ontology:cs_reading_relation('0ddd608e-a3e7-4113-a631-281ada30ce86', john_1_1_logos__orthodox_christological, coexists_with).
narrative_ontology:cs_reading_relation('0ddd608e-a3e7-4113-a631-281ada30ce86', john_1_1_logos__non_incarnational_monotheist, influences).
narrative_ontology:cs_axiom('0ddd608e-a3e7-4113-a631-281ada30ce86', foundational, logos_is_created_being).
narrative_ontology:cs_axiom_status(logos_is_created_being, holdable).
narrative_ontology:cs_axiom_grounding('0ddd608e-a3e7-4113-a631-281ada30ce86', logos_is_created_being, empirically_contingent).
narrative_ontology:cs_axiom('0ddd608e-a3e7-4113-a631-281ada30ce86', foundational, logos_subordinate_not_cosubstantial).
narrative_ontology:cs_axiom_status(logos_subordinate_not_cosubstantial, holdable).
narrative_ontology:cs_axiom_grounding('0ddd608e-a3e7-4113-a631-281ada30ce86', logos_subordinate_not_cosubstantial, empirically_contingent).
narrative_ontology:cs_reference_frame('0ddd608e-a3e7-4113-a631-281ada30ce86', johannine_logos_as_distinct_agent).
narrative_ontology:cs_drift_state('0ddd608e-a3e7-4113-a631-281ada30ce86', post_nicene_council_enforcement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0ddd608e-a3e7-4113-a631-281ada30ce86', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, monarchian_interpreters).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, anti_nicene_theologians).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, non_chalcedonian_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, nicene_orthodox_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_sacramental_authority).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, trinitarian_councils).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__subordinationist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is CLAIMED as Tangled Rope because it combines two structural features: (1) a genuine coordination function — solving the interpretive problem of reconciling John's language with monotheistic theology through a literalist reading of Logos-subordination, which benefits monarchian interpreters and non-Chalcedonian traditions who gain scriptural authority and autonomy; (2) asymmetric extraction — the Nicene councils and their institutional successors must actively suppress subordinationist readings to maintain their authority, imposing costs on high-church traditions (who must continuously defend against the reading's logical coherence), on early Christian historical voice (which is censored), and on independent scholars (who work under institutional pressure to align with orthodoxy). Extractiveness is moderate-high (0.58) because the constraint's persistence depends on institutional enforcement, not participant preference — councils and magisteria continuously defend against subordinationism, which they would not do if the reading were obviously incoherent. Suppression is high (0.72) and rising in modern contexts (2025 = 0.72) because subordinationist Christology resurfaces in textual scholarship and must be managed through scholarly consensus-building and institutional boundary-maintenance. Theater is moderate (0.41) because a significant portion of enforcement activity is rhetorical: defending the councils' historical legitimacy, asserting 'apostolic tradition,' and framing subordinationism as illogical, rather than materially preventing its utterance. The measurement series track enforcement intensity from the Nicene settlement (T=325) through institutional consolidation (Chalcedon, 451), doctrinal ossification (Schism, 1054), Protestant reformation (1517), Enlightenment pressures (1800), mid-20th-century scholarly revival of subordinationist exegesis (1950), and contemporary academic context (2025). Extractiveness stays roughly flat after the Reformation because neither Protestants nor Catholics can afford to cede the Logos-divinity claim. Suppression decreases from 1517–1950 as institutional enforcement loosens in secular contexts, then rises again after 1950 as academic Johannine scholarship explicitly engages subordinationist readings and councils' successors must defend coherence more carefully. Theater remains high in modern contexts because much enforcement is now discursive (scholarly articles, theological treatises, conference dominance) rather than institutional penalty.
 *
 * PERSPECTIVAL GAP:
 *   The nicene_orthodox_traditions and high_church_sacramental_authority seats compute this as pure coordination (a necessary constraint grounding sacramental efficacy) — from their position, the reading is simply false and its suppression is truth-keeping, not extraction. The monarchian_interpreters and non_chalcedonian_traditions seats compute it as tangled_rope (coordination function is real, but extraction is sustained by institutional power) — from their position, the constraint suppresses an equally valid reading to maintain institutional revenue and authority. Independent_biblical_scholars compute it as both, or suspended between: they observe that the constraint operates through institutional suppression even while acknowledging that Logos-subordination has genuine exegetical support. The claim/metric gap is intentional: the constraint is CLAIMED as Tangled Rope (the author's structural judgment that both coordination and extraction are present) while acknowledging that the orthodox seats would claim it as Mountain (natural law of correct reading) or pure Rope (genuine coordination without extraction). The engine computes per-seat, revealing this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Monarchian interpreters (beneficiary, organized, constrained exit: d ≈ 0.25) gain exegetical authority and scriptural literalism but remain institutionally constrained and regionally isolated. High-church sacramental authority (victim, institutional, identity-locked: d ≈ 0.85) bears the cost of continuous defense and doctrinal boundary-maintenance; they cannot exit without reforming their entire institutional identity, so their directionality is pushed toward the target end. Nicene councils (agenda_setter, institutional, analytical exit: d ≈ 0.50) set the rule but derive their legitimacy from it — if subordinationism were permitted, their authority would dissolve, making their 'exit' effectively trapped despite analytical classification. The councils' apparent institutional power masks a structural dependency on the constraint itself. Non-Chalcedonian traditions (beneficiary, organized, constrained exit: d ≈ 0.20) benefit from subordinationist arguments as ammunition but are not the primary targets of suppression. The overall picture: the constraint extracts from high-church traditions (who pay in institutional stress and doctrinal vigilance) and transfers that extraction to the councils' institutional legitimacy, while beneficiaries (monarchian interpreters) enjoy exegetical authority but remain marginalized. This is not a simple power asymmetry; it is an identity-lock asymmetry — the victims cannot exit without becoming different institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (4th-century Arian controversy, settling Christ's ontological status) is LIVE in one sense: the constraint still operates, councils still enforce Nicene orthodoxy, subordinationism is still suppressed. But in another sense, the founding problem is DEAD: modern Christendom no longer faces a unified Arian political threat, Christian communities have fragmented denominationally, and subordinationism is now sustained by small-scale exegetical communities, not by organized military or political forces. The constraint persists as institutional theater and historical inertia: the councils' authority depends on having settled this question 1700 years ago, and their institutional successors must continue to enforce Nicene orthodoxy to claim continuity. If the founding problem is dead (no unified Arian threat), the constraint becomes a Piton — a degraded enforcer of an obsolete boundary, maintained partly by rhetorical performance and partly by genuine institutional identity-fusion (to be Nicene Orthodox is to affirm Logos divinity; rejecting it means leaving the institution). The mandatrophy surfaces in the gap between founding_problem_status (contested: did 4th-century councils solve a real problem or create doctrinal hegemony?) and disappearance_verdict (world_rearranges: removing the constraint would alter ecclesiology and sacramental claims). A live founding problem + rearranging world = genuine Tangled Rope. A dead founding problem + rearranging world = Piton masquerading as Rope. The measurement series track this: suppression_requirement rises again after 1950 (projected to 0.72 by 2025) because academic scholarship revived subordinationist exegesis as a live historical option, forcing councils' successors to defend the orthodoxy more actively. This late-stage enforcement surge suggests institutional theater (defending a historical victory rather than meeting a live threat), pushing the constraint toward Piton classification. The author's claim of Tangled Rope rests on the judgment that the coordination function (solving the Logos-ontology problem) is real enough to distinguish from pure extraction, and that the constraint's benefits to orthodox traditions (doctrinal coherence, institutional unity, sacramental authority) are genuine even if the founding threat is diminished. A different analyst might read the same data as Piton (theater_ratio 0.41 is substantial; suppression is actively renewed; beneficiaries could survive without the constraint but don't want to admit it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apostolic_tradition_vs_councils_innovation,
    'Does Nicene orthodoxy (Logos full divinity, homoousios) represent recovery of apostolic tradition, or does it represent doctrinal innovation by the councils?',
    'Textual-historical analysis of pre-Nicene Christian writings (Clement, Ignatius, Justin, Origen, Tertullian) to determine whether subordinationist or fully-divine Christologies dominated early tradition. Modern scholarship (Ehrman, Kelly, Norris) has documented that pre-Nicene Christianity was more theologically diverse than post-Nicene histories admit, with subordinationism present and defended.',
    'If councils innovated (rather than recovered tradition), their legitimacy erodes and subordinationism becomes ''equally apostolic.'' If Nicene orthodoxy was apostolic, councils confirmed something always-present and subordinationism was always heretical. This determines whether the constraint is defending true doctrine or enforcing institutional innovation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(apostolic_tradition_vs_councils_innovation, empirical, 'Whether Nicene orthodoxy was apostolic recovery or institutional innovation by the councils.').

omega_variable(
    john_1_1_grammatical_ambiguity,
    'Does John 1:1c (''and the Word was God'') grammatically and semantically permit a subordinationist reading (Logos as divine but not fully God), or does it logically entail full identity with God?',
    'Detailed grammatical analysis of Greek theos without article (anarthrous predicate noun) in John 1:1c and parallel constructions in John''s Gospel and early Christian literature. Does lack of article necessarily entail identity or merely predication of a quality? Expert linguistic consensus on Johannine grammar.',
    'If the grammar permits subordinationism, the constraint''s suppression of the reading is arbitrary institutional preference, not textual truth-keeping. If the grammar entails full divinity, subordinationism is indeed exegetically indefensible and the constraint is truth-preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(john_1_1_grammatical_ambiguity, empirical, 'Whether John 1:1c grammatically entails or merely permits full Logos divinity.').

omega_variable(
    institutional_identity_fusion_suppression,
    'To what extent is the suppression of subordinationist readings structural (enforced through material exclusion and institutional penalties) versus internalized (the victims have fused their identity with Nicene orthodoxy such that subordinationism is perceived as existentially unthinkable)?',
    'Post-suppression trajectory analysis: if high-church traditions abandon Nicene Christology and were suddenly permitted to teach subordinationism, would they choose to do so? If not, suppression is largely internalized identity-fusion, not structural coercion. Ethnographic or interview data from non-Chalcedonian communities or independent scholars who have adopted subordinationist readings after initial institutional resistance.',
    'If suppression is internalized, the measured suppression metric (0.72) understates the constraint''s actual psychological hold. The victims carry the suppression with them even in contexts permitting subordinationism. If structural, the suppression metric reflects real enforcement and loosening in secular contexts should release subordinationist theology rapidly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_fusion_suppression, empirical, 'Whether suppression is structural institutional penalty or internalized identity-fusion.').

omega_variable(
    subordinationism_logical_viability,
    'Is subordinationist Christology logically viable on its own terms, or does it collapse into internal contradiction?',
    'Systematic reconstruction of subordinationist metaphysics: can a created Logos be ''God'' in some meaningful sense without collapsing into tritheism or denying Logos divinity entirely? Comparison to medieval and modern subordinationist theology (Jehovah''s Witnesses, Latter-day Saint theology, some Adventist positions). Do these traditions maintain logical consistency, or do they conceal unresolved tensions?',
    'If subordinationism is logically viable, the constraint''s enforcement looks like institutional preference enforcement rather than heresy suppression. If it collapses into contradiction, the constraint is protecting against a position that refutes itself, and enforcement is truth-keeping. This determines whether the constraint is Tangled Rope (coordination + extraction) or Mountain (natural logical necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinationism_logical_viability, conceptual, 'Whether subordinationist Christology is internally coherent or logically self-defeating.').

omega_variable(
    kernel_reading_boundary,
    'Is the Subordinationist reading a distinct interpretation of a single shared kernel (John 1:1-14), or is it a reading of a different kernel (e.g., early Christian pre-Nicene belief, Arian theological tradition) that only appears to be about John 1:1?',
    'Historical genealogy of subordinationist exegesis: did subordinationists read John''s text directly, or did they read John through Arian metaphysical commitments? Did their reading of John evolve as councils established orthodoxy, or did it become frozen in reactive defense? If subordinationism is a theological tradition secondarily reading John (rather than John''s text generating subordinationism), the readings are not siblings of the same kernel — they are distinct kernels.',
    'If subordinationism is a reading OF John 1:1, it is a legitimate sibling of orthodox and non-incarnational readings. If subordinationism is a theological tradition USING John 1:1, the kernel contest is between different theological systems, not between exegetical options. This affects the classification: readings of the same kernel can coexist or foreclose each other; distinct kernels relate through network influence, not reading relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether subordinationism is a reading of John 1:1 or a theological tradition secondarily reading John.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t325, john_1_1_logos__subordinationist, theater_ratio, 325, 0.18).
narrative_ontology:measurement(john_tr_t451, john_1_1_logos__subordinationist, theater_ratio, 451, 0.28).
narrative_ontology:measurement(john_tr_t1054, john_1_1_logos__subordinationist, theater_ratio, 1054, 0.34).
narrative_ontology:measurement(john_tr_t1517, john_1_1_logos__subordinationist, theater_ratio, 1517, 0.38).
narrative_ontology:measurement(john_tr_t1800, john_1_1_logos__subordinationist, theater_ratio, 1800, 0.4).
narrative_ontology:measurement(john_tr_t1950, john_1_1_logos__subordinationist, theater_ratio, 1950, 0.41).
narrative_ontology:measurement(john_tr_t2025, john_1_1_logos__subordinationist, theater_ratio, 2025, 0.41).

% Extraction over time
narrative_ontology:measurement(john_be_t325, john_1_1_logos__subordinationist, base_extractiveness, 325, 0.42).
narrative_ontology:measurement(john_be_t451, john_1_1_logos__subordinationist, base_extractiveness, 451, 0.51).
narrative_ontology:measurement(john_be_t1054, john_1_1_logos__subordinationist, base_extractiveness, 1054, 0.55).
narrative_ontology:measurement(john_be_t1517, john_1_1_logos__subordinationist, base_extractiveness, 1517, 0.53).
narrative_ontology:measurement(john_be_t1800, john_1_1_logos__subordinationist, base_extractiveness, 1800, 0.56).
narrative_ontology:measurement(john_be_t1950, john_1_1_logos__subordinationist, base_extractiveness, 1950, 0.59).
narrative_ontology:measurement(john_be_t2025, john_1_1_logos__subordinationist, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t325, john_1_1_logos__subordinationist, suppression_requirement, 325, 0.65).
narrative_ontology:measurement(john_su_t451, john_1_1_logos__subordinationist, suppression_requirement, 451, 0.74).
narrative_ontology:measurement(john_su_t1054, john_1_1_logos__subordinationist, suppression_requirement, 1054, 0.76).
narrative_ontology:measurement(john_su_t1517, john_1_1_logos__subordinationist, suppression_requirement, 1517, 0.71).
narrative_ontology:measurement(john_su_t1800, john_1_1_logos__subordinationist, suppression_requirement, 1800, 0.68).
narrative_ontology:measurement(john_su_t1950, john_1_1_logos__subordinationist, suppression_requirement, 1950, 0.64).
narrative_ontology:measurement(john_su_t2025, john_1_1_logos__subordinationist, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:boltzmann_floor_override(john_1_1_logos__subordinationist, 0.12).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% The john_1_1_logos constraint family decomposes John 1:1-14 into three structurally distinct claims: (1) non_incarnational_monotheist — Logos is wisdom/speech, not hypostatic agent, ε ≈ 0.15, Mountain; (2) orthodox_christological — Logos is fully divine, cosubstantial, incarnate, ε ≈ 0.35, Rope/tangled_rope; (3) subordinationist (this story) — Logos is created/subordinate agent, ε ≈ 0.58, Tangled Rope. Each reading instantiates a different constraint because they have different ε-values (measuring how much each reading extracts from high-church traditions, differs from scholarly consensus, and requires institutional enforcement), different beneficiary/victim structures, and different classifications. The three readings are not observables of a single constraint; they are siblings inhabiting a single kernel text. Each has its own story_uid and provenance. They are linked via network.affects_constraints to model their conceptual kinship and institutional competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(john_1_1_logos__subordinationist, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
