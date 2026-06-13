% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Marriage Sacrament: Hierarchical Indissolubility Reading
 *   domain: religious/institutional/political
 *
 * SUMMARY:
 *   The Roman Catholic Church's sacramental teaching on marriage holds that a
 *   valid marriage is an ontological reality — a reality in being, not a
 *   linguistic or legal construct — and that this reality cannot be dissolved
 *   by any human power, not even by the Church itself. Under the hierarchical
 *   indissolubility reading, a divorced Catholic who remarries without an
 *   annulment of the first marriage commits adultery by the act of
 *   remarriage, and is excluded from Eucharist and other sacraments until the
 *   first marriage is declared null. This reading treats indissolubility as a
 *   constitutive fact about marriage itself, not as an aspirational ideal
 *   subject to pastoral mercy in individual cases. The constraint operates
 *   through a tribunal system that adjudicates whether prior marriages were
 *   'validly' contracted, a process that often produces verdicts at odds with
 *   the lived experience of the divorced person. The hierarchical reading is
 *   ONE reading of the contested kernel 'marriage_sacrament'; the sibling
 *   reading 'civic_pastoral_reading' treats marriage as a pastoral
 *   relationship subject to human failure and permits sacramental inclusion
 *   of divorced Catholics through a process of discernment rather than
 *   tribunal judgment.
 *
 * KEY AGENTS:
 *   - institutional_magisterium: Hierarchical authority setting and enforcing the indissolubility doctrine; d near 0.0 (full beneficiary, controls the rules)
 *   - divorced_catholics_seeking_remarriage: Primary victims (excluded from sacraments, trapped in tribunal apparatus); d near 1.0 (full target)
 *   - parish_clergy: Agenda-setters enforcing sacramental denial; identity-locked to institutional role; d near 0.3 (partially captured by the institution they serve)
 *   - tribunal_personnel: Adjudicate validity; caught between doctrine and doubt; d near 0.4 (execute the constraint but question its coherence)
 *   - validly_married_catholics: Incidental beneficiaries (institutional reinforcement of their marriage); d near 0.1 (low extraction, high benefit)
 *   - progressive_theologians: Excluded voices advocating pastoral alternative; d=0.5 (analytical, outside the mechanism)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.68).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.72).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Marriage Sacrament: Hierarchical Indissolubility Reading").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious/institutional/political").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '210afee3-2603-42e5-b42e-3b718028c7fa').
narrative_ontology:cs_kernel_codification('210afee3-2603-42e5-b42e-3b718028c7fa', formalized).
narrative_ontology:cs_authority_grounding('210afee3-2603-42e5-b42e-3b718028c7fa', extraction).
narrative_ontology:cs_interpretation_layer_present('210afee3-2603-42e5-b42e-3b718028c7fa').
narrative_ontology:cs_reading_relation('210afee3-2603-42e5-b42e-3b718028c7fa', marriage_sacrament__civic_pastoral_reading, influences).
narrative_ontology:cs_axiom('210afee3-2603-42e5-b42e-3b718028c7fa', foundational, marriage_ontological_indissolubility).
narrative_ontology:cs_axiom_status(marriage_ontological_indissolubility, holdable).
narrative_ontology:cs_axiom_grounding('210afee3-2603-42e5-b42e-3b718028c7fa', marriage_ontological_indissolubility, deontological).
narrative_ontology:cs_axiom('210afee3-2603-42e5-b42e-3b718028c7fa', foundational, hierarchical_adjudication_required).
narrative_ontology:cs_axiom_status(hierarchical_adjudication_required, holdable).
narrative_ontology:cs_axiom_grounding('210afee3-2603-42e5-b42e-3b718028c7fa', hierarchical_adjudication_required, conventional).
narrative_ontology:cs_reference_frame('210afee3-2603-42e5-b42e-3b718028c7fa', immutable_sacramental_indissolubility).
narrative_ontology:cs_drift_state('210afee3-2603-42e5-b42e-3b718028c7fa', contemporary_reformed_catholicism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('210afee3-2603-42e5-b42e-3b718028c7fa', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, institutional_magisterium).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint systematically denies sacramental access to a named victim class (divorced Catholics) and conditions reinstatement on navigation of a costly, time-consuming, often-futile tribunal process. The extraction is not incidental to coordination — it is the enforcement mechanism itself. Suppression is similarly high (0.72) because the constraint operates by denying access to fundamental goods (sacraments) that are constitutive of Catholic ritual life, and by treating the divorced person's own account of their marriage ('it ended') as ontologically false ('it never validly existed'). The suppression is internalized through doctrinal claim (the reading asserts an ontological fact divorced Catholics are supposed to accept) and structural (sacramental exclusion is the material enforcement). Theater ratio is moderate (0.41) because the tribunal process appears to engage in adjudication (decision-making, weighing grounds, issuing judgments) but increasingly serves to ratify the constraint rather than to discover truth about marriage validity. The rising theater ratio over the interval (0.28 → 0.41) reflects the growing awareness that annulment procedures bear little relationship to why marriages actually end, and that the constraint persists more through institutional inertia and identity investment than through genuine sacramental coherence. The accessibility_collapse metric (0.58) reflects that alternatives do exist (civil divorce is legally recognized; other Christian traditions permit remarriage; leaving the faith is possible) but are all costly: they require abandoning a religious identity, community, and spiritual framework that may have been lifelong. The resistance metric (0.71) reflects substantial active resistance from progressive theologians, remarried Catholics, and even sympathetic clergy, plus the structural pressure of civil law and ecumenical practice — the constraint persists not because resistance is absent but because institutional authority overrides it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (divorced Catholics, identity-locked clergy) and the agenda-setter seat (magisterium) should compute dramatically differently. From the magisterium's institutional position, the reading constitutes a genuine coordination function: it defends the permanence of the marital covenant and provides a coherent framework for sacramental practice. From the divorced Catholic's position, the same structure operates as institutional extraction: the denial of sacraments is experienced as punishment for a life-circumstance (divorce) framed as sin, and the tribunal apparatus is experienced as a gatekeeping mechanism whose verdicts bear little relationship to lived reality. The identity-locked clergy sit in a liminal position: they are partially captured by institutional role (their identity is constituted through priesthood) and partially sympathetic to the payer position (they witness the pastoral harm of sacramental exclusion). These structural differences should produce different classifications when computed per-seat. The engine's job is to detect this divergence from the structural data — the hierarchical reading's claim of coordination conceals extraction that becomes visible when directionality is computed from the victim set and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation from structural data: (1) The institutional magisterium is the declared beneficiary (controls the definition, sets the agenda, collects institutional loyalty and authority); it has institutional power, civilizational time horizon, and analytical exit (it is not trapped — it can change the doctrine at any time and did so incrementally through Vatican II and Pope Francis reforms). Its d should derive to the beneficiary end (0.0–0.2). (2) Divorced Catholics are the declared victims (explicitly excluded from sacraments, bear tribunal costs, trapped by identity-lock to Catholic faith and community); they have powerless power, biographical time horizon, and trapped exit (leaving the faith means abandoning spiritual identity and community). Their d should derive to the target end (0.8–1.0). (3) Parish clergy have secondary role as both agenda-setters (they enforce) and payers (they are bound by doctrine they often doubt); they have organized power, biographical time horizon, and identity-locked exit (priesthood is their professional and spiritual identity). Their d should derive to 0.3–0.5 (partially captured, partially sympathetic to victims). (4) Tribunal personnel have moderate power, biographical horizon, constrained exit (they work within the system but can exit; their identity is professional rather than ontologically bound). Their d should derive to 0.4–0.5. No directionality overrides are needed — the derivation chain produces structurally coherent values.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is present and structurally clear. The founding problem (preserve marital permanence and provide authoritative adjudication) was originally live and meaningful: when civil divorce laws were emerging and local pastoral variation was creating confusion, a coherent hierarchical doctrine served a genuine coordination function. The founding problem is now dead or substantially transformed. (1) Marital permanence is not threatened by anyone except the person in the marriage — civil law has solved the coordination problem by recognizing divorce as a legal fact and requiring adjudication by civil courts; hierarchical insistence that ontologically the marriage persists is no longer required to protect the permanence-ideal because civil permanence is not the goal. (2) Authoritative adjudication is undermined by the fact that the tribunal apparatus's verdicts (declarations that marriages 'never validly existed') are increasingly recognized as metaphysical claims disconnected from the actual reasons marriages end — the founding problem of settling local pastoral confusion is solved better by pastoral guidelines than by tribunal metaphysics. (3) The constraint persists despite the founding problem's death because it serves extractive functions the original mandate did not: it maintains clerical authority over the sacramental boundary, preserves institutional control over marital status, extracts institutional loyalty from the divorced by conditioning sacramental access on compliance with annulment, and vindicates the proposition that only hierarchical authority can adjudicate marriage validity. The theater ratio's rise (0.28 → 0.41) is evidence of mandatrophy: as the coordination function atrophies, the constraint's persistence relies increasingly on performative maintenance (tribunal procedure) rather than genuine function. The measurement series shows base_extractiveness and suppression_requirement both rising while the founding problem atrophies — classic mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (sacramental exclusion as external barrier) or internalized (divorced Catholics accepting the teaching that they are living in sin)?',
    'Post-exit observation: track divorced Catholics who leave the church and remarry — if suppression lifts structurally but persists internally, the component is internalized; study of testimony from divorced Catholics weighing remarriage vs. sacramental life reveals the proportion each component weighs in the decision.',
    'If suppression is substantially internalized, the effective suppression is higher than the structural measure suggests — the target carries the constraint with them after exit, limiting the exit option''s real value. This would support reclassification from tangled_rope toward snare. If primarily structural, the trap is the sacramental exclusion itself; leaving eliminates the suppression cleanly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of divorced Catholics is structural exclusion or internalized doctrinal acceptance.').

omega_variable(
    ontological_vs_pastoral_incommensurability,
    'Is the hierarchical reading''s claim that marriage is an ontological reality a different claim from the pastoral reading''s claim that indissolubility is an ideal, or are they the same claim framed differently?',
    'Formal logical analysis: if the readings entail incompatible practical conclusions about the same marriage (one says the marriage persists ontologically, the other says it may be pastorally released), they are incommensurable readings. If they entail the same practical conclusion (sacraments are withheld until annulment or pastoral release) through different metaphysical grounds, they are coherent variations.',
    'If incommensurable, the readings genuinely foreclose each other — a framework cannot hold both (change relation from coexists_with to forecloses). If they are coherent variations, they coexist and influence but do not foreclose each other. The incommensurability is fundamental: the hierarchical reading treats indissolubility as a property of the marriage itself (real, objective, unchosen); the pastoral reading treats it as an aspiration the church encourages (ideal, subjective, chosen).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_pastoral_incommensurability, conceptual, 'Whether ontological and pastoral framings of indissolubility are logically incommensurable or coherent variations.').

omega_variable(
    founding_problem_death_vs_transformation,
    'Has the founding problem (establish marital permanence and provide authoritative adjudication) died, or has it been transformed into a different problem (pastoral inclusion without tribunal)?',
    'Compare the original problem (18th–19th century: divorce laws emerging, local pastoral variation causing confusion) with contemporary problem (21st century: remarriage is a lived reality for ~50% of Catholics who divorce; tribunal apparatus is experienced as bureaucratic obstacle rather than sacramental protection). If the original problem is replaced rather than solved, the constraint is mandatrophic (persist despite founding problem death).',
    'If the founding problem is dead, the constraint is clearly mandatrophic and the extracted extraction should be reclassified as pure rent-seeking. If transformed, the constraint might retain a weaker coordination function (pastoral guidance rather than tribunal metaphysics) and would be less clearly mandatrophic. The measurement series (base_extractiveness and suppression rising while theater rises) suggests the founding problem is dead, not transformed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_death_vs_transformation, empirical, 'Whether the founding problem persists, has died, or has been transformed into a different problem.').

omega_variable(
    tribunal_verdicts_and_truth,
    'Do tribunal verdicts declaring marriages ''null'' track the actual reasons marriages end, or do they ratify a predetermined conclusion that no valid marriage existed?',
    'Audit of tribunal decisions and the grounds cited: if most null verdicts rest on grounds (e.g., ''lack of due discretion,'' ''excluded indissolubility'') that bear no relationship to the stated reason for dissolution (infidelity, incompatibility, abandonment), the tribunal is a ratification apparatus rather than an adjudicative one. If grounds track the reason, the tribunal is genuinely evaluating validity.',
    'If the tribunal is ratification apparatus, the verdicts are performative (theater) rather than truth-seeking. This supports both the theater_ratio assessment and the mandatrophy analysis: the constraint persists through procedure without genuine function. If grounds track reasons, the tribunal might have a genuine adjudicative role (though still extractive through costs and delays).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_verdicts_and_truth, empirical, 'Whether tribunal verdicts discover marriage validity or ratify a predetermined institutional position.').

omega_variable(
    committer_frame__hierarchical_vs_pastoral_readings,
    'This constraint is one reading of a contested kernel. How do the hierarchical and pastoral readings of marriage_sacrament differ structurally in their ε values, victim sets, and enforcement mechanisms?',
    'Comparison of this story (hierarchical_indissolubility_reading, ε=0.68, victims=divorced_catholics, enforcement=sacramental exclusion) with sibling story (civic_pastoral_reading, expected ε=0.2–0.3, victims=none or minimal, enforcement=pastoral guidance). The structural delta is precisely what the kernel contest turns on.',
    'The high ε of the hierarchical reading vs. the low ε of the pastoral reading demonstrates that the readings are not equivalent framings — they have fundamentally different extractiveness profiles. The choice between readings is not neutral; it determines whether divorced Catholics are victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame__hierarchical_vs_pastoral_readings, conceptual, 'Kernel contest: hierarchical reading is extractive (ε=0.68, victims, enforcement); pastoral reading is coordinative (ε~0.2–0.3, no victims, guidance). The readings are not empirically equivalent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__hierarchical_indissolubility_reading, 0.12).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% marriage_sacrament kernel has two readings: hierarchical_indissolubility_reading (this story, ε=0.68, tangled_rope) and civic_pastoral_reading (sibling story, ε~0.2–0.3, rope/scaffold). The readings coexist in institutional tension. The hierarchical reading frames indissolubility as ontological and enforces it through tribunal gatekeeping; the pastoral reading frames it as an ideal requiring mercy and permits sacramental inclusion through pastoral discernment. They influence each other: papal reforms moving toward the pastoral reading constrain how strictly the hierarchical reading can be enforced, and the hierarchical reading's costs drive demand for the pastoral alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
