% ============================================================================
% CONSTRAINT STORY: bounded_empathy_as_stabilizing_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bounded_empathy_as_stabilizing_mechanism, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bounded_empathy_as_stabilizing_mechanism
 *   human_readable: Bounded Empathy as a Stabilizing Mechanism in Adjudication
 *   domain: bureaucratic/institutional/procedural
 *
 * SUMMARY:
 *   This constraint follows downstream from
 *   categorical_nonexistence_as_soft_denial: where the upstream constraint
 *   describes the institution's category scheme failing to recognize certain
 *   claims at all, this constraint describes what happens inside the
 *   adjudicator who processes those claims anyway. The adjudicator's empathy
 *   is not fake — it is exercised diligently, held internally as a real
 *   weight, and expressed within an exact procedural shape ('sorry in the
 *   specific and limited way the Table's grammar permitted'). The 'held
 *   breath' moment is the observable: an internal monologue explicitly
 *   measuring feeling against procedural necessity, moment by moment, case by
 *   case. The genuineness of the feeling is precisely what makes the
 *   mechanism stable — a visibly callous adjudicator would generate
 *   resistance and reform pressure, while a genuinely empathetic one whose
 *   sympathy is nonetheless bounded generates legitimacy without generating
 *   pressure on the categories. The coordination function (consistent,
 *   humane, high-volume adjudication) and the extraction function (absorbing
 *   petitioners' unclassifiable grievances into felt-but-inert sorrow) run
 *   through the identical psychological mechanism, which is why this is
 *   tangled rope and not a clean rope or a clean snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bounded_empathy_as_stabilizing_mechanism, 0.61).
domain_priors:suppression_score(bounded_empathy_as_stabilizing_mechanism, 0.58).
domain_priors:theater_ratio(bounded_empathy_as_stabilizing_mechanism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bounded_empathy_as_stabilizing_mechanism, extractiveness, 0.61).
narrative_ontology:constraint_metric(bounded_empathy_as_stabilizing_mechanism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bounded_empathy_as_stabilizing_mechanism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bounded_empathy_as_stabilizing_mechanism, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(bounded_empathy_as_stabilizing_mechanism, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bounded_empathy_as_stabilizing_mechanism, tangled_rope).
narrative_ontology:human_readable(bounded_empathy_as_stabilizing_mechanism, "Bounded Empathy as a Stabilizing Mechanism in Adjudication").
narrative_ontology:topic_domain(bounded_empathy_as_stabilizing_mechanism, "bureaucratic/institutional/procedural").

domain_priors:requires_active_enforcement(bounded_empathy_as_stabilizing_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bounded_empathy_as_stabilizing_mechanism, institutional_continuity).
narrative_ontology:constraint_victim(bounded_empathy_as_stabilizing_mechanism, petitioners_with_unclassifiable_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bounded_empathy_as_stabilizing_mechanism, well_classified_petitioners).
narrative_ontology:constraint_victim(bounded_empathy_as_stabilizing_mechanism, the_adjudicator).
narrative_ontology:constraint_vindicates(bounded_empathy_as_stabilizing_mechanism, procedural_neutrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hears the petitioner, feels something real and unforced, and then measures that feeling against what the Table's grammar allows it to become — an apology of a specific, limited shape, a held breath, a note in the margin that cannot become a finding. The adjudicator experiences their own sympathy as genuine and simultaneously experiences its boundary as non-negotiable; the boundary is not felt as coercion because the adjudicator's professional self is constituted by holding it well. They cannot exit the role without ceasing to be who they professionally are, and the cost they pay is carried silently, case after case, as the specific shape of their sorrow.
narrative_ontology:constraint_stakeholder(bounded_empathy_as_stabilizing_mechanism, the_adjudicator, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(bounded_empathy_as_stabilizing_mechanism, the_adjudicator, payer).

% Is what is preserved every time an adjudicator's empathy stops exactly where procedure requires it to stop. It collects nothing directly and administers nothing itself, but the system's capacity to keep issuing consistent, appealable, precedent-stable rulings depends entirely on empathy never being permitted to become pressure against the categories. It is the abstract good the bounded-empathy mechanism exists to protect.
narrative_ontology:constraint_stakeholder(bounded_empathy_as_stabilizing_mechanism, institutional_continuity, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(bounded_empathy_as_stabilizing_mechanism, institutional_continuity).

% Bring a claim that does not fit the Table's existing categories and receive, in place of a remedy, a felt and witnessed sorrow that changes nothing about the outcome. They experience the adjudicator's genuine sympathy as confirmation that they were heard and simultaneously as proof that being heard does not translate into being classified, recognized, or relieved. Their only path forward is to reshape the claim to fit an existing category, appeal to a body bound by the same grammar, or abandon it.
narrative_ontology:constraint_stakeholder(bounded_empathy_as_stabilizing_mechanism, petitioners_with_unclassifiable_claims, payer,
    powerless, biographical, trapped, local).

% Designs and revises the procedural grammar that defines what an adjudicator's sympathy is permitted to become. Leadership benefits from adjudicators whose empathy is real, because real empathy produces legitimacy and public trust in the process, while bounded empathy ensures that legitimacy never converts into precedent-expanding pressure on the categories leadership must keep stable and predictable across thousands of cases.
narrative_ontology:constraint_stakeholder(bounded_empathy_as_stabilizing_mechanism, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Bring claims that fit existing categories cleanly and receive both the adjudicator's genuine attention and a remedy the procedure is built to deliver. For them the bounded-empathy mechanism looks like functioning, humane process — the felt sympathy and the granted remedy arrive together, and the boundary that constrains empathy elsewhere is invisible to them because it never binds their case.
narrative_ontology:constraint_stakeholder(bounded_empathy_as_stabilizing_mechanism, well_classified_petitioners, beneficiary,
    moderate, biographical, mobile, local).

% Review adjudicators' decisions for procedural conformity, not for the felt quality of the adjudicator's sympathy. They can see the 'sorry in the specific and limited way the Table's grammar permitted' documented in a record but are structurally positioned to evaluate only whether the boundary was correctly held, not whether the boundary itself should exist.
narrative_ontology:constraint_stakeholder(bounded_empathy_as_stabilizing_mechanism, appellate_reviewers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bounded_empathy_as_stabilizing_mechanism, institutional_continuity).
narrative_ontology:fixing_cost_class(bounded_empathy_as_stabilizing_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuine, diligently exercised empathy in adjudicators produces legitimacy, humane treatment, and consistent application of categories across a very large caseload — it solves the real problem of adjudicators becoming either callous automatons or unpredictable, precedent-breaking sympathizers.
% TRANSFER_FUNCTION: Moves the emotional labor of acknowledgment from the institution to the adjudicator, and moves the cost of unclassifiability from the institution's categorical scheme onto petitioners whose claims do not fit — the adjudicator's felt sorrow is transferred to the petitioner as a substitute for remedy.
% ABSENT_VOICES: Petitioners whose claims fall outside the Table's categories are heard in the room but their structural objection — that the categories themselves should expand — has no procedural channel; the adjudicator who might be sympathetic to that objection is bound by the same grammar that produced it.
% DISAPPEARANCE_RATIONALE: If bounded empathy vanished — either replaced by pure mechanical indifference or by empathy unbounded by procedure — the system would rearrange dramatically: unbounded empathy would produce inconsistent, precedent-eroding rulings driven by individual adjudicator sympathy, while pure indifference would collapse the perceived legitimacy that lets the institution keep issuing rulings petitioners generally accept even when they lose.
% FOUNDING_PROBLEM: Adjudicative bodies needed a way to process high case volume consistently while still treating petitioners as persons rather than files, avoiding both bureaucratic callousness and ad hoc sympathetic overreach that would make outcomes unpredictable.
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership and appellate reviewers attest the problem remains live — consistency and legitimacy are ongoing needs. Advocates for petitioners with unclassifiable claims, along with independent procedural-justice scholars outside the institution, attest that the bounded-empathy solution has calcified into a mechanism that specifically protects the categorical scheme from pressure, functioning now as insulation for institutional continuity rather than as a genuine humane counterweight to bureaucratic process.
narrative_ontology:disappearance_verdict(bounded_empathy_as_stabilizing_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(bounded_empathy_as_stabilizing_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bounded_empathy_as_stabilizing_mechanism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-17',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(bounded_empathy_as_stabilizing_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(bounded_empathy_as_stabilizing_mechanism, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bounded_empathy_as_stabilizing_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bounded_empathy_as_stabilizing_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bounded_empathy_as_stabilizing_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61) reflects that unclassifiable petitioners receive acknowledgment without remedy — a real transfer of relief-substitute for relief. Suppression (0.58) is moderate: petitioners are not physically coerced, but the categorical grammar itself forecloses the argument that the categories should expand, and the adjudicator who might otherwise advocate for that expansion is professionally bound not to. Theater ratio (0.42) captures that a meaningful share of the empathy's function is now performative stabilization — visible sorrow substituting for structural change — while accessibility_collapse (0.66) reflects that once a petitioner understands the grammar's limits, alternative avenues (reclassification, appeal within the same grammar, abandonment) are the only remaining options. Resistance (0.47) is moderate: petitioners and some reform-minded observers push back, but the mechanism's legitimacy-generating quality (genuine felt empathy) dampens broader resistance that a visibly indifferent system would attract.
 *
 * PERSPECTIVAL GAP:
 *   From institutional leadership's seat this looks like humane, well-calibrated procedure — exactly the coordination function it was designed to be. From the adjudicator's own seat it is experienced as a private, repeated moral cost: feeling something real and then watching it become procedurally inert. From the unclassifiable petitioner's seat it is confirmation of being heard combined with confirmation that being heard changes nothing. The engine should compute these divergently from the same structural facts — the adjudicator's identity-locked, cost-bearing position produces a different per-seat type than institutional leadership's arbitrage position, even though both stand inside the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional continuity is the structural beneficiary: it is not an actor that collects anything directly, but every case in which empathy is successfully bounded is a case in which the categorical scheme survives unpressured, which is precisely what continuity requires — hence its listing as a non-agent beneficiary (vindicated proposition adjacent, but here treated as the collecting abstraction per the beneficiary/vindicated-proposition distinction: continuity is the thing preserved, not merely a doctrine asserted). Petitioners with unclassifiable claims are the clearest victims: trapped exit, powerless, bearing the transfer of inert sorrow in place of remedy. The adjudicator occupies an unusual position — identity-locked rather than simply extracted-from or benefiting — their professional self is constituted by holding the boundary well, so exit from the bounded-empathy pattern would mean exit from their professional identity itself, not merely a change of employer.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure snare matters because there is a genuine coordination function here — consistent, humane, non-arbitrary adjudication at volume is a real problem this mechanism solves, and well-classified petitioners genuinely benefit from it. Labeling the whole arrangement pure extraction would erase that real function and make the eventual reform argument (expand the categories, without abandoning the human diligence of hearing) illegible. Labeling it pure rope, conversely, would erase the real victims and the real transfer that occurs when acknowledgment substitutes for remedy. The tangled_rope classification holds both facts simultaneously, which is the correct structural reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empathy_boundary_origin,
    'Is the boundary on adjudicator empathy an inherent requirement of consistent adjudication at scale, or a constructed limit that could be redrawn to admit more categories without sacrificing consistency?',
    'Comparative study of adjudicative bodies that have expanded categorical schemes over time versus those that have held them fixed — track whether consistency and caseload manageability degrade when categories expand.',
    'If the boundary is inherent, the tangled_rope classification is durable and reform must target category expansion directly. If constructed, the bounded-empathy mechanism is closer to a snare wearing coordination''s clothing, and the coordination story is largely cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empathy_boundary_origin, conceptual, 'Whether the empathy boundary is structurally necessary or a redrawable institutional choice.').

omega_variable(
    adjudicator_identity_lock_reversibility,
    'If the adjudicator''s professional identity were reconstituted around advocating for category expansion rather than holding boundaries, would the bounded-empathy mechanism collapse or merely relocate?',
    'Track outcomes in jurisdictions or institutions where adjudicators have been given explicit mandate and channel to recommend categorical expansion, observing whether the felt-sorrow-as-substitute pattern persists.',
    'If the pattern persists even when adjudicators are freed to advocate, the mechanism is deeper than identity-lock and points to institutional continuity as the true fixed point; if it dissolves, identity-lock was the load-bearing structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adjudicator_identity_lock_reversibility, empirical, 'Whether adjudicator identity-lock or institutional structure is the true stabilizing element.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bounded_empathy_as_stabilizing_mechanism, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boun_tr_t0, bounded_empathy_as_stabilizing_mechanism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(boun_tr_t4, bounded_empathy_as_stabilizing_mechanism, theater_ratio, 4, 0.29).
narrative_ontology:measurement(boun_tr_t8, bounded_empathy_as_stabilizing_mechanism, theater_ratio, 8, 0.33).
narrative_ontology:measurement(boun_tr_t12, bounded_empathy_as_stabilizing_mechanism, theater_ratio, 12, 0.36).
narrative_ontology:measurement(boun_tr_t16, bounded_empathy_as_stabilizing_mechanism, theater_ratio, 16, 0.39).
narrative_ontology:measurement(boun_tr_t20, bounded_empathy_as_stabilizing_mechanism, theater_ratio, 20, 0.41).
narrative_ontology:measurement(boun_tr_t24, bounded_empathy_as_stabilizing_mechanism, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(boun_be_t0, bounded_empathy_as_stabilizing_mechanism, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(boun_be_t4, bounded_empathy_as_stabilizing_mechanism, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(boun_be_t8, bounded_empathy_as_stabilizing_mechanism, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(boun_be_t12, bounded_empathy_as_stabilizing_mechanism, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(boun_be_t16, bounded_empathy_as_stabilizing_mechanism, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(boun_be_t20, bounded_empathy_as_stabilizing_mechanism, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(boun_be_t24, bounded_empathy_as_stabilizing_mechanism, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(boun_su_t0, bounded_empathy_as_stabilizing_mechanism, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(boun_su_t4, bounded_empathy_as_stabilizing_mechanism, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(boun_su_t8, bounded_empathy_as_stabilizing_mechanism, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(boun_su_t12, bounded_empathy_as_stabilizing_mechanism, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(boun_su_t16, bounded_empathy_as_stabilizing_mechanism, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(boun_su_t20, bounded_empathy_as_stabilizing_mechanism, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(boun_su_t24, bounded_empathy_as_stabilizing_mechanism, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bounded_empathy_as_stabilizing_mechanism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bounded_empathy_as_stabilizing_mechanism, 0.12).
narrative_ontology:affects_constraint(bounded_empathy_as_stabilizing_mechanism, categorical_nonexistence_as_soft_denial).

% DUAL FORMULATION NOTE:
% This constraint is downstream of categorical_nonexistence_as_soft_denial: the upstream story addresses the institution's categorical scheme failing to recognize certain claims at the level of classification; this story addresses what happens inside the adjudicator processing those unrecognized claims — the psychological/procedural mechanism (bounded, genuine empathy) that keeps the upstream soft denial from generating destabilizing pressure. Different ε, different stakeholders, different observable (internal monologue vs. categorical absence), linked by causal dependency: the soft denial upstream is what the adjudicator's bounded empathy downstream is asked to absorb and stabilize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
