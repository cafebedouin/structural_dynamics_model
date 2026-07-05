% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of the Constitutional Authority Boundary
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   constitutional_authority_boundary: the judicial supremacy reading, under
 *   which the constitutional text is understood to establish courts as final,
 *   unchallengeable arbiters of all constitutional questions, with power to
 *   invalidate legislative and executive acts and no institutional remedy
 *   available to the branches whose acts are struck down. This is
 *   structurally distinct from the coordinate_construction_reading
 *   (distributed interpretive authority across three co-equal branches, no
 *   single final arbiter) and the parliamentary_primacy_reading (legislative
 *   sovereignty subordinates constitutional text to ordinary or entrenched
 *   statute). Under THIS reading, the judiciary becomes a genuine beneficiary
 *   of interpretive monopoly rents and the legislature becomes a structural
 *   victim of permanently constrained policy space — a beneficiary/victim
 *   structure that does not exist under the sibling readings, where
 *   interpretive authority is either distributed or ultimately legislative.
 *   The high extractiveness (0.66 by interval end) reflects the
 *   counter-majoritarian veto power this reading vests in an unelected,
 *   life-tenured body with no override mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.66).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of the Constitutional Authority Boundary").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '2d76c20e-a355-4721-b21c-671d4ea82b52').
narrative_ontology:cs_kernel_codification('2d76c20e-a355-4721-b21c-671d4ea82b52', fixed_text).
narrative_ontology:cs_authority_grounding('2d76c20e-a355-4721-b21c-671d4ea82b52', lineage).
narrative_ontology:cs_interpretation_layer_present('2d76c20e-a355-4721-b21c-671d4ea82b52').
narrative_ontology:cs_reading_relation('2d76c20e-a355-4721-b21c-671d4ea82b52', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('2d76c20e-a355-4721-b21c-671d4ea82b52', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('2d76c20e-a355-4721-b21c-671d4ea82b52', foundational, judicial_interpretation_is_final_and_unreviewable).
narrative_ontology:cs_axiom_status(judicial_interpretation_is_final_and_unreviewable, holdable).
narrative_ontology:cs_axiom_grounding('2d76c20e-a355-4721-b21c-671d4ea82b52', judicial_interpretation_is_final_and_unreviewable, conventional).
narrative_ontology:cs_axiom('2d76c20e-a355-4721-b21c-671d4ea82b52', foundational, counter_majoritarian_review_is_necessary_to_bind_self_judging_branches).
narrative_ontology:cs_axiom_status(counter_majoritarian_review_is_necessary_to_bind_self_judging_branches, holdable).
narrative_ontology:cs_axiom_grounding('2d76c20e-a355-4721-b21c-671d4ea82b52', counter_majoritarian_review_is_necessary_to_bind_self_judging_branches, instrumental).
narrative_ontology:cs_reference_frame('2d76c20e-a355-4721-b21c-671d4ea82b52', founding_text_enforcement_gap).
narrative_ontology:cs_drift_state('2d76c20e-a355-4721-b21c-671d4ea82b52', contemporary_judicial_review_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2d76c20e-a355-4721-b21c-671d4ea82b52', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, appellate_bar_specialists).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, rule_of_law_stability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final, unreviewable authority to declare what the constitution means and to invalidate legislative and executive acts, with no institutional remedy available to the branches whose acts are struck down. Administers the doctrine that establishes its own supremacy, sets the tests and standards of review it applies to itself, and controls the pace and scope of constitutional change through case selection. Its rulings cannot be overridden by ordinary politics.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary, beneficiary).

% A specialized professional class whose expertise in constitutional litigation before the supreme interpretive body is the primary channel through which policy disputes are now resolved. Their professional standing, fees, and influence depend on constitutional questions routing through courts rather than being settled by legislatures or referenda.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, appellate_bar_specialists, beneficiary,
    organized, biographical, mobile, national).

% Passes statutes reflecting electoral mandates, only to see them invalidated by judicial constitutional interpretation with no override mechanism available — no supermajority vote, no constitutional amendment process short of an extraordinarily high bar, restores the legislature's authority over the specific question. Bears the cost of having its policy space permanently constrained by interpretations it cannot revisit through ordinary democratic process.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature, payer,
    powerful, generational, trapped, national).

% Executive actions and orders are subject to invalidation on constitutional grounds determined solely by the judiciary. The executive can appoint judges over time as a slow lever, but faces no ability to contest an adverse ruling once rendered. Its short electoral horizon versus judicial life tenure structurally disadvantages its bargaining position.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    powerful, biographical, constrained, national).

% Vote for legislative and executive candidates whose enacted policies can be nullified by judicial constitutional interpretation without any direct electoral remedy against the judiciary itself. Their preferences, even when expressed through supermajority electoral mandates, carry no formal weight against a contrary constitutional ruling. They are not party to the interpretive process and have no vote on constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, electoral_majorities, excluded,
    organized, immediate, trapped, national).

% Study the comparative structure of judicial review regimes across jurisdictions, documenting where judicial supremacy produces counter-majoritarian outcomes versus where coordinate or parliamentary readings distribute interpretive authority differently. Their analysis feeds academic and sometimes legislative debate but does not bind any branch.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, final decision-maker for constitutional disputes, preventing the paralysis and cyclical conflict that could arise if each branch asserted final interpretive authority over the same text simultaneously.
% TRANSFER_FUNCTION: Moves effective policy-making authority over constitutionally-adjacent questions from elected, accountable legislative and executive branches to appointed, life-tenured judges; moves litigation-driven influence and fees toward the specialized appellate bar.
% ABSENT_VOICES: Electoral majorities whose enacted preferences are nullified have no institutional voice in the interpretive process itself — no vote, no veto, no formal channel to contest a constitutional ruling short of the extraordinarily difficult amendment process. Legislatures raise this objection in dissenting political rhetoric but have no procedural avenue to act on it.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight and interpretive authority reverted to being contested or shared, legislatures would immediately reassert authority to enact statutes previously foreclosed by precedent, executive action would face different (likely political rather than judicial) checks, and the appellate bar's centrality to policy resolution would diminish sharply in favor of legislative and electoral channels.
% FOUNDING_PROBLEM: Early constitutional orders faced the problem of textual supremacy without an enforcement mechanism: if the legislature or executive could unilaterally decide the constitutionality of its own acts, the constitution would function as a mere aspiration rather than a binding limit on power.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and much of the constitutional bar attest the problem remains live — that without final judicial authority, majoritarian factions would erode minority and structural protections. Comparative constitutional scholars operating outside the judiciary and the bar (analytical observers, not beneficiaries) attest that coordinate and parliamentary alternatives address the same enforcement problem without concentrating final authority in one unelected body, and that the judicial-supremacy solution has, in several jurisdictions, outlived demonstrable necessity and instead entrenched policy preferences of appointing coalitions.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.42 to 0.66) as case law accumulates and the judiciary's substantive policy footprint through constitutional interpretation grows relative to its founding enforcement function. Suppression (0.58) reflects the structural absence of any override mechanism, not active coercive violence — the suppression is procedural: no supermajority vote, referendum, or ordinary legislative act can reverse an adverse constitutional ruling. Theater ratio remains comparatively low (0.28) because the interpretive function is genuinely exercised, not merely performed — courts do resolve real disputes — but a moderate and growing share of theater is the doctrine of stare decisis and 'neutral principles' framing used to present policy choices as pure textual derivation.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits at the beneficiary end: it collects interpretive monopoly rents (final say, no remedy against it, arbitrage-level mobility across which cases to hear) and administers the very doctrine that entrenches its own supremacy. The appellate bar is a secondary beneficiary, its professional relevance tied to disputes routing through courts. The legislature and executive sit at the target end: both are powerful institutional actors, but both are structurally trapped/constrained with respect to this specific constraint — their policy acts can be nullified with no path back except an amendment process typically requiring supermajorities this reading itself deems appropriately hard to achieve. Electoral majorities are excluded entirely from the interpretive conversation despite bearing the downstream policy consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing self-judging branches from evading constitutional limits — is genuinely live in some form; a constitution with zero enforcement mechanism risks becoming aspirational only. But the judicial supremacy reading's specific solution (concentrating FINAL, unchallengeable authority in one body) is one of at least three live structural answers to that same founding problem, and its persistence as the dominant reading in many jurisdictions is not solely a function of that founding problem remaining unsolved elsewhere by superior means — it is also a function of the judiciary's own institutional interest in the reading that grants it supremacy. The tangled_rope classification is deliberate: the coordination function (a stable final decision-maker preventing constitutional deadlock) is genuine, but it rides alongside asymmetric extraction (permanent, irreversible-by-ordinary-means removal of policy space from elected branches) that requires active enforcement (courts must continue asserting and defending the supremacy doctrine against legislative and executive pushback) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is judicial supremacy the constitutionally correct reading of the founding text''s grant of interpretive authority, or is it one contested construction among coordinate and parliamentary alternatives that the judiciary itself has entrenched through self-referential precedent?',
    'Comparative analysis of the founding text''s original grant of authority, the historical record of contemporaneous debate over interpretive finality, and cross-jurisdictional comparison with coordinate_construction_reading and parliamentary_primacy_reading jurisdictions to assess whether the founding problem (self-judging branches evading limits) is resolved equally well under each reading.',
    'If judicial supremacy is found to be a self-entrenched construction rather than a textually compelled reading, the extraction attributed to interpretive monopoly rents strengthens considerably; if textually compelled, the extraction is better understood as the necessary cost of the coordination function rather than surplus rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether judicial supremacy is the textually necessary reading or a self-entrenched institutional construction among live alternatives.').

omega_variable(
    interpretive_monopoly_natural_vs_constructed,
    'Does the judiciary''s role as final arbiter constitute a natural extension of adjudicative function (courts must decide the cases before them, including constitutional ones), or is the FINALITY and UNCHALLENGEABILITY specifically a constructed doctrinal choice that benefits the judiciary as an institution?',
    'Examine whether early practice under the same constitutional text featured meaningful legislative or executive pushback against judicial constitutional rulings (e.g., non-enforcement, jurisdiction-stripping, court-packing threats) that later doctrinal developments foreclosed — a shift from contested to settled finality would indicate construction rather than natural extension.',
    'If finality was itself achieved through political struggle and doctrinal self-assertion rather than being inherent to the judicial function, the beneficiary status of the judiciary is more clearly an artifact of institutional self-interest rather than incidental to necessary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_natural_vs_constructed, empirical, 'Whether unchallengeable finality is inherent to adjudication or a constructed institutional achievement.').

omega_variable(
    override_mechanism_practical_availability,
    'Is the constitutional amendment process a genuine, if difficult, override mechanism (making this reading closer to a very costly rope) or is it, in practice, so functionally unavailable that it should be treated as no remedy at all (supporting the snare-adjacent reading of extraction)?',
    'Empirical count of successful constitutional amendments that specifically overrode a judicial constitutional interpretation, versus the base rate of judicial rulings that have stood for multiple generations without legislative or amendment-based reversal.',
    'A functioning-but-costly override mechanism would support classifying this closer to tangled_rope with a real, if expensive, exit; a functionally dead amendment process would push the classification toward snare, since the coordination story would then be pure cover for irreversible extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(override_mechanism_practical_availability, empirical, 'Whether the constitutional amendment process functions as a real override or is practically dead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cons_tr_t60, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(cons_tr_t80, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(cons_tr_t100, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(cons_be_t60, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(cons_be_t80, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(cons_be_t100, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 100, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(cons_su_t60, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(cons_su_t80, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(cons_su_t100, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kernel constitutional_authority_boundary. coordinate_construction_reading distributes interpretive authority across three co-equal branches with no single final arbiter (rope-leaning, no counter-majoritarian veto concentration). parliamentary_primacy_reading subordinates constitutional text to ordinary or entrenched legislative sovereignty (inverts the beneficiary/victim structure, empowering the legislature). judicial_supremacy_reading (this story) vests final, unchallengeable interpretive authority in the judiciary, producing the highest ε of the three due to irreversibility and counter-majoritarian veto power. Each reading has its own stable ε, beneficiary/victim structure, and classification — they are not the same constraint viewed from different angles but three structurally distinct constructions of the same textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
