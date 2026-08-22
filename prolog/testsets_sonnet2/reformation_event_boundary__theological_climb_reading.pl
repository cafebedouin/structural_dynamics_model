% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: The Reformation as Theological Climb: Justification by Faith Alone as Doctrinal Breakthrough Requiring Institutional Separation
 *   domain: historical_epistemology/religious_history/commitment_system
 *
 * SUMMARY:
 *   This story instantiates the theological_climb reading of the
 *   reformation_event_boundary kernel: the Reformation is read as a genuine
 *   doctrinal breakthrough (justification by faith alone) that necessitated
 *   institutional separation from Rome, with the Catholic hierarchy
 *   positioned as a corrected party and believers as beneficiaries of
 *   doctrinal liberation. Extraction here is measured on the standing
 *   arrangement this reading is about — the confessionally-divided
 *   territorial church settlement (culminating in the 1555 Peace of Augsburg)
 *   — assessed by the reading's own lights, not by the endorsed theological
 *   outcome (justification by faith itself, which the reading treats as pure
 *   gain with ε≈0). The rising extractiveness and suppression trajectory
 *   tracks how a doctrinal dispute becomes an enforced territorial
 *   settlement: 1517 (Ninety-Five Theses, low suppression, a debate) through
 *   1555 (cuius regio eius religio formalized, suppression high, dissenters
 *   trapped by territorial confessional assignment).
 *
 * KEY AGENTS:
 *   - believers_freed_from_indulgence_doctrine: primary beneficiary on this reading (powerless/constrained) — gains doctrinal liberation but not individual choice of confession
 *   - catholic_church_hierarchy: primary victim on this reading (institutional/trapped) — loses doctrinal authority and revenue, framed here as legitimate correction rather than expropriation
 *   - evangelical_princes: secondary beneficiary (powerful/arbitrage) — political and material gains treated as downstream of, not causal to, the theological event
 *   - religious_minorities_under_cuius_regio: residual victim class this reading struggles to metabolize — coerced conformity that the 'freedom from false doctrine' framing does not obviously cover
 *   - modern_church_historians: analytical observer assessing sufficiency of the theological-cause account
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.62).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.7).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "The Reformation as Theological Climb: Justification by Faith Alone as Doctrinal Breakthrough Requiring Institutional Separation").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '94442b0e-f6c5-4a4a-9449-d31eb0646716').
narrative_ontology:cs_kernel_codification('94442b0e-f6c5-4a4a-9449-d31eb0646716', distributed).
narrative_ontology:cs_authority_grounding('94442b0e-f6c5-4a4a-9449-d31eb0646716', practice).
narrative_ontology:cs_interpretation_layer_present('94442b0e-f6c5-4a4a-9449-d31eb0646716').
narrative_ontology:cs_reading_relation('94442b0e-f6c5-4a4a-9449-d31eb0646716', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('94442b0e-f6c5-4a4a-9449-d31eb0646716', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('94442b0e-f6c5-4a4a-9449-d31eb0646716', foundational, justification_by_faith_alone_is_scriptural_recovery).
narrative_ontology:cs_axiom_status(justification_by_faith_alone_is_scriptural_recovery, holdable).
narrative_ontology:cs_axiom_grounding('94442b0e-f6c5-4a4a-9449-d31eb0646716', justification_by_faith_alone_is_scriptural_recovery, theological).
narrative_ontology:cs_axiom('94442b0e-f6c5-4a4a-9449-d31eb0646716', foundational, doctrinal_error_justifies_institutional_separation).
narrative_ontology:cs_axiom_status(doctrinal_error_justifies_institutional_separation, holdable).
narrative_ontology:cs_axiom_grounding('94442b0e-f6c5-4a4a-9449-d31eb0646716', doctrinal_error_justifies_institutional_separation, deontological).
narrative_ontology:cs_reference_frame('94442b0e-f6c5-4a4a-9449-d31eb0646716', medieval_sacramental_soteriology).
narrative_ontology:cs_drift_state('94442b0e-f6c5-4a4a-9449-d31eb0646716', peace_of_augsburg_1555, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('94442b0e-f6c5-4a4a-9449-d31eb0646716', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, believers_freed_from_indulgence_doctrine).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_clergy).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, evangelical_princes).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, religious_minorities_under_cuius_regio).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_fide_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_scriptura_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Laypeople previously subject to the indulgence system and works-based penitential economy. On this reading, Luther's rediscovery of justification by faith alone releases them from a coercive sacramental-financial apparatus that extracted payment for assurance of salvation. Their exit was constrained by geography and princely allegiance — they received the new doctrine if their ruler adopted it, not by individual choice.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, believers_freed_from_indulgence_doctrine, beneficiary,
    powerless, generational, constrained, regional).

% Pastors, theologians, and university faculty (Wittenberg, Geneva) who adopt and propagate the new doctrine. They gain theological authority, printing-driven audience, and institutional positions in the new church structures. Some faced martyrdom risk but many found expanded platforms; their doctrinal labor is what stabilizes the new kernel reading of scripture.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformed_clergy, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, reformed_clergy, agenda_setter).

% Territorial rulers who adopt the Reformation and thereby also acquire church lands, judicial authority over religion in their territory, and independence from papal taxation. On this reading their political gain is a downstream consequence of a genuine theological event, not its cause — but they benefit from the settlement regardless.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, evangelical_princes, beneficiary,
    powerful, generational, arbitrage, regional).

% The papacy, bishops, and religious orders whose doctrinal authority, revenue base (indulgences, tithes, land), and institutional unity are broken by the schism. On this reading they are the party corrected by a real theological discovery — victims not of political predation but of being doctrinally wrong, which the reading treats as a legitimate basis for their losses.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Individuals whose confession does not match their ruler's chosen confession under cuius regio eius religio (formalized 1555). They must convert, conceal belief, or emigrate. Even on the theological-climb reading, which credits the settlement as doctrinal correction, this population bears a cost the reading has difficulty absorbing into 'freedom from false doctrine.'
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, religious_minorities_under_cuius_regio, payer,
    powerless, biographical, trapped, regional).

% Anabaptists and other radical reformers who accepted core Reformation premises (sola scriptura, critique of Rome) but drew different institutional conclusions (believer's baptism, church-state separation). They are suppressed by both Catholic and magisterial-Protestant authorities alike; the theological-climb reading's clean two-party frame (Rome vs. Wittenberg) has no seat for them.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, radical_reformers, excluded,
    powerless, biographical, trapped, regional).

% Scholars assessing whether the doctrinal content of Luther's breakthrough is sufficient, on its own, to explain the scale and durability of the institutional rupture, or whether theology functioned as necessary-but-insufficient cause alongside political and economic drivers.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, modern_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a genuine theological crisis — an incoherence in the late medieval doctrine of justification and the indulgence economy built on it — by returning to a scriptural warrant (sola fide, sola scriptura) that a critical mass of clergy and laity found compelling, and organizing new ecclesial structures around that resolved doctrine.
% TRANSFER_FUNCTION: Moves doctrinal authority, clerical office, and (via territorial adoption) church property and taxation rights from the Roman hierarchy to reformed churches and their princely patrons; moves religious assurance from a sacramental-transactional system to a faith-alone framework for those who convert.
% ABSENT_VOICES: Radical reformers (Anabaptists) who share the sola scriptura premise but reject the church-state fusion that both Rome and the magisterial reformers preserve; and religious minorities under cuius regio who did not choose their territory's confession. Neither appears in the theological-climb reading's central drama of Luther versus the papacy.
% DISAPPEARANCE_RATIONALE: If the doctrinal breakthrough is denied as a real event (i.e., if the theological content is reduced entirely to rationalization, per the political_swap sibling reading), the entire moral architecture of this reading collapses: the Catholic hierarchy would no longer be a 'victim of correction' but a victim of expropriation, and the reformed churches would lose the primary warrant used to justify their separation and territorial gains.
% FOUNDING_PROBLEM: The late medieval Western church's soteriology (justification through sacramental participation, penance, and indulgence purchase) had, on this reading, developed doctrinal error and pastoral abuse (notably indulgence sales) that a scripturally-grounded correction was needed to resolve.
% FOUNDING_PROBLEM_CORROBORATION: Reformed theological tradition and much confessional Protestant historiography attest the doctrinal-crisis diagnosis as real and resolved by 1555. Catholic historiography (including post-Vatican II Catholic scholarship) and a substantial current of secular social historians attest that the indulgence controversy was a proximate trigger, not the primary driver, and that the doctrinal content cannot on its own explain the timing, scale, and territorial pattern of the schism — corroboration for the 'genuine breakthrough requiring separation' framing comes overwhelmingly from within the tradition it vindicates.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is authored moderately high because even under the most favorable (theological-climb) reading, the settlement that stabilizes the doctrinal victory — territorial establishment, property transfer, coerced conformity under cuius regio — carries real costs for the Catholic hierarchy and for religious minorities regardless of whether the underlying doctrine was correct. Suppression (0.7) is high and rises steeply between 1517 and 1555 because doctrinal debate hardens into enforced territorial religious monopoly; this is a structural property of the settlement, not scaled by the reading's sympathy for the theology. Theater ratio is kept low (0.2) because the coordination function (resolving genuine doctrinal incoherence) is treated by this reading as substantively real, not performative — this is a claim internal to the reading, not an engine verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Believers, reformed clergy, and evangelical princes are declared beneficiaries because the reading treats the doctrinal correction as a real transfer of value to them (spiritual assurance, ecclesial office, and territorial/fiscal control respectively). Catholic hierarchy and religious minorities under cuius regio are declared victims: the hierarchy loses authority and assets on grounds the reading treats as theologically legitimate, while minorities bear the settlement's coercive machinery without having chosen a side. Radical reformers are excluded rather than victimized in the standard sense — the reading's two-party (Rome/Wittenberg) frame has no natural seat for them, which is itself diagnostic of the reading's narrowness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem trio is answered as 'contested' deliberately: the theological-climb reading's own historiographical tradition treats the doctrinal crisis as fully live and its resolution as durable and correct, while corroboration from outside that tradition (Catholic and secular historiography) treats the theological content as necessary but insufficient. This divergence is exactly what the R5 corroboration field is designed to surface — a genealogy attested chiefly by the reading's own beneficiaries is flagged, not adjudicated, by this framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_sufficiency_vs_political_cause,
    'Is the doctrinal content of justification by faith alone sufficient on its own to explain the scale, timing, and territorial pattern of the institutional rupture, or does the theological-climb reading require political and economic factors it treats as merely downstream?',
    'Comparative case analysis: examine reform movements with similar theological content but different political conditions (e.g., pre-Lutheran reform movements suppressed without princely backing, such as the Hussites) to test whether doctrinal content alone predicts institutional separation.',
    'If theological content alone is insufficient without princely political backing, the theological_climb reading''s causal ordering (theology drives separation, politics follows) is undermined in favor of the political_swap or composite readings, and the beneficiary/victim structure authored here (Church as victim of correction) would need to be reassessed as victim of expropriation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_sufficiency_vs_political_cause, conceptual, 'Whether the theological-climb reading''s causal sufficiency claim survives comparison to failed reform movements lacking political sponsorship.').

omega_variable(
    kernel_reading_selection_criterion,
    'What principled criterion selects the theological_climb reading as the primary lens over the political_swap or composite_overdetermination readings, rather than treating the selection itself as a contested historiographical commitment?',
    'This is not resolvable by additional historical data alone — it depends on prior commitments about what counts as a ''cause'' in complex institutional events (proximate trigger vs. sufficient condition vs. necessary condition vs. irreducible co-determination). Document the criterion used by each reading''s proponents and whether they are answering the same question.',
    'If no principled criterion exists to prefer one reading, the composite_overdetermination_reading gains warrant as the more defensible frame, and single-driver readings (this one and political_swap) should be understood as partial, reading-indexed accounts rather than competing factual claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'Whether reading selection among the three kernel readings is a factual or a framing question.').

omega_variable(
    minority_coercion_absorption,
    'Can the coercion experienced by religious minorities under cuius regio eius religio be coherently absorbed into a reading that frames the overall settlement as ''freedom from false doctrine,'' or does this population''s experience falsify the reading''s normative framing even if its causal claims hold?',
    'Examine primary sources (emigration records, conversion mandates, minority community testimony) from territories that changed confession to assess whether affected minorities experienced the settlement as liberation or as coercion, independent of doctrinal content.',
    'If minorities under the reading''s own settlement experienced net coercion rather than liberation, the beneficiary declaration for ''believers_freed_from_indulgence_doctrine'' should be scoped more narrowly (to majority-confession adopters only), and victim declarations expanded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_coercion_absorption, empirical, 'Whether the reading''s liberation framing survives contact with minority experience under the territorial settlement it produced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(refo_tr_t1521, reformation_event_boundary__theological_climb_reading, theater_ratio, 1521, 0.13).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__theological_climb_reading, theater_ratio, 1530, 0.16).
narrative_ontology:measurement(refo_tr_t1546, reformation_event_boundary__theological_climb_reading, theater_ratio, 1546, 0.18).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.2).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1521, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1521, 0.45).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1530, 0.55).
narrative_ontology:measurement(refo_be_t1546, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1546, 0.6).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.4).
narrative_ontology:measurement(refo_su_t1521, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1521, 0.55).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1530, 0.62).
narrative_ontology:measurement(refo_su_t1546, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1546, 0.68).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the reformation_event_boundary kernel, decomposed per the ε-invariance principle because 'the Reformation' as a colloquial label conflates structurally distinct causal claims with different beneficiary/victim structures and different periodizations. theological_climb_reading (this story) treats doctrine as the primary driver and periodizes tightly (1517-1555, Theses to Peace of Augsburg). political_swap_reading treats theology as rationalization for princely expropriation of church assets and authority, with a correspondingly different victim/beneficiary map (secular rulers as primary beneficiaries, theology instrumentalized rather than causal). composite_overdetermination_reading declines to assign primary causal status to any single driver and treats the periodization itself as under-determined. All three share the same underlying historical events but author different ε, different ontological status for the Catholic Church (corrected party vs. expropriated party vs. one thread among several), and different sufficiency claims about theological content.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
