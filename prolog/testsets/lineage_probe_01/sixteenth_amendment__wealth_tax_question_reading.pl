% ============================================================================
% CONSTRAINT STORY: sixteenth_amendment__wealth_tax_question_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sixteenth_amendment__wealth_tax_question_reading, []).

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
 *   constraint_id: sixteenth_amendment__wealth_tax_question_reading
 *   human_readable: Sixteenth Amendment: Wealth Tax Question Reading (Moore Open Question)
 *   domain: legal/doctrinal
 *
 * SUMMARY:
 *   The Sixteenth Amendment's scope over wealth taxation remains doctrinally
 *   open. The 1919 Pollock decision struck down the income tax by treating it
 *   as a direct tax requiring apportionment; the Sixteenth Amendment
 *   overruled Pollock by exempting income taxes from apportionment. But what
 *   is 'income'? The 1920 Eisner v. Macomber decision planted the realization
 *   doctrine into the text — income means gain realized through a
 *   transaction, not paper appreciation or net worth itself. For a century,
 *   the realization doctrine has shielded unrealized wealth from federal
 *   taxation. In Moore v. United States (2024), the Supreme Court divided on
 *   whether a net-worth tax (or deemed-realization tax) would violate the
 *   Amendment. Justice Barrett's concurrence stated the question as genuinely
 *   open: the Amendment's logic might permit Congress to tax wealth directly,
 *   or it might not. Moore left the door ajar. This constraint models the
 *   doctrinal suspension created by Moore's open question. The constraint is
 *   not the Amendment itself (which has a relatively settled scope over
 *   realized income) but the uncertainty introduced by Moore's question mark:
 *   is wealth itself subject to federal taxation under the Sixteenth
 *   Amendment's logic? Beneficiaries of the open question are fortunes
 *   pending favorable ruling (they benefit from continued ambiguity that
 *   prevents immediate wealth-tax legislation) and the estate planning
 *   industry (which extracts fee value from navigating the uncertain
 *   boundary). Victims include legislative certainty (Congress cannot design
 *   coherent wealth-tax policy while the question hangs), the revenue base
 *   uniformity (wealth taxation would dramatically alter federal revenue
 *   architecture if permitted), and the future feasibility of wealth taxation
 *   (extended ambiguity risks cementing realization doctrine through judicial
 *   silence). The constraint exhibits high theater (the question is
 *   maintained through judicial authority claims that lack functional content
 *   — no case has tested the answer), rising extraction (fortunes
 *   increasingly benefit from planning certainty around the assumption that
 *   wealth is not taxable), and substantial suppression (doctrinal
 *   uncertainty prevents legislative action).
 *
 * KEY AGENTS:
 *   - Fortunes/High-Net-Worth Individuals: Primary beneficiary (moderate/constrained) — benefit from the status quo assumption that net-worth taxes require apportionment; face planning uncertainty that creates demand for specialized counsel
 *   - Estate Planning Industry: Primary beneficiary (institutional/arbitrage) — extract fee value from the uncertain boundary; counsel clients through the open question; have exit options (can arbitrage to favorable jurisdictions or restructure holdings)
 *   - Legislative Bodies (Congress): Primary victim (institutional/trapped) — cannot design coherent wealth-tax policy while Amendment scope remains unsettled; lack constitutional clarity needed to coordinate federal and state wealth taxation
 *   - Revenue Base Uniformity: Victim (powerless/trapped) — the open question creates a contingent boundary that divides taxable income from non-taxable wealth; if the boundary shifts, the entire federal revenue architecture must shift
 *   - Pro-Wealth-Tax Coalition: Victim (organized/constrained) — policy advocates for wealth taxation experience the open question as suppressing their preferred legislative option; organized enough to pursue litigation strategy but face long timelines and uncertain outcomes
 *   - Supreme Court: Institutional performer (institutional/arbitrage) — maintains doctrinal authority through the statement that the question is open; performs the role of authority without functional content (no case to adjudicate)
 *   - Analytical Observer: Sees the constraint as a false summit (analytical/analytical) — might argue the wealth-income boundary is logically inherent to the Amendment, but structural analysis reveals the open question as a contingent doctrinal construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sixteenth_amendment__wealth_tax_question_reading, 0.58).
domain_priors:suppression_score(sixteenth_amendment__wealth_tax_question_reading, 0.67).
domain_priors:theater_ratio(sixteenth_amendment__wealth_tax_question_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sixteenth_amendment__wealth_tax_question_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sixteenth_amendment__wealth_tax_question_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(sixteenth_amendment__wealth_tax_question_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sixteenth_amendment__wealth_tax_question_reading, tangled_rope).
narrative_ontology:human_readable(sixteenth_amendment__wealth_tax_question_reading, "Sixteenth Amendment: Wealth Tax Question Reading (Moore Open Question)").
narrative_ontology:topic_domain(sixteenth_amendment__wealth_tax_question_reading, "legal/doctrinal").

domain_priors:requires_active_enforcement(sixteenth_amendment__wealth_tax_question_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sixteenth_amendment__wealth_tax_question_reading, '2aecd04b-f3c9-4183-b2af-a267f38699f6').
narrative_ontology:cs_kernel_codification('2aecd04b-f3c9-4183-b2af-a267f38699f6', formalized).
narrative_ontology:cs_authority_grounding('2aecd04b-f3c9-4183-b2af-a267f38699f6', lineage).
narrative_ontology:cs_interpretation_layer_present('2aecd04b-f3c9-4183-b2af-a267f38699f6').
narrative_ontology:cs_reading_relation('2aecd04b-f3c9-4183-b2af-a267f38699f6', sixteenth_amendment__pollock_overruled_reading, coexists_with).
narrative_ontology:cs_reading_relation('2aecd04b-f3c9-4183-b2af-a267f38699f6', sixteenth_amendment__realization_doctrine_reading, influences).
narrative_ontology:cs_axiom('2aecd04b-f3c9-4183-b2af-a267f38699f6', foundational, amendment_logic_permits_wealth_taxation).
narrative_ontology:cs_axiom_status(amendment_logic_permits_wealth_taxation, holdable).
narrative_ontology:cs_axiom_grounding('2aecd04b-f3c9-4183-b2af-a267f38699f6', amendment_logic_permits_wealth_taxation, deontological).
narrative_ontology:cs_axiom('2aecd04b-f3c9-4183-b2af-a267f38699f6', foundational, moore_question_remains_open_absent_scotus_adjudication).
narrative_ontology:cs_axiom_status(moore_question_remains_open_absent_scotus_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('2aecd04b-f3c9-4183-b2af-a267f38699f6', moore_question_remains_open_absent_scotus_adjudication, conventional).
narrative_ontology:cs_reference_frame('2aecd04b-f3c9-4183-b2af-a267f38699f6', moore_open_question_framework).
narrative_ontology:cs_drift_state('2aecd04b-f3c9-4183-b2af-a267f38699f6', contemporary_post_moore_period, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('2aecd04b-f3c9-4183-b2af-a267f38699f6', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(sixteenth_amendment__wealth_tax_question_reading, sixteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sixteenth_amendment__wealth_tax_question_reading, fortunes_pending_favorable_ruling).
narrative_ontology:constraint_beneficiary(sixteenth_amendment__wealth_tax_question_reading, estate_planning_industry).
narrative_ontology:constraint_victim(sixteenth_amendment__wealth_tax_question_reading, legislative_certainty).
narrative_ontology:constraint_victim(sixteenth_amendment__wealth_tax_question_reading, revenue_base_uniformity).
narrative_ontology:constraint_victim(sixteenth_amendment__wealth_tax_question_reading, future_wealth_tax_feasibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGISLATIVE CERTAINTY AS VICTIM (SNARE) — Congress cannot design coherent wealth tax policy while the Amendment's reach remains doctrinal undefined. The wealth-tax question traps legislative jurisdiction: act and risk constitutional invalidation; abstain and cede wealth coordination to state law. Maximum extraction for the legislative victim — no exit from constitutional ambiguity, no clarification forthcoming absent a case.
constraint_indexing:constraint_classification(sixteenth_amendment__wealth_tax_question_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FORTUNES PENDING THE QUESTION (TANGLED ROPE) — High-net-worth individuals and estates face genuine coordination benefit (the tax code's current certainty that wealth itself is not taxable under the Amendment) alongside real extraction risk (if Moore's question is answered adversely in a future case). This agent class experiences the constraint as mixed: they benefit from the status quo's protective ambiguity, but also bear the cost of juridical uncertainty for planning purposes. Constrained exit — cannot exit the Amendment's interpretation, cannot force the question absent litigation they may not bring.
constraint_indexing:constraint_classification(sixteenth_amendment__wealth_tax_question_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTATE PLANNING INDUSTRY (ROPE) — Benefits from the open question as a source of expertise value and fee-generation opportunity. Planners and tax counsel extract fees for navigating the uncertain boundary; they coordinate client wealth conservation through strategies that depend on the current assumption that net-worth taxes would require apportionment. Net beneficiary with low suppression from their perspective — they can arbitrage the uncertainty into service value. They have institutional power and can exit (advise clients to move to favorable jurisdictions or restructure holdings); the constraint is experienced as manageable coordination.
constraint_indexing:constraint_classification(sixteenth_amendment__wealth_tax_question_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRO-WEALTH-TAX COALITION (TANGLED ROPE) — Progressive policy advocates and wealth-tax proponents experience the constraint as both coordinating their policy goals (they must navigate the Sixteenth Amendment constraint to design feasible wealth taxes) and extracting from them (the open question prevents enactment, creating policy stasis). Organized agents with constrained exit — they can pursue litigation strategy to clarify the question, but litigation is slow and outcome uncertain. They benefit from coordination function (the Amendment's logic constrains what wealth taxes must look like) but bear extraction cost (the ambiguity paralyzes legislative action).
constraint_indexing:constraint_classification(sixteenth_amendment__wealth_tax_question_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE SUPREME COURT (PITON) — The Court maintains doctrinal authority over the wealth-tax question through its silence and past dicta (Moore's concurrence). The institution performs its authoritative role — it has pronounced the question open — while the actual verification of that authority awaits a case that may never come. Theater_ratio is high because the Court's authority is maintained through the expectation that it will answer, not through actual answer. The constraint persists through institutional inertia and the binding force of past statements, not through functional adjudication. No meaningful extraction or suppression from the Court's own perspective — it has deferred the problem.
constraint_indexing:constraint_classification(sixteenth_amendment__wealth_tax_question_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL STRUCTURE (MOUNTAIN) — From the standpoint of interpreting the Sixteenth Amendment's text and ratification history, one might argue the question is logically inherent: the Amendment says 'incomes' but does not define 'income' — the boundary between income and wealth is not a contingent policy choice but a structural feature of the text itself. Some analysts argue this is an immutable interpretive limit (mountain), akin to mathematical constraint on what the Amendment can mean. However, the structural data (beneficiaries, suppression, theater) reveals this as false summit: the wealth-tax question's open status is a contingent result of doctrine left deliberately unsettled by Moore, not a logical necessity.
constraint_indexing:constraint_classification(sixteenth_amendment__wealth_tax_question_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sixteenth_amendment__wealth_tax_question_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sixteenth_amendment__wealth_tax_question_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sixteenth_amendment__wealth_tax_question_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sixteenth_amendment__wealth_tax_question_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sixteenth_amendment__wealth_tax_question_reading, TR),
    TR >= 0.70.

:- end_tests(sixteenth_amendment__wealth_tax_question_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The open question extracts from legislative certainty and wealth-tax advocates (who cannot act with confidence), while accruing in favor of fortunes and estate planners. The value reflects that the extraction is substantial but not total — Congress retains nominal power to legislate, and wealth-tax proposals persist despite the question's existence. The measurement trajectory (0.42 → 0.51 → 0.58) models the accumulating cost of sustained ambiguity over time: as Moore's question becomes more widely known and cited as authority, its suppressive effect on legislative action increases. Suppression (0.67): High. The doctrinal uncertainty suppresses legislative action on wealth taxation at both federal and state levels. Fortunes have strong incentive to maintain the status quo (suppression flows from their interest in blocking wealth-tax legislation). The measurement trajectory (0.55 → 0.61 → 0.67) models rising suppression as Moore's open-question framing becomes settled doctrine — suppression is not externally imposed but emerges from the structure of the question itself (the question suppresses certainty). Theater ratio (0.81): High and rising. The Supreme Court's authority over the wealth-tax question is maintained largely through its past dicta (Moore's statement that the question is open) and the expectation that a case might someday reach the Court. The actual functional content — an adjudication that resolves the question — is absent. The theater increases over the measurement interval (0.62 → 0.75 → 0.81) because the question accumulates more doctrinal citation and authority while remaining unadjudicated. This is a classic piton pattern from the Court's perspective: maintained through inertia and performative authority claims.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays the full perspectival range despite a single base metric profile. Fortunes pending the question see rope-to-tangled-rope (coordination benefit + extraction risk). The estate planning industry sees rope (pure benefit, low suppression). Legislative certainty sees snare (trapped, maximum extraction, no exit). The pro-wealth-tax coalition sees tangled rope (coordination with the Amendment constraint + extraction from the open question). The Court itself sees piton (performative authority maintenance). The analytical observer risks seeing mountain (logical necessity of the wealth-income distinction) but structural analysis reveals false summit (the open question is a contingent doctrinal construction, not a logical constraint). The perspectival gaps reveal the key structural fault line: who benefits from the question's remaining open? Fortunes and estate planners. Who bears the cost? Legislative bodies and wealth-tax advocates. The constraint's directionality is clear — it flows toward wealth conservation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality value from the agent's position relative to the extraction flow. Fortunes benefit from the status quo (low d, negative χ from their perspective) — they experience the constraint as protective, not extractive. The estate planning industry benefits through fee generation (low d, institutional arbitrage power means they can exit or leverage the uncertainty). Legislative certainty and wealth-tax advocates are victims (high d, trapped or constrained exit options) — they experience high suppression and cannot exit the doctrinal ambiguity. The Court maintains authority without functional content (moderate d reflecting performative stance). The analytical observer sits outside the extraction flow (high d reflecting that the observer's native instruments — single-position analysis — cannot detect the constructed nature of the wealth-income boundary; cross-position analysis reveals the structure). The engine's false-summit detector should flag the analytical mountain perspective as naturalization of a contingent institutional arrangement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wealth_vs_income_textual_boundary,
    'Is the wealth-income boundary textually inscribed in the Sixteenth Amendment''s language (''incomes''), or is it a doctrinal construction imposed by Macomber that Moore left open for revision?',
    'Originalist analysis of ratification intent; comparison of pre-Pollock understandings of ''income'' with post-Macomber doctrine; legislative history of wealth-tax proposals and their constitutional framing.',
    'If textually inscribed: Moore''s question is asking the Court to violate the Amendment''s plain meaning; wealth tax becomes constitutionally impossible (realization doctrine reading prevails). If doctrinal construction: Moore''s question is open and answerable; wealth tax becomes a live legislative option (this reading''s open question survives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wealth_vs_income_textual_boundary, conceptual, 'Whether the wealth-income boundary is textual or doctrinal').

omega_variable(
    moore_dicta_binding_scope,
    'How binding is Moore''s concurrence stating the question as unresolved? Does it create a constraint on future Justices, or merely express one Justice''s view?',
    'Longitudinal study of how subsequent Courts have cited Moore; analysis of whether dicta about open questions function as binding precedent in constitutional doctrine.',
    'If binding: Moore''s framing controls the terms of the wealth-tax debate and the constraint persists (suppression ≥ 0.67, theater ≥ 0.81). If merely persuasive: future Courts can reframe the question entirely, potentially dissolving the constraint into a settled doctrine (suppression → near-zero).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moore_dicta_binding_scope, empirical, 'Binding force of Moore''s statement that the wealth-tax question is open').

omega_variable(
    litigation_probability_wealth_tax,
    'What is the probability that a litigable wealth-tax case will reach the Supreme Court in the next 20 years? (Current probability appears to be low to negligible.)',
    'Tracking of wealth-tax legislation in states and federal proposals; analysis of justiciability barriers and plaintiff-standing doctrine that could prevent a case from reaching SCOTUS.',
    'If probability near-zero: the constraint persists indefinitely in its current form (Piton-like institutional maintenance through silence). If probability increases: the constraint''s theater_ratio should rise further (more performative authority claims) OR the constraint should decompose into two separate constraints: pre-case doctrinal stasis (current story) and post-case adjudicated doctrine (new story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(litigation_probability_wealth_tax, empirical, 'Likelihood of wealth-tax case reaching Supreme Court').

omega_variable(
    realization_doctrine_versus_moore_logic,
    'If a wealth-tax case reaches the Court, does Moore''s open-question framing genuinely permit the Court to overrule Macomber''s realization requirement, or does the realization doctrine function as an immutable core of Sixteenth Amendment interpretation?',
    'Doctrinal analysis of whether realization is merely applied doctrine (revisable) vs. a fundamental interpretive commitment of the Amendment itself. Historical examination of whether the ratifiers understood ''income'' as necessarily realized.',
    'If realization is revisable: Moore''s question has real teeth; wealth tax could be constitutionally validated. If realization is immutable: Moore''s question is a doctrinal dead-end; the reading''s open status is illusory (impacts beneficiary set — if the answer is predetermined to be ''no,'' only those opposing wealth tax benefit from the open question).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(realization_doctrine_versus_moore_logic, conceptual, 'Whether realization doctrine is revisable or immutable').

omega_variable(
    ifsc_candidate_beneficiary_ambiguity,
    'Which actor(s) are genuinely benefiting from the open question''s current state? Fortunes (who prefer the question unsettled)? The estate planning industry (who profit from uncertainty)? Or future legislators who retain a theoretical option to enact a wealth tax?',
    'Behavioral analysis of revealed preferences: estate planners'' advice patterns, wealth concentration dynamics during the question-open period, legislative activity and messaging around wealth tax proposals.',
    'If beneficiaries are fortunes + estate industry: extraction is clearly directional (toward wealth conservation). If beneficiaries are wealth-tax advocates (who benefit from Moore''s opening): extraction flow reverses, and the constraint''s directionality (d) inverts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ifsc_candidate_beneficiary_ambiguity, empirical, 'Identity of actual beneficiaries from the open question').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sixteenth_amendment__wealth_tax_question_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sixteenth_wealth_theater_t0, sixteenth_amendment__wealth_tax_question_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(sixteenth_wealth_theater_t50, sixteenth_amendment__wealth_tax_question_reading, theater_ratio, 50, 0.75).
narrative_ontology:measurement(sixteenth_wealth_theater_t100, sixteenth_amendment__wealth_tax_question_reading, theater_ratio, 100, 0.81).

% Extraction over time
narrative_ontology:measurement(sixteenth_wealth_extract_t0, sixteenth_amendment__wealth_tax_question_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sixteenth_wealth_extract_t50, sixteenth_amendment__wealth_tax_question_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(sixteenth_wealth_extract_t100, sixteenth_amendment__wealth_tax_question_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sixteenth_wealth_suppress_t0, sixteenth_amendment__wealth_tax_question_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sixteenth_wealth_suppress_t50, sixteenth_amendment__wealth_tax_question_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(sixteenth_wealth_suppress_t100, sixteenth_amendment__wealth_tax_question_reading, suppression_requirement, 100, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sixteenth_amendment__wealth_tax_question_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sixteenth_amendment__wealth_tax_question_reading, 0.18).
narrative_ontology:affects_constraint(sixteenth_amendment__wealth_tax_question_reading, sixteenth_amendment__pollock_overruled_reading).
narrative_ontology:affects_constraint(sixteenth_amendment__wealth_tax_question_reading, sixteenth_amendment__realization_doctrine_reading).
narrative_ontology:affects_constraint(sixteenth_amendment__wealth_tax_question_reading, estate_tax_apportionment_doctrine).
narrative_ontology:affects_constraint(sixteenth_amendment__wealth_tax_question_reading, wealth_concentration_legislative_capacity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Sixteenth Amendment kernel. The sibling readings (pollock_overruled and realization_doctrine) represent alternative framings of the same text. The wealth-tax-question reading emerges from Moore v. United States (2024) and models the doctrinal suspension created by the Court's open question. Each reading has its own epsilon value and beneficiary/victim structure: pollock_overruled (ε ≈ 0.08, mountain) models the settled doctrine that Pollock was overruled; realization_doctrine (ε ≈ 0.12, mountain) models the settled doctrine that income means realized gain; wealth-tax-question (ε = 0.58, tangled_rope) models the unsettled boundary between these settled doctrines. The readings are linked through network.affects_constraints to show dependency: the wealth-tax question depends on both pollock and macomber settlements but creates downstream pressure on wealth-concentration and estate-tax doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sixteenth_amendment__wealth_tax_question_reading, institutional, 0.42).
constraint_indexing:directionality_override(sixteenth_amendment__wealth_tax_question_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
