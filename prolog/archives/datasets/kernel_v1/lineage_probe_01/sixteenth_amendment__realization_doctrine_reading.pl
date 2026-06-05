% ============================================================================
% CONSTRAINT STORY: sixteenth_amendment__realization_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sixteenth_amendment__realization_doctrine_reading, []).

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
 *   constraint_id: sixteenth_amendment__realization_doctrine_reading
 *   human_readable: Sixteenth Amendment Realization Doctrine Reading
 *   domain: constitutional_law/tax_jurisprudence
 *
 * SUMMARY:
 *   The realization doctrine, crystallized in Commissioner v. Macomber
 *   (1920), interprets the Sixteenth Amendment to permit taxation only of
 *   realized gains, not unrealized appreciation. This constraint operates as
 *   one reading of the contested Sixteenth Amendment kernel — a foundational
 *   commitment whose interpretation determines the boundary between taxable
 *   income and protected wealth. The Macomber reading plants realization as a
 *   semantic requirement, claiming that 'income' necessarily means gain
 *   received, not gain accrued. This reading systematically benefits holders
 *   of appreciating assets (stocks, real estate, intellectual property) by
 *   deferring taxation until sale. It simultaneously forecloses alternative
 *   taxation mechanisms (accrual-basis, mark-to-market, constructive
 *   realization) as inconsistent with the Amendment's text. The doctrine's
 *   theater ratio has increased over 40 years: modern financial instruments
 *   (derivatives, synthetic positions, unrealized gain loans, charitable
 *   pledges of appreciated stock) have enabled economic gains without
 *   triggering realization events, forcing the IRS to maintain the doctrine
 *   through substance-over-form doctrines and anti-abuse rules rather than
 *   through the clean verification Macomber promised. The constraint exhibits
 *   both genuine coordination (the doctrine provides administrable clarity)
 *   and asymmetric extraction (the doctrine systematically enables wealth
 *   avoidance). It is a tangled_rope at the policy level, a snare for accrual
 *   taxation advocates, a rope for asset holders, and a degraded (piton)
 *   administrative mechanism.
 *
 * KEY AGENTS:
 *   - Holders of Appreciating Unsold Assets: Primary beneficiary (institutional/arbitrage) — captures indefinite deferral benefit; faces low suppression because the doctrine protects their interests
 *   - Accrual Taxation Advocates: Primary victim (powerless/trapped) — trapped by constitutional doctrine; suppressed from implementing mark-to-market or accrual designs without amendment
 *   - Tax Policy Reformers: Secondary actors (moderate/constrained) — experience mixed coordination and extraction; seek legislative workarounds within doctrinal boundaries
 *   - Internal Revenue Service: Institutional maintainer (institutional/arbitrage) — enforces the doctrine but experiences degradation as financial sophistication outpaces verification mechanisms
 *   - Constitutional Interpreters: Analytical context (analytical/analytical) — navigate the doctrinal equilibrium; risk naturalizing a contingent institutional reading as textual mandate
 *   - Supreme Court: Ultimate arbiter (institutional/arbitrage) — maintains the precedent through stability bias; reclassified doctrine through Moore v. United States (2024) consideration of accrual-basis wealth taxation, leaving door ajar
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sixteenth_amendment__realization_doctrine_reading, 0.52).
domain_priors:suppression_score(sixteenth_amendment__realization_doctrine_reading, 0.68).
domain_priors:theater_ratio(sixteenth_amendment__realization_doctrine_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sixteenth_amendment__realization_doctrine_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(sixteenth_amendment__realization_doctrine_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sixteenth_amendment__realization_doctrine_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sixteenth_amendment__realization_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(sixteenth_amendment__realization_doctrine_reading, "Sixteenth Amendment Realization Doctrine Reading").
narrative_ontology:topic_domain(sixteenth_amendment__realization_doctrine_reading, "constitutional_law/tax_jurisprudence").

domain_priors:requires_active_enforcement(sixteenth_amendment__realization_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sixteenth_amendment__realization_doctrine_reading, '9575c93b-f36a-4d0b-906a-4a6163358917').
narrative_ontology:cs_kernel_codification('9575c93b-f36a-4d0b-906a-4a6163358917', fixed_text).
narrative_ontology:cs_authority_grounding('9575c93b-f36a-4d0b-906a-4a6163358917', lineage).
narrative_ontology:cs_interpretation_layer_present('9575c93b-f36a-4d0b-906a-4a6163358917').
narrative_ontology:cs_reading_relation('9575c93b-f36a-4d0b-906a-4a6163358917', sixteenth_amendment__pollock_overruled_reading, coexists_with).
narrative_ontology:cs_reading_relation('9575c93b-f36a-4d0b-906a-4a6163358917', sixteenth_amendment__wealth_tax_question_reading, coexists_with).
narrative_ontology:cs_axiom('9575c93b-f36a-4d0b-906a-4a6163358917', foundational, income_means_realized_gain).
narrative_ontology:cs_axiom_status(income_means_realized_gain, holdable).
narrative_ontology:cs_axiom_grounding('9575c93b-f36a-4d0b-906a-4a6163358917', income_means_realized_gain, empirically_contingent).
narrative_ontology:cs_axiom('9575c93b-f36a-4d0b-906a-4a6163358917', foundational, wealth_appreciation_not_income).
narrative_ontology:cs_axiom_status(wealth_appreciation_not_income, holdable).
narrative_ontology:cs_axiom_grounding('9575c93b-f36a-4d0b-906a-4a6163358917', wealth_appreciation_not_income, conventional).
narrative_ontology:cs_reference_frame('9575c93b-f36a-4d0b-906a-4a6163358917', textual_realization_requirement).
narrative_ontology:cs_drift_state('9575c93b-f36a-4d0b-906a-4a6163358917', contemporary_financial_sophistication_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9575c93b-f36a-4d0b-906a-4a6163358917', '').
narrative_ontology:cs_kernel_id(sixteenth_amendment__realization_doctrine_reading, sixteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sixteenth_amendment__realization_doctrine_reading, holders_of_appreciating_unsold_assets).
narrative_ontology:constraint_victim(sixteenth_amendment__realization_doctrine_reading, mark_to_market_tax_design).
narrative_ontology:constraint_victim(sixteenth_amendment__realization_doctrine_reading, accrual_taxation_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCRUAL TAXATION ADVOCATES (SNARE) — Structurally trapped by doctrinal closure. The realization doctrine forecloses accrual taxation designs and mark-to-market regimes as inconsistent with the Amendment's text. Advocates face maximum suppression: the doctrine is enforced through constitutional interpretation, leaving no statutory workaround. Cannot exit without amending the Constitution or overruling Macomber.
constraint_indexing:constraint_classification(sixteenth_amendment__realization_doctrine_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TAX POLICY REFORMERS (TANGLED ROPE) — Experience genuine coordination function (the realization doctrine provides administrable clarity: taxable income is realized gain, verifiable and auditable) alongside asymmetric extraction (the doctrine enables wealth appreciation avoidance). Constrained by doctrinal precedent but not trapped — legislative workarounds exist (mark-to-market for specific asset classes, step-up basis rules, carried interest recharacterization), though costly. Mixed experience of both benefit and burden.
constraint_indexing:constraint_classification(sixteenth_amendment__realization_doctrine_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ASSET HOLDERS AND WEALTH MANAGERS (ROPE) — Primary beneficiary (institutional/arbitrage). The realization doctrine is experienced as pure coordination: it provides legal clarity on when gains become taxable, enabling rational planning. The constraint solves a coordination problem (how to define taxable income without ambiguity). Beneficiaries face low suppression and high exit options — they can arbitrage the doctrine through tax-deferred accounts, like-kind exchanges, and step-up basis at death. Net coordinator, not target.
constraint_indexing:constraint_classification(sixteenth_amendment__realization_doctrine_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNAL REVENUE SERVICE (PITON) — Maintains the realization doctrine through administrative practice and regulation, but the mechanism has degraded significantly. The doctrine once enabled clean verification (realization = sale event = unambiguous gain). Modern financial instruments (derivatives, synthetic positions, unrealized gain financing) have hollowed this verification: agents can realize economic gains without triggering realization events. IRS enforces the doctrine through theater (substance-over-form doctrines, constructive sale rules, TEFRA provisions) rather than through genuine clarity. High theater ratio reflects performative enforcement against sophisticated tax avoidance.
constraint_indexing:constraint_classification(sixteenth_amendment__realization_doctrine_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / TEXTUAL READING (MOUNTAIN) — From the perspective of constitutional text analysis, Macomber's interpretation of 'income' as realized gain is presented as a stable logical reading: the Amendment's language ('income from whatever source derived') was understood in 1913 to exclude unrealized appreciation because 'income' meant gain received, not gain accrued. This perspective treats the realization doctrine as an unalterable consequence of linguistic meaning. However, the structural data reveals this as a false summit: the 'textual' reading is itself one reading of an ambiguous founding kernel, benefiting identifiable actors (asset holders), and suppressing alternative readings (wealth-tax interpretation, mark-to-market interpretation). The engine will flag this false summit.
constraint_indexing:constraint_classification(sixteenth_amendment__realization_doctrine_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CONSTITUTIONAL INTERPRETER (TANGLED ROPE) — Recognizes the realization doctrine as a doctrinal equilibrium: the doctrine provides genuine administrability (coordination function) while systematically enabling wealth avoidance (asymmetric extraction). From this view, Macomber is not textually mandated but rather a contingent institutional choice that became locked in through precedent and benefit concentration. The doctrine persists not because it is the only coherent reading but because it suits institutional interests (capital gains holders, tax-planning industries) and has become costly to overturn. Active enforcement is required to maintain the doctrine against legislative pressure for accrual-basis alternatives.
constraint_indexing:constraint_classification(sixteenth_amendment__realization_doctrine_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sixteenth_amendment__realization_doctrine_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sixteenth_amendment__realization_doctrine_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sixteenth_amendment__realization_doctrine_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sixteenth_amendment__realization_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sixteenth_amendment__realization_doctrine_reading, TR),
    TR >= 0.70.

:- end_tests(sixteenth_amendment__realization_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The realization doctrine extracts wealth-deferral benefits for asset holders, but the extraction is not absolute — some mark-to-market rules exist (financial institutions, dealers), and statutory carve-outs moderate the benefit. The 52% value reflects that the doctrine systematically enables avoidance without being a pure extraction mechanism; it coordinates a measurable benefit. Suppression (0.68): High. Accrual taxation and mark-to-market designs are suppressed through constitutional doctrine. Reformers can attempt workarounds (statutory mark-to-market for specific assets, carried interest recharacterization) but face the ceiling of Macomber's interpretation. The suppression value reflects high barriers to alternative designs. Theater ratio (0.55): Moderate. The doctrine began with genuine administrability (realization = sale = verifiable) but has become partially performative. IRS enforcement through substance-over-form and constructive realization rules suggests the original verification mechanism no longer cleanly functions. The rising trajectory (0.35 → 0.55 over 40 years) reflects accumulating financial sophistication that the doctrine cannot capture. Claimed type (tangled_rope) reflects coordination (clarity on when gains are taxable) + asymmetric extraction (systematic wealth avoidance for asset holders) + active enforcement (IRS doctrinal policing).
 *
 * PERSPECTIVAL GAP:
 *   The realization doctrine produces stark perspectival divergence because it is one reading of an ambiguous constitutional kernel. Macomber's own interpretation — that 'income' textually requires realization — appears as an immutable mountain from the analytical/civilizational observer perspective, but the structural data reveals it as a false summit: the kernel (the Amendment's language) is genuinely ambiguous, the reading benefits identifiable actors (asset holders), and suppression is maintained through institutional power, not textual necessity. The accrual taxation advocate sees a snare: complete doctrinal suppression of alternative designs. The asset holder sees a rope: coordinating clarity that enables planning. The tax reformer sees a tangled_rope: genuine coordination utility mixed with extraction. The IRS sees a piton: degraded enforcement through theatrical substance-over-form doctrines. The false summit (textual mandate) versus true structure (one reading of ambiguous kernel with beneficiaries) is the core perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position relative to the constraint. Asset holders benefit from the doctrine, have arbitrage options (tax deferral strategies, like-kind exchanges, step-up basis), and derive d ≈ 0.15 (beneficiary + institutional + arbitrage) → negative f(d), producing experienced extraction χ < 0. Accrual taxation advocates are trapped by the doctrine (constitutional barrier to exit), victims of suppressed design space, deriving d ≈ 0.95 (victim + powerless + trapped) → high f(d) ≈ 1.42, producing high experienced extraction χ. Tax policy reformers are moderately constrained (legislative workarounds exist but are costly and partial), experiencing d ≈ 0.62 (mixed victim/coordinator + moderate + constrained) → moderate f(d), producing moderate χ. The IRS (institutional/arbitrage) derives d from its role as enforcement mechanism for a constraint that primarily benefits asset holders, producing d ≈ 0.25 (institutional beneficiary of stability + arbitrage exit) → low f(d) ≈ 0.02, though this is complicated by the IRS's secondary function as revenue collector (it would benefit from overturning Macomber). The analytical observer's d ≈ 0.72 (analytical) reflects the observer's structural distance from the extraction flow — high enough to see structure clearly, but reflecting that analysis itself is not neutral to the doctrinal outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all three readings of the Sixteenth Amendment kernel are logically possible interpretations of the founding text. The realization_doctrine_reading does not alone determine taxation policy; it is one equilibrium among multiple possibilities. The tension between Snare (for accrual advocates), Tangled Rope (for reformers), and Rope (for asset holders) reflects the genuine doctrinal multistability. The false summit (mountain classification at the analytical level) resolves by acknowledging that 'textual mandate' is itself a reading choice, not an immutable fact. The doctrine persists through institutional path-dependence and benefit concentration, not through logical necessity. If Macomber had been decided differently (as suggested by Moore's openness), the same Amendment would support a different reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_definition_kernel_ambiguity,
    'Does ''income'' in the Sixteenth Amendment''s text necessarily mean only realized gain, or is the realization requirement one viable reading of an ambiguous founding kernel?',
    'Historical textual analysis of 1913 usage of ''income''; contemporaneous legislative debates on the scope of the Amendment; comparison with international definitions of taxable income at the time of ratification; examination of whether the Framers explicitly rejected accrual-basis or mark-to-market language',
    'If textually mandated: realization doctrine is a mountain (immutable constitutional fact). If one reading of ambiguous kernel: doctrine is a tangled_rope or snare (contingent institutional choice with beneficiaries and victims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(income_definition_kernel_ambiguity, empirical, 'Whether realization is textually mandated or one reading of ambiguous kernel').

omega_variable(
    macomber_doctrinal_lock_depth,
    'Is the realization doctrine locked in by precedent and institutional path-dependence, or could it be overturned through legislative action without constitutional amendment?',
    'Analysis of post-Macomber statutory workarounds (mark-to-market for specific assets, carried interest, step-up basis); examination of whether the doctrine could be legislatively narrowed; comparison with other tax doctrines that survived constitutional challenge through statutory narrowing; test case scenario analysis under current Supreme Court doctrine',
    'If locked by precedent only: legislative pathway exists, and suppression is moderate (constrained, not trapped). If locked by constitutional text: suppression is high (trapped), and accrual taxation requires amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(macomber_doctrinal_lock_depth, empirical, 'Doctrinal lock-in depth and legislative workaround availability').

omega_variable(
    asymmetric_distributional_impact,
    'What is the measured distributional impact of the realization doctrine across wealth quintiles? Does the doctrine disproportionately benefit the top 1% or is the benefit distributed across all asset holders?',
    'Treasury estimate of revenue loss from realization deferral by asset class and wealth quintile; comparison of effective tax rates (income tax + capital gains tax) across income distribution with and without realization requirement; measurement of step-up basis utilization by estate size',
    'If concentrated in top 1%: extraction mechanism is severe and targeted (snare classification strengthens). If distributed: coordination function is more genuine and extraction is moderate (tangled_rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_distributional_impact, empirical, 'Distributional impact of realization doctrine across wealth distribution').

omega_variable(
    constitutional_amendment_feasibility,
    'Is constitutional amendment to overturn Macomber politically feasible, or is the doctrine effectively immutable within current political constraints?',
    'Historical analysis of wealth-tax amendment proposals (2019-2021 proposals, prior attempts); polling on public support for accrual taxation or wealth tax; analysis of state-level constitutional provisions addressing direct taxes; assessment of Article V amendment requirements and political coalitions',
    'If amendment feasible: doctrine is contingent and contestable (victims have long-term exit). If immutable: doctrine is trapped-level suppression for victims (mountain-adjacent or entrenched snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_amendment_feasibility, preference, 'Political feasibility of constitutional amendment to overturn realization doctrine').

omega_variable(
    textual_reading_contingency,
    'This constraint is one reading of the Sixteenth Amendment kernel. Could alternative readings (pollock_overruled or wealth_tax_question) coexist in the same constitutional framework, or does this reading logically foreclose them?',
    'Comparative jurisprudential analysis: can a constitutional framework simultaneously hold that (a) Pollock was overruled AND (b) realization is mandated? Can it hold that (a) wealth taxation is open AND (b) realization forecloses it? Examine whether the axioms of each reading are mutually exclusive or merely competing.',
    'If forecloses siblings: this reading is logically privileged within a single framework (mountain-adjacent). If coexists: readings are competing doctrinal equilibria (tangled_rope accurately captures the doctrinal multistability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_reading_contingency, conceptual, 'Logical relationship between realization_doctrine_reading and sibling readings of the Sixteenth Amendment kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sixteenth_amendment__realization_doctrine_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sixteenth_real_tr_t0, sixteenth_amendment__realization_doctrine_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sixteenth_real_tr_t20, sixteenth_amendment__realization_doctrine_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(sixteenth_real_tr_t40, sixteenth_amendment__realization_doctrine_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(sixteenth_real_be_t0, sixteenth_amendment__realization_doctrine_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sixteenth_real_be_t20, sixteenth_amendment__realization_doctrine_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(sixteenth_real_be_t40, sixteenth_amendment__realization_doctrine_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(sixteenth_real_su_t0, sixteenth_amendment__realization_doctrine_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sixteenth_real_su_t20, sixteenth_amendment__realization_doctrine_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(sixteenth_real_su_t40, sixteenth_amendment__realization_doctrine_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sixteenth_amendment__realization_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sixteenth_amendment__realization_doctrine_reading, sixteenth_amendment__pollock_overruled_reading).
narrative_ontology:affects_constraint(sixteenth_amendment__realization_doctrine_reading, sixteenth_amendment__wealth_tax_question_reading).
narrative_ontology:affects_constraint(sixteenth_amendment__realization_doctrine_reading, capital_gains_tax_deferral_mechanism).
narrative_ontology:affects_constraint(sixteenth_amendment__realization_doctrine_reading, step_up_basis_avoidance_pathway).

% DUAL FORMULATION NOTE:
% The Sixteenth Amendment is a contested kernel with three structurally distinct constraint readings. This file represents the realization_doctrine_reading (Macomber's interpretation). Sibling readings appear in separate constraint stories with different epsilon values and beneficiary/victim structures. The network links all three readings to enable cross-constraint contamination analysis and false summit detection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
