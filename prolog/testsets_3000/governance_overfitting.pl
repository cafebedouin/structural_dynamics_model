% ============================================================================
% CONSTRAINT STORY: governance_overfitting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_governance_overfitting, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: governance_overfitting
 *   human_readable: Hyper-Specific Compliance Lock-in
 *   domain: political/technological
 *
 * SUMMARY:
 *   Hyper-specific compliance lock-in emerges when regulatory frameworks are
 *   designed to prevent particular historical failure modes by prescribing
 *   detailed organizational structures, transaction limits, reporting
 *   cadences, and custody mechanisms. The framework is reactive: it encodes
 *   lessons from 2008 bank runs, 2012 custody crises, or domain-specific
 *   disasters into rigid rules that permit only pre-approved organizational
 *   forms. Novel coordination mechanisms—those that might solve the same
 *   underlying problems through structurally different means—face categorical
 *   exclusion. The governance framework has become overfitted to past edge
 *   cases, creating a constraint that simultaneously provides genuine risk
 *   coordination value (preventing recurrence of known failures) and
 *   extractive lock-in (suppressing adaptive innovation). The theater ratio
 *   (0.68) reflects that compliance industries have built elaborate
 *   certification and audit rituals around the hyper-specific rules, but
 *   these rituals often demonstrate conformity rather than safety. The
 *   framework extractiveness (0.52) indicates moderate-to-high asymmetry:
 *   incumbents and compliance vendors benefit from the rule set, while novel
 *   coordinators and adaptive regulators bear the costs of suppression. Over
 *   the 10-year interval, extractiveness has risen from 0.28 to 0.52 and
 *   theater ratio from 0.35 to 0.68, indicating regulatory capture and
 *   path-dependent accumulation have strengthened the lock-in.
 *
 * KEY AGENTS:
 *   - Novel Coordinators: Primary victims (powerless/trapped) — startups and alternative institutions seeking to introduce coordination mechanisms that don't fit hyper-specific frameworks
 *   - Adaptive Regulators: Secondary victims (organized/constrained) — regulators seeking to evolve governance frameworks face internal suppression from accumulated rule complexity
 *   - Regulatory Incumbents: Primary beneficiaries (institutional/arbitrage) — large institutions that conformed to post-crisis rules benefit from suppression of novel competitors
 *   - Compliance Industry: Primary beneficiary (institutional/arbitrage) — consulting, legal, and audit firms profit from rule interpretation and certification
 *   - Experimental Jurisdiction: Intermediate actor (organized/constrained) — regulatory sandboxes and fintech exemptions represent organized escape routes, but with constrained scope
 *   - Regulatory Agencies: Institutional actor (institutional/arbitrage) — narrate hyper-specific rules as essential safeguards; benefit from reduced discretion and clear enforcement pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(governance_overfitting, 0.52).
domain_priors:suppression_score(governance_overfitting, 0.65).
domain_priors:theater_ratio(governance_overfitting, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(governance_overfitting, extractiveness, 0.52).
narrative_ontology:constraint_metric(governance_overfitting, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(governance_overfitting, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(governance_overfitting, tangled_rope).
narrative_ontology:human_readable(governance_overfitting, "Hyper-Specific Compliance Lock-in").
narrative_ontology:topic_domain(governance_overfitting, "political/technological").

domain_priors:requires_active_enforcement(governance_overfitting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(governance_overfitting, regulatory_incumbents).
narrative_ontology:constraint_beneficiary(governance_overfitting, compliance_industry).
narrative_ontology:constraint_victim(governance_overfitting, novel_coordination_schemes).
narrative_ontology:constraint_victim(governance_overfitting, adaptive_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVEL COORDINATOR (SNARE) — A startup or alternative institution seeking to introduce new coordination mechanisms faces a regulatory framework designed to prevent the specific failure modes of 2008-2012. The framework has no category for coordination patterns that differ materially from past cases. The novel coordinator cannot exit: they must either conform to hyper-specific compliance rules designed for obsolete conditions or operate outside the regulated perimeter entirely. Maximum experienced extraction — the constraint explicitly suppresses novel organizational forms that would threaten incumbent regulatory models.
constraint_indexing:constraint_classification(governance_overfitting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADAPTIVE REGULATOR (TANGLED ROPE) — A regulator seeking to evolve governance frameworks faces the constraint from within. They benefit from the coordination function: hyper-specific rules prevent the recurrence of known failure modes and create predictable safe harbors. But the same rules suppress their ability to design novel regulatory categories. They experience both coordination gain (known risks contained) and asymmetric extraction (inability to adapt). Constrained exit — they could theoretically rewrite rules, but political economy and vested interest make this costly.
constraint_indexing:constraint_classification(governance_overfitting, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPLIANCE INDUSTRY (ROPE) — Consulting firms, legal services, audit houses, and compliance software vendors benefit from the hyper-specific framework. Each new edge-case rule creates demand for interpretation, implementation, and certification. The constraint functions as pure coordination from their perspective: it creates a stable rule set that enables predictable service offerings. They experience no extraction — they profit from the framework's existence. Arbitrage exit — they can reposition to adjacent regulatory domains if rules change.
constraint_indexing:constraint_classification(governance_overfitting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY INCUMBENT (ROPE) — Large incumbent institutions that conformed to the post-2012 rules have sunk compliance costs and organizational structures optimized to the existing framework. They experience the constraint as pure coordination: the rules protect them from novel competitors while creating predictable compliance pathways. They can arbitrage to adjacent regulatory domains or exit by restructuring. Net beneficiary — the constraint suppresses their competition.
constraint_indexing:constraint_classification(governance_overfitting, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EXPERIMENTAL JURISDICTION (SCAFFOLD) — Some regulatory domains (financial technology sandboxes, blockchain experimental zones, fintech regulatory exemptions) have begun creating time-bounded safe harbors for novel coordination mechanisms. These jurisdictions experience the overfitting constraint as temporary: they create exit ramps for experimental organizations. Sunset logic applies — as novel mechanisms prove safe, they graduate from sandbox to standard framework. Constrained exit within the framework, but the sunset clause provides real architectural exit.
constraint_indexing:constraint_classification(governance_overfitting, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-HOC REGULATORY NARRATIVE (PITON) — Regulatory agencies narrate their hyper-specific rules as essential safeguards against catastrophic failure. The narrative persists even as the original failure modes (2008 bank runs, 2012 custody crises) have receded from lived experience. The rules are substantially performative — they create the appearance of systematic risk prevention while blocking novel mechanisms that might actually reduce risk through structural diversity. Theater ratio high: agencies conduct extensive reporting and certification rituals that demonstrate compliance without demonstrating safety. The function (preventing 2008) has atrophied; the theater persists.
constraint_indexing:constraint_classification(governance_overfitting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational perspective, one might argue that the overfitting is simply the inevitable lag between regulation and innovation — a natural law of governance: rules always chase past failures and always suppress future possibilities. However, this naturalizes a contingent institutional choice: the decision to write hyper-specific rules rather than principle-based or adaptive frameworks. The structural data contradicts the mountain classification — the constraint reflects policy design choice and institutional path dependency, not physical/logical inevitability. False summit.
constraint_indexing:constraint_classification(governance_overfitting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance_overfitting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(governance_overfitting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance_overfitting, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(governance_overfitting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(governance_overfitting, TR),
    TR >= 0.70.

:- end_tests(governance_overfitting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint suppresses novel coordination mechanisms that would compete with incumbent institutions. The suppression is justified by legitimate risk prevention, but the justification has degraded over time—original failure modes (2008) are now historical rather than immediate threats. The extractiveness value reflects that while risk coordination provides real value (0.28 at interval start), regulatory capture has added pure extraction (reaching 0.52 by interval end). Suppression (0.65): High. Novel mechanisms face categorical exclusion unless they explicitly fit pre-approved categories. Some escape routes exist (sandboxes, exemptions), but these are bounded and time-limited, not genuine alternatives. Theater ratio (0.68): High. Regulatory agencies conduct extensive reporting, stress testing, and certification rituals that demonstrate conformity to rules but often don't meaningfully test novel-mechanism safety. The rules have become substantially performative—agencies enforce the ritual rather than the underlying risk principle. The rise from 0.35 to 0.68 indicates regulatory theater has accumulated as rules interact with enforcement practices.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates heterogeneous classification across structural positions. Novel coordinators perceive snare: the framework explicitly prevents their organizational form with no meaningful exit. Regulatory incumbents perceive rope: the framework solves the coordination problem (preventing 2008) and suppresses their competition. Compliance vendors perceive rope: pure coordination value from the rule set. Adaptive regulators perceive tangled_rope: they benefit from known-risk suppression but suffer from inability to innovate. Experimental jurisdictions perceive scaffold: they treat the overfitting as temporary, with sunset logic built into exemption windows. Regulatory agencies narrating the framework perceive piton: the performative audit and certification rituals suggest the rules have lost primary function and persist through institutional inertia. The analytical observer at civilizational scale risks perceiving mountain: treating regulatory lag as an inevitable feature of governance. The gap is not observational uncertainty but structural heterogeneity—different agents genuinely experience the same constraint differently based on their exit capacity and beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) reflects their structural position within the extraction flow. Novel coordinators are trapped with no exit options—they experience maximum d (≈0.95) and maximum f(d) (≈1.42), producing high experienced extractiveness. Regulatory incumbents benefit from the framework while retaining arbitrage options (can restructure, exit to adjacent domains)—they experience low d (≈0.10) and negative f(d) (≈-0.08), producing negative experienced extraction (pure coordination benefit). The compliance industry experiences pure coordination from beneficiary position with full arbitrage options—d ≈0.05, f(d) ≈-0.12. Adaptive regulators are constrained by internal institutional dynamics—they are not victims in the sense of trapped agents, but their exit options are limited by political economy and sunk organizational costs. They experience d ≈0.50-0.55 (symmetric cost-benefit) and f(d) ≈0.65-0.75, producing moderate experienced extraction. The sandbox perspective shows organized agents with constrained but real exit options (staged graduation)—d ≈0.45, f(d) ≈0.50, producing low-to-moderate experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE vs ATROPHY TENSION: The overfitting constraint exhibits classic mandatrophy. The original mandate was genuine: prevent recurrence of 2008-2012 failure modes through detailed rules that force safe organizational structures. The mandate was operationally sound—the framework did reduce the frequency of those specific failures. However, as the original failure modes receded from immediate threat (15+ years post-2008), the mandate atrophied: rules persist not because the underlying risk remains acute but because regulatory agencies and compliance industries have built organizational cultures and profit models around the rules. The theater ratio rising from 0.35 to 0.68 indicates the function (preventing 2008) has atrophied while the ritual (performing compliance) has strengthened. The constraint is now maintained by institutional inertia rather than by genuine risk coordination. The novel-coordinator perspective (snare) reveals that the true structural function has shifted from coordination to extraction: the rules now serve primarily to suppress competition for incumbents and to generate revenue for compliance vendors. RESOLUTION: The constraint moves from tangled_rope (genuine coordination + legitimate extraction) toward snare (pure extraction with coordination rationale degraded) as the original justification weakens. The experimental jurisdiction perspective suggests a real resolution path: establish principle-based frameworks for novel mechanisms that must demonstrate equivalence to risk-prevented standards, with time-bounded graduation paths from sandbox to standard framework. This would restore mandatrophy balance by renewing the coordination function while acknowledging that novel mechanisms might achieve the same risk-suppression through structurally different means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptability_cost_threshold,
    'What cost threshold distinguishes legitimate regulatory conservatism from extractive lock-in that exceeds the social value of risk suppression?',
    'Comparative analysis: innovation rate pre- vs post-rule adoption; cost-benefit analysis of novel mechanisms blocked vs tail risks prevented; measurement of novel-mechanism failure rates in jurisdictions with principle-based alternatives',
    'If social cost of blocked innovation exceeds prevented-failure value by 2x+: classification shifts toward snare across multiple perspectives. If prevention value exceeds opportunity cost: tangled_rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptability_cost_threshold, empirical, 'Cost threshold distinguishing conservatism from extractive lock-in').

omega_variable(
    novel_mechanism_failure_mode,
    'Do novel coordination mechanisms outside the hyper-specific framework reproduce the failure modes the framework was designed to prevent, or do they fail in structurally different ways?',
    'Post-mortem analysis of failed novel mechanisms; classification of failure modes; comparison to regulated-framework failure distribution; assessment of whether regulation captures the fundamental risk or only the historical manifestation',
    'If novel failures match regulated failures: overfitting constraint is partially justified (tangled_rope confirmed). If novel failures are orthogonal: overfitting is excessive suppression (snare from analytical context).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(novel_mechanism_failure_mode, empirical, 'Whether novel mechanisms reproduce or avoid regulated failure modes').

omega_variable(
    sandbox_graduation_rate,
    'What fraction of mechanisms graduating from regulatory sandboxes or experimental zones are later integrated into standard frameworks as legitimized novel categories?',
    'Longitudinal tracking of sandbox-to-standard graduation; measurement of time-to-graduation; policy analysis of integration success rates vs. mechanism innovation rate outside sandboxes',
    'If graduation rate > 50%: scaffold perspective confirmed — exit ramps are functional. If graduation rate < 10%: sandboxes are containment, not integration (snare variant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sandbox_graduation_rate, empirical, 'Rate of sandbox mechanisms graduating to standard framework').

omega_variable(
    regulatory_text_complexity_drift,
    'Is the hyper-specific nature of compliance rules a necessary response to genuine technical complexity, or does complexity represent regulatory capture and path-dependent accumulation?',
    'Comparative analysis of regulatory text complexity across jurisdictions; measurement of rule density per risk category; analysis of regulatory comment periods and influence of incumbent firms on rule specificity; historical analysis of rule accretion patterns',
    'If complexity driven by technical necessity: tangled_rope justified. If complexity driven by incumbent capture: snare from novel-coordinator perspective confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_text_complexity_drift, empirical, 'Whether regulatory complexity reflects necessity or path-dependent capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(governance_overfitting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(govfit_tr_t0, governance_overfitting, theater_ratio, 0, 0.35).
narrative_ontology:measurement(govfit_tr_t5, governance_overfitting, theater_ratio, 5, 0.52).
narrative_ontology:measurement(govfit_tr_t10, governance_overfitting, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(govfit_be_t0, governance_overfitting, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(govfit_be_t5, governance_overfitting, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(govfit_be_t10, governance_overfitting, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(governance_overfitting, enforcement_mechanism).
narrative_ontology:affects_constraint(governance_overfitting, financial_innovation_suppression).
narrative_ontology:affects_constraint(governance_overfitting, regulatory_capture_feedback_loop).
narrative_ontology:affects_constraint(governance_overfitting, institutional_path_dependency).

% DUAL FORMULATION NOTE:
% The overfitting constraint decomposes into two structural claims with different ε values: (1) RISK COORDINATION (ε≈0.12): The baseline value of hyper-specific rules in preventing known failure modes—genuine coordination function. (2) EXTRACTIVE LOCK-IN (ε≈0.52): The accumulated regulatory capture and institutional inertia that prevents novel mechanisms from being evaluated on merits—pure extraction. These are linked: incumbent institutions and compliance vendors use risk-coordination justification to defend extractive rules. The reported ε=0.52 reflects the current state where extraction dominates the constraint's function. The two decomposed constraints would be constraint_risk_coordination_baseline (mountain, ε≈0.12) and constraint_regulatory_capture_accumulation (snare, ε≈0.52), with network affects linking them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(governance_overfitting, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
