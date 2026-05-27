% ============================================================================
% CONSTRAINT STORY: retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_retributive_reading, []).

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
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: retributive_reading
 *   human_readable: Retributive Execution: Moral Balance via Proportionate Punishment
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The retributive reading of state execution authority claims that imposing
 *   a death sentence proportionate to heinous crimes restores moral balance
 *   to a community. This is one reading of a contested kernel — the
 *   constitutional and philosophical authority to execute — that permits
 *   multiple structurally distinct instantiations depending on the
 *   justificatory foundation (retributive, deterrent, incapacitative, or
 *   abolitionist). This constraint story instantiates ONLY the retributive
 *   reading: execution's legitimacy derives from its role in proportionately
 *   responding to grave moral wrongs, not from preventing future crimes or
 *   from incapacitating danger. Under this reading, the executed offender is
 *   the legitimate cost imposed to restore equilibrium; victims' families are
 *   primary beneficiaries (they receive recognition of their loss and
 *   state-enforced proportionate response); and the state's moral authority
 *   is consolidated through demonstrating its capacity to enforce
 *   proportionate justice. The constraint exhibits high extractiveness (0.68)
 *   because the retributive framework cannot substitute imprisonment for
 *   execution without abandoning its core claim — the heinousness of the
 *   crime demands a response of equivalent gravity, making execution the only
 *   exit from the proportionality requirement. Suppression is high (0.72)
 *   because condemned offenders have minimal meaningful exits: appeals are
 *   constrained by law, clemency is discretionary, and the framework itself
 *   denies any claim to stay the punishment. Theater ratio (0.55) reflects
 *   that while the framework makes a genuine moral claim (proportionality),
 *   its institutional expression through lengthy trials, appeals, and
 *   execution rituals contains significant performative elements — the
 *   restoration claim may not correspond to actual psychological or social
 *   outcomes for victims' families.
 *
 * KEY AGENTS:
 *   - Condemned Offender: Primary victim (powerless/trapped) — bears the extracted cost (death); no exit options; framework denies legitimacy of their claims against punishment
 *   - Offenders Under Capital Sentence: Secondary victim class (powerless/trapped) — future and current death-row populations; structurally vulnerable to the framework's forward application
 *   - Victims' Families: Primary beneficiaries (moderate/constrained) — receive state recognition of their loss and enforcement of proportionate response; experience both coordination function (state response to grievance) and extraction (closure claimed to require death penalty)
 *   - State Retributive Authority: Secondary beneficiary (institutional/arbitrage) — consolidates moral authority by enforcing proportionate justice; experiences constraint as coordinating mechanism that legitimates state power
 *   - Retributive Justice Framework: Institutional persistence actor (institutional/arbitrage) — persists through inertia despite global abolition trends; theater ratio indicates performative maintenance alongside genuine moral claims
 *   - Analytical Observer: Universal/civilizational perspective (analytical/analytical) — risks naturalizing the retributive framework as immutable justice principle rather than contingent institutional arrangement with identifiable beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(retributive_reading, 0.68).
domain_priors:suppression_score(retributive_reading, 0.72).
domain_priors:theater_ratio(retributive_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(retributive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(retributive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(retributive_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(retributive_reading, snare).
narrative_ontology:human_readable(retributive_reading, "Retributive Execution: Moral Balance via Proportionate Punishment").
narrative_ontology:topic_domain(retributive_reading, "criminal_justice/political_philosophy/constitutional_law").
narrative_ontology:cs_story_uid(retributive_reading, 'c83fddb0-ae7c-4f13-8f6a-ccc21ca1414f').
narrative_ontology:cs_created_at('c83fddb0-ae7c-4f13-8f6a-ccc21ca1414f', '').
narrative_ontology:cs_kernel_codification('c83fddb0-ae7c-4f13-8f6a-ccc21ca1414f', formalized).
narrative_ontology:cs_authority_grounding('c83fddb0-ae7c-4f13-8f6a-ccc21ca1414f', extraction).
narrative_ontology:cs_kernel_id(retributive_reading, state_execution_authority).

domain_priors:requires_active_enforcement(retributive_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(retributive_reading, state_moral_authority).
narrative_ontology:constraint_beneficiary(retributive_reading, retributive_justice_framework).
narrative_ontology:constraint_victim(retributive_reading, executed_offender).
narrative_ontology:constraint_victim(retributive_reading, offenders_under_capital_sentence).
narrative_ontology:constraint_victim(retributive_reading, judicial_system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED OFFENDER (SNARE) — Structurally trapped with no exit. The retributive logic claims the offender's death restores moral balance, making execution a legitimate cost imposed without alternative. High suppression: appeals are constrained by legal procedure; clemency is discretionary not guaranteed; the framework itself denies the condemned any claim against the punishment. Maximum experienced extraction — the constraint terminates the agent's biological existence.
constraint_indexing:constraint_classification(retributive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OFFENDERS UNDER CAPITAL SENTENCE (SNARE) — Future and current death-row populations. Trapped by the retributive framework's forward-applicability. The constraint will continue extracting from this class generationally. No meaningful exit — commutation is exceptional, not systematic. The retributive logic applies to any sufficiently heinous crime, making the constraint's reach structural rather than case-specific.
constraint_indexing:constraint_classification(retributive_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: VICTIMS' FAMILIES (TANGLED ROPE) — Primary beneficiaries of the retributive reading. Constrained by their status as victims, they experience the constraint as both a coordination mechanism (the state recognizes their loss and imposes proportionate response) and an extraction mechanism (closure is claimed to require the offender's death, not available through other means). The constraint enforces a particular conception of justice they may or may not hold. Some benefit from the moral validation; others face prolonged trauma through execution proceedings. Moderate power relative to state institutions but constrained by legal procedure and emotional exposure.
constraint_indexing:constraint_classification(retributive_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE RETRIBUTIVE AUTHORITY (ROPE) — The state apparatus that claims moral legitimacy through retributive execution. Benefits from the constraint: establishes the state's authority to define and enforce moral proportionality; demonstrates responsiveness to victims' families; consolidates judicial power. The state experiences the constraint as a coordination mechanism — it solves the problem of legitimating state power by grounding it in a moral principle (proportionate response to heinous crime). Low extraction from the state's perspective because the constraint aligns with its institutional interest. Arbitrage exit: the state can modify execution methods, clemency procedures, or evidentiary standards without abandoning the retributive framework itself.
constraint_indexing:constraint_classification(retributive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RETRIBUTIVE JUSTICE FRAMEWORK — PITON (INSTITUTIONAL PERSISTENCE) — The retributive logic persists institutionally in many jurisdictions despite widespread abolition globally and significant internal critiques (wrongful convictions, racial disparity, lack of deterrent effect). Theater ratio 0.55: significant portion of retributive justification is performative — the moral restoration claim is expressed through ritual (trials, appeals, executions) that may or may not achieve the claimed restoration for victims or serve the functional purpose of incapacitating danger. The framework's persistence is driven by institutional inertia and political difficulty of abandonment, not by demonstrated effectiveness or universal moral consensus. A piton reading from civilizational scope: the constraint carries forward institutional commitments whose primary justification has degraded relative to available alternatives.
constraint_indexing:constraint_classification(retributive_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW READING (MOUNTAIN) — From a universal/civilizational perspective, the retributive logic can appear as a timeless principle: that proportionate response to grave harm is inherent to justice itself, a structural feature of how moral communities maintain equilibrium. This reading treats retributive proportionality as an immutable law of ethics or as an irreducible feature of human social organization. However, the engine will detect this as a false summit: the beneficiary set (victims' families, state authority) is identifiable; the extraction mechanism is structural (death penalty as unique cost); and the framework is historically contingent, not natural. The false summit reveals that 'proportionality is natural justice' naturalizes what is a constructed institutional arrangement with identifiable beneficiaries and victims.
constraint_indexing:constraint_classification(retributive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(retributive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(retributive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(retributive_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(retributive_reading, TR),
    TR >= 0.70.

:- end_tests(retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The retributive framework requires that the offender's death is the appropriate response to heinous crime; no lesser penalty can satisfy the proportionality requirement. This creates irreducible extraction: the framework cannot be satisfied by imprisonment, restitution, or rehabilitation because these alternatives fail to achieve the claimed moral restoration through proportionate response. The extractiveness is not incidental to the constraint — it is central to the framework's legitimacy claim. Over the 20-year interval, extractiveness increases from 0.45 to 0.68 as wrongful conviction exonerations accumulate (creating a second-order extraction: innocent people bear the cost) and as evidence fails to support deterrent effects (the framework's only functional justification beyond retribution must be abandoned, leaving pure extraction). Suppression (0.72): High. Condemned offenders face multiple structural barriers: legal procedure constrains appeals, clemency is discretionary and statistically rare, the framework itself denies moral legitimacy to their claims against punishment, and escape from capital jurisdiction is impossible (except through fugitive status or jurisdictional changes, not available to imprisoned defendants). Victims' families face constrained suppression (0.40): they benefit from the framework, but their exit from the legal process is constrained by procedural requirements and their emotional investment in the case. Theater ratio (0.55): Moderate-high. The retributive framework is partly performative: the lengthy trial, multiple appeals, and ritual execution serve to demonstrate state authority and community moral consensus, with significant theatrical content that may not translate to actual victim closure. However, the framework also contains genuine moral reasoning (proportionality principle), preventing full piton classification. The ratio increases over the interval as institutional critiques (wrongful convictions, lack of deterrent effect, racial disparities) mount, making the performative maintenance aspect more visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap illustrates how the same structural arrangement appears as pure extraction (Snare) from the condemned offender's position and as coordination (Rope) from the state authority's position. The victims' families occupy an intermediate position: they are named as the constraint's beneficiaries, yet the framework constrains their actual exit choices and may not deliver the promised moral restoration. The piton classification (institutional persistence) reveals that the framework's functional justification has degraded — deterrent effects are not empirically supported — leaving only moral/retributive justification, which relies on the performative demonstration of proportionate response. The false-summit mountain classification from the analytical perspective masks the contingency of the retributive principle itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position: condemned offenders are pure victims with no exit (trapped), producing d≈0.95, high f(d)≈1.42, high experienced χ. Victims' families are constrained beneficiaries — they benefit from the framework's recognition of their loss but face constraints from legal procedure and the framework's claim that closure requires execution, producing d≈0.60, f(d)≈0.85, moderate χ. The state authority benefits with arbitrage exit (can modify procedures without abandoning framework), producing d≈0.10, f(d)≈-0.01, low/negative χ. The piton institutional perspective has constrained exit but sees the framework as functionally degraded, producing d≈0.55, f(d)≈0.75, moderate χ. The analytical observer has analytical exit (can withdraw to analytical position) but the mountain reading naturalizes contingency, producing a false summit d≈0.70, f(d)≈1.10, but classified as Mountain — a diagnostic gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the retributive reading instantiates a genuine Snare, not a Rope or Tangled Rope. The key constraint is that the retributive framework requires execution as the proportionate response to heinous crime; no lesser penalty can substitute. This creates asymmetric extraction (high from condemned offenders, moderate from victims' families constrained by the framework) with minimal coordination benefit beyond the state's institutional interest in consolidating power. The classification is NOT hedged between types because the ε value (0.68) is intrinsic to the retributive reading: the framework cannot reduce extractiveness without abandoning its core claim. Alternative readings (deterrence, abolition) would have different ε values and different types, appearing in separate constraint stories linked via network.affects_constraints. The mandatrophy-resolving insight is that this constraint and its sibling readings are structurally distinct problems, not perspectival views of a single problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_restoration_counterfactual,
    'Does execution actually restore moral balance or psychological closure for victims'' families, or does the retributive framework merely claim this restoration?',
    'Longitudinal psychological studies comparing closure outcomes in execution vs. non-execution jurisdictions controlling for crime severity, victim demographics, and memorial practices; interview data on victims'' families before and after execution vs. long-term imprisonment',
    'If restoration is real: Tangled Rope classification confirmed — the constraint solves a genuine coordination problem. If restoration is performative: reclassify to Snare — the constraint imposes death without functional benefit to the purported beneficiaries, using their interests as moral cover for state power consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_restoration_counterfactual, empirical, 'Whether execution produces moral restoration or only claims it').

omega_variable(
    wrongful_execution_framework_validity,
    'If the retributive framework cannot eliminate wrongful convictions (currently ~1-4% of death sentences), does this undermine its moral claim?',
    'Analysis of wrongful execution rates across jurisdictions; comparison of exoneration mechanisms (post-conviction DNA, institutional review) to error rates; assessment of whether retributive logic permits tragic error or requires infallibility',
    'If framework permits tragic error as inherent cost: extractiveness remains 0.68, Snare classification stands — the constraint treats wrongful execution as acceptable cost of proportionate punishment. If framework requires near-infallibility: reclassify to Scaffold with sunset — the constraint becomes unsustainable once error rates exceed acceptable threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_execution_framework_validity, empirical, 'Whether retributive logic can accommodate wrongful execution').

omega_variable(
    racial_and_economic_disparity_integration,
    'Are racial and economic disparities in capital sentencing incidental to the retributive framework or structural to it?',
    'Comparative analysis of execution rates by race/SES across similar crime severity; assessment of whether equitable implementation is possible within constraints of adversarial legal system and resource allocation; examination of whether proportionality itself is culturally indexed',
    'If incidental and correctable: extractiveness may be reducible to <0.50 via procedural reform. If structural: the constraint is a Snare with disproportionate extraction from marginalized groups, reclassifying victims as including ''offenders from marginalized communities'' (extractiveness rises to 0.75+, Snare confirmed with explicit racial disparity data).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racial_and_economic_disparity_integration, empirical, 'Whether disparities are incidental or structural to retributive punishment').

omega_variable(
    kernel_reading_sibling_delta,
    'How does the retributive reading instantiate a different constraint than the deterrence reading or abolition reading of the same state_execution_authority kernel?',
    'Recognize that this JSON instantiates ONLY the retributive reading. The deterrence and abolition readings are separate constraint stories (see network.affects_constraints for links to their constraint_ids, once authored). This omega documents the structural delta to enable comparison analysis but does NOT fold alternative readings into this constraint''s classification.',
    'Critical: this constraint must not hedge across readings. The retributive reading has ε=0.68, Snare, with clear beneficiary/victim structure. A reader comparing to the deterrence reading will see a different ε and different type — not from measurement ambiguity but from genuine structural difference between readings of the same kernel. The omega documents the difference; the network link enables contrastive analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Structural distinction between retributive, deterrence, and abolition readings of execution authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(retributive_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(retr_tr_t0, retributive_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(retr_tr_t10, retributive_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(retr_tr_t20, retributive_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(retr_be_t0, retributive_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(retr_be_t10, retributive_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(retr_be_t20, retributive_reading, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(retributive_reading, deterrence_reading).
narrative_ontology:affects_constraint(retributive_reading, abolition_reading).
narrative_ontology:affects_constraint(retributive_reading, incapacitation_reading).
narrative_ontology:cs_reading_relation('c83fddb0-ae7c-4f13-8f6a-ccc21ca1414f', deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c83fddb0-ae7c-4f13-8f6a-ccc21ca1414f', abolition_reading, coexists_with).
% Temporal layer: deontological natural-law framework; axioms not primarily empirically grounded.
% Relative to its own reference frame, this reading is internally stable.
% Engine computes: stable + minor → stable_pattern.
narrative_ontology:cs_reference_frame('c83fddb0-ae7c-4f13-8f6a-ccc21ca1414f', natural_law_proportional_justice).
narrative_ontology:cs_drift_state('c83fddb0-ae7c-4f13-8f6a-ccc21ca1414f', contemporary,
    gap(stable, minor, true)).

% DUAL FORMULATION NOTE:
% The state_execution_authority kernel admits multiple readings that produce structurally distinct constraints. This story (retributive_reading, ε=0.68, Snare) decomposes from a conceptual node that also generates deterrence_reading (ε≈0.55, likely Tangled Rope), abolition_reading (ε≈0.10, likely Rope), and incapacitation_reading (ε≈0.60, likely Tangled Rope). Each reading is a complete, ε-invariant constraint with its own perspectives, beneficiary/victim structure, and measurements. They are linked via network.affects_constraints to enable contrastive analysis. Do NOT use the presence of multiple readings to hedge the retributive reading's classification — this reading is a clean Snare with high extractiveness because the framework itself creates irreducible extraction cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(retributive_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
