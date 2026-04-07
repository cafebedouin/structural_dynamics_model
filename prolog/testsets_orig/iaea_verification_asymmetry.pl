% ============================================================================
% CONSTRAINT STORY: iaea_verification_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iaea_verification_asymmetry, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iaea_verification_asymmetry
 *   human_readable: IAEA Verification Asymmetry in Nuclear Non-Proliferation
 *   domain: international_security/nuclear_policy
 *
 * SUMMARY:
 *   The IAEA verification regime creates a structural asymmetry at the core
 *   of non-proliferation governance: non-weapons states accept intrusive
 *   inspections of their civilian nuclear programs while the five recognized
 *   weapons states (NPT Article IX signatories) retain arsenals without
 *   reciprocal verification. This constraint exhibits multiple DR types
 *   depending on the observer's structural position. To non-nuclear states,
 *   it is a Snare — trapped by the regime with no exit option that avoids
 *   security isolation. To the IAEA, it is a Tangled Rope — genuine
 *   coordination function (detecting diversion in peaceful programs) layered
 *   with asymmetric enforcement burden. To weapons states, it appears as Rope
 *   — coordination that stabilizes the nuclear order while preserving their
 *   monopoly. The theater ratio (0.64) reflects the performative character of
 *   safeguards: sophisticated proliferators can evade detection (Iran, North
 *   Korea historical cases), and IAEA inspections cannot prevent weapons
 *   development (only detect diversion in declared facilities). The regime's
 *   extractiveness has increased over the 20-year interval (0.35 → 0.58) as
 *   threshold states have developed indigenous enrichment capabilities while
 *   remaining nominally compliant, forcing IAEA into deeper verification
 *   procedures that increase theater and suppression without proportionally
 *   increasing transparency.
 *
 * KEY AGENTS:
 *   - Non-Nuclear States: Primary victim (powerless/trapped) — subject to intrusive inspections, unable to reciprocally verify weapons states, trapped by NPT commitments with security consequences for exit
 *   - Threshold States: Secondary actor (moderate/constrained) — constrained by verification regime but benefit from ambiguity it maintains; can develop dual-use capacity while claiming compliance
 *   - Nuclear Weapons States: Primary beneficiary (institutional/arbitrage) — retain arsenals without reciprocal inspection; use NPT framework to legitimize weapons possession while restricting proliferation
 *   - IAEA Institutional Authority: Primary enforcer (institutional/constrained) — mandated to verify non-weapons-state compliance but structurally unable to verify weapons states; benefits from institutional authority while facing verification burden
 *   - Verification Transparency: Diffuse victim (powerless/trapped) — abstract public good of knowledge about nuclear weapons programs; cannot organize or exit; bears full cost of asymmetric information
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iaea_verification_asymmetry, 0.58).
domain_priors:suppression_score(iaea_verification_asymmetry, 0.68).
domain_priors:theater_ratio(iaea_verification_asymmetry, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iaea_verification_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(iaea_verification_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(iaea_verification_asymmetry, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iaea_verification_asymmetry, tangled_rope).
narrative_ontology:human_readable(iaea_verification_asymmetry, "IAEA Verification Asymmetry in Nuclear Non-Proliferation").
narrative_ontology:topic_domain(iaea_verification_asymmetry, "international_security/nuclear_policy").

domain_priors:requires_active_enforcement(iaea_verification_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iaea_verification_asymmetry, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(iaea_verification_asymmetry, iaea_institutional_authority).
narrative_ontology:constraint_victim(iaea_verification_asymmetry, non_nuclear_states).
narrative_ontology:constraint_victim(iaea_verification_asymmetry, verification_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATE UNDER INSPECTION (SNARE) — Subject to intrusive IAEA inspections with no reciprocal verification rights over weapons states. Trapped by NPT commitments with no exit without reputational/security cost. Full extraction: inspected but not inspecting; disarmed but not reciprocally verified.
constraint_indexing:constraint_classification(iaea_verification_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THRESHOLD STATE (TANGLED ROPE) — Constrained by verification regime but also benefits from ambiguity it creates; can maintain deterrent effect while ostensibly compliant. High suppression (inspection pressure) but genuine coordination benefit (security stability). Extraction with asymmetric burden-sharing.
constraint_indexing:constraint_classification(iaea_verification_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NUCLEAR WEAPONS STATE (ROPE) — Benefits from IAEA as coordination mechanism that legitimizes weapons possession while appearing to restrict proliferation. No meaningful inspections of arsenals. Experiences constraint as coordination: NPT framework stabilizes the nuclear order while preserving their advantage.
constraint_indexing:constraint_classification(iaea_verification_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IAEA INSTITUTIONAL ACTOR (TANGLED ROPE) — Mandated to verify non-weapons-state compliance while structurally unable to verify weapons states (no authority, no access, no political will). Genuine coordination function (detecting diversion in peaceful programs) layered with extractive enforcement burden (asymmetric inspection pressure). Active enforcement required; benefits member states that control the board.
constraint_indexing:constraint_classification(iaea_verification_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SAFEGUARDS THEATER (PITON) — The IAEA inspection ritual is substantially performative: sophisticated states can conceal weapons programs (Iran, North Korea historical cases), and inspections cannot prevent weapons development (only detect diversion). The ritual persists through institutional inertia and legitimacy theater — countries participate because non-participation signals hostile intent, not because inspections reliably prevent proliferation. High theater ratio reflects the gap between stated verification goal and actual detection capacity.
constraint_indexing:constraint_classification(iaea_verification_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL ASYMMETRY VIEW (MOUNTAIN) — From a civilizational timescale, the asymmetry appears as a natural law of international relations: you cannot simultaneously maintain a weapons monopoly AND have credible verification of weapons elimination without reciprocal inspection. The NPT's core contradiction is structural, not contingent. The asymmetry is inherent to any regime that permits some states to retain weapons. However, this classification risks naturalizing what is a political choice: the five permanent Security Council members deliberately constructed a regime that protects their advantage. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(iaea_verification_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iaea_verification_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iaea_verification_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iaea_verification_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iaea_verification_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iaea_verification_asymmetry, TR),
    TR >= 0.70.

:- end_tests(iaea_verification_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The asymmetry extracts significant political and security advantage for weapons states, but the extraction is not maximal (0.70+) because threshold states can maintain ambiguity, and some non-weapons states derive security benefits from the NPT framework (normalization of non-weapons status, access to civilian nuclear technology). The value reflects that the extraction mechanism — mandatory inspection + inability to exit — is real but operates within a framework that provides some countervailing coordination benefits. Suppression (0.68): High. Multiple suppression mechanisms: (1) institutional — IAEA has inspection authority backed by Security Council; (2) reputational — withdrawal from NPT signals hostile intent and triggers sanctions/isolation; (3) material — weapons development is technically difficult and costly; (4) cognitive — normalization of non-weapons identity makes proliferation psychologically distant. Theater ratio (0.64): Moderate-high. The IAEA safeguards regime performs significant legitimacy theater — inspections are publicized as preventing weapons, but detection rates for clandestine programs are uncertain, and the regime cannot prevent a determined state with sufficient technical capacity. However, theater is not maximal (0.70+) because some actual information is generated: inspections do detect diversion when it occurs, and the regime creates friction that slows (if not prevents) proliferation.
 *
 * PERSPECTIVAL GAP:
 *   The widest perspectival gap is between the non-nuclear state (Snare) and the weapons state (Rope). Both experience the same institutional structure, but extraction flows in opposite directions. From the weapons state's view, the NPT is a coordination mechanism that solves the collective action problem of preventing proliferation while preserving their security advantage. From the non-nuclear state's view, it is a trap: accept inspections or face military/economic consequences, with no reciprocal rights. The IAEA's institutional perspective (Tangled Rope) is caught between: genuine coordination function (detecting diversion) requires active enforcement (inspections, intrusions), which falls asymmetrically on non-weapons states. The threshold state's perspective (Tangled Rope) is distinctive: constrained by the regime but benefits from the opacity the regime permits — can develop weapons capability while nominally compliant. The analytical observer risks seeing the asymmetry as an immutable structural feature (Mountain) — you cannot have a weapons monopoly and reciprocal verification simultaneously — but this naturalizes a political choice: the weapons states deliberately constructed a regime that protects their advantage, and alternative regimes (universal disarmament, reciprocal verification, graduated weapon elimination) are technically feasible but politically rejected.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values: Non-nuclear states experience high d (~0.88) because they are pure targets of inspection/suppression with minimal beneficiary status; their exit options are trapped (withdrawal triggers security consequences). Weapons states experience low d (~0.18) because they are net beneficiaries of the asymmetry; their exit options are arbitrage (they can maintain or expand arsenals with minimal verification cost). The IAEA experiences moderate d (~0.65) because it is structurally constrained between mandated verification (which targets non-weapons states) and institutional pressure from weapons states (which shield themselves from verification). The threshold state experiences moderate d (~0.52) because it bears inspection burden but benefits from the ambiguity the regime permits. These directionality values are derived from the explicit beneficiary/victim declarations: weapons states and IAEA institutional authority are beneficiaries; non-nuclear states and verification transparency are victims. The derived d values feed into the sigmoid f(d) to produce experienced extractiveness chi that varies across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy through perspectival relativism: all six types are legitimate readings from different structural positions. The non-nuclear state experiences Snare (pure extraction with no exit). The weapons state experiences Rope (coordination mechanism). The IAEA experiences Tangled Rope (mixed coordination-extraction). The threshold state experiences Tangled Rope (constrained with ambiguity benefit). The safeguards theater is Piton (performative ritual maintained by institutional inertia). The analytical observer experiences Mountain risk (naturalizing a political choice as structural law). No single type is 'correct' — the constraint's actual structure is the presheaf of all perspectival readings. The mandatrophy is resolved by showing that the asymmetry is neither pure coordination (Rope would require symmetric burden-sharing) nor pure extraction (Snare would require zero coordination benefit for victims), but a hybrid where coordination benefits some agents while extracting from others, with active enforcement required to maintain the asymmetry. The increased theater ratio over time (0.42 → 0.64) and extractiveness (0.35 → 0.58) indicate a degradation toward Snare/Piton classification: as weapons states develop more sophisticated concealment and non-weapons states develop dual-use capacity, the original coordination function (preventing proliferation) becomes harder to verify (theater rises) while the extraction mechanism (mandatory inspection of non-weapons states) becomes more intrusive (extractiveness rises).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocal_inspection_feasibility,
    'Could weapons states'' arsenals be verified via IAEA inspections without compromising strategic military security?',
    'Technical analysis of warhead verification protocols; comparison with arms control regimes (START treaties) that achieved non-NPT verification; assessment of transparency vs. military operational security tradeoffs',
    'If feasible: asymmetry is political choice, not structural necessity — classification stays Tangled Rope/Snare. If infeasible: asymmetry reflects genuine verification limits — Mountain classification gains structural grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_inspection_feasibility, empirical, 'Technical feasibility of reciprocal arsenal verification').

omega_variable(
    inspection_detection_reliability,
    'What is the true detection rate of IAEA safeguards for clandestine weapons programs in non-weapons states?',
    'Post-incident analysis (Iran, North Korea, Iraq) of what was detected vs. what was concealed; assessment of detection confidence intervals; comparison with baseline false positive/negative rates for complex technical verification',
    'If high (>80%): safeguards are functionally meaningful coordination, extraction is moderate (Tangled Rope confirmed). If low (<50%): safeguards are theater (Piton elevated), extraction is effective (Snare confirmed for victims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inspection_detection_reliability, empirical, 'Actual detection reliability of IAEA safeguards').

omega_variable(
    npt_exit_cost_structure,
    'What are the true security, economic, and reputational costs for a non-weapons state to withdraw from the NPT?',
    'Case studies of withdrawal threats (Iran, North Korea) and their consequences; modeling of security spiral if withdrawal threshold is crossed; assessment of whether costs are structural or contingent on current geopolitical distribution',
    'If costs are catastrophic and unavoidable: exit is truly ''trapped'' (d~0.95). If costs are severe but calculable: exit is ''constrained'' (d~0.70). This determines whether the classification shifts from Snare (trapped) to Tangled Rope (constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_exit_cost_structure, empirical, 'True exit costs for NPT withdrawal').

omega_variable(
    weapons_state_incentive_divergence,
    'Do all five weapons states benefit equally from the NPT asymmetry, or do some face hidden costs that offset the verification advantage?',
    'Analysis of proliferation threats to each weapons state; assessment of whether the NPT asymmetry actually prevents the specific threats each faces; identification of states for whom a reciprocal verification regime might be strategically preferable',
    'If some weapons states would prefer a reciprocal regime: NPT appears as coordination mechanism constrained by path dependence, not as pure extraction (Rope/Tangled Rope distinction). If all benefit equally: asymmetry is pure privilege (Snare from non-weapons state perspective confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weapons_state_incentive_divergence, conceptual, 'Whether weapons states uniformly benefit from NPT asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iaea_verification_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iaea_tr_t0, iaea_verification_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iaea_tr_t10, iaea_verification_asymmetry, theater_ratio, 10, 0.54).
narrative_ontology:measurement(iaea_tr_t20, iaea_verification_asymmetry, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(iaea_be_t0, iaea_verification_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iaea_be_t10, iaea_verification_asymmetry, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(iaea_be_t20, iaea_verification_asymmetry, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iaea_verification_asymmetry, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(iaea_verification_asymmetry, 0.18).
narrative_ontology:affects_constraint(iaea_verification_asymmetry, nuclear_threshold_state_ambiguity).
narrative_ontology:affects_constraint(iaea_verification_asymmetry, iaea_technical_safeguards_limits).
narrative_ontology:affects_constraint(iaea_verification_asymmetry, npt_compliance_reporting_asymmetry).

% DUAL FORMULATION NOTE:
% The IAEA verification asymmetry is downstream of the NPT's structural architecture and affects multiple related constraints: the technical limits of safeguards detection (can inspections reliably find clandestine programs?), the strategic behavior of threshold states (how do they exploit the ambiguity?), and the compliance reporting regime (how asymmetric is disclosure?). Each has its own extractiveness value reflecting different empirical status. The verification asymmetry story focuses on the regime's directional extraction; the safeguards limits story focuses on technical detection reliability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iaea_verification_asymmetry, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
