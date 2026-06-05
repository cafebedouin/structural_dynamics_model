% ============================================================================
% CONSTRAINT STORY: devolution_settlements__sewel_strain_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_devolution_settlements__sewel_strain_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: devolution_settlements__sewel_strain_reading
 *   human_readable: Sewel Convention Strain: Westminster's Recovered Supremacy over Devolved Consent
 *   domain: constitutional_law/devolution
 *
 * SUMMARY:
 *   The Sewel Convention constrains Westminster's legislative authority over
 *   devolved matters — 'it is normally the case that Westminster legislates
 *   on devolved matters only with the consent of the devolved legislature.'
 *   This constraint was stress-tested by Brexit. When the UK Parliament
 *   legislated to withdraw from the European Union in 2017, the Scottish
 *   Parliament withheld its consent (voting 69–49 against the EU Withdrawal
 *   Bill). Westminster proceeded anyway, overriding the devolved objection
 *   and citing the extraordinary nature of the referendum mandate and
 *   external relations as justification for bypassing the normal rule. This
 *   reading instantiates the constraint as it appears when the convention's
 *   coordination function collides with Westminster's claimed supremacy: the
 *   constraint is genuine (it coordinated devolved and Westminster
 *   legislatures for two decades), yet conditional (it can be overridden when
 *   Westminster deems circumstances extraordinary). The 'not normally' clause
 *   revealed itself as containing a permanent escape hatch: 'not until it
 *   matters' — not until Westminster's core authority is at stake. This is
 *   the sewel-strain reading: the settlement that appeared to entrench
 *   devolved consent actually entrenched Westminster's right to override
 *   consent when it chose.
 *
 * KEY AGENTS:
 *   - Westminster Parliament: Primary beneficiary (institutional/arbitrage) — recovers unrestricted supremacy through the override; maintains formal coordination machinery while retaining unilateral override capacity
 *   - Devolved Legislatures (Scottish Parliament, Welsh Assembly, Northern Ireland Executive): Primary victims (powerless/trapped) — consent withheld but overridden; no exit option except independence (which requires Westminster permission under established precedent)
 *   - Conventional Federalism Credibility: Secondary victim (powerless/trapped) — the credibility of devolution as genuine power-sharing is damaged by the override; the convention that was meant to protect devolved autonomy is revealed as waivable at Westminster's discretion
 *   - Independence Movement Coalition: Organized actor (organized/constrained) — benefits from devolved legislative experience but bears the cost of discovering Sewel protection is conditional; constrained by Westminster's refusal to permit independence referendums without invitation
 *   - Constitutional Convention Framework: Institutional actor (institutional/arbitrage) — maintains formal procedures and theater of consultation despite functional degradation of binding force
 *   - Analytical Observer (Parliamentary Supremacy tradition): Institutional actor (analytical/analytical) — sees no constraint at all, only the inevitable expression of Westminster supremacy that conventions momentarily masked
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(devolution_settlements__sewel_strain_reading, 0.58).
domain_priors:suppression_score(devolution_settlements__sewel_strain_reading, 0.65).
domain_priors:theater_ratio(devolution_settlements__sewel_strain_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(devolution_settlements__sewel_strain_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(devolution_settlements__sewel_strain_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(devolution_settlements__sewel_strain_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(devolution_settlements__sewel_strain_reading, tangled_rope).
narrative_ontology:human_readable(devolution_settlements__sewel_strain_reading, "Sewel Convention Strain: Westminster's Recovered Supremacy over Devolved Consent").
narrative_ontology:topic_domain(devolution_settlements__sewel_strain_reading, "constitutional_law/devolution").

domain_priors:requires_active_enforcement(devolution_settlements__sewel_strain_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(devolution_settlements__sewel_strain_reading, 'e30a42f8-f697-4d63-afa3-76e28f41c6e3').
narrative_ontology:cs_kernel_codification('e30a42f8-f697-4d63-afa3-76e28f41c6e3', formalized).
narrative_ontology:cs_authority_grounding('e30a42f8-f697-4d63-afa3-76e28f41c6e3', extraction).
narrative_ontology:cs_reading_relation('e30a42f8-f697-4d63-afa3-76e28f41c6e3', devolution_settlements__independence_pathway_reading, coexists_with).
narrative_ontology:cs_reading_relation('e30a42f8-f697-4d63-afa3-76e28f41c6e3', devolution_settlements__reserved_powers_model_reading, influences).
narrative_ontology:cs_axiom('e30a42f8-f697-4d63-afa3-76e28f41c6e3', foundational, sewel_consent_is_real_until_override).
narrative_ontology:cs_axiom_status(sewel_consent_is_real_until_override, holdable).
narrative_ontology:cs_axiom_grounding('e30a42f8-f697-4d63-afa3-76e28f41c6e3', sewel_consent_is_real_until_override, conventional).
narrative_ontology:cs_axiom('e30a42f8-f697-4d63-afa3-76e28f41c6e3', foundational, extraordinary_circumstances_permit_unilateral_override).
narrative_ontology:cs_axiom_status(extraordinary_circumstances_permit_unilateral_override, holdable).
narrative_ontology:cs_axiom_grounding('e30a42f8-f697-4d63-afa3-76e28f41c6e3', extraordinary_circumstances_permit_unilateral_override, conventional).
narrative_ontology:cs_reference_frame('e30a42f8-f697-4d63-afa3-76e28f41c6e3', devolution_as_binding_power_transfer).
narrative_ontology:cs_drift_state('e30a42f8-f697-4d63-afa3-76e28f41c6e3', post_brexit_override, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e30a42f8-f697-4d63-afa3-76e28f41c6e3', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(devolution_settlements__sewel_strain_reading, devolution_settlements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(devolution_settlements__sewel_strain_reading, westminster_parliament).
narrative_ontology:constraint_victim(devolution_settlements__sewel_strain_reading, devolved_legislatures).
narrative_ontology:constraint_victim(devolution_settlements__sewel_strain_reading, conventional_federalism_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVOLVED LEGISLATURES (SNARE) — Scottish Parliament and Welsh Assembly initially believed the Sewel Convention created binding constraint on Westminster — a coordination mechanism protecting their legislative sphere. Brexit revealed this was theater: consent withheld (Scottish Parliament voted against UK-EU withdrawal bill), yet Westminster proceeded anyway. No exit option: legislatures cannot overrule Westminster, cannot secede without Westminster's permission (despite independence referendum precedent), cannot withdraw legislative participation without losing all agency. Maximum experienced extraction — the convention proved waivable at the hard case, converting apparent coordination protection into naked subordination.
constraint_indexing:constraint_classification(devolution_settlements__sewel_strain_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENCE MOVEMENT COALITION (TANGLED ROPE) — The Sewel strain strengthens the independence pathway (genuine coordination function: the convention enabled devolved autonomy that made national parliament identity plausible), but also extracts: the discovery that Sewel is waivable at hard cases demonstrates the coordination is conditional, not structural. The coalition has constrained exit — holding independence referendums is possible but requires Westminster permission (established precedent, 2014), and Westminster's consent is not guaranteed. They benefit from the devolved institutional structure and legislative experience that Sewel protected, but bear the cost of discovering those protections are conditional on Westminster's unilateral judgment of what constitutes the 'normal case.'
constraint_indexing:constraint_classification(devolution_settlements__sewel_strain_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WESTMINSTER PARLIAMENT (ROPE) — Experiences Sewel as pure coordination: consulting devolved bodies makes legislative process smoother, builds consensus, and legitimates UK-wide decisions. The convention creates procedural coordination without binding Westminster to accept devolved positions. Sewel is a coordination mechanism that Westminster can exit at will (arbitrage: can choose to override consent if it deems the matter outside the 'normal legislative process'). The Brexit invocation of override — 'this is extraordinary, not normal' — demonstrates Westminster's supremacy was never waivable; only its *exercise* was conventionally constrained. Net beneficiary.
constraint_indexing:constraint_classification(devolution_settlements__sewel_strain_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTITUTIONAL CONVENTION FRAMEWORK (PITON) — The Sewel Convention as a formalized constitutional principle (codified in legislation, referenced in Scotland Act 1998, Wales Act 2017) is theatrically maintained but functionally eroded. The convention persists through institutional inertia — legislatures continue to consult, procedures are followed, the ritual of seeking consent is performed — but the mechanism is degraded. Theater ratio (0.48) reflects that pre-Brexit, the convention was substantially functional; post-Brexit, it is substantially performative. The consultations continue and superficially resemble coordination, but the subordination is now visible. The piton persists because formal repudiation would be politically costly; instead, the override is justified through the 'not normal' exception clause, which functions as a permanent escape hatch that prevents the convention from ever becoming binding.
constraint_indexing:constraint_classification(devolution_settlements__sewel_strain_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / PARLIAMENTARY SUPREMACY (MOUNTAIN) — From a civilizational perspective grounded in Westminster constitutional doctrine, Parliamentary supremacy is an immutable feature of the UK constitutional order: no Parliament can bind its successors, and no convention can override statute. The Sewel Convention has never been legally enforceable — it is a courtesy, not a constraint. From this view, the Brexit override demonstrates not the convention's breakdown but the accurate description of what the convention always was: a self-imposed restraint subject to override when sovereignty deemed it necessary. This perspective naturalizes Westminster supremacy as an intrinsic property of the constitutional system itself.
constraint_indexing:constraint_classification(devolution_settlements__sewel_strain_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONDITIONAL FEDERALISM (TANGLED ROPE) — Alternatively, from a reading that honors the devolution settlements as genuine constitutional commitments (not just Westminster courtesies), the Sewel strain reveals a hybrid constraint: the settlements genuinely devolved legislative power (coordination function — legislatures coordinate with Westminster on matters affecting both), AND Westminster's unilateral right to override in 'extraordinary circumstances' extracts supremacy back (asymmetric extraction). The 'not normally' clause creates a permanent condition where the devolved consent is real until it matters, at which point Westminster recovers full authority. This is tangled rope precisely because both elements — real coordination and waivable extraction — are structurally present and neither can be eliminated without breaking the union itself.
constraint_indexing:constraint_classification(devolution_settlements__sewel_strain_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(devolution_settlements__sewel_strain_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(devolution_settlements__sewel_strain_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(devolution_settlements__sewel_strain_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(devolution_settlements__sewel_strain_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(devolution_settlements__sewel_strain_reading, TR),
    TR >= 0.70.

:- end_tests(devolution_settlements__sewel_strain_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint coordinates devolved and Westminster legislatures on matters affecting both (genuine coordination function reducing conflict and enabling joint problem-solving). However, Westminster's unilateral right to override in 'extraordinary circumstances' extracts back the supremacy that devolution nominally granted. The measurement trajectory shows the key dynamic: before devolution (t=0, ε=0.15), Westminster's supremacy was unconstrained; post-devolution (t=20, ε=0.25), the Sewel Convention created genuine coordination (extraction minimal — Westminster now must justify overrides as extraordinary). Post-Brexit override (t=25, ε=0.58), extraction rises sharply because the 'extraordinary' exception is demonstrated to be invokable when Westminster's interests require it. The extractiveness value reflects that the convention is real (not pure supremacy theater like the mountain perspective suggests) but conditional (not binding coordination like the rope perspective suggests). Suppression (0.65): High. Devolved legislatures face substantial barriers to asserting their consent as binding: (1) Westminster can override with minimal constitutional friction, (2) challenging an override requires constitutional litigation with uncertain outcome, (3) independence as an exit is blocked pending Westminster permission, (4) the 'not normal' escape clause is undefined and context-dependent, giving Westminster discretion to invoke it. Theater ratio (0.48): Moderate. The measurement trajectory reveals why this is the key diagnostic metric. Pre-devolution (t=0, theater=0.95), Westminster supremacy was pure theater — no consultation occurred because none was required. Post-devolution (t=20, theater=0.35), the constraint become substantially functional — genuine consultations, real deliberation, substantive coordination. Post-Brexit override (t=25, theater=0.48), theater rises again as the consultative ritual continues but the binding force is revealed as conditional. The ritual persists (legislatures meet, committees review, consent-seeking occurs) but the audience now knows the consent can be overridden, making the continued performance of consultation a form of theater that maintains the appearance of coordination while operating under newly revealed supremacy.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives is stark. Westminster sees rope (pure coordination that it controls and can exit at will). Devolved legislatures see snare (they consented, believing the convention bound Westminster, and the override trapped them with no recourse). The independence movement sees tangled rope (the devolved institutions that made independence plausible are revealed to operate under Westminster's ultimate veto). The constitutional convention framework sees piton (the formal procedures persist but the mechanism is degraded). The parliamentary-supremacy analytical observer sees mountain (Westminster's authority is an immutable constitutional law, and conventions can never bind Parliament). The conditional-federalism analytical observer sees tangled rope (the settlement genuinely devolved power AND preserved Westminster supremacy, making a hybrid constraint permanent). The perspectival gap is not a minor variance in interpretation — it is a fundamental disagreement about the nature of the constraint itself. What one perspective experiences as coordination, another experiences as disguised extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the constraint. Westminster Parliament's institutional power + arbitrage exit options → low d (beneficiary status with escape capacity) → negative effective extraction (Westminster experiences the constraint as net benefit — coordination that it can exit unilaterally). Devolved legislatures' powerless status + trapped exit options → high d (victim status with no exit) → high effective extraction (they experience the constraint as binding until Westminster override, then as nakedly subordinate). The independence coalition's organized power + constrained exit (referendums require Westminster permission) → moderate-high d (mixed victim-partial-beneficiary status, as they benefit from devolved institutions but cannot exit to independence without Westminster consent) → moderate extraction. The conditional-federalism analyst sees the constraint as genuinely hybrid (d ≈ 0.50, f(d) ≈ 0.65) because both coordination and extraction are structurally present. The parliamentary-supremacy analyst sees no extraction at all (d ≈ 0.00, the constraint is Westminster expressing its nature, not extracting from subordinates).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sewel_legal_bindingness_contest,
    'Is the Sewel Convention a legal constraint on Parliament, or a purely political/conventional restraint subject to parliamentary override?',
    'Constitutional jurisprudence and legislative codification: the Scotland Act 2016 and Wales Act 2017 each contain formalized Sewel clauses. If courts eventually rule that these clauses create legally enforceable obligations, the convention shifts from convention to law (extraction changes). If courts confirm the ''courtesy'' reading, the convention remains politically enforceable but legally waivable.',
    'If legal bindingness established: devolved consent becomes genuine coordination (suppression drops, theater drops, constraint reclassifies toward Rope). If legal waivability confirmed: Westminster supremacy remains unrestricted (extraction rises, theater rises, constraint remains Tangled Rope or shifts toward Snare from devolved perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sewel_legal_bindingness_contest, empirical, 'Whether Sewel Convention is legally binding or purely political restraint').

omega_variable(
    normal_vs_extraordinary_case_boundary,
    'What constitutes the ''normal'' legislative process that triggers Sewel consultation versus the ''extraordinary'' circumstance that permits override?',
    'Future constitutional practice: clarification through parliament''s own handling of subsequent devolved-matter legislation, or through formal constitutional codification. The Brexit case was treated as extraordinary under the reasoning that EU membership affects the entire union''s external relations. Would similar overrides apply to immigration, taxation, trade policy within the union?',
    'If ''extraordinary'' remains undefined and context-dependent: Westminster retains absolute discretion to decide when override is justified (extraction maximized, suppression maximized). If ''extraordinary'' is defined narrowly (external relations only): consent becomes more binding for domestic legislation (coordination strengthened). If ''extraordinary'' is eliminated entirely: Sewel becomes genuine binding coordination (Rope from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normal_vs_extraordinary_case_boundary, conceptual, 'Definition of ''extraordinary'' vs ''normal'' that determines override applicability').

omega_variable(
    kernel_reading_contest_resolution,
    'Which reading of the devolution settlements is constitutionally determinative: the independence pathway reading (settlements enabled exit capacity), the reserved-powers reading (settlements reversed the default to expansive devolution), or the sewel-strain reading (settlements'' coordination was conditional on Westminster''s unilateral judgment)?',
    'Constitutional practice over the next generational horizon: will independence referendums become repeatable without Westminster permission? Will reserved-powers interpretation expand or contract devolved authority? Will the Sewel strain persist or will the convention be either legally codified or formally abandoned?',
    'If independence pathway dominates: settlements are exit ramps (sibling reading forecloses sewel-strain reading — independence trajectories and conditional coordination are incompatible). If reserved-powers model dominates: settlements are genuine expansive devolution (coexists with sewel-strain but influences it — reserved-powers residue creates leverage for devolved bodies to resist extraordinary override claims). If sewel-strain reading is consolidated: conditional coordination becomes the stable equilibrium (settlements maintain union by making devolved consent nominally binding but actually waivable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Which reading of the contested devolution settlements kernel becomes institutionally dominant').

omega_variable(
    devolved_consent_strategic_value_post_strain,
    'After the Sewel override, does devolved consent retain strategic value for Westminster, or has it been devalued by the revelation that it is waivable?',
    'Behavioral analysis: does Westminster continue to seek Sewel consent in subsequent legislation despite having overridden it once? Do devolved legislatures continue to grant consent, knowing it can be overridden? Does the norm persist or atrophy?',
    'If consent retains value: Westminster prefers coordination (Sewel norm is maintained as courtesy despite waivability; theater ratio stays moderate; constraint remains Tangled Rope). If consent is devalued: Westminster abandons consultation on devolved matters (theater rises, suppression rises, extraction rises, constraint shifts toward Snare). The atrophy of the norm despite its formal codification is a degradation signal (toward Piton territory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devolved_consent_strategic_value_post_strain, empirical, 'Whether devolved consent retains strategic value post-override or atrophies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(devolution_settlements__sewel_strain_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sewel_theater_pure_supremacy_pre_1998, devolution_settlements__sewel_strain_reading, theater_ratio, 0, 0.95).
narrative_ontology:measurement(sewel_theater_coordination_phase_1998_2016, devolution_settlements__sewel_strain_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(sewel_theater_post_brexit_degradation_2019, devolution_settlements__sewel_strain_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(sewel_extractiveness_pre_devolution_act_1998, devolution_settlements__sewel_strain_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sewel_extractiveness_post_devolution_act_1998, devolution_settlements__sewel_strain_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(sewel_extractiveness_post_brexit_override_2019, devolution_settlements__sewel_strain_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sewel_suppression_pre_devolution, devolution_settlements__sewel_strain_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sewel_suppression_post_devolution_1998, devolution_settlements__sewel_strain_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(sewel_suppression_post_brexit_override_2019, devolution_settlements__sewel_strain_reading, suppression_requirement, 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(devolution_settlements__sewel_strain_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(devolution_settlements__sewel_strain_reading, devolution_settlements__independence_pathway_reading).
narrative_ontology:affects_constraint(devolution_settlements__sewel_strain_reading, devolution_settlements__reserved_powers_model_reading).
narrative_ontology:affects_constraint(devolution_settlements__sewel_strain_reading, scottish_independence_referendum_mandate).

% DUAL FORMULATION NOTE:
% The devolution settlements kernel decomposes into three structurally distinct constraint readings with different extractiveness values and different beneficiary/victim structures. The sewel_strain_reading (this constraint, ε=0.58) describes the coordination-with-override hybrid. The independence_pathway_reading describes the settlement as creating exit capacity (expected ε lower, classification toward Rope or Scaffold for independence coalition). The reserved_powers_model_reading describes the settlement as reversing power allocation defaults (expected ε lower, classification toward Rope or Tangled Rope for devolved legislatures). All three readings affect each other: the sewel strain weakens the credibility of conventional federalism, which increases the salience of the independence pathway; the independence pathway is enabled by the reserved-powers reversal; the sewel strain demonstrates that reserved powers alone do not constrain Westminster override.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(devolution_settlements__sewel_strain_reading, institutional, 0.12).
constraint_indexing:directionality_override(devolution_settlements__sewel_strain_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
