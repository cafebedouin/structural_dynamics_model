% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market as Natural Default (Hybrid Amnesia Reading)
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   Between 1930 and 1970, alternatives to market coordination (guild
 *   socialism, syndicalism, Soviet planning, cooperative commonwealth models)
 *   gradually faded from intellectual and political memory, not primarily
 *   through active suppression but through the success of market economies at
 *   capital accumulation, the prestige of neoclassical economics, and the
 *   displacement of radical intellectual movements by war and Cold War. From
 *   1980 onward, beneficiaries (incumbent economists, finance institutions,
 *   neoclassical departments) inherit this pre-existing amnesia and
 *   deliberately rationalize it, producing a narrative that markets are
 *   natural and inevitable rather than contingent and inherited through
 *   forgetting. The constraint operates in two stages: genuine historical
 *   lapse (1930-1970, extractiveness low because the forgetting was not yet
 *   weaponized) followed by defensive rationalization (1980-present,
 *   extractiveness rising as beneficiaries actively maintain the amnesia and
 *   suppress the counter-narrative that alternatives were deliberately closed
 *   off). This reading differs structurally from lapsed_alternative_reading
 *   (which treats the amnesia as passive residue) and
 *   beneficiary_maintained_reading (which treats beneficiary defense as
 *   primary rather than parasitic on pre-existing forgetting).
 *
 * KEY AGENTS:
 *   - Incumbent market beneficiaries (academic economists, finance): inherit amnesia, then actively maintain the forgetting post-1980
 *   - General economists post-1970: identity-locked into the market-natural framing; unable to exit without professional death
 *   - Policy makers: constrained not by force but by collapsed alternatives in the intellectual landscape
 *   - Alternative economy advocates: excluded not by censorship but by intellectual implausibility within the dominance frame
 *   - Historical scholars: possess the full record but are marginal in professional economics and policy discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market as Natural Default (Hybrid Amnesia Reading)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, 'e3019ecb-22c9-4ad9-98a3-4d16e1171278').
narrative_ontology:cs_kernel_codification('e3019ecb-22c9-4ad9-98a3-4d16e1171278', distributed).
narrative_ontology:cs_authority_grounding('e3019ecb-22c9-4ad9-98a3-4d16e1171278', extraction).
narrative_ontology:cs_interpretation_layer_present('e3019ecb-22c9-4ad9-98a3-4d16e1171278').
narrative_ontology:cs_reading_relation('e3019ecb-22c9-4ad9-98a3-4d16e1171278', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('e3019ecb-22c9-4ad9-98a3-4d16e1171278', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('e3019ecb-22c9-4ad9-98a3-4d16e1171278', foundational, amnesia_enables_extraction).
narrative_ontology:cs_axiom_status(amnesia_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e3019ecb-22c9-4ad9-98a3-4d16e1171278', amnesia_enables_extraction, empirically_contingent).
narrative_ontology:cs_axiom('e3019ecb-22c9-4ad9-98a3-4d16e1171278', secondary, coordination_function_justification_decayed).
narrative_ontology:cs_axiom_status(coordination_function_justification_decayed, holdable).
narrative_ontology:cs_axiom_grounding('e3019ecb-22c9-4ad9-98a3-4d16e1171278', coordination_function_justification_decayed, empirically_contingent).
narrative_ontology:cs_reference_frame('e3019ecb-22c9-4ad9-98a3-4d16e1171278', market_as_inevitable_coordination).
narrative_ontology:cs_drift_state('e3019ecb-22c9-4ad9-98a3-4d16e1171278', contemporary_2020, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e3019ecb-22c9-4ad9-98a3-4d16e1171278', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_beneficiaries).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, alternative_economy_proponents).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, state_planning_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.20) in 1930 because the amnesia is genuine forgetting, not yet a strategic device — the constraint operates mostly as passive lapse. It rises steadily (0.45 by 2020) as beneficiaries inherit the amnesia and then weaponize it, deliberately defending market naturalism against revival of alternatives. Theater ratio rises sharply (0.15 to 0.58) because the constraint's function shifts from coordination (early) to legitimacy-defense (late). After 1980, economic justifications for markets become increasingly rhetorical (defending liberalization against alternatives that have been made intellectually impossible rather than empirically defeated). Suppression rises throughout because the constraint must actively prevent the counter-narrative (alternatives were viable and their closure was chosen) from becoming credible. Accessibility collapse is high (0.71) because once the amnesia is accepted, alternatives become literally unthinkable within the professional discourse — you cannot even conceive of what you have forgotten. Resistance is moderate (0.48) because beneficiaries do not need to use crude force; intellectual gatekeeping (peer review, hiring, textbook authority) accomplishes the suppression structurally.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (incumbent economists and finance), the constraint appears as legitimate intellectual leadership defending correct doctrine against unscientific alternatives. From the alternative-economy-advocates' seat, the same constraint appears as active intellectual capture: a closing of the space of legitimate discussion around pre-existing amnesia. The engine computes these divergent positions from the asymmetric exit (beneficiaries have arbitrage options and global institutional positions; advocates are constrained to regional, marginal platforms) and the asymmetric knowledge (beneficiaries inherit and rationalize forgetting; advocates try to document what was forgotten). The perspectival gap is structural, not a matter of disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent beneficiaries sit at d ≈ 0.1 (full beneficiaries): they collect professional prestige, policy influence, and intellectual authority from the constraint's persistence; they have arbitrage options (they could acknowledge alternatives but choose not to). General economists sit at d ≈ 0.4 (mild target): they are fused with the doctrine through identity but are not the captorers of its benefits. Policy makers sit at d ≈ 0.75 (target): they cannot execute alternatives they have forgotten exist. Alternative advocates sit at d ≈ 0.9 (full target): they bear the extraction (exclusion from legitimate discourse) and have trapped/constrained exit (must argue within the frame that has declared them illegitimate). The constraint has genuinely asymmetric extraction: beneficiaries gain intellectual and material advantage; payers lose the ability to perceive and choose alternatives. This asymmetry is what makes it Tangled Rope (coordination function present, but riding on extraction) rather than pure Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-1945 coordination without centralized planning) is dead by 2000 — computers, input-output analysis, and decentralized information technology made alternative coordination mechanisms feasible. Yet the intellectual settlement that markets are natural and inevitable persists. This is exactly the mandatrophy signature: the constraint was built to solve a live problem and still functions to coordinate, but that coordinating function no longer justifies the extraction it carries. The persistence of market naturalism after 1980 is not the persistence of a justified settlement but the inertial operation of an outdated intellectual apparatus kept alive by institutional beneficiaries. The constraint computes as Tangled Rope because it still carries a real coordination element (prices do signal and coordinate) but increasingly operates as enforced extraction (defending beneficiary authority against alternatives that could now work if they were thinkable). The mandatrophy trajectory is: necessary coordination (1945-1970) → increasingly surplus extraction (1970-2020). This is why theater_ratio rises sharply post-1980: the same institutional apparatus that once served genuine coordination now serves mostly to maintain beneficiary authority by defending against alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_strategic_amnesia_boundary,
    'At what point did the amnesia about alternatives transition from genuine historical lapse to actively maintained strategic forgetting? Are there discrete events marking the transition, or is it a continuous drift?',
    'Archival analysis of academic debates, policy advisory records, and textbook evolution from 1970-1980; interviews with economists who witnessed the shift; documentation of when alternative frameworks stopped being taught and defended.',
    'If genuine lapse dominated until 1980, the constraint is less culpable in early period (natural intellectual drift) and only becomes extractive post-1980 (active suppression). If strategic maintenance began earlier, the entire constraint is more classifiable as intentional extraction. The transition point matters for assigning responsibility and calibrating remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_strategic_amnesia_boundary, empirical, 'The boundary between passive forgetting and active amnesia maintenance.').

omega_variable(
    identity_lock_reversibility,
    'Are economists identity-locked into market naturalism in a way that makes exit genuinely impossible for them, or is the lock primarily institutional (career risk, gatekeeping) rather than psychological (actually unable to conceive alternatives)?',
    'Documented cases of economists who shifted away from market naturalism and their stated reasons; surveys of economists on whether they feel capable of considering non-market coordination; analysis of what would be required for an economist to switch frameworks without losing professional credibility.',
    'If identity-lock is genuine (economists cannot think alternatives), the constraint has a mountain-like component of natural cognitive limit. If lock is primarily institutional (economists could think alternatives but will lose status), the constraint is purely extractive through gatekeeping. The distinction matters for whether education/deliberation alone can break the constraint or whether institutional restructuring (hiring, tenure, journal control) is necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity fusion is cognitive or institutional.').

omega_variable(
    sibling_reading_foreclosure_asymmetry,
    'Does this reading foreclose the beneficiary_maintained_reading by asserting a two-stage mechanism that the beneficiary reading denies? Or do the readings coexist as different valid frames of the same constraint?',
    'If the stage transition (1930-1970 genuine lapse vs. 1980-present strategic defense) is real and documented, it forecloses the beneficiary reading''s claim that beneficiaries have been active all along. If both stages are real but the beneficiary reading simply emphasizes the second stage and de-emphasizes the first, then coexistence is possible — the readings emphasize different parts of the same process.',
    'Foreclosure would mean only one reading can be true and policy should follow that one. Coexistence would mean the readings reflect genuinely different perspectives from different seats, and both warrant consideration in design of remedies. The field work above (empirical test on when strategic maintenance began) partially resolves this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_asymmetry, conceptual, 'Whether the two-stage mechanism is a real structural difference or an emphasis difference.').

omega_variable(
    accessibility_collapse_measurement,
    'Is the high accessibility_collapse (0.71) measuring genuine cognitive impossibility (alternatives literally unthinkable within the frame) or institutional suppression (alternatives thinkable but excluded from legitimate discourse)?',
    'Cognitive science experiments on economic reasoning, analysis of the conceptual apparatus required to even pose alternatives, documentation of what happened to alternative economic thought when it re-emerged (in heterodox economics, MMT, postcapitalism discourse) — was it technically possible for mainstream economists to engage, or did the frame prevent engagement?',
    'If cognitive, the constraint has a mountain-like component (natural limit to what can be thought within a frame) and remedies require frame change. If institutional, the constraint is pure extraction through gatekeeping and remedies focus on opening discourse. The measurement value (0.71) suggests a mix — partial cognitive closure, not total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_collapse_measurement, empirical, 'Nature of the collapse of alternatives: cognitive vs. institutional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 1930, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1930, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(mark_tr_t1945, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(mark_tr_t1970, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(mark_tr_t1980, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1980, 0.48).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(mark_tr_t2020, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2020, 0.58).

% Extraction over time
narrative_ontology:measurement(mark_be_t1930, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1930, 0.2).
narrative_ontology:measurement(mark_be_t1945, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1945, 0.22).
narrative_ontology:measurement(mark_be_t1970, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(mark_be_t1980, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(mark_be_t2020, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1930, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1930, 0.3).
narrative_ontology:measurement(mark_su_t1945, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1945, 0.32).
narrative_ontology:measurement(mark_su_t1970, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement(mark_su_t1980, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(mark_su_t2000, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(mark_su_t2020, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2020, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__hybrid_amnesia_reading, 0.18).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economic_hegemony__textbook_gatekeeping).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, policy_framework_narrowing__development_discourse).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'market_as_natural_default.' The sibling readings (lapsed_alternative_reading and beneficiary_maintained_reading) instantiate the same kernel through different causal mechanisms. This reading specifies a two-stage process where genuine forgetting (1930-1970) is inherited and weaponized by beneficiaries (1980-present). The divergence is structural (different causal chain), not observational (different measurement of the same chain). All three are linked via network.affects_constraints to enable corpus analysis of how the three readings cluster empirically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__hybrid_amnesia_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
