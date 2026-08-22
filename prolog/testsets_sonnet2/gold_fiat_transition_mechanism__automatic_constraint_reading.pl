% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Elimination of the Gold-Backed Automatic Money-Creation Ceiling
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This story instantiates the automatic-constraint reading of the
 *   gold-to-fiat transition kernel: the Nixon Shock and its aftermath are
 *   read as the removal of a self-enforcing physical ceiling on money
 *   creation (convertibility into a finite metal stock) and its replacement
 *   with the discretionary judgment of central banking institutions. Under
 *   this reading, the coordination benefit (credible anchoring against
 *   arbitrary debasement) that gold provided automatically now has to be
 *   manufactured institutionally — through central bank independence,
 *   inflation targets, and reputational discipline — and that manufactured
 *   substitute is weaker, contestable, and captured by the same authorities
 *   it is meant to restrain. The sibling readings
 *   (creditor_discipline_reading, composite_overdetermination_reading) are
 *   separate constraint stories describing the same historical event through
 *   different structural lenses; they are not blended into this one. See
 *   kernel_context for detail.
 *
 * KEY AGENTS:
 *   - monetary_authorities: agenda_setter/beneficiary (institutional/arbitrage) — administers money creation, now unconstrained by physical stock
 *   - sovereign_fiscal_executives: beneficiary (institutional/arbitrage) — gains deficit-financing flexibility
 *   - creditor_class: payer (powerful/constrained) — loses automatic protection from debasement
 *   - fixed_income_savers: payer (powerless/trapped) — bears inflation risk with no hedge capacity
 *   - gold_producing_states: excluded (moderate/constrained) — lost structural leverage, no voice in transition
 *   - economic_historians: observer (analytical/analytical) — evaluates competing structural readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.71).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.62).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Elimination of the Gold-Backed Automatic Money-Creation Ceiling").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, 'a7a73160-b8a0-45d9-b530-a3260a153a04').
narrative_ontology:cs_kernel_codification('a7a73160-b8a0-45d9-b530-a3260a153a04', formalized).
narrative_ontology:cs_authority_grounding('a7a73160-b8a0-45d9-b530-a3260a153a04', extraction).
narrative_ontology:cs_interpretation_layer_present('a7a73160-b8a0-45d9-b530-a3260a153a04').
narrative_ontology:cs_reading_relation('a7a73160-b8a0-45d9-b530-a3260a153a04', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7a73160-b8a0-45d9-b530-a3260a153a04', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('a7a73160-b8a0-45d9-b530-a3260a153a04', foundational, physical_scarcity_provides_superior_credibility_to_discretion).
narrative_ontology:cs_axiom_status(physical_scarcity_provides_superior_credibility_to_discretion, holdable).
narrative_ontology:cs_axiom_grounding('a7a73160-b8a0-45d9-b530-a3260a153a04', physical_scarcity_provides_superior_credibility_to_discretion, empirically_contingent).
narrative_ontology:cs_axiom('a7a73160-b8a0-45d9-b530-a3260a153a04', secondary, institutional_discretion_requires_active_enforcement_to_substitute_for_automatic_limits).
narrative_ontology:cs_axiom_status(institutional_discretion_requires_active_enforcement_to_substitute_for_automatic_limits, holdable).
narrative_ontology:cs_axiom_grounding('a7a73160-b8a0-45d9-b530-a3260a153a04', institutional_discretion_requires_active_enforcement_to_substitute_for_automatic_limits, instrumental).
narrative_ontology:cs_reference_frame('a7a73160-b8a0-45d9-b530-a3260a153a04', gold_convertibility_automatic_anchor).
narrative_ontology:cs_drift_state('a7a73160-b8a0-45d9-b530-a3260a153a04', post_nixon_shock_fiat_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a7a73160-b8a0-45d9-b530-a3260a153a04', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_fiscal_executives).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and treasury officials who administer money creation. Before the transition, reserve ratios against gold holdings set a hard, publicly verifiable ceiling on how much currency could be issued. After the transition, they set policy through discretionary judgment about inflation targets, employment, and financial stability, unconstrained by any external physical stock. They now decide, rather than merely execute against, the constraint.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, beneficiary).

% National governments that previously faced a hard balance-of-payments and reserve-adequacy limit on deficit spending and war finance. After the transition, they can run larger and more persistent deficits because the central bank can monetize debt without triggering a convertibility crisis, shifting the cost of that flexibility onto currency holders through inflation and depreciation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_fiscal_executives, beneficiary,
    institutional, generational, arbitrage, national).

% Holders of long-duration fixed claims — bondholders, pension funds, foreign central banks holding reserve currency — who previously relied on gold convertibility as an automatic check against debasement: any government printing too freely would face redemption pressure and reserve depletion. That automatic protection is gone; they now depend entirely on the discretionary restraint of the same authorities who benefit from the flexibility, and can only partially hedge through diversification or indexed instruments.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, generational, constrained, global).

% Ordinary households holding savings accounts, pensions, and nominal-wage income. They have no capacity to diversify into inflation hedges the way institutional creditors can, and no political voice comparable to bondholders. The removal of the automatic ceiling exposes their savings to whatever inflation trajectory the discretionary authority tolerates or generates, with no mechanical backstop.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Countries whose economic leverage derived partly from gold production and reserves lost a structural source of monetary influence once currency value was decoupled from metal. They were not consulted in the transition and had no seat in the decision to sever the link.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_producing_states, excluded,
    moderate, generational, constrained, global).

% Study the transition's mechanics and consequences, comparing the automatic-constraint framing against alternative readings (creditor-discipline shift, composite overdetermination). Their analysis does not itself alter the operating constraint but shapes how legitimacy claims about discretionary policy are evaluated.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A gold-backed reserve requirement solved a genuine credibility problem: it gave currency holders an externally verifiable, self-enforcing limit on issuance that did not depend on trusting the issuer's discretion or restraint.
% TRANSFER_FUNCTION: The transition moves control over the rate of currency debasement from an automatic physical mechanism (gold stock, redemption pressure) to the discretionary judgment of monetary authorities, transferring the real value previously protected from creditors and savers to whichever sovereign or private debtors benefit from expanded monetary flexibility.
% ABSENT_VOICES: Long-horizon savers and foreign holders of the reserve currency had no seat in the 1971 decision; gold-producing and gold-reserve-holding states whose leverage depended on the metal standard were likewise not parties to the change. Their objections surface after the fact, in inflation-era political mobilization and in foreign-exchange diversification, rather than in the founding decision itself.
% DISAPPEARANCE_RATIONALE: If discretionary authority were replaced overnight by a restored automatic physical constraint, monetary policy would lose its capacity to respond countercyclically, sovereign debt monetization would become impossible without triggering convertibility crises, and the real value of fixed claims would again be mechanically protected — global financial markets, debt structures, and central bank institutions built around discretion would need to reorganize entirely.
% FOUNDING_PROBLEM: The automatic gold constraint was originally intended to prevent arbitrary debasement and provide a credible, verifiable anchor for currency value that did not depend on trusting any government's self-restraint.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities attest the founding problem (credible anchoring) is now solved by inflation-targeting frameworks and institutional independence, making the physical constraint obsolete. Independent monetary historians and some creditor-class representatives outside the beneficiary group attest the underlying problem — credible restraint on debasement — remains live and unsolved, citing recurrent high-inflation episodes and central bank balance-sheet expansion as evidence discretion has not reliably substituted for the automatic mechanism it replaced.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71 by interval end) because the automatic-constraint reading holds that the physical ceiling's removal transferred real control over currency value from a verifiable external mechanism to an interested discretionary authority — precisely the asymmetric extraction pattern of a tangled rope. Suppression (0.62) reflects the legal-tender enforcement and institutional apparatus (central bank independence statutes, inflation-targeting mandates, capital controls at various points) required to make discretionary money creation function without immediate loss of confidence — this is active enforcement, not passive drift. Theater ratio rises over the interval (0.15 to 0.40) as inflation-targeting frameworks and 'independence' rhetoric increasingly substitute rule-bound-sounding language for what remains, under this reading, discretionary judgment — a Goodhart-style proxy substitution where the appearance of rule-following replaces the automatic rule itself.
 *
 * PERSPECTIVAL GAP:
 *   From the monetary-authority seat, the transition looks like maturation: a crude, pro-cyclical, deflation-prone physical constraint was replaced by more sophisticated, responsive governance. From the creditor and saver seats, the same event looks like the removal of the one mechanism that did not depend on trusting the issuer — replaced by an authority whose interests are not aligned with theirs. The engine should compute these as structurally different experiences of one arrangement, not reconcile them into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and fiscal executives are coded as beneficiaries because the transition directly expanded their operational discretion and financing capacity — d sits near the beneficiary end. Creditor class and fixed-income savers are coded as victims because they held claims whose real value was protected by the removed mechanism and now depends on discretionary restraint they cannot compel — d sits near the target end, amplified for fixed_income_savers by trapped exit (no diversification capacity) relative to creditor_class's merely constrained exit (institutional creditors can partially hedge via currency diversification).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing a credible anchor against arbitrary debasement — has not disappeared; it has been re-routed through institutional proxies (central bank independence, inflation targets) that are administered by the same class of authority the original mechanism was designed to constrain. This is exactly the mandatrophy pattern: the mandate (credible restraint) persists as live in principle while the mechanism that made it self-enforcing has been replaced by a mechanism that must be trusted rather than verified. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (a discretionary central bank can do real countercyclical work a rigid metal standard cannot) while still registering the asymmetric extraction that the automatic-constraint reading identifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_as_genuine_substitute_or_capture,
    'Does institutional discretion (inflation targeting, central bank independence) function as a genuine functional substitute for the automatic gold constraint''s credibility function, or is ''independence'' itself captured by the same interests the automatic mechanism restrained?',
    'Compare realized inflation and currency-debasement outcomes under discretionary regimes against counterfactual gold-standard-era outcomes, controlling for the different macroeconomic shocks each era faced; examine whether central bank independence statutes have been weakened or strengthened during fiscal stress episodes.',
    'If discretion functions as a credible substitute, this reading''s high extractiveness score overstates the case and the constraint is closer to a genuine rope with transitional friction; if discretion is substantially captured, the tangled_rope classification with high extraction is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_as_genuine_substitute_or_capture, empirical, 'Whether discretionary monetary authority genuinely substitutes for the automatic gold anchor or merely launders continued debasement capacity.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the ''automatic constraint removal'' framing the correct primary causal lens for the gold-fiat transition, or does it overstate a single mechanism relative to the creditor-discipline and composite-overdetermination readings?',
    'Historical and econometric decomposition of the causal weight of (a) convertibility suspension itself, (b) prior creditor-nation leverage erosion, and (c) independent technological/political shifts (Eurodollar markets, labor bargaining power, telecommunications) in explaining the observed post-1971 monetary regime change.',
    'If the automatic-constraint mechanism was the dominant causal driver, this reading''s beneficiary/victim structure (monetary_authorities vs creditor_class) is the most structurally accurate framing; if composite overdetermination dominates, this reading captures only one strand of a multi-causal shift and its extraction attribution to a single mechanism change is partial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the automatic-constraint-removal framing is the primary or merely one contributing causal lens on the historical transition.').

omega_variable(
    natural_scarcity_vs_constructed_convention,
    'Was gold''s monetary role itself a natural, discovered feature of monetary history, or a constructed convention that could equally have been organized around another commodity or a rule-based fiat system from the outset?',
    'Comparative monetary history across civilizations that used silver, cowrie shells, or other standards, examining whether gold''s specific physical properties were functionally necessary or contingently selected.',
    'If gold''s role was itself a constructed convention rather than a natural constraint, then the ''automatic physical constraint'' this reading treats as a pre-existing natural baseline is itself a prior institutional choice — weakening the naturalization implicit in framing 1971 as removing something ''automatic'' rather than replacing one convention with another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_scarcity_vs_constructed_convention, conceptual, 'Whether the gold standard itself was a natural physical limit or a prior constructed convention, which bears on how naturalized this reading''s baseline is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gold_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(gold_tr_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(gold_tr_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(gold_tr_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(gold_tr_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gold_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(gold_be_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(gold_be_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(gold_be_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(gold_be_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 50, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gold_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(gold_su_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(gold_su_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(gold_su_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(gold_su_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gold_fiat_transition_mechanism kernel. automatic_constraint_reading (this story) centers the removal of a self-enforcing physical ceiling and discretionary authority capture. creditor_discipline_reading centers the loss of creditor veto power and the geopolitical shift toward reserve-currency-issuer flexibility. composite_overdetermination_reading denies single-mechanism causality altogether, treating the event as convergent structural change. All three share the same historical referent (the 1971-73 collapse of dollar-gold convertibility) but author different ε, different beneficiary/victim sets, and different classifications, per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
