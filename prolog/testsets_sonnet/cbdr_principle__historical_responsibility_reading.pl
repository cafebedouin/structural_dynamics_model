% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR — Historical Responsibility Reading (Binding Emissions Cuts + Loss/Damage Finance)
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   This story instantiates the historical-responsibility reading of the
 *   Common But Differentiated Responsibilities (CBDR) kernel: the claim that
 *   developed nations owe binding, treaty-enforceable emissions reductions
 *   scaled to their cumulative historical contribution to atmospheric carbon,
 *   plus loss/damage financing to compensate climate-vulnerable developing
 *   nations for harms already incurred. This is a distinct constraint from
 *   the voluntary_commitment_reading (nationally determined contributions
 *   with technology transfer as the primary developed-nation obligation) —
 *   the two readings produce different victim sets, different enforcement
 *   mechanisms, and different epsilon values, and are authored as separate
 *   stories per the epsilon-invariance principle. From 1992 (Rio, UNFCCC's
 *   founding CBDR language) through the 2022 Loss and Damage Fund
 *   establishment at COP27 and its 2024 operationalization, this reading has
 *   moved from aspirational text toward increasingly binding instruments,
 *   though full quantum and enforceability remain contested.
 *
 * KEY AGENTS:
 *   - small_island_developing_states: Primary beneficiary (powerless/trapped) — receives financing and enforceable emissions ceilings on emitters
 *   - developed_nation_treasuries: Primary payer (institutional/constrained) — bears binding financial and mitigation obligations
 *   - developed_nation_heavy_industry: Secondary payer (powerful/constrained) — bears sectoral decarbonization costs
 *   - unfccc_secretariat_and_treaty_negotiators: Agenda-setter (institutional/analytical) — drafts and administers the binding-commitment architecture
 *   - emerging_high_emitters: Excluded beneficiary-adjacent actor (powerful/constrained) — benefits from continued developing-country classification without being a principal negotiator of the historical-responsibility formula
 *   - climate_policy_analysts: Analytical observer — tracks cumulative emissions accounting across competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.58).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.42).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR — Historical Responsibility Reading (Binding Emissions Cuts + Loss/Damage Finance)").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, '16246773-5c78-42f1-a892-b1b2042eb0f9').
narrative_ontology:cs_kernel_codification('16246773-5c78-42f1-a892-b1b2042eb0f9', fixed_text).
narrative_ontology:cs_authority_grounding('16246773-5c78-42f1-a892-b1b2042eb0f9', distributed).
narrative_ontology:cs_reading_relation('16246773-5c78-42f1-a892-b1b2042eb0f9', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('16246773-5c78-42f1-a892-b1b2042eb0f9', foundational, cumulative_historical_causation_grounds_binding_liability).
narrative_ontology:cs_axiom_status(cumulative_historical_causation_grounds_binding_liability, holdable).
narrative_ontology:cs_axiom_grounding('16246773-5c78-42f1-a892-b1b2042eb0f9', cumulative_historical_causation_grounds_binding_liability, empirically_contingent).
narrative_ontology:cs_axiom('16246773-5c78-42f1-a892-b1b2042eb0f9', foundational, loss_and_damage_compensation_is_owed_not_gifted).
narrative_ontology:cs_axiom_status(loss_and_damage_compensation_is_owed_not_gifted, holdable).
narrative_ontology:cs_axiom_grounding('16246773-5c78-42f1-a892-b1b2042eb0f9', loss_and_damage_compensation_is_owed_not_gifted, deontological).
narrative_ontology:cs_reference_frame('16246773-5c78-42f1-a892-b1b2042eb0f9', rio_1992_common_but_differentiated_responsibilities_text).
narrative_ontology:cs_drift_state('16246773-5c78-42f1-a892-b1b2042eb0f9', post_paris_agreement_2015_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('16246773-5c78-42f1-a892-b1b2042eb0f9', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, small_island_developing_states).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_vulnerable_coastal_populations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_treasuries).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_heavy_industry).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_taxpayers).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, polluter_pays_principle).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, intergenerational_equity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face existential territorial loss from sea level rise caused overwhelmingly by emissions they did not produce. Under this reading, they receive binding loss/damage financing and emissions-reduction commitments enforceable through treaty mechanisms rather than pledges. They have no alternative source of comparable capital and cannot exit the climate system that threatens them.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, small_island_developing_states, beneficiary,
    powerless, generational, trapped, global).

% Bear disproportionate climate impacts (drought, crop failure, displacement) with minimal historical contribution to cumulative emissions. This reading entitles them to adaptation and loss/damage transfers scaled to donor countries' historical emissions share, administered through UNFCCC-linked mechanisms they did not design and cannot unilaterally enforce.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, least_developed_countries, beneficiary,
    powerless, generational, trapped, global).

% Communities facing displacement from flooding and storm intensification. They are the intended end-recipients of loss/damage financing but depend entirely on national governments and multilateral funds to translate treaty obligations into actual transfers reaching them.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_vulnerable_coastal_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Would be obligated under this reading to fund loss/damage mechanisms and accept binding, historically-scaled emissions targets rather than self-set nationally determined contributions. They can negotiate treaty language, delay ratification, or attach conditionality, but full withdrawal from climate multilateralism carries diplomatic and economic costs that constrain unilateral exit.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_treasuries, payer,
    institutional, generational, constrained, national).

% Faces binding sectoral decarbonization mandates and potential carbon-liability exposure tied to historical emissions accounting. Can lobby for weaker domestic implementation, relocate operations to jurisdictions with laxer rules (carbon leakage), or absorb compliance costs, but cannot escape the treaty framework if ratified domestically.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_heavy_industry, payer,
    powerful, biographical, constrained, national).

% Fund loss/damage transfers and domestic decarbonization subsidies through taxation. Have democratic voice through elections but limited direct control over international treaty commitments once ratified; bear costs diffusely without individually negotiating terms.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_taxpayers, payer,
    moderate, biographical, constrained, national).

% Administers the CBDR framework, drafts binding-commitment language, and adjudicates historical-emissions accounting methodologies. Determines which reading of CBDR is operationalized in treaty text, though final ratification power rests with national governments.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, unfccc_secretariat_and_treaty_negotiators, agenda_setter,
    institutional, generational, analytical, global).

% Rapidly industrializing economies with rising current emissions but comparatively low cumulative historical totals. Under this reading they largely retain developing-country status and looser binding obligations despite growing present-day emissions share, a classification they benefit from but that developed nations increasingly contest. They are not formally excluded from negotiations but their preferred continued classification is under active challenge from developed-nation negotiators, giving them structural interest in defending this reading without being the ones who authored it.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, emerging_high_emitters, excluded,
    powerful, generational, constrained, national).

% Study cumulative emissions accounting, equity formulas, and compliance trajectories across competing CBDR readings, without a stake in either the transfer or the payment.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, diffuse).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global response to a genuine collective-action problem — atmospheric carbon is a shared sink and no single nation's mitigation succeeds without aggregate reduction — by allocating the burden according to a defensible, auditable metric (cumulative historical emissions) rather than leaving contribution levels to unilateral discretion.
% TRANSFER_FUNCTION: Moves binding mitigation obligations and loss/damage capital from developed-nation treasuries, industry, and taxpayers to climate-vulnerable developing nations and their populations, scaled to each developed nation's share of cumulative historical emissions since industrialization.
% ABSENT_VOICES: Emerging high-emitters with rapidly rising current-year emissions but low cumulative historical totals have a stake in this reading's classification boundary but are not the ones negotiating its terms from either side; developed-nation domestic industries facing carbon-leakage competition from unregulated jurisdictions are similarly not directly party to loss/damage negotiations despite bearing compliance costs.
% DISAPPEARANCE_RATIONALE: If binding historical-responsibility obligations disappeared, loss/damage financing would revert to voluntary pledges (as under the sibling reading), vulnerable states would lose their strongest legal lever for compelling transfers, developed-nation treasuries would face no enforceable liability schedule, and the entire architecture of differentiated-but-binding mitigation targets would collapse back into nationally self-determined commitments.
% FOUNDING_PROBLEM: Industrialized nations built their wealth on two centuries of largely unconstrained fossil fuel emissions; the resulting atmospheric carbon burden now causes disproportionate harm to nations that emitted comparatively little, creating a fairness and causation gap that voluntary frameworks failed to close through three decades of non-binding pledges.
% FOUNDING_PROBLEM_CORROBORATION: IPCC attribution science and independent carbon-accounting bodies (e.g. Global Carbon Project, Carbon Brief historical emissions datasets) corroborate the cumulative-emissions causation claim from outside both the developing-nation beneficiary bloc and developed-nation negotiating parties; several developed-nation domestic courts (e.g. Dutch Urgenda ruling, German constitutional court climate ruling) have independently affirmed government obligations grounded in historical-contribution reasoning, though developed-nation treasuries themselves largely contest the binding financing quantum.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a genuine but substantially asymmetric transfer: developed nations are formally required to move real capital and accept binding mitigation ceilings, but the transfer rides on top of a real coordination problem (shared atmospheric carbon sink) that no purely extractive story would need. Suppression (0.42) is moderate — developed nations retain meaningful exit through treaty non-ratification, reservation clauses, and delayed implementation (the Paris Agreement's own architecture shows this reading has never achieved full bindingness), so this is not a trapped-target constraint in the way a domestic tax mandate would be. Theater ratio (0.4) captures that a substantial share of activity under this reading — pledging conferences, non-binding communiques, unfulfilled prior finance commitments (the unmet $100bn/year pledge) — has historically been performative relative to actual enforceable transfer, though the 2022 Loss and Damage Fund marks a shift toward more concrete mechanism. Accessibility collapse is low (0.35) because developed nations have visibly and repeatedly exercised the alternative (non-ratification, non-payment, reinterpretation toward the voluntary reading) — alternatives have not collapsed, they remain the dominant lived practice. Resistance is high (0.72): this reading meets sustained, well-resourced opposition from developed-nation negotiators precisely because it would bind them, which is itself evidence this is not a settled natural allocation but a contested political claim.
 *
 * PERSPECTIVAL GAP:
 *   From the UNFCCC secretariat and vulnerable-state seats, this reading operationalizes polluter-pays justice — a genuine coordination solution to a causation problem. From developed-nation treasury and industry seats, the same binding architecture reads as an open-ended, historically-retroactive liability regime imposed on entities (current taxpayers, current firms) who did not personally emit the historical carbon in question. The engine's per-seat computation should reflect this: the beneficiary seats likely compute close to rope/tangled_rope depending on enforcement weight, while the payer seats experience amplified effective extraction precisely because they are treaty-bound targets with constrained (not mobile) exit once ratified.
 *
 * DIRECTIONALITY LOGIC:
 *   Small island states, least developed countries, and vulnerable coastal populations are declared beneficiaries — they are trapped (cannot geographically or economically escape climate impacts) and their d sits near the full-beneficiary end because the constraint, in this reading, subsidizes them via enforceable transfers. Developed-nation treasuries, industry, and taxpayers are declared victims/payers — their exit is constrained (they can delay or attach conditions to ratification but full withdrawal from climate multilateralism carries severe diplomatic and market costs), pushing their derived d toward the target end. Emerging high-emitters are neither cleanly beneficiary nor victim under this reading — they retain developing-country treatment and thus a favorable position, but they are excluded from being principal authors of the historical-responsibility formula that protects their classification, which is why they are marked excluded rather than beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (developed-nation historical emissions causing disproportionate harm to non-contributing states) remains live and empirically corroborated by attribution science independent of either negotiating bloc. This blocks a mandatrophy read: unlike a piton, where the founding function has died and only performance remains, this constraint's coordination function (addressing a real, ongoing causation asymmetry) is still operative — the tension is not that the function is dead but that the binding mechanism to fulfill it remains incompletely built, making the theater_ratio meaningful (partial fulfillment, not zero function) rather than indicating pure inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_responsibility_reading_selection,
    'Is the historical-responsibility reading of CBDR, versus the voluntary-commitment reading, the operative legal interpretation of UNFCCC Article 3.1 and Paris Agreement Article 2.2, or are both simultaneously live and contested interpretations with no adjudicating authority resolving between them?',
    'International Court of Justice advisory opinion on state climate obligations (requested by UN General Assembly, pending as of 2024), or binding arbitral rulings under UNFCCC dispute mechanisms, would authoritatively select between readings. Absent such a ruling, the readings coexist as competing negotiating positions.',
    'If the historical-responsibility reading were authoritatively adopted, developed-nation obligations would become legally enforceable rather than politically negotiated, sharply raising both extractiveness and suppression as authored here. If the voluntary reading prevails, this constraint''s binding character dissolves into the sibling constraint''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_responsibility_reading_selection, conceptual, 'Whether this reading or its voluntary-commitment sibling is the legally operative interpretation of CBDR.').

omega_variable(
    cumulative_emissions_accounting_boundary,
    'What baseline date and accounting methodology (production-based vs. consumption-based, per-capita vs. absolute) determines ''cumulative historical emissions,'' and does the choice of methodology itself shift which nations fall into the victim set?',
    'Comparative analysis of Global Carbon Project, PIK, and CAIT accounting methodologies against proposed treaty formulas; convergence or persistent divergence across methods would indicate whether the historical-responsibility metric is a stable, non-arbitrary basis for binding obligation.',
    'A methodology-sensitive victim set would suggest the historical-responsibility reading, while normatively grounded, has an underdetermined operational boundary — potentially reclassifying which developed nations bear how much obligation, without changing the reading''s basic structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cumulative_emissions_accounting_boundary, empirical, 'Sensitivity of the victim/beneficiary boundary to historical emissions accounting methodology choice.').

omega_variable(
    loss_damage_fund_capture_risk,
    'Will the operationalized Loss and Damage Fund (established COP27, 2022) actually disburse capital proportional to cumulative historical emissions, or will disbursement be captured by administrative overhead, donor conditionality, and geopolitical prioritization that diverges from the historical-responsibility formula this reading claims to enforce?',
    'Track disbursement data against the Fund''s stated allocation formula over 2025-2030; persistent divergence would indicate theater_ratio is understated in this story and should rise in future measurement.',
    'If disbursement diverges substantially from the historical-responsibility formula, the constraint drifts toward a tangled_rope with rising theater component, or even toward scaffold-with-failed-sunset if the binding mechanism never fully materializes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(loss_damage_fund_capture_risk, empirical, 'Whether Loss and Damage Fund disbursement will track the historical-responsibility allocation formula this reading claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1992, cbdr_principle__historical_responsibility_reading, theater_ratio, 1992, 0.55).
narrative_ontology:measurement(cbdr_tr_t1997, cbdr_principle__historical_responsibility_reading, theater_ratio, 1997, 0.5).
narrative_ontology:measurement(cbdr_tr_t2005, cbdr_principle__historical_responsibility_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(cbdr_tr_t2015, cbdr_principle__historical_responsibility_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(cbdr_tr_t2022, cbdr_principle__historical_responsibility_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement(cbdr_tr_t2024, cbdr_principle__historical_responsibility_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1992, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1992, 0.3).
narrative_ontology:measurement(cbdr_be_t1997, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1997, 0.35).
narrative_ontology:measurement(cbdr_be_t2005, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(cbdr_be_t2015, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(cbdr_be_t2022, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2022, 0.55).
narrative_ontology:measurement(cbdr_be_t2024, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1992, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(cbdr_su_t1997, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1997, 0.25).
narrative_ontology:measurement(cbdr_su_t2005, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(cbdr_su_t2015, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2015, 0.34).
narrative_ontology:measurement(cbdr_su_t2022, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2022, 0.4).
narrative_ontology:measurement(cbdr_su_t2024, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.12).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_nationally_determined_contributions).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_fund_governance).

% DUAL FORMULATION NOTE:
% This story is one of a two-member family decomposing the colloquial concept 'CBDR' per the epsilon-invariance principle: historical_responsibility_reading (this file, tangled_rope-leaning, binding transfer + enforcement) and voluntary_commitment_reading (sibling file, expected rope/scaffold-leaning, self-determined ambition + technology transfer). The two readings share a kernel (the CBDR text itself, UNFCCC Article 3.1 and Paris Agreement Article 2.2) but instantiate structurally distinct constraints with different epsilon values, different victim sets, and different enforcement postures. They are linked bidirectionally via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
