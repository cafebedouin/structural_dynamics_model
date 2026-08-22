% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Spectrum (Capacity-Legitimacy Classification Regime)
 *   domain: international law/political philosophy/global governance
 *
 * SUMMARY:
 *   This story instantiates the graduated_sovereignty reading of the
 *   westphalian_sovereignty kernel as a standalone, epsilon-invariant
 *   constraint: the operative norm that state sovereignty is not a uniform
 *   legal status but a graded position on a capacity-legitimacy spectrum,
 *   continuously assessed by external measurement apparatus (governance
 *   indices, fragility rankings, lender rating systems, Council suitability
 *   findings) and made consequential through conditionality, mandate design,
 *   and intervention authorization. Under this reading the standing
 *   arrangement is a membership hierarchy administered by capacity-holding
 *   powers: a state's domestic authority is provisional, revocable by
 *   reclassification, and priced against externally set benchmarks. The
 *   epsilon referent is this standing graded-membership arrangement as it
 *   operates, assessed by this reading's own lights, not the
 *   absolute-equality order the first sibling defends nor the
 *   violation-triggered order of the second; those are separate constraint
 *   files joined through network.affects_constraints. Stated assumptions:
 *   interval 0-35 maps approximately to 1990-2025, the period in which the
 *   graded frame moved from academic proposal to operational regime
 *   (failed-states discourse, the 2005 adoption of responsibility-language,
 *   fragility-agenda mainstreaming); sibling constraint_ids are assumed to
 *   follow the kernel-prefixed pattern used here; the single 'powerful'-atom
 *   override binds to regional_hegemons, the story's only seat at that power
 *   level.
 *
 * KEY AGENTS:
 *   - intervening_great_powers: agenda-setting seat (institutional/arbitrage) — writes the classification criteria, authorizes mandates, applies the standard selectively while sitting outside the graded categories
 *   - international_financial_institutions: dual seat (institutional/arbitrage) — co-produces the ratings that classify and collects leverage and client dependence from the classification
 *   - weak_postcolonial_states: primary target (powerless/trapped) — bears reclassification risk and the permanent probation of domestic authority
 *   - populations_of_classified_states: deep target (powerless/trapped) — bears both the underlying incapacity and the externally administered remedies
 *   - regional_hegemons: intermediate seat (powerful/constrained) — licensed to enforce the spectrum regionally while graded by it globally
 *   - unrepresented_small_states: excluded seat (powerless/trapped) — inherits categories drafted without them
 *   - international_administrative_apparatus: beneficiary seat (institutional/mobile) — staffs and services the classification-and-administration pipeline
 *   - international_law_scholars: analytical observer (analytical/analytical) — documents the doctrinal shift and the gap between rationale and application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.64).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.58).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.64).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Spectrum (Capacity-Legitimacy Classification Regime)").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international law/political philosophy/global governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, 'a7f7818e-4504-4d9b-8b00-401872f58e2e').
narrative_ontology:cs_kernel_codification('a7f7818e-4504-4d9b-8b00-401872f58e2e', fixed_text).
narrative_ontology:cs_authority_grounding('a7f7818e-4504-4d9b-8b00-401872f58e2e', practice).
narrative_ontology:cs_interpretation_layer_present('a7f7818e-4504-4d9b-8b00-401872f58e2e').
narrative_ontology:cs_reading_relation('a7f7818e-4504-4d9b-8b00-401872f58e2e', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('a7f7818e-4504-4d9b-8b00-401872f58e2e', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('a7f7818e-4504-4d9b-8b00-401872f58e2e', foundational, sovereignty_is_performance_contingent).
narrative_ontology:cs_axiom_status(sovereignty_is_performance_contingent, holdable).
narrative_ontology:cs_axiom_grounding('a7f7818e-4504-4d9b-8b00-401872f58e2e', sovereignty_is_performance_contingent, empirically_contingent).
narrative_ontology:cs_axiom('a7f7818e-4504-4d9b-8b00-401872f58e2e', secondary, legal_equality_without_capacity_is_fictional).
narrative_ontology:cs_axiom_status(legal_equality_without_capacity_is_fictional, holdable).
narrative_ontology:cs_axiom_grounding('a7f7818e-4504-4d9b-8b00-401872f58e2e', legal_equality_without_capacity_is_fictional, empirically_contingent).
narrative_ontology:cs_reference_frame('a7f7818e-4504-4d9b-8b00-401872f58e2e', capacity_graded_membership_order).
narrative_ontology:cs_drift_state('a7f7818e-4504-4d9b-8b00-401872f58e2e', post_libya_selectivity_critique, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a7f7818e-4504-4d9b-8b00-401872f58e2e', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, intervening_great_powers).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_administrative_apparatus).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_postcolonial_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, populations_of_classified_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, regional_hegemons).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, regional_hegemons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the criteria by which states are placed on the capacity-legitimacy spectrum, control the Security Council votes that authorize mandates, and decide which findings become operational. They apply the standard to others while their own conduct and that of their allies sits outside the graded categories. They collect the discretionary power the spectrum creates: the ability to intervene, impose conditions, or grant deference, case by case, without a fixed rule binding them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, intervening_great_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Co-produce the ratings and country assessments that place states on the spectrum, and convert classification into leverage: lending, debt treatment, and market access are priced against the assigned tier. They collect policy influence and client dependence from the arrangement while bearing none of the domestic costs of the policies they certify. Their own governance is weighted toward the capacity-holding members who set the benchmarks.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, agenda_setter).

% The class of international civil servants, special representatives, monitoring missions, consultants, and implementing contractors whose employment exists because states are classified as needing external administration. Staff move between missions and agencies; careers are built on the pipeline of classified territories and reform programs. Their livelihoods depend on the classification continuing to generate mandates.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_administrative_apparatus, beneficiary,
    institutional, biographical, mobile, global).

% Governments whose domestic authority is permanently provisional: subject to periodic reassessment, conditionality, and possible mandate or sanction, with no forum where the classification can be appealed on equal terms. Recognition, trade, credit, and security cooperation all run through the system doing the grading, so leaving it is not an option. Policy autonomy is traded continuously for market access and budget support, and past colonial administration means the graded categories land on a population already shaped by earlier external rule.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_postcolonial_states, payer,
    powerless, biographical, trapped, national).

% People living under low-tier classification experience both the underlying state incapacity and the externally administered remedies: austerity conditions attached to finance, externally designed governance reforms, peacekeeping forces with mixed records, and transitional authorities that persist for decades. They bear the costs of the arrangement in either direction and rarely sit in the rooms where their polity's tier is decided.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, populations_of_classified_states, payer,
    powerless, generational, trapped, national).

% Regional powers licensed to enforce the spectrum in their neighborhoods, leading intervention coalitions and stabilization missions with external blessing and financing. The license is valuable: it legitimizes their regional primacy. At the same time they remain graded subjects globally, their own governance assessed by the same apparatus, and their regional interventions require great-power acquiescence they cannot compel. They enforce a hierarchy they do not control.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, regional_hegemons, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, regional_hegemons, payer).

% States without Security Council representation, index-methodology input, or creditor-seat voice inherit categories drafted entirely elsewhere. They would contest the weighting of capacity indicators, the treatment of historical legacies, and the selectivity of enforcement if they had a seat at the drafting table; they learn their tier when it is published.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, unrepresented_small_states, excluded,
    powerless, generational, trapped, regional).

% Document the doctrinal movement from formal sovereign equality toward graded membership, track the gap between the stated capacity rationale and the observed distribution of interventions, and preserve the record of dissenting traditions. They publish assessments that the agenda-setting seats may cite or ignore, and they bear no operational stake in the arrangement's continuation.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, intervening_great_powers).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared decision procedure for engaging polities that cannot secure territory, deliver services, or honor commitments: it tells lenders, donors, peacekeeping planners, and intervention coalitions which governments to treat as ordinary counterparts, which to assist, and which to administer, and it pools the costs of stabilization and humanitarian response that no single actor would carry alone.
% TRANSFER_FUNCTION: Moves domestic decision-making authority over fiscal, security, and regulatory affairs from governments of low-tier states to external bodies (Security Council mandates, lender conditionality, donor coordination structures, transitional administrations); moves classification authority to the capacity-holding powers; and moves resources nominally toward capacity-building while binding recipients to externally designed policy.
% ABSENT_VOICES: Populations of classified states are absent from mandate-design and tier-review rooms; small states without Council seats or methodology input inherit categories they never drafted; successor societies of formerly colonized territories were never consulted on the criteria that now grade them. Dissenting traditions holding sovereignty unconditional persist outside the rooms where the standards are written, and their absence from those rooms is itself maintained by the arrangement.
% DISAPPEARANCE_RATIONALE: If the graded frame vanished overnight, lender conditionality would lose its legitimating vocabulary, intervention mandates would revert to ad hoc political justification with no principled tier to cite, the assessment-and-rating industry would lose its operative purpose, and low-tier governments would recover formal bargaining parity in finance and security fora. Regional intervention arrangements would need renegotiation from scratch. The world rearranges because a large institutional superstructure is organized around the spectrum.
% FOUNDING_PROBLEM: After the Cold War the state system confronted collapsed polities, mass-atrocity failures, and cross-border spillovers that formal sovereign equality seemed unable to process: legal equals included entities that could not govern, and the system lacked a principled, repeatable basis for treating them differently from functioning states.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian agencies and peacekeeping historians corroborate the original coordination gap (Somalia 1992, Rwanda 1994) from outside the benefiting parties. On the other side, decolonization scholars and Global South governments attest that the remedy rebuilt the trusteeship structure it replaced, and post-mission audits of long-running administrations document outcomes decoupled from the stated protective aims. Corroboration exists on both sides; no attestation is free of seat interest, and the sharpest external evidence against the frame's current operation comes from outcome audits rather than from any party's self-description.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.64 because the arrangement's distinctive product is discretionary classification power held by seats that are not accountable to the classified: the rate at which authority transfers outward is set by the transferee, revisable at will, and decoupled from any fixed trigger. Suppression is authored at 0.58 as a raw structural property, unscaled by power or scope per specification (the engine owns any contextual scaling): exit from the international system is unavailable to any state, the unconditional-sovereignty alternative is actively delegitimized as outdated, and resistance meets finance and trade isolation. Theater is 0.48: a large assessment industry (indices, review cycles, partnership compacts, capacity-scorecards) produces classifications and meetings more often than it produces capacity, though peacekeeping and humanitarian logistics remain genuinely functional. Accessibility_collapse is 0.48 — alternative framings remain live in doctrine and scholarship but are practically closed to weak states, who cannot opt out of recognition, credit, or security structures. Resistance is 0.55: non-aligned caucusing, refusal of tribunal jurisdiction, alternative creditor arrangements, and explicit sovereignty-reassertion rhetoric by rival blocs. The claimed type (snare) is authored from structure — the coordination story functions as legitimation for classification discretion, persistence depends on coercive enforcement, exits are suppressed, and victims are identifiable — while the metrics are authored independently as descriptive estimates; the engine computes per-seat types from the structural data and any divergence from the claim is the datum. The temporal series run on one shared eight-point grid (T0-T35, roughly 1990-2025) so every tracked metric is authored at every examined time point; epsilon rises steeply through the regime-building years, plateaus after the post-2011 selectivity backlash caps further ratcheting, and theater grows monotonically with the index industry.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seats and the target seats should compute very different types from identical structural data. From the great-power and lender seats the arrangement is an order they built and service: peacekeeping that ends wars, finance that stabilizes budgets, a vocabulary that lets them act where inaction was the prior default. From the trapped target seats the same structure is permanent probation without appeal: authority held at the pleasure of an assessor, terms set by the creditor, categories inherited rather than negotiated. Regional hegemons straddle the divide, computing a hybrid position — enforcers locally, graded subjects globally. The engine computes this divergence from power, exit, and directional data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: intervening_great_powers, international_financial_institutions, and international_administrative_apparatus all sit near the subsidized end, amplified toward full beneficiary by their arbitrage-grade exit (the rules bind others; they retain discretionary exemption). Victim declarations map to high directionality: weak_postcolonial_states and populations_of_classified_states sit near the full-target end, pushed further by trapped exit — no state can leave the system that grades it, and the global scope of the classification apparatus makes verification of fairness harder, which the engine reflects in effective extraction. The single override corrects the derivation for the story's only 'powerful'-atom seat, regional_hegemons: role=beneficiary alone would derive a strongly subsidized directionality, but their actual position is mixed — they collect the regional enforcement license while submitting to global grading they cannot veto — so d is overridden to 0.35. Unrepresented_small_states derive high directionality as excluded payers-in-waiting; international_law_scholars derive the analytical neutral position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — processing polities that cannot govern — retains live instances, so the genealogy status is contested rather than dead: famine, collapse, and atrocity did not end in 1990. But the instruments built as emergency measures hardened into steady-state administration: transitional mandates conceived as bridges (Kosovo, Bosnia, Timor-Leste) ran for decades, and the emergency vocabulary normalized into a permanent hierarchy. Mandatrophy analysis prevents two opposite mislabels. Reading the whole apparatus as pure extraction erases the genuine coordination core — pooled peacekeeping and humanitarian logistics — that a good-faith version of the frame would still perform. Reading it as pure coordination erases the classification discretion that is precisely this reading's marginal contribution over its siblings: the conditional reading already licenses triggered intervention without a permanent spectrum, so what the graded frame adds beyond it is continuous reassessment power, and that increment is where the extraction concentrates. The mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): the flag raised is dispute about function, not zombie capture — the machinery would be missed if abolished, which is exactly why its extraction is durable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_interest_alignment,
    'Do classification outcomes track measured state capacity, or do they track the strategic and economic interests of the classifying powers?',
    'Systematic audit of classification decisions (fragility listings, IFI rating tiers, mandate eligibility) against objective capacity indicators, controlling for intervener alignment, resource endowments, and geopolitical alignment of the classified state.',
    'If classifications track intervener interest, the spectrum operates as discretionary extraction machinery and epsilon is understated; if they track capacity, a larger share of measured burden is the price of the coordination the frame performs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_interest_alignment, empirical, 'Whether the grading apparatus measures capacity or serves classifier interest.').

omega_variable(
    counterfactual_autonomy_baseline,
    'Relative to the absolute-sovereignty counterfactual, does externally administered guardianship leave low-classified states and their populations better or worse off?',
    'Paired comparison of comparable states experiencing similar capacity crises under different normative frames (graded administration versus non-intervention), tracking welfare, autonomy, and institutional recovery over a decade.',
    'If the guardianship outcome dominates, part of the measured burden is coordination cost the sibling readings cannot organize and the type shifts toward a hybrid; if the counterfactual dominates, the full epsilon stands as net extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_autonomy_baseline, conceptual, 'Net welfare of graded administration versus the non-intervention counterfactual.').

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the westphalian_sovereignty kernel; how would the sibling readings (absolute_sovereignty, conditional_sovereignty) restructure the victim set and the classification if instantiated instead?',
    'Comparative instantiation: the absolute reading yields a constraint whose targets are would-be interveners and whose victims are intervention-target populations; the conditional reading yields violation-triggered targets; this reading yields continuously reassessed low-capacity states. The disagreement is located in the scope-condition of sovereignty: fixed by law, triggered by violation, or continuously graded by assessment.',
    'Adopting a sibling reading dissolves this constraint''s victim set and replaces it with a different one; cross-reading epsilon comparisons are invalid because the referent arrangements differ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: reading-of-kernel location and sibling deltas.').

omega_variable(
    index_performativity,
    'Do governance and fragility indices measure state capacity, or do they manufacture the category they rank by channeling finance, legitimacy, and reform pressure toward the measured dimensions?',
    'Natural experiments where index methodology changed discontinuously: if classified trajectories track methodology changes rather than underlying governance, the indices are performative rather than observational.',
    'High performativity raises the theater ratio and supports reading the assessment layer as maintenance ritual for the hierarchy rather than measurement serving coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_performativity, empirical, 'Whether the measurement apparatus observes capacity or produces the ranked reality.').

omega_variable(
    compliance_internalization,
    'Is low-classified-state compliance with externally set policy structural (finance, trade, and security dependence) or internalized (elite socialization into donor framings that persists after dependence eases)?',
    'Post-dependence trajectory: track policy behavior of states whose financing constraints relaxed (commodity windfalls, alternative creditors); if donor-congruent policy persists after the structural lever is removed, part of the suppression is internalized.',
    'If internalized, effective suppression exceeds the structural measure and outlives the material dependency that produced it, hardening the regime against reform even as leverage declines.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_internalization, empirical, 'Structural versus internalized mechanism of classified-state compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0, 0.22).
narrative_ontology:measurement(west_tr_t5, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 5, 0.27).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 10, 0.32).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 15, 0.37).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 20, 0.41).
narrative_ontology:measurement(west_tr_t25, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 25, 0.44).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 30, 0.46).
narrative_ontology:measurement(west_tr_t35, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 35, 0.48).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(west_be_t5, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(west_be_t25, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(west_be_t35, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 35, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(west_su_t5, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(west_su_t25, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(west_su_t35, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 35, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the westphalian_sovereignty kernel per the epsilon-invariance principle: the colloquial label 'sovereignty' covers three structurally distinct claims with different victim sets and different epsilon values. This file instantiates the graduated reading (continuous capacity-legitimacy grading; victims are low-classified states). The absolute reading (unconditional domestic authority; targets are would-be interveners) and the conditional reading (violation-triggered intervention; targets are violating states) are separate stories. The upstream sibling (absolute) supplies the legal-equality baseline this reading modifies; the conditional sibling borrows this reading's capacity vocabulary while retaining fixed triggers. Each member links the others through affects_constraints; epsilon comparisons across the family are invalid because the referent arrangements differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
