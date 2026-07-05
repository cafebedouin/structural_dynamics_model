% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__climate_incorporation, []).

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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Mandate Reading: Climate Risk Incorporation via Article 11 TFEU Environmental Integration
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Article 127 ECB
 *   mandate kernel: the climate_incorporation reading, under which Article 11
 *   TFEU's environmental integration clause is read as extending into
 *   monetary policy instruments, requiring the ECB to integrate climate risk
 *   into collateral eligibility and asset purchase allocation. Under this
 *   reading, the mandate ceases to be a narrow price-stability rule and
 *   becomes a vehicle carrying a distributive climate-transition function:
 *   green-labeled issuers and coalition member states gain, carbon-intensive
 *   issuers, treasuries, and dependent workforces pay through collateral
 *   haircuts and reduced purchase eligibility. Two sibling readings exist as
 *   separate constraint stories (orthodox_price_stability and
 *   expansive_secondary_objectives, not modeled here) — under those readings
 *   the same treaty text yields different beneficiary/victim sets and
 *   different epsilon values. This is not the same constraint measured
 *   differently; it is a structurally distinct claim about what Article 127
 *   requires, decomposed per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: agenda_setter (institutional/analytical) — administers the tilting mechanism and calibrates its scope
 *   - renewable_energy_issuers and green_bond_underwriters: beneficiaries (moderate-organized/mobile) — collect financing-cost advantage without administering the rule
 *   - climate_policy_coalition_states: beneficiary and secondary agenda-setter (institutional/constrained) — shaped the reading's adoption through Governing Council representation
 *   - fossil_fuel_sector_issuers and carbon_intensive_member_state_treasuries: payers (powerful-institutional/constrained-trapped) — bear the collateral and purchase-eligibility penalty
 *   - coal_dependent_regional_workforces: payers (powerless/trapped) — bear diffuse employment consequences with no institutional voice
 *   - orthodox_price_stability_bloc: excluded — dissents but is outvoted
 *   - european_court_of_justice: observer — has not yet definitively adjudicated the scope question this reading depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.58).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.61).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate Reading: Climate Risk Incorporation via Article 11 TFEU Environmental Integration").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '7dd32b15-d8fb-4f1c-95cb-6064750ec331').
narrative_ontology:cs_kernel_codification('7dd32b15-d8fb-4f1c-95cb-6064750ec331', fixed_text).
narrative_ontology:cs_authority_grounding('7dd32b15-d8fb-4f1c-95cb-6064750ec331', lineage).
narrative_ontology:cs_interpretation_layer_present('7dd32b15-d8fb-4f1c-95cb-6064750ec331').
narrative_ontology:cs_reading_relation('7dd32b15-d8fb-4f1c-95cb-6064750ec331', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('7dd32b15-d8fb-4f1c-95cb-6064750ec331', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('7dd32b15-d8fb-4f1c-95cb-6064750ec331', foundational, environmental_integration_binds_monetary_instruments).
narrative_ontology:cs_axiom_status(environmental_integration_binds_monetary_instruments, holdable).
narrative_ontology:cs_axiom_grounding('7dd32b15-d8fb-4f1c-95cb-6064750ec331', environmental_integration_binds_monetary_instruments, conventional).
narrative_ontology:cs_axiom('7dd32b15-d8fb-4f1c-95cb-6064750ec331', secondary, unpriced_transition_risk_is_financial_stability_risk).
narrative_ontology:cs_axiom_status(unpriced_transition_risk_is_financial_stability_risk, holdable).
narrative_ontology:cs_axiom_grounding('7dd32b15-d8fb-4f1c-95cb-6064750ec331', unpriced_transition_risk_is_financial_stability_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('7dd32b15-d8fb-4f1c-95cb-6064750ec331', narrow_price_stability_primacy_framework).
narrative_ontology:cs_drift_state('7dd32b15-d8fb-4f1c-95cb-6064750ec331', post_2021_strategy_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7dd32b15-d8fb-4f1c-95cb-6064750ec331', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, renewable_energy_issuers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_bond_underwriters).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_policy_coalition_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector_issuers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_member_state_treasuries).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, coal_dependent_regional_workforces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the collateral haircut schedule and asset purchase tilting rules that incorporate climate risk scoring. Justifies the policy as both a prudential risk measure (climate risk as financial risk) and a treaty obligation under Article 11 TFEU. Controls the calibration of tilting parameters and can expand or contract their scope without further legislative approval.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from preferential collateral treatment and disproportionate representation in ECB corporate bond purchase tilting, lowering their cost of capital relative to carbon-intensive peers. Do not administer the constraint; simply collect the financing advantage it produces.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, renewable_energy_issuers, beneficiary,
    moderate, biographical, mobile, continental).

% Structure and place green-labeled debt that qualifies for favorable treatment under the tilted framework; the fee-generating pipeline expands as issuers seek the preferential collateral status.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_bond_underwriters, beneficiary,
    organized, biographical, mobile, continental).

% A bloc of member states (concentrated in northern and western Europe) that pushed for Article 11 integration into monetary policy, gaining political cover for domestic decarbonization targets by having the central bank absorb part of the transition-financing burden. They cannot unilaterally exit the ECB's collateral framework but can shape it through Governing Council representation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_policy_coalition_states, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, climate_policy_coalition_states, agenda_setter).

% Face rising collateral haircuts and reduced eligibility for central bank asset purchases as climate risk scores are folded into eligibility criteria, raising their cost of capital directly. Have lobbying capacity but cannot exit the eurozone collateral framework without abandoning access to central bank refinancing operations entirely.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector_issuers, payer,
    powerful, biographical, constrained, continental).

% Sovereign issuers whose economies remain reliant on carbon-intensive industry see their national champions' bonds and the broader domestic credit channel disadvantaged by the tilting framework, while having no mechanism to opt their jurisdiction out of eurosystem-wide collateral rules set by the Governing Council.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_member_state_treasuries, payer,
    institutional, generational, trapped, national).

% Workers in coal and heavy-carbon regions bear the employment consequences when financing costs rise for the firms and utilities that employ them, without any voice in the Governing Council's calibration decisions and with limited regional labor mobility.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, coal_dependent_regional_workforces, payer,
    powerless, biographical, trapped, regional).

% A faction within the Eurosystem and among some member states holds that climate tilting exceeds the primary price-stability mandate and risks politicizing monetary policy; their objection is registered in dissenting statements but does not alter the adopted framework once the Governing Council majority commits to the climate-incorporation reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_price_stability_bloc, excluded,
    organized, generational, constrained, continental).

% Would adjudicate any legal challenge to whether Article 11 TFEU's environmental integration clause can operate through monetary policy instruments without violating central bank independence or the price-stability primacy of Article 127 TFEU. Has not yet ruled definitively on the scope of this specific instrumental linkage.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, european_court_of_justice, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, diffuse).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns central bank balance-sheet operations with the EU's treaty-wide climate transition objective, internalizing a systemic financial risk (stranded carbon assets, transition risk) that would otherwise remain unpriced in collateral and purchase decisions.
% TRANSFER_FUNCTION: Moves financing-cost advantage from carbon-intensive issuers and their dependent regional economies to green-labeled issuers and the coalition of states favoring accelerated decarbonization, channeled through collateral haircuts and asset purchase tilting weights administered by the Governing Council.
% ABSENT_VOICES: The orthodox price-stability bloc registers formal dissent but is outvoted; coal-dependent regional workforces have no institutional seat at the Governing Council table at all — their interests are represented only indirectly through national treasuries whose own leverage over Eurosystem-wide collateral rules is limited.
% DISAPPEARANCE_RATIONALE: If climate risk incorporation were withdrawn from the collateral framework and purchase programs overnight, fossil fuel issuers would regain unimpeded access to Eurosystem refinancing at prior terms, financing-cost differentials between green and carbon-intensive issuers would compress substantially, and the coalition states would lose a mechanism for offloading transition-financing burden onto monetary policy — domestic fiscal and regulatory tools would have to absorb the gap.
% FOUNDING_PROBLEM: Two intertwined problems: (1) unpriced climate transition risk sitting on Eurosystem collateral and balance sheets that could threaten future financial stability; (2) a treaty-level obligation under Article 11 TFEU requiring environmental protection to be integrated into all Union policies, including monetary policy, which the orthodox reading had treated as inapplicable to the ECB.
% FOUNDING_PROBLEM_CORROBORATION: The ECB itself and the climate policy coalition states attest the founding problem (systemic transition risk plus treaty obligation) is live and central. Independent legal scholars writing on ECB mandate scope, along with the dissenting orthodox price-stability bloc within the Eurosystem, attest that the treaty obligation reading is a contested extension rather than settled law, and that the risk-based justification increasingly functions as cover for a distributive policy choice the Treaty did not clearly assign to the central bank.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__climate_incorporation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__climate_incorporation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.28 to 0.58) as the tilting framework matures from pilot disclosure requirements to binding collateral haircuts and purchase-weight adjustments — the extraction is real but was initially modest and has hardened. Theater ratio starts elevated (0.55) reflecting early performative disclosure exercises with limited operational bite, then falls as the framework becomes substantively binding (0.42) — the inverse of the typical piton trajectory, appropriate to a mechanism moving from symbolic gesture toward operational enforcement. Suppression rises steadily (0.35 to 0.61) as portfolio tilting hardens from voluntary guidance into binding eligibility criteria enforced through the collateral framework itself — a novel suppression mechanism distinct from classical regulatory coercion, since no fossil fuel issuer is barred by law from issuing debt, but access to central bank refinancing is conditioned on climate scoring.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB Governing Council's seat, this reading is a coordinated response to a genuine systemic risk plus a treaty-mandated integration duty — a rope. From the fossil fuel issuer and carbon-intensive treasury seats, the identical mechanism computes as enforced extraction riding on a risk-management justification whose calibration decisions they cannot contest. The engine's per-seat computation should surface this divergence structurally rather than requiring either seat's framing to be privileged.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy issuers and green bond underwriters sit near the beneficiary end: mobile exit options, direct financing-cost gains, no administrative burden. Climate policy coalition states are dual-positioned — beneficiaries of the political cover the mechanism provides, and partial co-authors of the rule through Governing Council votes, hence the secondary agenda_setter role. Fossil fuel issuers and carbon-intensive treasuries sit near the target end: constrained-to-trapped exit (national treasuries cannot exit Eurosystem-wide collateral rules; fossil issuers cannot exit and retain refinancing access), direct cost from haircuts. Coal-dependent workforces are the most trapped and powerless payer — diffuse harm, zero institutional voice, geographic and skill immobility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unpriced transition risk plus treaty obligation) has empirical support but is contested as to whether it required THIS instrument (monetary policy collateral tilting) versus fiscal or regulatory tools assigned elsewhere in the Treaty. Classifying as tangled_rope rather than snare avoids mislabeling a genuine, if contested, risk-management coordination function as pure extraction; classifying as tangled_rope rather than rope avoids treating the concentrated collateral-cost transfer to carbon-intensive issuers and dependent workforces as costless coordination. The mandate has not clearly outlived its function — the transition risk it targets remains live — but the instrument choice and its distributive consequences remain genuinely contested rather than resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_scope_of_article_11_over_monetary_instruments,
    'Does Article 11 TFEU''s cross-cutting environmental integration duty operate directly on ECB monetary policy instrument choice, or does it apply only to policies where the EU institutions have been assigned discretionary balancing authority (which orthodox readings hold the ECB''s price-stability mandate excludes)?',
    'A definitive ruling from the European Court of Justice on whether Article 11''s integration clause creates an operative obligation on the ECB''s instrument design, as opposed to a general interpretive principle without direct instrumental force. Pending litigation or preliminary reference could resolve this.',
    'If the ECJ rules Article 11 does not directly bind instrument choice, this reading''s treaty-obligation justification collapses and the climate tilting framework would need to be re-grounded purely in prudential risk management (a narrower, less extractive framing) or withdrawn as ultra vires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_scope_of_article_11_over_monetary_instruments, conceptual, 'Whether Article 11 TFEU creates a direct operative duty on ECB instrument design or only a general interpretive principle.').

omega_variable(
    prudential_risk_vs_distributive_policy_boundary,
    'Is climate risk integration into collateral frameworks best characterized as prudential risk management (pricing a real financial risk that was previously mispriced) or as a distributive climate policy instrument wearing risk-management framing?',
    'Compare the calibration of climate risk scores against independent physical and transition risk models; if the scoring systematically diverges from risk-based calibration in ways that track political priorities (e.g., disproportionate penalty relative to measured default/transition risk), the distributive-policy reading gains support.',
    'A finding of systematic divergence from risk-based calibration would strengthen the case that this reading functions as tangled_rope (coordination cover for extraction) rather than a risk-adjusted rope; convergence with independent risk models would support a more benign coordination reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prudential_risk_vs_distributive_policy_boundary, empirical, 'Whether climate collateral haircuts track measured financial risk or track political-distributive priorities beyond risk.').

omega_variable(
    central_bank_independence_tension,
    'Does incorporating a treaty-wide policy objective (climate integration) into monetary instrument design compromise the operational independence the Treaty grants the ECB specifically to protect price stability from political capture — even if the climate objective is itself treaty-grounded?',
    'Comparative institutional analysis of whether the Governing Council''s climate calibration decisions are insulated from political pressure to the same degree as its core rate-setting decisions, and whether coalition member states use Governing Council influence over tilting parameters as a channel for pursuing domestic climate goals through the back door of monetary policy.',
    'If independence is found to be compromised, this reading would be more accurately classified nearer snare (an extraction mechanism using treaty-obligation language to launder political influence over the central bank); if independence is preserved, tangled_rope remains the more accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_bank_independence_tension, conceptual, 'Whether climate incorporation opens a political-influence channel that compromises ECB operational independence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ecb__tr_t4, ecb_mandate_article_127__climate_incorporation, theater_ratio, 4, 0.52).
narrative_ontology:measurement(ecb__tr_t8, ecb_mandate_article_127__climate_incorporation, theater_ratio, 8, 0.49).
narrative_ontology:measurement(ecb__tr_t12, ecb_mandate_article_127__climate_incorporation, theater_ratio, 12, 0.46).
narrative_ontology:measurement(ecb__tr_t16, ecb_mandate_article_127__climate_incorporation, theater_ratio, 16, 0.44).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__climate_incorporation, theater_ratio, 20, 0.43).
narrative_ontology:measurement(ecb__tr_t24, ecb_mandate_article_127__climate_incorporation, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ecb__be_t4, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(ecb__be_t8, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(ecb__be_t12, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(ecb__be_t16, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(ecb__be_t24, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ecb__su_t4, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(ecb__su_t8, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(ecb__su_t12, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(ecb__su_t16, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(ecb__su_t24, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 24, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__climate_incorporation, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the ecb_mandate_article_127 kernel, decomposed per the epsilon-invariance principle: orthodox_price_stability (Mountain-leaning, minimal extraction, exclusive inflation-target focus), expansive_secondary_objectives (moderate discretionary balancing without specific climate mandate), and this reading, climate_incorporation (tangled_rope, substantial and rising extraction via collateral tilting). Each reading has a distinct beneficiary/victim structure and a distinct epsilon; they are not the same constraint viewed from different angles but three structurally different claims about what Article 127 plus Article 11 TFEU jointly require of ECB instrument design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
