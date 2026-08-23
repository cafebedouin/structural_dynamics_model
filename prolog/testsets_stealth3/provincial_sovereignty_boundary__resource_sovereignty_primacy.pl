% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__resource_sovereignty_primacy, []).

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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty Primacy (Division-of-Powers Kernel Reading)
 *   domain: political-economy/federalism/resource-governance
 *
 * SUMMARY:
 *   A contested constitutional boundary in Canadian federalism: provinces own
 *   their natural resources (entrenched as s.92A, Constitution Act 1982)
 *   while the federal tier prices carbon, conditions transfers, and gates
 *   interprovincial infrastructure. This file instantiates ONE reading of the
 *   kernel provincial_sovereignty_boundary - resource_sovereignty_primacy -
 *   under which that ownership grounds absolute territorial sovereignty,
 *   making federal climate and fiscal overlays on the resource estate
 *   illegitimate takings and unilateral exit a constitutional right. Epsilon
 *   is authored for the standing arrangement under contest (ownership plus
 *   overlays), assessed by this reading's own lights: heavily burdensome from
 *   the resource-province side (0.72). The reading's endorsed alternative -
 *   fully sovereign provinces - is not the referent and contributes nothing
 *   to the score. Sibling readings of the same kernel are separate files
 *   linked via network.affects_constraints (see dual_formulation_note). Claim
 *   and metrics are independent authored facts: the tangled_rope claim
 *   reflects my structural read (real pooled functions, asymmetric burden,
 *   active enforcement); the engine computes per-seat types from the data.
 *
 * KEY AGENTS:
 *   - federal_executive_and_parliament: agenda-setter (institutional/arbitrage) - sets targets, administers the backstop, conditions transfers, substitutes instruments freely
 *   - supreme_court_of_canada: enforcement arm of the legal boundary (institutional/analytical)
 *   - hydrocarbon_exporting_provinces: principal burden-bearing seat (institutional/constrained) - also collects royalties, genuinely dual-positioned
 *   - oil_and_gas_producers: burden-bearing seat with partial exit (powerful/arbitrage)
 *   - energy_sector_workforces: place-bound burden-bearing seat (organized/constrained)
 *   - equalization_recipient_provinces: principal collecting seat (institutional/constrained)
 *   - indigenous_nations_on_resource_territories: excluded claimant (moderate/trapped) - title overlays the ground both levels claim
 *   - municipal_governments_hosting_development: excluded (powerless/local)
 *   - constitutional_scholars: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.72).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.66).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.72).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty Primacy (Division-of-Powers Kernel Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political-economy/federalism/resource-governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '927bf667-2f9d-44ac-ad5d-b6e39121e826').
narrative_ontology:cs_kernel_codification('927bf667-2f9d-44ac-ad5d-b6e39121e826', formalized).
narrative_ontology:cs_authority_grounding('927bf667-2f9d-44ac-ad5d-b6e39121e826', lineage).
narrative_ontology:cs_interpretation_layer_present('927bf667-2f9d-44ac-ad5d-b6e39121e826').
narrative_ontology:cs_reading_relation('927bf667-2f9d-44ac-ad5d-b6e39121e826', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('927bf667-2f9d-44ac-ad5d-b6e39121e826', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('927bf667-2f9d-44ac-ad5d-b6e39121e826', foundational, resource_control_constitutes_territorial_sovereignty).
narrative_ontology:cs_axiom_status(resource_control_constitutes_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('927bf667-2f9d-44ac-ad5d-b6e39121e826', resource_control_constitutes_territorial_sovereignty, conventional).
narrative_ontology:cs_axiom('927bf667-2f9d-44ac-ad5d-b6e39121e826', foundational, federal_resource_governance_without_provincial_consent_is_usurpation).
narrative_ontology:cs_axiom_status(federal_resource_governance_without_provincial_consent_is_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('927bf667-2f9d-44ac-ad5d-b6e39121e826', federal_resource_governance_without_provincial_consent_is_usurpation, conventional).
narrative_ontology:cs_reference_frame('927bf667-2f9d-44ac-ad5d-b6e39121e826', province_proprietary_sovereignty).
narrative_ontology:cs_drift_state('927bf667-2f9d-44ac-ad5d-b6e39121e826', contemporary_post_carbon_pricing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('927bf667-2f9d-44ac-ad5d-b6e39121e826', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_executive_and_parliament).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, equalization_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, hydrocarbon_exporting_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, oil_and_gas_producers).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, energy_sector_workforces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, hydrocarbon_exporting_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, oil_and_gas_producers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets national emissions targets and administers the federal fuel charge in provinces that declined equivalent schemes; attaches conditions to health and social transfers; runs the impact-assessment regime that can decline interprovincial projects a province has approved. Collects fuel-charge revenue, returns the large majority as household rebates in the province of collection, and retains the balance as program revenue. Any single instrument can be swapped for another - tax, regulation, or spending substitute freely.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_executive_and_parliament, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_executive_and_parliament, beneficiary).

% Adjudicates division-of-powers disputes; in the 2021 carbon-pricing reference it confirmed that Parliament may regulate greenhouse gases as a matter of national concern. Its judgments fix what the boundary legally tolerates in both directions. It holds no fiscal stake in royalties or transfers.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, supreme_court_of_canada, agenda_setter,
    institutional, generational, analytical, national).

% Own and lease the subsurface resource estate, set royalties, and fund services substantially from resource revenue. They face a federal fuel charge their legislatures never enacted, an equalization formula whose demands grow as resource revenue lifts their fiscal capacity, and an approval regime that can stop export corridors. They answered with declaratory sovereignty statutes and litigation. Leaving the union would sever their firms' market access and banking relationships; staying means absorbing federal overlays they regard as exceeding the federal mandate. Royalty collections and internal administration of the resource estate flow to them.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, hydrocarbon_exporting_provinces, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, hydrocarbon_exporting_provinces, beneficiary).

% Hold provincial tenures and ship crude and gas to export markets; pay compliance costs under federal pricing and carry stranded-project risk when federal review declines export infrastructure. Investment capital is internationally mobile - it can move to Gulf Coast or Permian basins - while booked reserves cannot. Secure tenure under provincial law and access to the domestic market are worth preserving.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, oil_and_gas_producers, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, oil_and_gas_producers, beneficiary).

% Concentrated in extraction, upgrading, and construction trades tied to a few corridors; wages track project cycles. Compliance costs pass through to fuel bills and job risk concentrates in cancelled expansions. Moving means relocating households or retraining out of the trade; staying means riding commodity and policy cycles. Provincial royalties fund the schools and hospitals their towns rely on.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, energy_sector_workforces, payer,
    organized, biographical, constrained, regional).

% Receive unconditional transfers that raise fiscal capacity toward a national standard; several have drawn the grant continuously for decades. The formula rewards lower own-source revenue, and resource-driven growth elsewhere enlarges the pool they share. Withdrawing from the union would forfeit net receipts; their voice is exercised through bargaining over the formula itself.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, equalization_recipient_provinces, beneficiary,
    institutional, generational, constrained, continental).

% Hold historic treaties or assert Aboriginal title over much of the producing geography. Development proceeds under provincial permitting with consultation duties, while the sovereignty contest treats the subsurface as provincially owned outright. Litigation capacity is real but slow and expensive, and the territory itself cannot be relocated. Royalty-sharing and co-management remain negotiated exceptions rather than defaults.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations_on_resource_territories, excluded,
    moderate, generational, trapped, regional).

% Host wellsites, mines, rail lines, and port infrastructure; collect property tax but almost none of the royalty. They absorb boom-and-bust swings in service demand and infrastructure wear with narrow revenue tools, and hold no seat in either the royalty bargain or the federal climate design.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, municipal_governments_hosting_development, excluded,
    powerless, immediate, trapped, local).

% Analyze the division of powers, the secession reference, and the fiscal union; publish competing accounts of what entrenched resource ownership does and does not entail. No material stake in royalties or transfers; reputational stakes ride partly on which account prevails.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, equalization_recipient_provinces).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A federal union pools defence, currency, diplomacy, and interprovincial trade rules; compensates fiscally weaker regions through transfers; and provides a single negotiating front for cross-boundary problems - climate, waters, migratory species - that no province can address alone.
% TRANSFER_FUNCTION: Moves purchasing power from higher-fiscal-capacity and emissions-intensive regions - disproportionately the hydrocarbon-exporting provinces - toward lower-capacity provinces through equalization and federal programming; and moves discretionary authority over resource-project approval from provincial cabinets to federal regulators.
% ABSENT_VOICES: Indigenous nations holding treaty or asserted title to the resource territories are largely outside the sovereignty contest as framed - the reading's absolutism presupposes unencumbered provincial title. Municipal governments that host development while capturing minimal royalty, and the generations who will bear unpriced climate damages, likewise have no seat at the table.
% DISAPPEARANCE_RATIONALE: Resource-province partisans predict rearrangement into prosperity once federal levies and gatekeeping lift off the resource estate; federalists predict degradation of pooled defence, currency, trade, and climate coordination, plus beggar-thy-neighbour interprovincial barriers; recipient provinces predict collapse of services funded by transfers. No party accepts another's counterfactual, so the verdict is contested by construction.
% FOUNDING_PROBLEM: Secure provincial control over natural-resource revenues so provinces - particularly the western provinces that received their Crown lands and resources only in the 1930 transfer agreements - could finance their own institutions without federal interference; the guarantee was entrenched textually as s.92A at patriation in 1982.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the 1930 Natural Resources Transfer Agreements and the 1980-82 patriation record corroborates the original founding problem and its resolution, and Supreme Court jurisprudence confirms the textual guarantee. No corroborating source outside the provincial-governments set attests that the original problem remains unsolved - the live grievances (equalization incidence, federal carbon pricing, corridor approval) are materially distinct from the 1930/1982 problem. That gap is itself signal: the mandate as founded is spent, and the current contest is a successor dispute.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.72 (referent: the standing arrangement, this reading's lights): the federal fuel charge operates inside legislatures that never enacted it; equalization entitlements grow mechanically as resource revenue lifts provincial fiscal capacity; the federal assessment regime can decline export corridors a province has approved. Rebates return most household fuel-charge revenue, which is why the score stops short of the near-total range a pure-levy account would yield. Suppression 0.66 is the raw structural enforcement load - spending-power conditions, the backstop, the national-concern pathway confirmed in 2021 - and is authored unscaled, per the rule that only extractiveness is scaled downstream. Theater 0.38 counts performative share on both sides: federal advisory and just-transition machinery alongside declaratory provincial statutes whose operative bite is narrow. Accessibility collapse 0.45: exits exist (capital relocation, alternate corridors, litigation, declaratory statutes) but none escapes the federal perimeter, and the secession reference converted exit from a right into a negotiation. Resistance 0.65: sustained litigation, two sovereignty statutes, a roughly 62 percent abolition vote in Alberta's 2021 equalization referendum, and four decades of corridor politics. All series share one eight-point grid (1985-2025) so no metric borrows another's timeline; suppression_requirement is authored because the story specifically tracks enforcement buildup (Kyoto-era soft coordination to the 2019 backstop ratchet and its 2021 judicial confirmation) - a rising trajectory, not a static picture. Coalition check: the burden-bearing seats here are institutional provinces with demonstrated coalition power - the 1930 transfers and s.92A itself were won by provincial coalitions - which is why resistance stays high and the arrangement remains contested rather than settled. Fixing is prohibitive relative to benefit: unwinding requires either formal constitutional amendment or unilateral federal retreat that breaks treaty commitments and destabilizes recipient-province finances, against diffuse gains.
 *
 * PERSPECTIVAL GAP:
 *   Seats should diverge. From the federal executive seat the arrangement computes close to a coordination mechanism it built: pooled defence, currency, internal trade, a single climate front, with most priced revenue rebated. From the recipient-province seat it computes as net-benefit coordination. From the resource-province seat the same facts compute as enforced taking under constrained exit. Producer seats register less of the burden than workforce seats because arbitrage-grade exit (moving investment abroad) dampens what reaches them, while workers and host municipalities are place-bound and absorb the full pass-through. The excluded Indigenous seat registers an injury neither side's vocabulary captures: the sovereignty contest presupposes provincial title while treaty and title claims overlay the same ground. Scholarly observers divide along the same joint as the readings themselves. Identity note: what binds the resource provinces is economic and legal rather than identity-fused - grievance identity attaches to the contest, not to the union; if economic diversification broke the resource-economy frame, resistance would fall faster than exit would rise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the federal executive (agenda-setting gains, retained program revenue, instrument substitutability) and the recipient provinces (terminal accrual of the transfer stream) sit near the subsidized end. Victim declarations map to high directionality: the exporting provinces bear the overlays with constrained exit (market access and banking tie them in), workforces are place-bound, and producers sit somewhat lower via capital mobility. No directionality overrides were authored: beneficiary/victim plus exit data derive the spread, and an override keyed to the institutional power atom would smear seats that point in opposite directions - the federal executive and the provincial governments share an atom but sit at opposite ends. Receipt: the transfer stream terminates in recipient-province treasuries, so gain_flow names that seat; the federal seat intermediates leverage but rebates most priced revenue, which is why it is a beneficiary of the arrangement without being the seat the transfer stream lands in.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim blocks two mislabels. Reading the arrangement as pure taking ignores functions every seat consumes - currency, defence, internal trade, risk-pooling - so the coordination half of the gate is genuinely met. Reading it as pure coordination ignores the documented asymmetry: formula mechanics that scale with resource fiscal capacity and a levy imposed on non-consenting legislatures. On mandatrophy: the founding guarantee - secure provincial resource revenue, won in 1930 and entrenched in 1982 - is achieved; the live conflict is a successor dispute about climate-era authority wearing the old banner. Hence founding_problem_status is contested and disappearance is contested, and no zombie flag fires; the honest residual risk is the reverse drift, a boundary treated as permanently closed in Ottawa and perpetually reopenable in Edmonton, which converts the settlement itself into standing negotiation theater. The mandatrophy_resolved boolean is deliberately left undeclared: the constraint retains live functions; only its founding problem is superseded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_delta,
    'This constraint is one reading of kernel provincial_sovereignty_boundary; what would the sibling readings change structurally?',
    'Authoring the sibling files (constitutional_subordination, compact_federalism) and comparing computed per-seat types and epsilon over the same referent.',
    'Under constitutional_subordination the same federal impositions read as legitimate coordination and the burden-bearing seat''s effective extraction collapses toward zero; under compact_federalism they read as renegotiable breach of compact terms, with exit framed as negotiated rather than unilateral. The disagreement is located at one joint: whether s.92A ownership confers sovereignty or mere proprietary jurisdiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Kernel-membership omega recording sibling deltas and the locus of disagreement.').

omega_variable(
    property_to_sovereignty_bridge,
    'Does ownership of natural resources actually entail territorial sovereignty - immunity from federal climate law and a unilateral right of exit - or only proprietary jurisdiction subject to federal paramountcy?',
    'Doctrinal test: no court has held that s.92A ownership grounds sovereignty, and the secession reference rejected a unilateral right. The bridge is built if a court adopts the property-as-sovereignty theory or a constitutional amendment entrenches it.',
    'If the bridge fails doctrinally, this reading reduces to a strong provincial-rights position: the secession-right implication lapses and the burden-bearing seat''s exit stays constrained rather than open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_to_sovereignty_bridge, conceptual, 'Whether the property-to-sovereignty inference holds as law or only as political theory.').

omega_variable(
    indigenous_title_overlay,
    'How far do unresolved Aboriginal title and treaty entitlements encumber the absolute provincial title this reading presupposes?',
    'Land-claims litigation outcomes and modern-treaty implementation; the cumulative geographic extent of declared title across producing regions.',
    'Widespread recognized title would make absolute provincial sovereignty legally impossible, forcing this reading to either accept shared sovereignty or abandon its absolutist premise; the beneficiary and burden maps would each gain a third party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_title_overlay, empirical, 'Title encumbrance against the absolutist provincial-title premise.').

omega_variable(
    equalization_net_incidence,
    'Is the net fiscal flow actually outward from the resource provinces once federal expenditures located in them, rebates, and per-capita program spending are counted against contributions?',
    'Parliamentary Budget Officer and Statistics Canada incidence studies reconciling gross contributions, located expenditures, and rebate flows by province.',
    'If net flows are small or reversed, the fiscal-taking component anchoring this reading''s epsilon weakens sharply and the grievance shifts wholly to regulatory authority rather than money.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equalization_net_incidence, empirical, 'Net incidence of the fiscal union on the resource provinces.').

omega_variable(
    emissions_ledger_direction,
    'Whose ledger records exported emissions: are federal climate costs on the resource provinces a taking from them, or are unpriced climate damages from exported hydrocarbons a taking by them from the federation and the global commons?',
    'An agreed attribution protocol - production-based versus consumption-based accounting - adopted by the disputants or imposed through litigation.',
    'Reversing the ledger reverses the burden-bearing seat''s directionality: the resource provinces become net debtors and this reading''s core extraction claim inverts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emissions_ledger_direction, conceptual, 'Externality attribution decides which direction the extraction runs.').

omega_variable(
    sovereignty_statute_efficacy,
    'Do the declaratory provincial sovereignty statutes (Alberta 2022, Saskatchewan 2023) change federal behaviour, or do they operate as position-taking with narrow operative bite?',
    'Track invocations of the statutes'' mechanisms against federal regulatory decisions over a multi-year window; count cases where a federal measure was altered, delayed, or resolved differently because of them.',
    'Purely symbolic operation would mean the measured resistance overstates effective counter-pressure, and this reading''s practical persistence would rest on court politics rather than enacted sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_statute_efficacy, empirical, 'Operative versus theatrical character of the provincial sovereignty statutes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1985, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1985, 0.14).
narrative_ontology:measurement(prov_tr_t1993, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1993, 0.17).
narrative_ontology:measurement(prov_tr_t2001, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(prov_tr_t2009, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2009, 0.27).
narrative_ontology:measurement(prov_tr_t2016, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2016, 0.32).
narrative_ontology:measurement(prov_tr_t2019, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2019, 0.36).
narrative_ontology:measurement(prov_tr_t2022, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2022, 0.38).
narrative_ontology:measurement(prov_tr_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(prov_be_t1985, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1985, 0.3).
narrative_ontology:measurement(prov_be_t1993, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1993, 0.34).
narrative_ontology:measurement(prov_be_t2001, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement(prov_be_t2009, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2009, 0.5).
narrative_ontology:measurement(prov_be_t2016, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement(prov_be_t2019, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement(prov_be_t2022, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2022, 0.7).
narrative_ontology:measurement(prov_be_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1985, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1985, 0.2).
narrative_ontology:measurement(prov_su_t1993, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1993, 0.23).
narrative_ontology:measurement(prov_su_t2001, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2001, 0.3).
narrative_ontology:measurement(prov_su_t2009, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2009, 0.38).
narrative_ontology:measurement(prov_su_t2016, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2016, 0.5).
narrative_ontology:measurement(prov_su_t2019, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2019, 0.61).
narrative_ontology:measurement(prov_su_t2022, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2022, 0.65).
narrative_ontology:measurement(prov_su_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_allocation).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, compact_federalism).

% DUAL FORMULATION NOTE:
% The colloquial label 'provincial resource sovereignty' decomposes along the sovereignty-source axis into three readings of one kernel (provincial_sovereignty_boundary). This file instantiates resource_sovereignty_primacy (ownership entails absolute sovereignty; the same observable federal impositions read as takings, epsilon high, burden-bearing seats are the resource provinces). The sibling file constitutional_subordination instantiates the creature-of-the-constitution reading, under which the identical impositions read as legitimate coordination and the payer-seat burden collapses toward zero; the sibling compact_federalism instantiates residual-sovereignty-via-compact, under which the impositions read as renegotiable breach rather than usurpation. Same events, different epsilon - hence separate files, each with a single stable epsilon, linked here per the epsilon-invariance rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
