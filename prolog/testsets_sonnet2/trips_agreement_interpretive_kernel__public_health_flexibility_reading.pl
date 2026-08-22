% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Reading — Compulsory Licensing and Parallel Import Latitude
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested TRIPS interpretive
 *   kernel: the public health flexibility reading, under which Articles 31
 *   and 31bis and the 2001 Doha Declaration are read as embedding broad
 *   compulsory licensing and parallel import latitude for member states
 *   facing public health needs. Under this reading, generic manufacturers and
 *   health ministries in low- and middle-income countries gain expanded
 *   negotiating leverage and eventual market access; originator patent
 *   holders and the trade negotiators representing innovator-economy
 *   pharmaceutical sectors bear pricing erosion and exclusivity loss. This is
 *   NOT the strong_exclusivity_reading (which construes the same text as
 *   mandating narrow flexibilities to protect innovation incentives) and it
 *   is NOT the dispute_settlement_interpretive_authority reading (which
 *   locates ultimate interpretive power in WTO panels rather than in the
 *   text's own embedded latitude). Each of these is a separate constraint
 *   with its own ε and stakeholder structure; this file describes only the
 *   flexibility reading as its own proponents and beneficiaries understand
 *   and invoke it.
 *
 * KEY AGENTS:
 *   - generic_pharmaceutical_manufacturers: beneficiary (organized/mobile) — gains licensing and export pathways
 *   - low_and_middle_income_health_ministries: agenda_setter (moderate/constrained) — invokes the flexibility
 *   - patients_dependent_on_essential_medicines: beneficiary (powerless/trapped) — ultimate recipient of access
 *   - originator_pharmaceutical_patent_holders: payer (institutional/constrained) — bears exclusivity erosion
 *   - innovator_country_trade_negotiators: payer/excluded (institutional/constrained) — weakened negotiating position
 *   - wto_dispute_settlement_panels: observer (institutional/analytical) — adjudicates but does not author this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.58).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.52).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Reading — Compulsory Licensing and Parallel Import Latitude").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '6f22d4ec-449f-459e-b9f0-4c6cf5b378bc').
narrative_ontology:cs_kernel_codification('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', fixed_text).
narrative_ontology:cs_authority_grounding('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', distributed).
narrative_ontology:cs_reading_relation('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_axiom('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', foundational, public_health_necessity_overrides_exclusivity_default).
narrative_ontology:cs_axiom_status(public_health_necessity_overrides_exclusivity_default, holdable).
narrative_ontology:cs_axiom_grounding('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', public_health_necessity_overrides_exclusivity_default, deontological).
narrative_ontology:cs_axiom('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', secondary, doha_declaration_constitutes_authoritative_interpretation).
narrative_ontology:cs_axiom_status(doha_declaration_constitutes_authoritative_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', doha_declaration_constitutes_authoritative_interpretation, conventional).
narrative_ontology:cs_reference_frame('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', doha_declaration_consensus_baseline).
narrative_ontology:cs_drift_state('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', post_covid19_waiver_dispute_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6f22d4ec-449f-459e-b9f0-4c6cf5b378bc', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_and_middle_income_health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_dependent_on_essential_medicines).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, innovator_country_trade_negotiators).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, doha_declaration_public_health_primacy).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__public_health_flexibility_reading, sovereign_regulatory_flexibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positioned to obtain compulsory licenses or supply under parallel-import arrangements once a health ministry invokes the flexibility, allowing production and export of patented formulations without originator consent. Their market access depends entirely on this reading holding; a narrow-flexibility ruling from a dispute panel closes the door on the licenses they currently pursue.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Invoke compulsory licensing or parallel importation to secure affordable medicine supply during epidemics or chronic disease burdens, citing the Doha Declaration's public-health primacy language. Their leverage in bilateral trade talks and WTO council debates rests on this reading of the text; they face diplomatic and sometimes retaliatory pressure from origin-country trade representatives when they invoke it broadly.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_and_middle_income_health_ministries, agenda_setter,
    moderate, generational, constrained, national).

% Access to antiretrovirals, cancer treatments, and other patented drugs depends on whether their government's health ministry can successfully invoke and defend a compulsory license without provoking a trade dispute. They have no direct voice in the interpretive contest but bear its consequences most acutely.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_dependent_on_essential_medicines, beneficiary,
    powerless, immediate, trapped, national).

% Hold the patents subject to compulsory licensing and see market exclusivity and pricing power eroded whenever a licensing action succeeds or a parallel-import channel opens. They lobby aggressively for the narrow-flexibility reading and route disputes to trade representatives and WTO panels, but under this reading their exclusivity claims are read down.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_patent_holders, payer,
    institutional, biographical, constrained, global).

% Represent states whose pharmaceutical sectors depend on strong exclusivity; they negotiate bilateral and multilateral pressure against broad invocation of flexibilities, sometimes attaching TRIPS-plus provisions to trade agreements to counteract the flexibility reading. Under this reading their negotiating position is weakened relative to what a strong-exclusivity ruling would give them.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, innovator_country_trade_negotiators, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, innovator_country_trade_negotiators, excluded).

% Adjudicate disputes over whether a given compulsory license or parallel import falls within TRIPS's Article 31 flexibilities or breaches patent-holder rights. Their rulings do not resolve which reading is 'true' but determine, case by case, which reading prevails in enforceable practice — this is itself a separate kernel reading (dispute_settlement_interpretive_authority), not this constraint.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_settlement_panels, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the international patent regime with public health emergency response by preserving textual latitude (Articles 31, 31bis, and the Doha Declaration) for states to authorize generic production or importation when patent exclusivity would block access to essential medicines.
% TRANSFER_FUNCTION: Moves negotiating leverage and eventual production/distribution rights from originator patent holders to generic manufacturers and the health ministries that license them, and moves realized surplus (avoided drug expenditure) to patients and public health budgets in the invoking country.
% ABSENT_VOICES: Patients in the invoking country are the ultimate beneficiaries but have no seat in the interpretive contest itself; originator R&D investors and future-drug-development interests argue reduced expected returns will suppress future innovation, a cost borne by future patients not yet identifiable and structurally absent from any negotiation table.
% DISAPPEARANCE_RATIONALE: If this reading of the TRIPS text were abandoned in favor of the strong-exclusivity reading, compulsory licensing actions taken during health emergencies (e.g., HIV/AIDS antiretroviral access campaigns, COVID-19 vaccine equity disputes) would lose their textual grounding; health ministries would need alternative legal bases or would face heightened dispute-settlement risk, generic manufacturers would lose a central legal pathway to market entry, and pricing leverage would shift substantially back toward originator firms.
% FOUNDING_PROBLEM: The original TRIPS Agreement (1994) was drafted primarily to harmonize and strengthen intellectual property protection for cross-border trade; the flexibility reading emerged to address the specific problem that uniform strong patent enforcement, applied without qualification, would leave low- and middle-income countries unable to respond to public health emergencies like the HIV/AIDS epidemic.
% FOUNDING_PROBLEM_CORROBORATION: The 2001 Doha Declaration on TRIPS and Public Health was adopted by consensus among all WTO member states, including originator-country governments, explicitly affirming that TRIPS 'can and should be interpreted' to support public health measures — corroboration exists from outside the direct beneficiary set (originator-country governments themselves signed the declaration), though pharmaceutical industry associations continue to contest the scope of that consensus in practice.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-substantial) because the reading redistributes real economic value — patent exclusivity has genuine market value, and the flexibility reading systematically reallocates a portion of it toward generic producers and public budgets. This is not zero-cost coordination: it is redistribution enabled by a genuine coordination function (protecting public health access during crises). Suppression is authored at 0.52 because invoking the flexibility broadly requires health ministries to withstand real diplomatic and trade pressure from patent-holder governments — the flexibility is textually available but its exercise is actively contested and resisted, which is itself a form of suppression pressure on the beneficiary side. Theater ratio is moderate-low (0.3) reflecting that most invocations (South Africa's Medicines Act contest, Brazil and Thailand's ARV licenses, the COVID-19 TRIPS waiver debate) involved substantive legal and diplomatic conflict rather than pure performance. The temporal series shows extraction declining from 1995 (when the text's ambiguity was unresolved and patent holders' expected exclusivity was highest) through the 2001 Doha Declaration and subsequent case law establishing the flexibility more firmly, then a modest uptick by 2025 reflecting renewed pushback via TRIPS-plus bilateral agreements that erode the flexibility's practical availability even as the textual reading holds.
 *
 * DIRECTIONALITY LOGIC:
 *   Generic manufacturers and health ministries sit toward the beneficiary end of directionality because the flexibility reading is the mechanism through which they gain concrete leverage and market access — this reading exists, in the reading's own terms, to serve them. Patients are beneficiaries in outcome but powerless and trapped, receiving benefit without agency in the interpretive contest itself. Originator patent holders and innovator-country negotiators sit toward the target end: their exclusivity and negotiating power are precisely what this reading erodes, and their exit options are constrained because they cannot simply withdraw from TRIPS or from the WTO system that embeds this text. WTO dispute panels are analytical observers under THIS constraint — their own interpretive authority is a separate constraint (dispute_settlement_interpretive_authority) not authored here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that uniform strong patent enforcement without health-crisis carve-outs leaves populations unable to respond to epidemics — remains live (the COVID-19 TRIPS waiver debate 2020-2022 demonstrated the problem persists in acute form), so this is not a mandatrophy case of a dead purpose maintained by inertia. The classification as tangled_rope rather than a clean rope reflects that the coordination function (protecting public health) is genuine but is achieved through an enforcement-backed redistribution mechanism that imposes real, contested costs on identifiable patent-holder parties — precisely the tangled_rope signature of coordination and extraction riding the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flexibility_reading_textual_determinacy,
    'Is the broad public-health flexibility genuinely embedded in the TRIPS text and its negotiating history (making the strong_exclusivity_reading a subsequent narrowing move), or is the flexibility reading itself a post-hoc political construction layered onto ambiguous text by the Doha Declaration''s consensus process?',
    'Close textual and travaux préparatoires analysis of the original 1994 TRIPS negotiations compared against the 2001 Doha Declaration''s interpretive claims; examination of whether Doha declared existing law or created new law.',
    'If the flexibility was original to the text, this reading has stronger genealogical legitimacy against the strong_exclusivity_reading; if it was a subsequent political achievement, both readings are better understood as competing constructions layered onto genuinely ambiguous original text, with neither claiming clean textual priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_reading_textual_determinacy, conceptual, 'Whether the flexibility reading was original to TRIPS or a subsequent interpretive achievement via Doha.').

omega_variable(
    innovation_incentive_tradeoff_magnitude,
    'How much does broad compulsory licensing under this reading actually suppress future pharmaceutical R&D investment, versus how much is this claim used strategically by originator firms to resist any licensing action regardless of its marginal effect on innovation incentives?',
    'Empirical study comparing R&D investment trajectories in therapeutic areas subject to frequent compulsory licensing (e.g., HIV/AIDS) against areas with minimal licensing activity, controlling for market size and disease burden.',
    'If the innovation-suppression effect is small, the extraction imposed on patent holders is closer to pure rent redistribution with minimal offsetting future cost; if large, the flexibility reading''s beneficiaries today are borrowing against future patients'' access to yet-undeveloped treatments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_incentive_tradeoff_magnitude, empirical, 'Magnitude of the innovation-incentive cost genuinely attributable to broad compulsory licensing.').

omega_variable(
    trips_plus_erosion_of_flexibility,
    'To what extent do bilateral and regional trade agreements containing TRIPS-plus provisions functionally override this reading''s flexibilities for the countries that sign them, even though the underlying multilateral text still supports the flexibility reading?',
    'Comparative analysis of compulsory licensing invocation rates and outcomes in countries bound by TRIPS-plus bilateral agreements versus those operating under baseline TRIPS obligations only.',
    'If TRIPS-plus provisions substantially neutralize the flexibility in practice for signatory countries, this reading''s real-world beneficiary set is narrower than its textual scope suggests — the flexibility is textually broad but practically constrained by an overlapping and more restrictive treaty layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trips_plus_erosion_of_flexibility, empirical, 'Whether bilateral TRIPS-plus agreements erode this reading''s practical availability despite its textual persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.32).
narrative_ontology:measurement(trip_tr_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.6).
narrative_ontology:measurement(trip_be_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2008, 0.56).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(trip_su_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the TRIPS Agreement's public health provisions.' The three readings share a kernel (trips_agreement_interpretive_kernel) but instantiate structurally distinct constraints with different beneficiary/victim sets and different epsilon values: public_health_flexibility_reading (this file, tangled_rope, moderate-substantial extraction redistributing exclusivity value toward generic manufacturers/health ministries/patients), strong_exclusivity_reading (the sibling reading construing the same text as mandating narrow flexibilities, inverting the beneficiary/victim structure), and dispute_settlement_interpretive_authority (a meta-level constraint about which reading WTO panels enforce, not about the text's substantive content). Per the epsilon-invariance principle, these are NOT the same constraint viewed from different angles — they have different epsilon values, different victim sets, and different classifications, and are linked here via network edges rather than merged into one hedged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
