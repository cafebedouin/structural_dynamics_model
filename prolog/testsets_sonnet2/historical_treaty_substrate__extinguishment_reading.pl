% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Historical Treaty Substrate — Extinguishment (Completed Property Transaction) Reading
 *   domain: legal_anthropology/indigenous_law/constitutional_theory
 *
 * SUMMARY:
 *   This story authors ONE reading of the historical_treaty_substrate kernel:
 *   the extinguishment reading, which treats historical treaties between
 *   Indigenous nations and settler states as completed property transactions
 *   — territorial sovereignty permanently ceded in exchange for defined
 *   reserve lands and fixed payments. Under this reading, Indigenous nations
 *   exit any victim classification with respect to territorial jurisdiction
 *   they voluntarily and finally alienated, and enter the beneficiary set
 *   narrowly, for the treaty rights explicitly enumerated (reserves,
 *   annuities, specified harvesting rights). The settler state becomes the
 *   sole legitimate sovereign over ceded territory, with courts, land
 *   registries, and resource licensing regimes operating on that premise.
 *   This is a distinct ε from the sibling readings (stewardship_reading
 *   treats no sovereignty as ceded at all; nation_to_nation_reading treats
 *   the relationship as ongoing and consent-contingent) — each is authored as
 *   its own constraint file per the ε-invariance principle, linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.81).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.76).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaty Substrate — Extinguishment (Completed Property Transaction) Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, 'e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6').
narrative_ontology:cs_kernel_codification('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', fixed_text).
narrative_ontology:cs_authority_grounding('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', extraction).
narrative_ontology:cs_interpretation_layer_present('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6').
narrative_ontology:cs_reading_relation('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', foundational, sovereignty_cession_was_complete_and_final).
narrative_ontology:cs_axiom_status(sovereignty_cession_was_complete_and_final, holdable).
narrative_ontology:cs_axiom_grounding('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', sovereignty_cession_was_complete_and_final, conventional).
narrative_ontology:cs_axiom('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', foundational, written_treaty_text_is_sole_controlling_instrument).
narrative_ontology:cs_axiom_status(written_treaty_text_is_sole_controlling_instrument, holdable).
narrative_ontology:cs_axiom_grounding('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', written_treaty_text_is_sole_controlling_instrument, conventional).
narrative_ontology:cs_axiom('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', secondary, enumerated_treaty_rights_constitute_full_consideration).
narrative_ontology:cs_axiom_status(enumerated_treaty_rights_constitute_full_consideration, holdable).
narrative_ontology:cs_axiom_grounding('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', enumerated_treaty_rights_constitute_full_consideration, instrumental).
narrative_ontology:cs_reference_frame('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', settler_sovereign_succession_doctrine).
narrative_ontology:cs_drift_state('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', post_reconciliation_commission_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e9bdfcfc-d6bc-4c48-87eb-11f3bb3fccb6', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_land_administration).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, resource_extraction_industries).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, non_indigenous_landholders).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, treaty_beneficiary_bands).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations_territorial_jurisdiction).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, future_generations_land_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, treaty_beneficiary_bands).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, doctrine_of_discovery_successor_title).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, crown_underlying_title_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, certainty_of_title_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers title registries, grants resource licenses, and defends the position in court that treaty signing extinguished Indigenous territorial sovereignty in exchange for reserves and fixed payments. Collects tax revenue, resource royalties, and jurisdictional authority over the ceded lands. Litigates aggressively against any reopening of the cession question, treating the treaties as closed transactions rather than living relationships.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state_land_administration, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, settler_state_land_administration, beneficiary).

% Obtain leases, permits, and clear title over ceded territory on the strength of the extinguishment reading; the completed-transaction framing is what lets them treat land access as a settled commercial matter rather than an ongoing negotiation with a sovereign counterpart. They can relocate capital elsewhere if title becomes contested, giving them exit the Indigenous nations lack.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, resource_extraction_industries, beneficiary,
    powerful, generational, mobile, national).

% Hold deeds, farms, and municipal property whose validity depends on the treaties having permanently and completely transferred sovereignty. Their security of title, mortgages, and inherited wealth rest on the extinguishment premise remaining legally uncontested.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, non_indigenous_landholders, beneficiary,
    moderate, generational, constrained, regional).

% Receive the enumerated reserve lands, annuities, and specific treaty rights (hunting, fishing) that the extinguishment reading treats as the full and final consideration for ceded territory. These payments and reserves are real and administratively enforceable, but arrive framed as closed compensation rather than an installment on an ongoing relationship — accepting them under this reading forecloses claims to the wider ceded territory.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, treaty_beneficiary_bands, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, treaty_beneficiary_bands, payer).

% Lose recognized jurisdiction over the vast majority of ancestral territory under this reading; retain only the enumerated reserve parcels. Cannot exit the constraint — the reading is enforced through courts, land registries, and policing of reserve boundaries, and asserting continuing sovereignty over ceded land is treated as legally void or criminal trespass regardless of oral treaty understandings to the contrary.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations_territorial_jurisdiction, payer,
    organized, civilizational, trapped, national).

% Inherit a jurisdictional baseline already fixed by the extinguishment reading before they had any voice in its formation. Their land base, resource rights, and self-governance capacity are permanently constrained by a transaction their ancestors are read to have completed on their behalf, regardless of documented discrepancies between written treaty text and oral negotiation records.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, future_generations_land_claimants, payer,
    powerless, civilizational, trapped, national).

% The Indigenous negotiators present at treaty signings, whose oral understandings (often mediated through interpreters of variable fidelity, and often explicitly describing shared stewardship rather than sale) are not the controlling legal text. Their actual words and intentions are excluded from the extinguishment reading's evidentiary basis, which privileges the English-language written instrument.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, treaty_text_drafters_historical, excluded,
    powerless, biographical, trapped, regional).

% Adjudicate disputes over treaty interpretation, choosing between the extinguishment reading and competing readings when litigating title, resource rights, and self-government claims. Their doctrinal choices determine which reading of the kernel carries legal force in a given era.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_state_land_administration).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides settler governments, industry, and non-Indigenous landholders a single, administrable, litigation-resistant answer to 'who holds jurisdiction here' — collapsing a contested relational history into a bright-line completed transaction that title registries, courts, and resource licensing regimes can operate on without re-litigating history in each instance.
% TRANSFER_FUNCTION: Moves territorial jurisdiction, resource rights, and the capacity for ongoing political self-determination from Indigenous nations to the settler state, in exchange for fixed reserve lands and annuity payments whose value was set once, at signing, and is not renegotiated as circumstances (population growth, resource discovery, inflation) change.
% ABSENT_VOICES: The Indigenous negotiators' oral testimony and the interpreters who mediated the signings are structurally excluded from the reading's evidentiary basis — the written English text, often drafted unilaterally and translated only partially, is treated as the complete and controlling record. Elders and oral historians who preserve contrary accounts are not parties the extinguishment reading's courts are obligated to hear as co-authoritative.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading were displaced by a stewardship or nation-to-nation reading, resource licenses issued on the strength of clear settler title would become contestable, land registries would need to accommodate concurrent or shared jurisdiction claims, and the entire administrative apparatus of provincial/state resource management over ceded territory would require renegotiation. This is precisely why the reading is defended so vigorously in litigation.
% FOUNDING_PROBLEM: Settler governments and settling populations needed a legally stable, administrable basis for asserting exclusive jurisdiction and issuing secure title over territory occupied by Indigenous nations, in order to support colonization, agricultural settlement, and resource extraction without perpetual contestation.
% FOUNDING_PROBLEM_CORROBORATION: The settler state and resource industries (both beneficiaries) attest the problem — securing stable, transferable title — remains live and treaty extinguishment remains the operative legal doctrine in most domestic case law. Independent corroboration from outside the beneficiary set comes from constitutional courts' own doctrinal histories (e.g. judicial acknowledgment in obiter that the 'completed sale' framing does not match the documented oral record of negotiations) and from historians and linguists who have examined interpreter records and found systematic divergence between the English text and the terms as translated to Indigenous signatories at the time.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.81 by interval end) because the reading forecloses renegotiation of jurisdiction as circumstances change — resource discoveries, population growth, and inflation on the ceded territory accrue entirely to the settler-state side of the ledger while the reserve/annuity consideration was fixed at signing and does not scale. Suppression is high and slightly declining (0.85 -> 0.76) reflecting an initial era of direct coercive enforcement (military presence, criminalization of off-reserve movement) gradually giving way to legal-doctrinal enforcement (case law precedent, statutory limitation periods) that requires less overt force but still forecloses the same outcomes. Theater ratio rises modestly (0.20 -> 0.42) as reconciliation commissions, land-claim tribunals, and consultation processes are layered onto the extinguishment framework without altering its jurisdictional conclusion — performative engagement increases even as the underlying doctrine holds.
 *
 * PERSPECTIVAL GAP:
 *   From the settler-state and resource-industry seats, the arrangement is a genuinely closed, legitimate transaction — Tangled Rope's coordination half is real (title certainty, administrable jurisdiction) and defensible on its own terms. From the Indigenous nations' territorial-jurisdiction seat, the identical structure is experienced as ongoing enforced extraction of a relationship that was never, by their own oral record, a sale. The engine computing divergent per-seat classifications from the same structural data is the intended output — it is not an error that the claimed_type (tangled_rope, reflecting the genuine coordination function for title administration) sits alongside metrics showing high extraction and suppression from the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler state land administration is the clearest structural beneficiary — it collects jurisdiction, tax base, and resource authority, and actively maintains the reading through litigation (d near the beneficiary end). Resource industries and non-Indigenous landholders benefit derivatively through secure title. Treaty beneficiary bands occupy a genuinely dual position: they receive real, administratively enforced consideration (reserves, annuities) that is not fictional, but accepting it under THIS reading forecloses the broader jurisdictional claim — hence beneficiary primary with payer secondary. Indigenous nations' territorial jurisdiction and future land claimants sit at the full-target end: trapped exit, civilizational time horizon, no capacity to renegotiate the foundational premise through the settler legal system as currently doctrinally constituted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settler governments needing administrable, litigation-resistant title) remains live by the settler state's own account, which is why founding_problem_status is authored 'live' rather than 'dead' — this is not a piton or an atrophied function. It is a functioning extraction structure whose coordination function (title certainty) is real and whose extraction (foreclosed jurisdictional renegotiation) is also real and ongoing, which is exactly the tangled_rope signature rather than either pure rope or pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    written_text_vs_oral_record_authority,
    'Should the controlling legal authority for what a treaty means be the English-language written instrument (as extinguishment doctrine holds) or the oral record of what was actually communicated to and understood by Indigenous signatories through interpreters at the time?',
    'Comparative linguistic and historical analysis of interpreter records, oral history testimony preserved by Indigenous communities, and cross-referencing against contemporaneous settler administrator correspondence describing what was represented to signatories versus what the final text states.',
    'If oral record is held controlling, the extinguishment reading''s foundational premise (a completed sale of sovereignty) collapses for treaties where the oral terms described shared stewardship rather than cession — shifting classification toward the stewardship_reading''s structure for those specific treaties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_text_vs_oral_record_authority, conceptual, 'Whether written text or oral negotiation record is the authoritative kernel content.').

omega_variable(
    consideration_adequacy_and_renegotiation,
    'Does the fixed, non-scaling nature of the reserve/annuity consideration (set once at signing, never renegotiated as ceded territory''s value grew through resource discovery and population growth) undermine the extinguishment reading''s claim that this was a fair, completed, arm''s-length property transaction?',
    'Economic historical analysis comparing the real (inflation- and resource-discovery-adjusted) value of treaty consideration received against the market value the ceded territory has generated since signing.',
    'A severe adequacy gap would support characterizing the transaction as unconscionable or coerced rather than a fair completed sale, undermining the extinguishment reading''s coordination-function claim and shifting the classification toward pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consideration_adequacy_and_renegotiation, empirical, 'Whether treaty consideration was adequate at the time and remains adequate as ceded territory''s value has grown.').

omega_variable(
    is_this_reading_naturalized_or_contested,
    'Is the extinguishment reading treated by the settler legal system as settled natural fact (a completed, closed transaction not open to re-examination) or as one live, contestable interpretation among several — and does that treatment itself vary by era and jurisdiction?',
    'Track the trajectory of doctrinal shifts in constitutional courts (e.g., movements toward duty-to-consult, honour-of-the-Crown, and reconciliation-framework jurisprudence) as evidence of whether the reading is hardening or softening over time.',
    'If courts increasingly treat the reading as one contestable interpretation subject to modern reconciliation principles rather than settled fact, its effective suppression and accessibility_collapse would be lower than authored here and trending further downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(is_this_reading_naturalized_or_contested, conceptual, 'Whether the extinguishment reading is treated as closed fact or as a contestable, evolving legal doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__extinguishment_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__extinguishment_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__extinguishment_reading, theater_ratio, 75, 0.34).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__extinguishment_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement(hist_tr_t125, historical_treaty_substrate__extinguishment_reading, theater_ratio, 125, 0.4).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__extinguishment_reading, theater_ratio, 150, 0.42).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 75, 0.75).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 100, 0.78).
narrative_ontology:measurement(hist_be_t125, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 125, 0.8).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 150, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 25, 0.83).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 100, 0.77).
narrative_ontology:measurement(hist_su_t125, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 125, 0.76).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 150, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% Part of a three-member constraint family reading the historical_treaty_substrate kernel: extinguishment_reading (this file — territorial sovereignty permanently ceded, high ε, tangled_rope), stewardship_reading (no cession occurred, co-sovereignty persists, expected low ε for the settler-exclusivity claim and high ε for the unilateral-jurisdiction assertion), and nation_to_nation_reading (ongoing consent-contingent relationship, expected structure closer to a contested/renegotiable rope with the settler state's unilateral assertions read as extractive departures from the nation-to-nation baseline). Each reading has its own beneficiary/victim structure and its own ε — this is not one constraint measured three ways, but three structurally distinct constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
