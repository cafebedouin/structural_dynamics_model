% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Treaties as Completed Property Transactions (Extinguishment Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This story authors ONLY the extinguishment reading of the historical
 *   treaty substrate: the doctrine that historical treaties between
 *   Indigenous nations and settler states constituted final, completed
 *   property transactions in which territorial sovereignty passed permanently
 *   to the settler state in exchange for defined reserve lands and fixed
 *   payments. Under this reading, Indigenous nations retain only the specific
 *   rights the treaty text enumerates (hunting, fishing, reserve occupancy),
 *   and all broader jurisdictional and sovereignty claims are extinguished.
 *   This is a distinct constraint from the nation_to_nation_reading (which
 *   treats the same historical instruments as ongoing agreements between
 *   sovereign equals requiring continuing consent) and the
 *   stewardship_reading (which reads them as relational coexistence pacts
 *   involving no cession at all). The three readings are not the same
 *   constraint measured differently — they have different beneficiary/victim
 *   structures, different ε, and different legal consequences. They are
 *   linked as siblings of the historical_treaty_substrate kernel via network
 *   edges, not merged here.
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
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Treaties as Completed Property Transactions (Extinguishment Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, 'edda0d49-6df1-4532-b929-34aadad80707').
narrative_ontology:cs_kernel_codification('edda0d49-6df1-4532-b929-34aadad80707', fixed_text).
narrative_ontology:cs_authority_grounding('edda0d49-6df1-4532-b929-34aadad80707', extraction).
narrative_ontology:cs_interpretation_layer_present('edda0d49-6df1-4532-b929-34aadad80707').
narrative_ontology:cs_reading_relation('edda0d49-6df1-4532-b929-34aadad80707', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('edda0d49-6df1-4532-b929-34aadad80707', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_axiom('edda0d49-6df1-4532-b929-34aadad80707', foundational, sovereignty_cession_was_total_and_final).
narrative_ontology:cs_axiom_status(sovereignty_cession_was_total_and_final, holdable).
narrative_ontology:cs_axiom_grounding('edda0d49-6df1-4532-b929-34aadad80707', sovereignty_cession_was_total_and_final, conventional).
narrative_ontology:cs_axiom('edda0d49-6df1-4532-b929-34aadad80707', foundational, treaty_rights_limited_to_enumerated_text).
narrative_ontology:cs_axiom_status(treaty_rights_limited_to_enumerated_text, holdable).
narrative_ontology:cs_axiom_grounding('edda0d49-6df1-4532-b929-34aadad80707', treaty_rights_limited_to_enumerated_text, conventional).
narrative_ontology:cs_axiom('edda0d49-6df1-4532-b929-34aadad80707', secondary, crown_underlying_title_precedes_and_survives_treaty).
narrative_ontology:cs_axiom_status(crown_underlying_title_precedes_and_survives_treaty, holdable).
narrative_ontology:cs_axiom_grounding('edda0d49-6df1-4532-b929-34aadad80707', crown_underlying_title_precedes_and_survives_treaty, conventional).
narrative_ontology:cs_reference_frame('edda0d49-6df1-4532-b929-34aadad80707', crown_underlying_title_at_signing).
narrative_ontology:cs_drift_state('edda0d49-6df1-4532-b929-34aadad80707', contemporary_duty_to_consult_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('edda0d49-6df1-4532-b929-34aadad80707', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_land_administration).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, resource_extraction_industries).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, non_indigenous_titleholders).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, non_signatory_indigenous_descendants).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, doctrine_of_discovery_successor_title).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, crown_underlying_title_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers land registries, resource permits, and jurisdiction on the basis that the historical treaties finally and completely transferred underlying territorial title to the state, leaving only the specific reserve lands and enumerated payments as outstanding obligations. Issues resource leases, adjudicates boundary disputes, and defends the extinguishment reading in litigation because reopening it would unsettle the entire land tenure system built atop it.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state_land_administration, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, settler_state_land_administration, beneficiary).

% Obtain mining, forestry, and energy leases over ceded territory on the legal premise that sovereignty was extinguished and only narrow, enumerated treaty rights (hunting, fishing on specified lands) survive. Lobby against expansive readings of treaty rights because a stewardship or nation-to-nation reading would require negotiated consent for every extraction project, not a permit from the state alone.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, resource_extraction_industries, beneficiary,
    powerful, generational, mobile, continental).

% Hold fee-simple title to former treaty territory, purchased or granted downstream of the state's assumption of underlying title. Their property security depends entirely on the extinguishment reading holding — a nation-to-nation or stewardship reading would cast doubt on the chain of title itself, which is why title insurers and settler landholder associations consistently intervene to defend this reading in court.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, non_indigenous_titleholders, beneficiary,
    organized, generational, arbitrage, national).

% Signed agreements their oral and documentary traditions describe as sharing, alliance, or coexistence arrangements, not sales. Under this reading, they lose enforceable jurisdiction over ceded lands and retain only the narrow rights the state chooses to recognize as textually enumerated. Litigation, treaty rights tribunals, and international forums are the only available channels, and all operate inside a legal system that presupposes the very extinguishment they are contesting — there is no exit to a neutral adjudicator outside the settler state's own courts.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations, payer,
    organized, civilizational, trapped, national).

% Descendants of nations whose ancestors negotiated the original agreements but who now live off-reserve or across territories reorganized by subsequent extinguishment-based land administration. They bear the downstream consequences of the extinguishment reading — reduced land base, restricted resource access — without having been party to, or benefiting materially from, the annuities and reserve allocations the reading treats as full consideration.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, non_signatory_indigenous_descendants, payer,
    powerless, generational, trapped, local).

% Adjudicate treaty rights litigation and must choose, case by case, between competing readings of the same historical texts. Their doctrine has partially eroded the pure extinguishment reading (recognizing 'living tree' interpretation and duty to consult) without fully repudiating it, producing a body of case law that is internally inconsistent about whether cession was total or partial.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% UN mechanisms and international law scholars applying UNDRIP and self-determination frameworks would characterize the extinguishment reading as inconsistent with contemporary indigenous rights norms, but their findings are advisory and carry no binding force inside domestic property and constitutional law, so their voice does not enter the operative legal record.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_human_rights_bodies, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, final, administrable answer to 'who holds underlying title' so that land registries, resource permitting, and property markets can function without perpetually relitigating sovereignty — a real coordination need for any functioning land-tenure system.
% TRANSFER_FUNCTION: Moves de facto and de jure control of vast territories from Indigenous nations to the settler state and its licensees, in exchange for defined reserve lands and fixed annuity payments whose real value has been eroded by inflation and non-renegotiation over a century or more.
% ABSENT_VOICES: International human rights bodies and Indigenous legal traditions that never accepted the transactional framing are structurally outside the domestic courts and land registries where this reading is operative; their objections are documented in scholarship and UN reporting but do not bind the property system built on this reading.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading were displaced by a nation-to-nation or stewardship reading, the entire chain of downstream title, resource leases, and jurisdictional boundaries built atop 'the Crown holds underlying title' would require renegotiation — this is precisely why settler-state land administration and title-dependent industries treat the reading as non-negotiable rather than as one interpretation among several.
% FOUNDING_PROBLEM: European and settler-state legal systems needed a doctrinally coherent basis to claim sovereign and proprietary authority over territories already occupied and governed by Indigenous nations, without which colonization could not be reconciled with the settlers' own legal and moral self-conception.
% FOUNDING_PROBLEM_CORROBORATION: Settler-state courts and land registries attest the founding problem was resolved definitively at treaty signing. Independent historians, Indigenous legal scholars, and international human rights bodies — sources outside the beneficiary set — attest that the founding problem (establishing a legitimate basis for territorial authority) remains unresolved, pointing to documented mismatches between treaty text, oral treaty accounts, and subsequent unilateral state assertions of underlying title never actually negotiated in the original agreements.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.81) and has risen over the interval as the value of ceded resources and land far outstripped fixed annuity payments never renegotiated for inflation, resource discovery, or population growth. Suppression starts very high (0.85) reflecting the coercive conditions of original treaty-making (military pressure, starvation policy, unequal bargaining power) and gradually eases as direct coercion is replaced by embedded legal and administrative machinery — then ticks back up in recent decades (0.76) as states litigate more aggressively to defend the reading against emerging duty-to-consult jurisprudence. Theater ratio rises over time (0.10 to 0.42) as more of the enforcement apparatus becomes about defending the DOCTRINE (title insurance, consultation-lite processes) rather than performing the original coordination function of settling land claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Settler-state land administration and resource industries are structural beneficiaries — the extinguishment reading is what makes their title and permits legally secure, so they get low d. Indigenous treaty nations are the clearest targets: trapped exit options (no neutral forum outside the very legal system whose doctrine they contest), high d. Non-signatory descendants bear diffuse downstream costs without direct treaty consideration, placing them near the target end despite never having a seat at the original negotiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The extinguishment reading's founding-problem status is authored as contested rather than dead or live: settler courts treat the sovereignty question as settled precedent (a live doctrine, not an obsolete one from their seat), while independent historians and Indigenous nations argue the underlying legitimacy problem the doctrine was meant to paper over was never actually resolved — it was suppressed. This divergence is exactly the seat-relative classification the engine is built to surface: from the agenda_setter seat this looks like completed, functioning coordination (a real title system that lets land markets operate); from the payer seat it looks like ongoing extraction dressed as history.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cession_versus_relationship_textual_ambiguity,
    'Do the original treaty texts and their contemporaneous oral counterparts actually support a reading of complete, permanent cession of sovereignty, or does that reading depend on later unilateral settler-state legal doctrine (doctrine of discovery, terra nullius successor doctrines) layered onto ambiguous or mistranslated instruments?',
    'Comparative textual and oral-history analysis of the treaty negotiations, including surviving Indigenous oral accounts, interpreter records, and the specific language used in the original (often non-English) negotiation versus the settler-drafted written instrument.',
    'If the cession reading is a later doctrinal imposition rather than the actual negotiated agreement, the extinguishment reading is not merely one interpretation among three but a constructed reading whose legitimacy is substantially weaker than its operative legal dominance suggests — this would not change this story''s ε (which is authored as the reading''s own internal structure) but would sharpen the omega documented across all three sibling stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cession_versus_relationship_textual_ambiguity, empirical, 'Whether the extinguishment reading reflects the actual negotiated agreement or a later doctrinal overlay.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Given that three structurally distinct readings of the same treaty texts coexist and produce different beneficiary/victim structures, what mechanism determines which reading operates as binding law in any given jurisdiction — legal precedent, political power, or ongoing negotiation?',
    'Track which reading is operative in each jurisdiction''s highest court doctrine over time, and correlate shifts (e.g., toward duty-to-consult frameworks) with changes in relative bargaining power, international law pressure, and Indigenous political mobilization.',
    'If reading selection tracks power rather than textual or historical fidelity, this substantiates that the extinguishment reading''s continued dominance is itself an artifact of the asymmetric enforcement power documented in this story''s stakeholders, not a neutral interpretive outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'What determines which kernel reading operates as binding law.').

omega_variable(
    annuity_value_erosion_measurement,
    'What is the actual present-value gap between the fixed historical annuities/reserve allocations and the market value of the resources and territory ceded, adjusted for the resource revenue actually extracted over the treaty period?',
    'Economic historical analysis comparing (a) cumulative resource revenue extracted from ceded territories since treaty signing, against (b) cumulative real value of annuity payments and reserve land value, adjusted for inflation and opportunity cost.',
    'A wide gap would quantify the extraction claim directly and could support renegotiation or compensation claims; a narrow gap would support the extinguishment reading''s ''completed fair transaction'' framing on its own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(annuity_value_erosion_measurement, empirical, 'Quantifying the value gap between treaty consideration and extracted resource value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__extinguishment_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__extinguishment_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__extinguishment_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__extinguishment_reading, theater_ratio, 100, 0.36).
narrative_ontology:measurement(hist_tr_t125, historical_treaty_substrate__extinguishment_reading, theater_ratio, 125, 0.4).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__extinguishment_reading, theater_ratio, 150, 0.42).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 50, 0.74).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 75, 0.78).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(hist_be_t125, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 125, 0.81).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 150, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 25, 0.83).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(hist_su_t125, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 125, 0.72).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 150, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, stewardship_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the historical_treaty_substrate kernel. extinguishment_reading (this story) treats the treaties as completed property transactions with full sovereignty cession; nation_to_nation_reading treats them as ongoing sovereign-to-sovereign agreements requiring continuing consent under modern treaty law; stewardship_reading treats them as relational coexistence pacts with no cession at all. Each sibling has a distinct beneficiary/victim structure and distinct ε — they are linked here via network edges rather than merged into one constraint, per the ε-invariance principle and the kernel/reading authoring rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
