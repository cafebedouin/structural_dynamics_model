% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Historical Treaty Substrate — Extinguishment Reading (Completed Property Transaction)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This story authors the extinguishment reading of the historical treaty
 *   substrate kernel: treaties are understood as completed, final property
 *   transactions in which Indigenous nations ceded territorial sovereignty in
 *   exchange for a discharged, closed set of obligations (reserve lands,
 *   annuities, narrow harvesting rights). This is the doctrine that
 *   underwrote settler-state land registries, resource licensing regimes, and
 *   taxation authority across ceded territories for roughly a century and a
 *   half, and that many jurisdictions' courts have only partially moved away
 *   from since the late twentieth century (e.g., duty-to-consult
 *   jurisprudence, honour-of-the-Crown doctrine). Under this reading
 *   Indigenous nations exit the sovereignty/jurisdiction victim set in one
 *   narrow sense — they are compensated beneficiaries of the specific treaty
 *   terms — but remain the payer of the reading's central transfer: the loss
 *   of territorial jurisdiction over the vastly larger ceded area, for
 *   compensation fixed at the moment of signing and administratively frozen
 *   thereafter. Two sibling constraints exist for the same underlying kernel
 *   and are NOT part of this file: the nation-to-nation reading (treaties as
 *   ongoing agreements between sovereign equals subject to modern treaty-law
 *   renegotiation) and the stewardship reading (treaties as relational pacts
 *   involving no cession of sovereignty at all, only shared stewardship
 *   obligations). Each sibling has its own ε, its own beneficiary/victim
 *   structure, and its own classification — see network.affects_constraints.
 *
 * KEY AGENTS:
 *   - settler_state_governments: primary agenda_setter and structural beneficiary — administers the extinguishment doctrine and collects near-total jurisdictional authority and resource rents over ceded territory
 *   - resource_extraction_industries: secondary beneficiary with mobile exit — depends on the extinguishment reading remaining dominant for licensing certainty
 *   - indigenous_nations_ceded_territory: primary payer, trapped exit — bears the loss of territorial jurisdiction the reading treats as permanently and completely transferred
 *   - treaty_annuitant_bands: dual-positioned — genuine beneficiary of the narrow enumerated treaty rights while simultaneously bearing the reading's core cost (jurisdictional loss)
 *   - non_signatory_indigenous_descendants and future_indigenous_generations: powerless, trapped payers who never had standing to consent to a transaction the reading treats as final
 *   - constitutional_courts: analytical observer seat, the actual site where this reading contests the sibling readings case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.81).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.87).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaty Substrate — Extinguishment Reading (Completed Property Transaction)").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '65e05604-5c45-4773-b228-71f1598f9840').
narrative_ontology:cs_kernel_codification('65e05604-5c45-4773-b228-71f1598f9840', fixed_text).
narrative_ontology:cs_authority_grounding('65e05604-5c45-4773-b228-71f1598f9840', lineage).
narrative_ontology:cs_interpretation_layer_present('65e05604-5c45-4773-b228-71f1598f9840').
narrative_ontology:cs_reading_relation('65e05604-5c45-4773-b228-71f1598f9840', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('65e05604-5c45-4773-b228-71f1598f9840', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('65e05604-5c45-4773-b228-71f1598f9840', foundational, sovereignty_cession_complete_at_signing).
narrative_ontology:cs_axiom_status(sovereignty_cession_complete_at_signing, holdable).
narrative_ontology:cs_axiom_grounding('65e05604-5c45-4773-b228-71f1598f9840', sovereignty_cession_complete_at_signing, conventional).
narrative_ontology:cs_axiom('65e05604-5c45-4773-b228-71f1598f9840', secondary, enumerated_treaty_terms_discharge_all_state_obligation).
narrative_ontology:cs_axiom_status(enumerated_treaty_terms_discharge_all_state_obligation, holdable).
narrative_ontology:cs_axiom_grounding('65e05604-5c45-4773-b228-71f1598f9840', enumerated_treaty_terms_discharge_all_state_obligation, conventional).
narrative_ontology:cs_reference_frame('65e05604-5c45-4773-b228-71f1598f9840', sovereign_cession_at_signing).
narrative_ontology:cs_drift_state('65e05604-5c45-4773-b228-71f1598f9840', post_duty_to_consult_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('65e05604-5c45-4773-b228-71f1598f9840', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, resource_extraction_industries).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, non_indigenous_landholders).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, treaty_annuitant_bands).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations_ceded_territory).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, non_signatory_indigenous_descendants).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, future_indigenous_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, treaty_annuitant_bands).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, doctrine_of_discovery_derived_title).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, settler_state_territorial_sovereignty).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, treaty_as_final_settlement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the treaty as a closed, one-time conveyance: sovereignty and underlying title passed at signing; the state's obligations are limited to the specific enumerated promises (reserve lands, annuity payments, hunting/fishing rights in narrow form). Courts, land registries, and resource licensing regimes are built on this reading. The state sets legislative policy on treaty interpretation, litigates to preserve the extinguishment frame, and collects the near-totality of taxation, resource royalties, and jurisdictional authority over the ceded territory.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state_governments, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, settler_state_governments, beneficiary).

% Obtains mineral, timber, and energy rights over ceded territory through state-issued licenses that presuppose the state holds clear underlying title. Depends entirely on the extinguishment reading remaining legally dominant — a shift to the nation-to-nation or stewardship reading would reopen consent and consultation requirements industry currently treats as settled. Can relocate capital to other jurisdictions if the legal foundation becomes unstable, giving it functional exit unavailable to the other seats.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, resource_extraction_industries, beneficiary,
    powerful, generational, mobile, national).

% Holds fee-simple title to land within the ceded territory, purchased or inherited on the assumption that Indigenous title was fully extinguished at treaty signing. Their property security depends on the extinguishment reading holding; a successful reassertion of unceded jurisdiction would create legal uncertainty over their own tenure, though most would not lose land outright under any realistic remedy.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, non_indigenous_landholders, beneficiary,
    moderate, generational, constrained, regional).

% Receives the enumerated treaty benefits — reserve lands, annuity payments (often nominal and fixed at historical rates never adjusted for inflation), and narrowly defined harvesting rights. These benefits are real and administratively significant to daily life, which is why this reading places these bands in the beneficiary set for the narrow rights bargain. At the same time they bear the extinguishment reading's core cost: the loss of jurisdiction over the vastly larger ceded territory, for compensation frozen at terms set generations ago and never renegotiated at the ceding party's initiative.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, treaty_annuitant_bands, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, treaty_annuitant_bands, payer).

% Under this reading, the nation's sovereignty and underlying territorial jurisdiction were permanently and completely transferred to the settler state at the moment of signing, in exchange for the treaty's fixed terms. Any claim to broader land use, resource revenue sharing, or governance authority over the ceded territory beyond what the treaty text enumerates is treated as legally foreclosed. Exit is not available: the nation cannot relitigate the underlying cession without overturning the doctrinal architecture (Crown sovereignty, doctrine of discovery-derived title) that every other institution in the jurisdiction depends on.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations_ceded_territory, payer,
    organized, civilizational, trapped, national).

% Descendants of individuals who were not present, not consulted, or dissenting at the original signing (in many historical treaties, signatories represented only a subset of the affected people, or signed under conditions of famine, disease, or military pressure). Under the extinguishment reading their ancestors' territorial rights were extinguished regardless of the adequacy of consent at signing. They have no separate legal standing to contest the transaction's validity distinct from the band's official position.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, non_signatory_indigenous_descendants, payer,
    powerless, generational, trapped, regional).

% Inherit a jurisdictional settlement negotiated without their consent and, under this reading, permanently binding regardless of changed circumstances (population growth, discovery of resource wealth, evolving human rights norms). Their capacity to ever renegotiate territorial jurisdiction is foreclosed by the doctrine that the original transaction was final and complete.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, future_indigenous_generations, payer,
    powerless, civilizational, trapped, national).

% Adjudicates disputes over treaty interpretation, choosing among the extinguishment, nation-to-nation, and stewardship readings case by case. Some jurisdictions have moved doctrine away from strict extinguishment toward duty-to-consult and honour-of-the-Crown frameworks, creating live tension between this reading and its siblings within the same court system.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively stable answer to 'who holds jurisdiction here' — land registries, resource licensing, taxation, and law enforcement all require a determinate answer to operate, and the extinguishment reading supplies one without requiring case-by-case renegotiation of authority.
% TRANSFER_FUNCTION: Moves territorial jurisdiction, resource rents, and governmental authority from Indigenous nations to the settler state, in exchange for fixed, generally non-indexed compensation (reserve lands and annuities) set at the moment of signing and treated as discharging the state's obligations in full thereafter.
% ABSENT_VOICES: Non-signatory descendants and future generations were structurally unable to consent to a transaction framed as final and complete; contemporary Indigenous governance bodies calling for treaty renegotiation or nation-to-nation renewal are treated by courts operating under this reading as raising a political rather than legal claim, kept outside the adjudicative frame this reading recognizes as legitimate.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading were abandoned as the operative legal doctrine, resource licensing regimes, land registries, and provincial/state taxation authority over vast ceded territories would require renegotiation or reopening; the settler state's claim to sole territorial jurisdiction over treaty lands would no longer rest on settled doctrine, triggering the kind of consultation, revenue-sharing, and co-governance disputes the nation-to-nation and stewardship readings anticipate.
% FOUNDING_PROBLEM: European and settler-colonial states needed a legally cognizable mechanism to establish exclusive, transferable title to Indigenous-occupied land in order to support settlement, resource extraction, and the internal coherence of state sovereignty claims recognized by other states.
% FOUNDING_PROBLEM_CORROBORATION: Settler state legal institutions and resource industries attest the founding problem (need for clear, final title) remains live and correctly solved. Independent historians, comparative law scholars, and international human rights bodies (e.g., UN Declaration on the Rights of Indigenous Peoples commentary, truth and reconciliation commission findings in multiple jurisdictions) attest from outside the beneficiary set that the 'completed transaction' framing does not match the documented understanding of many original Indigenous signatories, who by treaty-council oral records and settler negotiator correspondence often understood the agreements as ongoing relationships of shared land use rather than final cession — corroboration for the founding-problem's contested status comes from parties with no stake in either reading's victory.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.81) because under this reading's own terms the transfer is total and irreversible: territorial jurisdiction over the ceded area passes permanently in exchange for compensation that was never renegotiated at the ceding party's initiative and in most jurisdictions was never indexed to inflation, population growth, or resource value discovered after signing. Suppression is authored even higher (0.87) because the reading's persistence depends on active legal and administrative enforcement — court doctrine, land registry law, and licensing regimes that treat any claim to broader jurisdiction as foreclosed rather than negotiable; this is not passive settlement but continuously defended doctrine. Theater ratio is moderate and rising (0.15 to 0.42 over the interval) reflecting the growing gap between the extinguishment doctrine's formal persistence in property and resource law and its substantive erosion in constitutional jurisprudence (duty-to-consult, honour-of-the-Crown, and comparable doctrines increasingly treat treaty obligations as ongoing rather than discharged) — much of the doctrine's current maintenance is defensive/performative rather than reflecting genuine doctrinal consensus. All temporal metrics share one time grid (1850, 1900, 1950, 1982, 2000, 2025) per the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   From the settler_state_governments seat, this reading is a closed transaction correctly administered: sovereignty passed, compensation was paid, the matter is settled. From the indigenous_nations_ceded_territory seat, the same structure is an ongoing, actively enforced extraction of jurisdiction whose 'completion' is precisely what is contested. The engine should compute these as different seat-level classifications from the same structural data, not reconcile them — that divergence is what a kernel-reading decomposition is for.
 *
 * DIRECTIONALITY LOGIC:
 *   settler_state_governments and resource_extraction_industries sit near the full-beneficiary end: they collect jurisdiction, resource rents, and licensing certainty respectively, and industry additionally holds mobile exit. treaty_annuitant_bands are dual-positioned by structural necessity — the reading genuinely allocates them real, if narrow, benefits (this is why the expected structural delta places bands in the beneficiary set for enumerated rights) while the same reading fixes their jurisdictional loss as final; the engine should compute a directionality nearer target than beneficiary given the asymmetry in what is exchanged. indigenous_nations_ceded_territory, non_signatory_indigenous_descendants, and future_indigenous_generations sit at or near the full-target end: they are trapped (the doctrinal architecture forecloses exit) and bear an extraction whose scope is national and whose time horizon is civilizational — precisely the profile the directionality derivation should amplify toward full target.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) is deliberate: the treaty relationship did solve a genuine coordination problem for the state system (establishing determinate title enabling settlement, taxation, and resource administration) and the enumerated treaty benefits are real, not fictional. But the same structure that solves that coordination problem simultaneously extracts territorial jurisdiction from the ceding nations at a scale and permanence the compensation does not track, and it requires continuous active enforcement (litigation, doctrine defense) to hold against contrary readings the sibling constraints represent. Reading this as a pure snare would erase the genuine, real benefits enumerated treaty rights provide; reading it as a pure rope would erase the asymmetric, coercively defended jurisdictional transfer. The tangled_rope classification is what prevents either mislabeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Which of the three readings of the historical treaty substrate — extinguishment, nation-to-nation, or stewardship — best captures the original signatories'' understanding and the doctrinally correct legal characterization today?',
    'Comparative analysis of treaty-council oral records, settler negotiator correspondence, and evolving constitutional jurisprudence (duty-to-consult, honour-of-the-Crown, modern treaty law) across jurisdictions; no single resolution mechanism can adjudicate this because the three readings rest on different foundational premises about what sovereignty transfer even means in a cross-cultural negotiation context.',
    'If the nation-to-nation or stewardship reading is judicially or politically vindicated as the operative doctrine, the extinguishment reading''s beneficiary/victim structure inverts substantially: settler state jurisdiction over ceded territory would require renegotiated consent, and current resource licensing built on extinguishment doctrine would face legal exposure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which of three structurally distinct readings of the treaty kernel is doctrinally and historically correct — routed here per Rule 2 rather than folded into this reading''s classification.').

omega_variable(
    annuity_indexation_omission,
    'Was the failure to index treaty annuities and payments to inflation, population growth, or discovered resource value a deliberate design feature of the extinguishment doctrine, or an administrative oversight correctable within the reading''s own terms?',
    'Legislative and administrative history review of annuity-setting processes and subsequent renegotiation refusals across signatory jurisdictions.',
    'If deliberate, it strengthens the case that this reading functions as sustained extraction rather than a one-time settled bargain; if administrative oversight, it suggests the extraction is more contingent and potentially correctable without abandoning the extinguishment frame itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annuity_indexation_omission, empirical, 'Whether frozen compensation terms are structural to the extinguishment doctrine or a fixable administrative defect.').

omega_variable(
    consent_adequacy_at_signing,
    'To what extent did documented conditions at treaty signing (famine, disease pressure, military asymmetry, translation failures, partial band representation) undermine the validity of consent the extinguishment reading treats as having been freely given?',
    'Historical record review by independent historians and truth-and-reconciliation-style commissions using primary sources from multiple sides of the negotiations.',
    'Severe consent-adequacy problems would undermine the extinguishment reading''s foundational premise that a valid, final property transaction occurred at all, strengthening grounds for the nation-to-nation or stewardship readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_adequacy_at_signing, empirical, 'Whether the original transaction meets the consent standard the property-transaction framing presupposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 1850, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1850, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement_basis(hist_tr_t1850, observed).
narrative_ontology:measurement(hist_tr_t1900, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement_basis(hist_tr_t1900, observed).
narrative_ontology:measurement(hist_tr_t1950, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement_basis(hist_tr_t1950, observed).
narrative_ontology:measurement(hist_tr_t1982, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1982, 0.35).
narrative_ontology:measurement_basis(hist_tr_t1982, observed).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement_basis(hist_tr_t2000, observed).
narrative_ontology:measurement(hist_tr_t2025, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(hist_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t1850, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement_basis(hist_be_t1850, observed).
narrative_ontology:measurement(hist_be_t1900, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1900, 0.78).
narrative_ontology:measurement_basis(hist_be_t1900, observed).
narrative_ontology:measurement(hist_be_t1950, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1950, 0.83).
narrative_ontology:measurement_basis(hist_be_t1950, observed).
narrative_ontology:measurement(hist_be_t1982, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1982, 0.8).
narrative_ontology:measurement_basis(hist_be_t1982, observed).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement_basis(hist_be_t2000, observed).
narrative_ontology:measurement(hist_be_t2025, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2025, 0.81).
narrative_ontology:measurement_basis(hist_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1850, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1850, 0.9).
narrative_ontology:measurement_basis(hist_su_t1850, observed).
narrative_ontology:measurement(hist_su_t1900, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1900, 0.93).
narrative_ontology:measurement_basis(hist_su_t1900, observed).
narrative_ontology:measurement(hist_su_t1950, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1950, 0.88).
narrative_ontology:measurement_basis(hist_su_t1950, observed).
narrative_ontology:measurement(hist_su_t1982, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1982, 0.82).
narrative_ontology:measurement_basis(hist_su_t1982, observed).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement_basis(hist_su_t2000, observed).
narrative_ontology:measurement(hist_su_t2025, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2025, 0.87).
narrative_ontology:measurement_basis(hist_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, stewardship_reading).

% DUAL FORMULATION NOTE:
% This file is one of three sibling constraint stories decomposing the natural-language concept 'the historical treaty substrate' per the ε-invariance principle: extinguishment_reading (this file, tangled_rope, ε=0.81), nation_to_nation_reading (ongoing sovereign-equals framing, expected lower ε for the state's extraction and a different beneficiary/victim allocation), and stewardship_reading (relational-pact framing, expected to classify closer to a contested tangled_rope or snare depending on breach patterns, with sovereignty never ceded at all). These are not the same constraint measured three ways — each reading produces a structurally distinct claim about what was transferred, to whom, and under what terms, with different ε values, different victim sets, and different classifications. Linked here per the network decomposition rule; each sibling file documents the same relationship in its own commentary.narrative_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
