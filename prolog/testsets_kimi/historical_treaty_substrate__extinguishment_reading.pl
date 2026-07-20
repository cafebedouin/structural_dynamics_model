% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Treaties as Completed Property Transactions (Extinguishment Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   The extinguishment reading interprets historical treaties between
 *   Indigenous nations and settler states as completed property transactions:
 *   Indigenous parties ceded territorial sovereignty in exchange for defined
 *   reserves and payments. This reading is a contested kernel reading
 *   (historical_treaty_substrate); siblings (stewardship, nation-to-nation)
 *   produce radically different structural outcomes. Under this reading, the
 *   settler state becomes the sole legitimate territorial authority, while
 *   Indigenous nations are repositioned as beneficiaries of narrow treaty
 *   rights rather than sovereigns. The constraint coordinates a final
 *   settlement of title but extracts territorial sovereignty asymmetrically.
 *   Active enforcement through courts and land registries persists, though
 *   overt military coercion has declined.
 *
 * KEY AGENTS:
 *   - settler_state (agenda_setter, institutional power, arbitrage exit)
 *   - indigenous_treaty_nations (payer/beneficiary hybrid, organized power, constrained exit)
 *   - excluded_alternative_reading_advocates (excluded from domestic framework)
 *   - international_human_rights_observers (analytical observer)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.72).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.55).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Treaties as Completed Property Transactions (Extinguishment Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '8874f985-fa07-4bb3-8c0d-50669c530957').
narrative_ontology:cs_kernel_codification('8874f985-fa07-4bb3-8c0d-50669c530957', fixed_text).
narrative_ontology:cs_authority_grounding('8874f985-fa07-4bb3-8c0d-50669c530957', lineage).
narrative_ontology:cs_interpretation_layer_present('8874f985-fa07-4bb3-8c0d-50669c530957').
narrative_ontology:cs_reading_relation('8874f985-fa07-4bb3-8c0d-50669c530957', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('8874f985-fa07-4bb3-8c0d-50669c530957', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_axiom('8874f985-fa07-4bb3-8c0d-50669c530957', foundational, permanent_cession_doctrine).
narrative_ontology:cs_axiom_status(permanent_cession_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('8874f985-fa07-4bb3-8c0d-50669c530957', permanent_cession_doctrine, conventional).
narrative_ontology:cs_axiom('8874f985-fa07-4bb3-8c0d-50669c530957', secondary, narrow_benefits_satisfy_exchange).
narrative_ontology:cs_axiom_status(narrow_benefits_satisfy_exchange, holdable).
narrative_ontology:cs_axiom_grounding('8874f985-fa07-4bb3-8c0d-50669c530957', narrow_benefits_satisfy_exchange, conventional).
narrative_ontology:cs_reference_frame('8874f985-fa07-4bb3-8c0d-50669c530957', extinguished_sovereignty_framework).
narrative_ontology:cs_drift_state('8874f985-fa07-4bb3-8c0d-50669c530957', contemporary_reconciliation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8874f985-fa07-4bb3-8c0d-50669c530957', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and enforces the legal framework that treaties were final property transactions extinguishing Indigenous title. Operates courts, land registries, and resource licensing under this doctrine. Could in principle adopt alternative treaty readings but retains this one because it secures territorial jurisdiction, resource wealth, and constitutional stability.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state, agenda_setter,
    institutional, generational, arbitrage, continental).

% Their ancestors entered agreements now interpreted as complete territorial cessions. They receive defined treaty benefitsâreserve lands, annuities, narrow rightsâwhile their broader territorial sovereignty is treated as permanently extinguished. Their exit is constrained by the legal monopoly of settler courts and the absence of recognized ongoing sovereign jurisdiction; they can litigate narrow treaty terms but not the underlying title.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations, payer,
    organized, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations, beneficiary).

% Advance stewardship or nation-to-nation treaty interpretations. These readings are structurally excluded from operative domestic law; they surface in dissent, academic criticism, Indigenous governance practices, and international petitions but are not enforceable within the extinguishment framework.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, excluded_alternative_reading_advocates, excluded,
    moderate, generational, constrained, national).

% Monitor and report on gaps between the extinguishment reading and international human rights standards, including UNDRIP. Provide external analytical pressure but lack direct enforcement authority over domestic constitutional interpretation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces contested Indigenous territorial occupancy with a definitive, once-and-for-all property transfer, establishing clear settler-state title and bounded Indigenous reserves in a single structured transaction.
% TRANSFER_FUNCTION: Moves territorial sovereignty and comprehensive jurisdictional authority from Indigenous nations to the settler state; moves reserve land allocations, annuity payments, and narrow usufructuary rights from the settler state to Indigenous signatories.
% ABSENT_VOICES: Indigenous nations and scholars advancing stewardship or nation-to-nation readings are structurally excluded from the operative domestic legal framework; their positions appear only in dissent, international forums, or extra-legal Indigenous governance, not in enforceable doctrine.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading vanished, the settler state's claimed radical title to vast territories would revert to contested or unceded status, reserve boundaries would become provisional rather than settled, and the entire domestic land-title edifice would require reconstruction on a different constitutional foundation.
% FOUNDING_PROBLEM: Colonial territorial expansion required legal legitimacy and an alternative to perpetual military conflict; the extinguishment reading provided a peaceable doctrinal mechanism to acquire sovereign title while offering Indigenous parties defined, limited compensatory benefits.
% FOUNDING_PROBLEM_CORROBORATION: Settler-state legal historians attest the problem was orderly title acquisition. Indigenous historians and international human rights bodies attest the problem was colonial dispossession and that the arrangement persists as structural dominance; no corroboration exists from a seat outside the disputeâaccounts are entirely seat-dependent.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because the framework enabled the taking of vast territories under color of legal exchange. Suppression is moderate (0.55) because while overt military enforcement has declined, the legal framework actively excludes alternative readings through constitutional doctrine and court rulings. Theater ratio (0.42) reflects the persistent legal ritual of 'solemn agreements' and 'benefits' that obscures the asymmetry of the exchange. Accessibility collapse is very high (0.80) because once the reading is embedded in constitutional law, Indigenous claimants have almost no domestic institutional path to assert territorial sovereignty. Resistance (0.55) reflects sustained Indigenous legal and political mobilization. The measurement series track the hardening of the doctrine from early ambiguity (t=0) through peak extractiveness and theater around t=100, with modest decline in overt suppression in recent decades as enforcement shifted to legal-bureaucratic normalization.
 *
 * PERSPECTIVAL GAP:
 *   The settler-state seat experiences the constraint as foundational coordination that legitimized its territorial existence and ended conflict. The Indigenous treaty nations seat experiences the same structure as asymmetric extraction where sovereignty was taken and only narrow rights returned. The engine computes this divergence from the structural data: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler state is the structural beneficiary of territorial authority (low d, subsidized by the constraint). Indigenous treaty nations bear the cost of sovereignty loss (high d) while receiving narrow treaty benefits that partially offset the extraction, producing a hybrid directionality. The excluded advocacy seats are not directly governed by the constraint's benefits but are suppressed by its enforcement (high d, excluded from coordination). International observers sit at analytical distance (analytical exit, neutral d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcolonial territorial legitimacyâwas substantially resolved by the mid-twentieth century as settler states consolidated control. Yet the arrangement persists because it underpins the entire land-title system. The R5 status is dead paired with disappearance_verdict world_rearranges, flagging a mandatrophy risk. However, the constraint is not a piton because active enforcement remains robust and the settler state continues to capture massive gains from the reading; it is a living tangled rope with inertial mass, not merely theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinguishment_historical_fiction,
    'Is the extinguishment reading a historically accurate description of treaty negotiations, or a retroactive judicial fiction imposed by settler courts decades later?',
    'Archival and ethnohistorical analysis of treaty negotiation records, wampum belts, and contemporaneous accounts from both parties.',
    'If a retroactive fiction, the constraint''s legitimacy collapses and its extraction component dominates; reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinguishment_historical_fiction, empirical, 'Whether extinguishment reflects historical intent or imposed doctrine.').

omega_variable(
    sovereignty_cession_capacity,
    'Did Indigenous nations possess, under their own legal frameworks, the constitutional capacity to permanently alienate territorial sovereignty?',
    'Comparative Indigenous legal scholarship and international law review on inalienability of communal territory.',
    'If sovereignty was inalienable, the ''exchange'' was structurally impossible and the reading masks unilateral seizure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_cession_capacity, conceptual, 'Capacity to cede sovereignty under Indigenous and international law.').

omega_variable(
    extinguishment_kernel_reading_ambiguity,
    'How would classification change if the stewardship or nation-to-nation reading supplanted the extinguishment reading as operative law?',
    'Jurisdictional experiments or constitutional recognition of alternative readings.',
    'Would reclassify Indigenous nations from payer-victims to symmetric partners or co-beneficiaries, collapsing extractiveness and suppression metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extinguishment_kernel_reading_ambiguity, conceptual, 'This constraint is one reading of historical_treaty_substrate; siblings produce divergent directionalities.').

omega_variable(
    enforcement_decay_or_transformation,
    'Has enforcement decayed, or has it transformed from military coercion to legal-bureaucratic normalization?',
    'Track enforcement modality shifts through the measurement series and qualitative institutional analysis.',
    'If transformed rather than decayed, suppression remains high but appears lower because it is internalized in legal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_or_transformation, empirical, 'Nature of enforcement change over the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__extinguishment_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__extinguishment_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__extinguishment_reading, theater_ratio, 75, 0.55).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__extinguishment_reading, theater_ratio, 100, 0.6).
narrative_ontology:measurement(hist_tr_t125, historical_treaty_substrate__extinguishment_reading, theater_ratio, 125, 0.55).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__extinguishment_reading, theater_ratio, 150, 0.5).
narrative_ontology:measurement(hist_tr_t175, historical_treaty_substrate__extinguishment_reading, theater_ratio, 175, 0.45).
narrative_ontology:measurement(hist_tr_t200, historical_treaty_substrate__extinguishment_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 75, 0.78).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 100, 0.85).
narrative_ontology:measurement(hist_be_t125, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 125, 0.82).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 150, 0.78).
narrative_ontology:measurement(hist_be_t175, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 175, 0.75).
narrative_ontology:measurement(hist_be_t200, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 200, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 75, 0.9).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 100, 0.85).
narrative_ontology:measurement(hist_su_t125, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 125, 0.7).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 150, 0.6).
narrative_ontology:measurement(hist_su_t175, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 175, 0.55).
narrative_ontology:measurement(hist_su_t200, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 200, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
