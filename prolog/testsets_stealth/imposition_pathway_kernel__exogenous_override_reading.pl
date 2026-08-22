% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: Meiji Decree Displacement of Calendar and Dress Commitments (Exogenous Override Reading)
 *   domain: historical sociology/state formation/commitment systems
 *
 * SUMMARY:
 *   Between the 1872 calendar edict and the turn of the century, the Meiji
 *   state displaced entrenched social commitments — lunar-solar timekeeping,
 *   status-coded dress — by decree backed by prefectural enforcement, rather
 *   than waiting for organic adoption. This story instantiates ONE reading of
 *   the imposition_pathway_kernel: the exogenous_override_reading, which
 *   holds that state capacity substituted for the fringe-adoption pathway
 *   entirely — there was no meaningful pre-decree adoption constituency for
 *   the calendar or official dress, the decree created the commitment through
 *   enforcement, and compliance was coerced before it was customary. The
 *   claim/metric gap is deliberate: the reading CLAIMS tangled_rope while the
 *   authored metrics describe the arrangement's actual operation — real
 *   coordination output (standardization, treaty revision enabled) carried on
 *   substantially coerced, asymmetrically distributed compliance. Per the
 *   epsilon-invariance principle, the sibling readings (endogenous_climb,
 *   hybrid_cascade) are separate stories sharing this referent and authoring
 *   their own epsilon by their own lights; they are linked, not folded in.
 *
 * KEY AGENTS:
 *   - meiji_state_council: agenda-setter (institutional/arbitrage) — issued and enforced the decrees; captures fiscal and diplomatic gains
 *   - prefectural_administrators: enforcer-beneficiary (institutional/constrained) — bears enforcement labor, collects careers
 *   - treaty_port_merchants: incidental beneficiary (organized/mobile) — decree ratifies practices they had already adopted
 *   - western_treaty_powers: external beneficiary (powerful/arbitrage) — demand satisfied, nothing paid
 *   - rural_taxpaying_households: primary target (powerless/trapped) — bears the shortened-month and schedule disruption
 *   - traditional_dress_artisans: target (moderate/constrained) — lose premium demand
 *   - former_samurai_estate: identity-locked target (moderate/identity_locked) — status markers dismantled by edict
 *   - historical_sociologists: analytical observer — sees the full structure and adjudicates between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.5).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.35).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "Meiji Decree Displacement of Calendar and Dress Commitments (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical sociology/state formation/commitment systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, 'f8c233c8-8adc-4c86-b122-02e7772c3196').
narrative_ontology:cs_kernel_codification('f8c233c8-8adc-4c86-b122-02e7772c3196', formalized).
narrative_ontology:cs_authority_grounding('f8c233c8-8adc-4c86-b122-02e7772c3196', expertise).
narrative_ontology:cs_interpretation_layer_present('f8c233c8-8adc-4c86-b122-02e7772c3196').
narrative_ontology:cs_reading_relation('f8c233c8-8adc-4c86-b122-02e7772c3196', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('f8c233c8-8adc-4c86-b122-02e7772c3196', imposition_pathway_kernel__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('f8c233c8-8adc-4c86-b122-02e7772c3196', foundational, state_capacity_substitutes_for_emergent_adoption).
narrative_ontology:cs_axiom_status(state_capacity_substitutes_for_emergent_adoption, holdable).
narrative_ontology:cs_axiom_grounding('f8c233c8-8adc-4c86-b122-02e7772c3196', state_capacity_substitutes_for_emergent_adoption, empirically_contingent).
narrative_ontology:cs_axiom('f8c233c8-8adc-4c86-b122-02e7772c3196', foundational, mset_incomplete_without_override_cell).
narrative_ontology:cs_axiom_status(mset_incomplete_without_override_cell, holdable).
narrative_ontology:cs_axiom_grounding('f8c233c8-8adc-4c86-b122-02e7772c3196', mset_incomplete_without_override_cell, instrumental).
narrative_ontology:cs_reference_frame('f8c233c8-8adc-4c86-b122-02e7772c3196', override_cell_typology).
narrative_ontology:cs_drift_state('f8c233c8-8adc-4c86-b122-02e7772c3196', contemporary_historical_sociology, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f8c233c8-8adc-4c86-b122-02e7772c3196', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, meiji_state_council).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, prefectural_administrators).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, treaty_port_merchants).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, western_treaty_powers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, rural_taxpaying_households).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, traditional_dress_artisans).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, former_samurai_estate).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__exogenous_override_reading, state_capacity_displacement_thesis).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__exogenous_override_reading, mset_override_cell_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1872 calendar edict and the successive court and official dress regulations; directed prefectural enforcement and made the Gregorian calendar mandatory for all official documents, taxation, and contracts. Captured fiscal synchronization, treaty-negotiation credibility, and administrative legibility. Could amend or rescind the edicts at any point; chose deepening instead.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, meiji_state_council, agenda_setter,
    institutional, generational, arbitrage, national).

% Administered the decrees locally: converted land-tax registers, prosecuted document noncompliance, and supervised dress rules at public offices. Career advancement ran through demonstrated implementation, so they bore real enforcement labor while collecting promotion and expanded bureaucratic purview.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, prefectural_administrators, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__exogenous_override_reading, prefectural_administrators, beneficiary).

% Gained uniform contract dates, shipping schedules, and accounting periods aligned with Western counterparties. Many had already kept dual reckonings voluntarily in the ports, so the decree ratified a practice they had pioneered at little additional cost to themselves.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, treaty_port_merchants, beneficiary,
    organized, biographical, mobile, global).

% Treated calendar conformity and visible Westernization as preconditions for treaty revision and diplomatic recognition. Paid nothing for the transition; their satisfaction was the decree's explicit diplomatic purpose, and they retained the option of withholding revision if conformity lagged.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, western_treaty_powers, beneficiary,
    powerful, generational, arbitrage, global).

% Woke up in the twelfth month of 1872 to find roughly twenty-nine days of reckoning removed: rent, interest, and tax due dates recalculated overnight, festivals and liturgical observances misaligned with the agricultural round. Had no consultative channel before enactment; objections survive as petitions and scattered riot records. Exit meant flight or concealment of continued lunar reckoning.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, rural_taxpaying_households, payer,
    powerless, biographical, trapped, local).

% Lost official and court demand when Western dress became mandatory at public functions, collapsing the premium segment of kimono and ceremonial costume trades. Pivoted to domestic markets at reduced margins; retooling skills and supply chains was slow and uncompensated.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, traditional_dress_artisans, payer,
    moderate, biographical, constrained, regional).

% Already stripped of stipends and swords; the hair, dress, and etiquette edicts attacked the last visible markers of inherited status. Resisted through conservative associations and the press. The relevant identity was not a garment but a lineage-self-conception, so abandoning the markers was experienced as self-erasure rather than wardrobe change.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, former_samurai_estate, payer,
    moderate, biographical, identity_locked, national).

% Reconstruct the episode from prefectural records, edict texts, petition archives, and enforcement caseloads. Adjudicate between rival accounts of how the new commitments took hold and whether the episode generalizes to other state formations.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, historical_sociologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__exogenous_override_reading, meiji_state_council).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaced regionally divergent lunar-solar timekeeping and status-coded dress with single national standards synchronized to Western treaty partners, solving the divergent-temporal-order problem and the diplomatic-legibility problem in a single legislative stroke.
% TRANSFER_FUNCTION: Moved compliance costs — the shortened twelfth month of 1872, mandated Western dress expenditure, collapsed premium demand for traditional costume, and eroded status markers — from the state and treaty-aligned commercial sectors onto rural households, artisans, and the former samurai estate; moved diplomatic credibility and administrative legibility to the state.
% ABSENT_VOICES: Rural taxpaying households had no representative channel in 1873; their objections exist only as post-enactment petitions and riot records. Traditional dress artisans and ritual specialists affected by calendar-liturgy misalignment were never surveyed. Former samurai opposed through press and association but held no formal seat in the decision.
% DISAPPEARANCE_RATIONALE: Without decree-plus-enforcement, calendar and dress convergence would have crawled through merchant diffusion over generations. The 1873 fiscal year, treaty negotiation schedules, conscription logistics, and the administrative integration built on the new temporal order would have been delayed or restructured around whatever partial standards emerged organically.
% FOUNDING_PROBLEM: Fragmented regional calendars and customary dress blocked synchronized taxation, treaty scheduling, and the civilizational-parity signaling required to renegotiate the unequal treaties.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic-history scholarship on treaty revision — written entirely outside the benefiting parties, who are deceased — attests the calendar and dress decrees served revision goals achieved by 1894 (Anglo-Japanese commercial treaty) and 1911 (tariff autonomy); fiscal histories corroborate the administrative integration. No living party attests the founding problem remains open; the mechanism persists instead as a reusable state instrument.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-substantial (0.50 at interval end): compliance costs were real, uncompensated, and concentrated on seats with no voice, but the arrangement also delivered a genuine public good — a unified temporal-administrative order that enabled treaty revision. Suppression (0.35 at end) reflects legal compulsion rather than covert coercion: official-document requirements, dress rules at public functions, prefectural prosecution. Accessibility_collapse is 0.60 — the decrees closed legal alternatives decisively, yet informal dual reckoning persisted in villages for decades, so alternatives were suppressed, not annihilated. Resistance 0.45 captures petitions, evasion, conservative press opposition, and scattered unrest. Theater stays low (0.22): the calendar actually changed; enforcement activity was functional before it became ceremonial. The suppression_requirement series is authored deliberately — this story specifically tracks enforcement-capacity change: machinery built to peak intensity at decree, then demobilized as compliance habituated. The decay pattern is itself contested evidence: this reading reads it as completed displacement (the commitment transferred, enforcement no longer needed); the hybrid reading reads the same curve as organic climb finishing the job. All three series run on one shared time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently by construction. From the council's position the decrees are statecraft it designed and owns — coordination it purchased with enforcement capacity. From the trapped rural seat the identical structure operates as uncompensated confiscation of time and custom. The identity-locked samurai seat experiences a third thing: not a price but an amputation. The engine computes these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate them. Coalition potential among the powerless rural seats existed in principle (petition campaigns, the 1870s uprisings) but never coordinated across the calendar grievance specifically — the diffuse, non-focal nature of the harm blunted class-level response.
 *
 * DIRECTIONALITY LOGIC:
 *   The council sits nearest the beneficiary pole (d near 0.0): it wrote the rules, pays almost nothing, and collects the fiscal-diplomatic surplus. Prefectural administrators derive low d from their beneficiary declaration but carry real enforcement labor — the structural derivation slightly understates their cost side, accepted here rather than overridden since the net position remains clearly subsidized. Treaty-port merchants and the western powers are beneficiaries with arbitrage-grade positions — the decree ratified practices they wanted at near-zero marginal cost. Rural households sit nearest the full-target pole (d near 1.0), amplified by trapped exit and the national scope of the decree, which makes verification of compliance cheap for the state and evasion expensive for the household. Artisans are targets with constrained exit; the samurai estate is a target whose identity lock pushes it to the extreme target end regardless of its moderate formal power.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Coding the arrangement as pure snare would erase the coordination function — the standardization was real, the treaty-revision payoff real, and the merchants' gains real; a pure-extraction story cannot explain why the state spent enforcement capacity on something with genuine public output. Coding it as pure rope would erase the coerced asymmetry — no participant consented, the costs landed on voiceless seats, and the benefit distribution tracked proximity to state power. On mandatrophy: the founding problems (temporal fragmentation, treaty-blockage) are dead — corroborated by the 1894/1911 revisions — yet the mechanism persists in the state repertoire as a reusable displacement instrument, which is precisely the dead-problem-plus-persistence signature the R5 mismatch consumer is built to catch. The low theater ratio keeps that flag from resolving into a piton verdict: what persists is capability, not performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_decree_adoption_extent,
    'How much pre-decree fringe adoption of Western calendar reckoning and dress actually existed — rangaku scholars, treaty-port residents, early military units — and does it constitute the invisible fringe stage the endogenous reading requires?',
    'Archival enumeration of adoption instances before 1868/1873 with population-base estimates: if pre-decree adopters were a negligible fraction outside port enclaves, the override pathway stands; a substantial hidden fringe collapses this reading toward endogenous or hybrid.',
    'This is the load-bearing uncertainty for the whole reading: a found fringe stage dissolves the distinct-mechanism claim and eliminates the need for a separate M-set override cell; its absence confirms the cell.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_decree_adoption_extent, empirical, 'Whether the Meiji case truly lacked a pre-decree adoption fringe.').

omega_variable(
    completion_mechanism_composition,
    'Was post-decree compliance sustained by continuing enforcement, or did voluntary habituation take over as enforcement decayed — and can the two be distinguished in the enforcement-caseload record?',
    'Compare prefectural enforcement caseloads against independent compliance indicators (document dating accuracy, dress observance at unofficial occasions) across 1873-1900: if compliance holds flat while prosecutions collapse, habituation completed the displacement (hybrid); if compliance tracks enforcement throughout, coercion carried it (this reading).',
    'Determines whether the override mechanism is self-sufficient (separate M-set cell warranted) or merely an initiator whose completion belongs to the ordinary climb mechanism (hybrid cell suffices).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(completion_mechanism_composition, empirical, 'Whether coerced compliance converted into internalized commitment or remained enforcement-dependent.').

omega_variable(
    override_generalizability_beyond_meiji,
    'Does the Meiji episode establish a general override mechanism available to states above a capacity threshold, or is it an artifact of Japan''s unusual combination of centralized bureaucracy, external existential threat, and a weakly entrenched incumbent commitment structure?',
    'Comparative analysis of decree-driven commitment displacement across state formations (Ottoman hat law, Soviet calendar experiments, French revolutionary calendar) controlling for state capacity, external threat, and incumbent-commitment entrenchment.',
    'If generalizable, the M-set override cell is a first-class mechanism type; if Meiji-specific, the cell degrades into a case annotation and the typology stays two-mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(override_generalizability_beyond_meiji, conceptual, 'Whether the override pathway generalizes or is a Meiji artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 1872, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imposition_exo_tr_t1872, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1872, 0.1).
narrative_ontology:measurement_basis(imposition_exo_tr_t1872, observed).
narrative_ontology:measurement(imposition_exo_tr_t1878, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1878, 0.12).
narrative_ontology:measurement_basis(imposition_exo_tr_t1878, observed).
narrative_ontology:measurement(imposition_exo_tr_t1884, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1884, 0.15).
narrative_ontology:measurement_basis(imposition_exo_tr_t1884, observed).
narrative_ontology:measurement(imposition_exo_tr_t1890, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1890, 0.17).
narrative_ontology:measurement_basis(imposition_exo_tr_t1890, observed).
narrative_ontology:measurement(imposition_exo_tr_t1895, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1895, 0.2).
narrative_ontology:measurement_basis(imposition_exo_tr_t1895, observed).
narrative_ontology:measurement(imposition_exo_tr_t1900, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement_basis(imposition_exo_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(imposition_exo_be_t1872, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1872, 0.66).
narrative_ontology:measurement_basis(imposition_exo_be_t1872, observed).
narrative_ontology:measurement(imposition_exo_be_t1878, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1878, 0.63).
narrative_ontology:measurement_basis(imposition_exo_be_t1878, observed).
narrative_ontology:measurement(imposition_exo_be_t1884, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1884, 0.59).
narrative_ontology:measurement_basis(imposition_exo_be_t1884, observed).
narrative_ontology:measurement(imposition_exo_be_t1890, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement_basis(imposition_exo_be_t1890, observed).
narrative_ontology:measurement(imposition_exo_be_t1895, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1895, 0.52).
narrative_ontology:measurement_basis(imposition_exo_be_t1895, observed).
narrative_ontology:measurement(imposition_exo_be_t1900, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement_basis(imposition_exo_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(imposition_exo_su_t1872, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1872, 0.72).
narrative_ontology:measurement_basis(imposition_exo_su_t1872, observed).
narrative_ontology:measurement(imposition_exo_su_t1878, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1878, 0.66).
narrative_ontology:measurement_basis(imposition_exo_su_t1878, observed).
narrative_ontology:measurement(imposition_exo_su_t1884, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1884, 0.58).
narrative_ontology:measurement_basis(imposition_exo_su_t1884, observed).
narrative_ontology:measurement(imposition_exo_su_t1890, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1890, 0.48).
narrative_ontology:measurement_basis(imposition_exo_su_t1890, observed).
narrative_ontology:measurement(imposition_exo_su_t1895, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1895, 0.41).
narrative_ontology:measurement_basis(imposition_exo_su_t1895, observed).
narrative_ontology:measurement(imposition_exo_su_t1900, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement_basis(imposition_exo_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'how do social commitments displace' decomposes into three readings of the imposition_pathway_kernel, each a separate story with its own epsilon assessed by its own lights over the shared referent (the Meiji decree-and-enforcement arrangement). This reading authors epsilon for the arrangement AS A STANDALONE OVERRIDE — decree creating commitment through enforcement — yielding moderate-high extraction with a real coordination core. The endogenous_climb_reading reads the same arrangement as a compressed climb (its epsilon weights the invisible-fringe hypothesis differently); the hybrid_cascade_reading reads it as initiated-override-completed-by-climb (splitting the extraction attribution between phases). The upstream/downstream structure runs from the empirical record through this reading to the framework-level claim (M-set completeness), which is why this story links to both siblings rather than deriving from them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
