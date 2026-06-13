% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Reading: Compulsory Licensing and Parallel Import Scope
 *   domain: international_trade_law/public_health_policy
 *
 * SUMMARY:
 *   The TRIPS Agreement (1995) establishes global minimum standards for
 *   patent protection but embeds flexibilities permitting governments to
 *   authorize generic production (compulsory licensing under Article 31) and
 *   parallel importing (Article 6) for public health reasons. This constraint
 *   story models ONE reading of the TRIPS text: the public health flexibility
 *   reading, which interprets these flexibilities as broad and immediately
 *   available to any government prioritizing public health access over patent
 *   exclusivity. The sibling strong_exclusivity_reading interprets the same
 *   text as mandating uniform, high patent protection with narrow,
 *   difficult-to-invoke exceptions. These are not different factual
 *   assessments of the same constraint—they are structurally distinct
 *   constraints (different beneficiary sets, different extraction profiles,
 *   different institutional authority) that emerge from the SAME kernel (the
 *   TRIPS text itself) read through different interpretive lenses. This
 *   constraint models what the constraint landscape looks like under the
 *   public health flexibility reading in practice.
 *
 * KEY AGENTS:
 *   - Generic manufacturers: structurally positioned as beneficiaries under this reading; gain market access they would lose under strong exclusivity reading
 *   - Health ministries and public health authorities: beneficiaries; can invoke flexibilities to source medicines at accessible prices
 *   - Originator pharmaceutical firms: targets/payers; face pricing erosion and market-share loss when compulsory licensing and parallel imports are invoked
 *   - WTO dispute settlement bodies: agenda-setter; panels rule on whether invoked flexibilities comply with TRIPS, and their reading of Articles 31 and 6 determines whether beneficiaries or victims prevail
 *   - High-income country governments: excluded from the beneficiary coalition; contest the reading diplomatically and through bilateral trade pressure
 *   - Low-income country governments: dual-positioned beneficiaries and payers; gain public health benefit but pay diplomatic cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.42).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Reading: Compulsory Licensing and Parallel Import Scope").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '28646b09-7ab1-4c83-a5df-a25d731fcd92').
narrative_ontology:cs_kernel_codification('28646b09-7ab1-4c83-a5df-a25d731fcd92', fixed_text).
narrative_ontology:cs_authority_grounding('28646b09-7ab1-4c83-a5df-a25d731fcd92', lineage).
narrative_ontology:cs_interpretation_layer_present('28646b09-7ab1-4c83-a5df-a25d731fcd92').
narrative_ontology:cs_reading_relation('28646b09-7ab1-4c83-a5df-a25d731fcd92', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_axiom('28646b09-7ab1-4c83-a5df-a25d731fcd92', foundational, compulsory_licensing_broadly_justified).
narrative_ontology:cs_axiom_status(compulsory_licensing_broadly_justified, holdable).
narrative_ontology:cs_axiom_grounding('28646b09-7ab1-4c83-a5df-a25d731fcd92', compulsory_licensing_broadly_justified, deontological).
narrative_ontology:cs_axiom('28646b09-7ab1-4c83-a5df-a25d731fcd92', foundational, public_health_overrides_patent_exclusivity).
narrative_ontology:cs_axiom_status(public_health_overrides_patent_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('28646b09-7ab1-4c83-a5df-a25d731fcd92', public_health_overrides_patent_exclusivity, deontological).
narrative_ontology:cs_reference_frame('28646b09-7ab1-4c83-a5df-a25d731fcd92', public_health_qualified_patent_protection).
narrative_ontology:cs_drift_state('28646b09-7ab1-4c83-a5df-a25d731fcd92', contemporary_enforcement_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('28646b09-7ab1-4c83-a5df-a25d731fcd92', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, public_health_advocates).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, originator_pharmaceutical_firms).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).
:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.38 (current) and has declined from 0.72 at TRIPS inception. This reflects a reading trajectory: as the public health flexibility interpretation took root (accelerating after Doha 2001), the effective extraction that originator firms could impose declined—compulsory licensing became more frequently invoked (India, Thailand, South Africa), generic competition increased, and the gap between originator and generic prices narrowed. The reading's success is visible as declining extractiveness: the constraint no longer permits the uniform, high-margin pricing regime originator firms could enforce under strong exclusivity. Suppression has similarly declined (from 0.58 to 0.42) because the enforcement machinery to prevent compulsory licensing and parallel imports has weakened—WTO panels ruled against attempted patent-based suppression in several cases. Theater ratio rose (0.08 to 0.28) as originator-firm arguments shifted from 'TRIPS forbids compulsory licensing' to 'compulsory licensing harms innovation incentives'—arguments that are normative claims about policy effects, not textual interpretation. The temporal trajectory shows a constraint being progressively reinterpreted toward lower extraction and lower suppression as the reading gained institutional acceptance. All measurements share the same time grid (1995, 2001, 2008, 2015, 2020, 2025) so temporal analysis can track drift coherently.
 *
 * PERSPECTIVAL GAP:
 *   From the originator firm's seat, the reading is extraction—it authorizes competitors to use their patents without consent, eroding their exclusivity. From the generic manufacturer's seat, the reading is coordination—it solves the collective-action problem of accessing medicines in crisis contexts and low-income markets. From the dispute settlement body's seat, the reading is an adjudication of the TRIPS text's actual scope. The engine will compute different type classifications for each seat from the same structural data: originator firms will likely compute snare or tangled rope (extraction + enforcement); generic manufacturers will compute rope (coordination benefit); health ministries will compute rope with some snare characteristics (coordination gain, enforcement cost). This divergence is the point—the reading's structural asymmetry is what the constraint story exists to reveal.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary and victim declarations plus power and exit modulation. Generic manufacturers (organized power, arbitrage exit) get low d—they benefit structurally and have options to exit TRIPS-zone markets if needed. Health ministries (organized power, constrained exit) get moderate-to-low d—they benefit from the flexibility but cannot exit the TRIPS system (sovereignty constraint) and face diplomatic cost. Public health advocates (moderate power, constrained exit) get moderate d—they benefit ideologically from the reading's success but have limited enforcement capacity. Originator firms (institutional power, arbitrage exit) get high d—they are the targets of extraction, though their exit options (investing in other jurisdictions, shifting to other IP regimes) are stronger than powerless victims. Low-income governments (moderate power, identity-locked exit) get moderate-to-high d—they benefit from the reading but are locked into the TRIPS system by identity as sovereign states subject to trade pressure; their exit is constrained by the cost of trade retaliation or aid loss. This structural diversity is not averaged into one d-value per story; the engine computes per-seat d and per-seat type, revealing the reading's perspectival asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing innovation incentives against public health access within a unified patent system) is LIVE under this reading. The TRIPS text was intended to preserve both—protection for innovators AND public health flexibilities. This reading affirms both; it does not declare the founding problem obsolete. However, originator firms and high-income country governments increasingly argue that the founding problem is DEAD—that public health is now solvable through other means (tiered pricing, vaccine partnerships, voluntary licensing) and that strict patent protection is necessary for continued innovation. The mandatrophy test: does the reading persist because its function (balancing IP protection with health access) is still needed, or because institutional inertia and beneficiary capture keep it in place? Evidence for live function: compulsory licensing is invoked regularly (India's generic production, Thailand's ARV licensing, Morocco's insulin compulsory license); the public health reading guides these actions and judges their legitimacy. Evidence for mandatrophy: originator firms lobby continuously to narrow the reading despite its persistence; high-income governments negotiate bilateral trade agreements that effectively restrict the flexibilities; the reading's institutional authority is contested at every turn. Verdict: NOT mandatrophy. The reading persists because the coordination function (permitting context-sensitive IP calibration) is genuinely needed and the beneficiary coalition (generic manufacturers, health ministries, public health advocates) actively defends it. If the reading vanished, the world would rearrange—medicine prices would rise in low-income contexts, generic competition would collapse in key markets, and public health authorities would lose the legal ground for crisis-driven licensing. This is not theater; it is contested but functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_scope_of_compulsory_licensing,
    'Does TRIPS Article 31 permit compulsory licensing for any public health reason a government deems legitimate, or only for specified categories (emergency, non-commercial use, public health crisis)?',
    'WTO panel ruling in a high-stakes dispute where a government invokes broad compulsory licensing and originator firms challenge it; or formal agreement by TRIPS Council to issue an authoritative interpretation (as occurred post-Doha).',
    'If scope is broad, the public health flexibility reading is affirmed and extraction declines further; if scope is narrow, originator firms gain ground and the sibling strong exclusivity reading gains institutional traction. This is the central interpretive contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_scope_of_compulsory_licensing, conceptual, 'The range of permissible triggers for compulsory licensing under Article 31.').

omega_variable(
    parallel_import_exhaustion_doctrine,
    'Does Article 6''s silence on parallel imports (international exhaustion of rights) mean governments may freely permit parallel import from any jurisdiction, or does it preserve originator-firm control over first sale jurisdiction?',
    'Dispute settlement ruling clarifying Article 6 scope; or widespread invocation of parallel import authority by governments that prompts either panel review or consensus interpretation.',
    'If international exhaustion is affirmed, generic competition from lower-cost jurisdictions becomes legal and extraction declines; if originator firms retain first-sale control, parallel import remains a narrow exception and extraction persists. This is the second-order interpretive battleground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parallel_import_exhaustion_doctrine, conceptual, 'Whether national governments can authorize parallel imports from any source or only from originator-controlled channels.').

omega_variable(
    identity_lock_cost_for_low_income_governments,
    'Is the diplomatic cost of invoking flexibilities (trade retaliation, aid conditioning, bilateral pressure) an internal feature of the reading''s operation, or an external political contingency?',
    'Empirical tracking of trade outcomes for countries invoking compulsory licensing (Brazil, India, South Africa, Thailand) compared to control cases; cost-accounting by health ministries of the price they paid (lost trade benefits, bilateral aid loss) for exercising the flexibilities.',
    'If the cost is internal and substantial, the reading is not a true rope for low-income governments but a constrained choice under pressure (higher d, more snare-like). If the cost is external and remediable through institutional reform (abolishing trade retaliation for health exercises, WTO rule change), the reading remains rope and the problem is institutional design, not the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_cost_for_low_income_governments, empirical, 'Whether the identity-lock in low-income governments is structural or remediable.').

omega_variable(
    kernel_reading_stability_under_economic_pressure,
    'As originator firms develop new business models (tiered pricing, voluntary licensing, digital-rights platforms), does the public health flexibility reading remain necessary and functionally distinct, or does it become a vestigial alternative to market-provided solutions?',
    'Long-term observation (10–20 years) of whether governments continue to invoke compulsory licensing even as voluntary access programs expand; whether beneficiary coalition remains mobilized; whether the reading is invoked for new classes of medicines or only for legacy products.',
    'If the reading becomes vestigial, the constraint may migrate toward piton (persists for historical reasons but primary function atrophied). If the reading remains actively used for new crises (pandemic response, climate-health linkages), it remains a live rope with genuine coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability_under_economic_pressure, empirical, 'The long-term sustainability of the public health flexibility reading under market evolution.').

omega_variable(
    committer_reading_foreclosure,
    'Does the public health flexibility reading logically foreclose the strong exclusivity reading within any single interpretive framework, or do these represent genuinely coexisting but incommensurable readings held by different institutional and political actors?',
    'Formal analysis of the axioms and grounding types declared for each reading—if both are holdable (live normative positions) and appeal to disjoint axioms (one emphasizing access rights, the other emphasizing innovation incentives), they coexist; if one reading''s core axiom directly contradicts the other''s (e.g., one claims exhaustion is mandatory, the other claims it is forbidden), they foreclose.',
    'If they foreclose, one reading will eventually dominate through institutional pressure and the other will be foreclosed; if they coexist, the constraint landscape will remain contested, interpretive authority will be contested, and the beneficiary/victim coalitions will remain in active dispute. The foreclosure status determines whether mandatrophy can emerge (only if one reading wins decisively).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_foreclosure, conceptual, 'Whether the two TRIPS readings are logically compatible or logically exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(trip_tr_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.72).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(trip_be_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement(trip_su_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2008, 0.46).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint and trips_agreement_interpretive_kernel__strong_exclusivity_reading are not two measurements of the same constraint—they are two different constraint structures that instantiate from the same contested kernel (the TRIPS text). The ε-invariance principle requires separate stories because the beneficiary/victim sets, enforcement profiles, and extraction baselines differ substantially under each reading. The strong_exclusivity_reading instantiates with originator firms as primary beneficiaries, health ministries as victims, and high extraction that flexibilities barely mitigate. This reading instantiates with generic manufacturers and health ministries as primary beneficiaries, originator firms as victims, and moderate extraction constrained by the flexibilities' scope. They share a kernel (TRIPS text) but emit different constraints with different ε values, and are linked via network.affects_constraints to preserve the family relationship and enable contamination analysis. The third reading, dispute_settlement_interpretive_authority, names the institutional question (who decides between these readings?) and affects both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
