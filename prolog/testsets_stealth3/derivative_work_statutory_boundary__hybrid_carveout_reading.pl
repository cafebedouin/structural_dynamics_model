% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary — Commercial Carveout Regime (Hybrid Reading)
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   An authorization regime for building on protected expression in which the
 *   requirement to obtain a rights holder's permission turns on commercial
 *   exploitation: transformative reuse without payment proceeds freely, while
 *   any commercial deployment requires negotiated authorization. The regime
 *   is administered jointly by rights-holder licensing operations (terms and
 *   rates), platform enforcement infrastructure (matching and takedown at
 *   scale), and courts (doctrinal gloss). The claim/metric relationship is
 *   deliberate: the constraint is CLAIMED as tangled_rope — a genuine
 *   two-track coordination function carrying an asymmetric commercial-side
 *   levy — while the metrics are authored from the regime's observed
 *   operation; the engine computes per-seat classifications from the
 *   structural data, and divergence between claim and computed type is
 *   signal, not error. Epsilon's referent is this standing two-tier
 *   arrangement as assessed by this reading's own lights, never a preferred
 *   alternative regime.
 *
 * KEY AGENTS:
 *   - rights_holder_licensing_estates: agenda-setter and collector (institutional/arbitrage) — sets commercial-side terms, collects the licensing stream
 *   - platform_enforcement_intermediaries: operational administrator (institutional/arbitrage) — implements the line at scale through matching defaults
 *   - commercial_derivative_producers: primary target (powerful/constrained) — pays clearance costs for unsubstitutable inputs
 *   - marginal_monetization_creators: secondary target (moderate/constrained) — bears automatic enforcement without leverage
 *   - noncommercial_transformative_creators: exempt beneficiary (organized/mobile) — works the free lane
 *   - general_transformative_audiences: incidental beneficiary (organized/mobile) — consumes both tracks
 *   - informal_economy_creators: excluded voice (powerless/trapped) — reached by enforcement, never offered terms
 *   - courts_and_policymakers: analytical observer (institutional/analytical) — supplies the doctrinal gloss
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.6).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary — Commercial Carveout Regime (Hybrid Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '34f4ea62-92e5-4b35-a561-25d5b4951c19').
narrative_ontology:cs_kernel_codification('34f4ea62-92e5-4b35-a561-25d5b4951c19', fixed_text).
narrative_ontology:cs_authority_grounding('34f4ea62-92e5-4b35-a561-25d5b4951c19', lineage).
narrative_ontology:cs_interpretation_layer_present('34f4ea62-92e5-4b35-a561-25d5b4951c19').
narrative_ontology:cs_reading_relation('34f4ea62-92e5-4b35-a561-25d5b4951c19', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('34f4ea62-92e5-4b35-a561-25d5b4951c19', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_axiom('34f4ea62-92e5-4b35-a561-25d5b4951c19', foundational, commercial_exploitation_requires_authorization).
narrative_ontology:cs_axiom_status(commercial_exploitation_requires_authorization, holdable).
narrative_ontology:cs_axiom_grounding('34f4ea62-92e5-4b35-a561-25d5b4951c19', commercial_exploitation_requires_authorization, conventional).
narrative_ontology:cs_axiom('34f4ea62-92e5-4b35-a561-25d5b4951c19', foundational, noncommercial_transformation_outside_boundary).
narrative_ontology:cs_axiom_status(noncommercial_transformation_outside_boundary, holdable).
narrative_ontology:cs_axiom_grounding('34f4ea62-92e5-4b35-a561-25d5b4951c19', noncommercial_transformation_outside_boundary, deontological).
narrative_ontology:cs_reference_frame('34f4ea62-92e5-4b35-a561-25d5b4951c19', commercial_noncommercial_tiered_boundary).
narrative_ontology:cs_drift_state('34f4ea62-92e5-4b35-a561-25d5b4951c19', contemporary_platform_monetization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('34f4ea62-92e5-4b35-a561-25d5b4951c19', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holder_licensing_estates).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_transformative_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_producers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, marginal_monetization_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_enforcement_intermediaries).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, general_transformative_audiences).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, creative_incentive_bargain).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, adaptation_market_capture_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold catalogs of protected expression across music, film, publishing, and software. Set the terms and rates at which commercial reuse of their catalogs is authorized, run clearance and sync-licensing operations, and enforce the boundary through litigation and takedown programs. Licensing revenue accrues to them; they can restructure portfolios, form collecting societies, and shift enforcement strategy across jurisdictions.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holder_licensing_estates, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holder_licensing_estates, beneficiary).

% Operate the upload-scanning, matching, and takedown infrastructure through which the boundary is administered at internet scale. Their match thresholds, dispute queues, and revenue-sharing defaults determine where the line bites for millions of uploaded works. They earn advertising and transaction revenue from the ecosystem the two-track system organizes, and can adjust enforcement defaults unilaterally within their platforms.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_enforcement_intermediaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, platform_enforcement_intermediaries, beneficiary).

% Studios, game developers, and software firms whose products build on existing protected expression. Before shipping they must identify every protected element, negotiate licenses, and carry clearance costs that scale with the number of incorporated works. Absorbing the cost compresses margins; passing it through raises prices; abandoning the underlying expression means rebuilding from scratch. Litigation is available to the largest of them.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_producers, payer,
    powerful, biographical, constrained, global).

% Video essayists, fan artists with tip jars, streamers, and small publishers whose work transforms protected expression and earns modest revenue. Enforcement systems classify any revenue as commercial, so they face takedowns, demonetization, and retroactive license demands despite operating at scales the licensing market never priced for. They hold no negotiating leverage and rarely litigate.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, marginal_monetization_creators, payer,
    moderate, biographical, constrained, global).

% Educators, critics, researchers, wiki editors, and unpaid fan creators who recut, annotate, translate, and remix protected expression without charging for it. The exempt lane lets them work without clearance; their practical security depends on staying visibly non-commercial, since monetizing tomorrow would move them across the line.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_transformative_creators, beneficiary,
    organized, biographical, mobile, global).

% Readers, viewers, players, and learners who consume both licensed commercial derivatives and free non-commercial transformations. Their attention and spending shape which licenses get bought and which fan practices flourish; they pay indirectly through prices on commercial works and not at all on the exempt lane.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, general_transformative_audiences, beneficiary,
    organized, generational, mobile, global).

% Creators and vendors in informal markets — local music scenes built on sampling, market vendors using character imagery, community broadcasters — whose reuse is commercial in fact but invisible to the licensing system. Enforcement reaches them as takedowns and seizures without ever offering clearance terms priced for their economies. Nobody represented their practices when the commercial line was drawn.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, informal_economy_creators, excluded,
    powerless, immediate, trapped, global).

% Adjudicate where the line sits case by case, weighing the fair-use factors and the scope of the adaptation right. Their opinions supply the authoritative gloss that licensing practice and platform defaults follow. They hear testimony from every other seat and can redraw the line through doctrine or legislation, though slowly.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, courts_and_policymakers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holder_licensing_estates).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Splits the reuse space into two tracks: commercial exploitation of protected expression is routed through negotiated licenses, solving the market-clearing problem between original and derivative producers; non-commercial transformation proceeds without clearance, preserving a free lane for criticism, education, research, and fan practice.
% TRANSFER_FUNCTION: Moves licensing revenue and clearance costs from commercial derivative producers to rights-holder estates; moves legal risk off non-commercial transformers and onto commercial ones; moves enforcement labor to platform intermediaries.
% ABSENT_VOICES: Informal-economy creators and marginal monetizers sit across the line without having been in the room when it was drawn: their practices look commercial to enforcement systems and non-commercial in spirit, and no seat representing them participated in the standing consultations or the case law that fixed the categorical split.
% DISAPPEARANCE_RATIONALE: Overnight removal of the commercial/non-commercial split forces a choice between universal licensing (every transformative use negotiates) and universal freedom (no authorization at all); licensing markets, platform enforcement apparatuses, and fan-practice norms would all reorganize within months around whichever replacement rule took hold.
% FOUNDING_PROBLEM: The derivative-work right was built to let authors capture adaptation markets — translations, dramatizations, film versions — so the value of second-generation formats would flow back to first creators and fund further creation.
% FOUNDING_PROBLEM_CORROBORATION: Copyright historians and legal scholars outside the estate beneficiaries corroborate the adaptation-control genealogy from legislative history; technology-law scholarship and platform-economy studies corroborate from outside that the founding problem no longer describes most governed cases (software interoperation, sampling, video essays, fan economies). No estate-independent body attests that the current scope still solves the original problem.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.58) rather than high because the exempt lane removes roughly half the reuse space from the payment requirement entirely; it stays well above coordination-cost levels because commercial-side clearance prices are set by catalog owners holding monopoly positions over necessary inputs, decoupled from the marginal harm of any particular derivative use. Suppression (0.60) reflects dependence on active enforcement — notice-and-takedown regimes, automated matching, litigation — that blocks unlicensed commercial routes while leaving the non-commercial lane open; blocking one lane but not the other keeps suppression below full-exclusion levels. Theater ratio (0.26): most day-to-day clearance is ordinary transacting, but the doctrinal apparatus that legitimates the line — factor-balancing performed case by case — increasingly stages a determinacy the categorical rule lacks, and a growing share of enforcement activity defends the line's existence rather than resolving genuine ambiguity. Accessibility collapse (0.40): alternatives remain reachable — open licenses, public-domain sources, original creation, and the exempt lane itself — so understanding the regime does not close the option space. Resistance (0.55): sustained litigation from large producers, recurring policy challenges from marginal creators and their advocates, and jurisdictional forum-shopping. Boltzmann coordination type resource_allocation: the regime's primary function is allocating rights to exploit creative inputs across a multi-party market; the type default floor applies, no override. The temporal series share one grid; suppression_requirement is tracked because the story's enforcement history is one of machinery build-up (statute-era notice regimes, then platform-scale matching), not static enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the estate seat the two-tier line is a balanced incentive architecture it designed and maintains: the exempt lane buys cultural legitimacy, the commercial lane funds the catalog. From the large-producer seat the same line is a toll gate priced above harm, survivable through scale and litigation. From the marginal-creator seat it is an arbitrary classifier that reads a tip jar as a business and issues takedowns accordingly. Same statute, same doctrine — three different experienced regimes, computed per seat from power, exit, and declared position rather than reconciled into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-holder estates sit near the beneficiary pole: they set terms, collect the revenue stream, and hold arbitrage-grade mobility across portfolios and jurisdictions. Commercial derivative producers sit near the target pole: they pay clearance costs for inputs they cannot substitute, and their exit — rebuilding without the underlying expression — is prohibitively expensive, pinning them near the full-target end despite their size. Marginal monetization creators sit nearest the full-target end of any seat: enforcement reaches them automatically through matching systems, they hold no negotiating leverage, and their practices were never priced into the line. Non-commercial transformative creators sit near the beneficiary pole: the exempt lane subsidizes their practice at zero clearance cost, conditioned on visible non-commerciality. General audiences sit near symmetric: they receive both tracks and pay only indirectly through commercial prices. Platform intermediaries occupy a dual position — they administer the boundary and draw revenue from the ecosystem it organizes, but also absorb liability pressure from both sides; the derivation from their declared beneficiary role approximates this adequately, so no explicit override is authored. Informal-economy creators are excluded rather than coordinated: the line was drawn without them, and enforcement reaches them as seizure rather than offer.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the regime as pure extraction would erase the exempt lane — a real, functioning subsidy to criticism, education, and fan practice that no purely extractive structure would tolerate. Reading it as pure coordination would erase the categorical mismatch: the line taxes by commercial category rather than by burden or harm, so it levies equally on transformative innovation that substitutes nothing and on trivial monetization that threatens nothing, while the founding problem — letting authors capture adaptation markets — now describes a shrinking fraction of governed cases. The tangled_rope classification holds both halves: coordination function present, asymmetric categorical levy present, active enforcement required to hold the line. On obsolescence: the founding problem is contested rather than dead — adaptation markets persist for major media — so no mandatrophy resolution is declared; the R5 mismatch consumer finds status=contested paired with verdict=world_rearranges, which flags neither zombie nor settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the hybrid_carveout_reading of the derivative_work_statutory_boundary kernel — how would the classification move under the sibling readings?',
    'Author enclosure_reading and coordination_reading as separate stories with their own beneficiary/victim structures and compare epsilon, computed types, and victim sets across the family.',
    'Under enclosure_reading the exempt lane closes and extraction rises toward universal-control levels (snare-flavored); under coordination_reading the commercial levy thins to incorporation-tested cases and extraction falls toward coordination-cost levels (rope-flavored). The tangled_rope verdict is specific to the commercial/non-commercial criterion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame contingency: classification is indexed to one reading of a contested kernel.').

omega_variable(
    boundary_criterion_principledness,
    'Is the commercial/non-commercial line a principled location for the authorization boundary, or an administrable proxy adopted because incorporation- and harm-testing are costly to adjudicate?',
    'Comparative analysis of jurisdictions and proposals that test the boundary by substitution harm or degree of incorporation instead of commercial status, measuring enforcement-error profiles against the commerciality line.',
    'If commerciality is a proxy, the regime''s extraction pattern tracks the proxy''s errors — taxing harmless commerce and exempting scaled ''non-commercial'' distribution — rather than any principled boundary, weakening the coordination-function justification for the commercial-side levy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_criterion_principledness, conceptual, 'Whether the categorical line reflects principle or administrative convenience.').

omega_variable(
    marginal_monetizer_systematic_misclassification,
    'Does the categorical commercial line systematically misclassify marginal monetization — tip-jar fan art, ad-supported criticism — as commercial exploitation warranting authorization?',
    'Audit enforcement actions (takedowns, demonetizations, license demands) against creator revenue scale and transformation character; measure the rate at which sub-subsistence monetization triggers full commercial treatment.',
    'If systematic, effective victims concentrate among low-power small creators rather than large commercial producers, raising effective extraction on the least-leveraged seats and pulling the payer-side experience toward exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_monetizer_systematic_misclassification, empirical, 'Whether enforcement systematically misclassifies marginal monetization as commercial.').

omega_variable(
    licensing_fee_vs_substitution_harm,
    'Do commercial license fees track the substitution harm of the specific derivative use, or are they priced off catalog monopoly over necessary inputs?',
    'Royalty-rate studies correlated with substitution analysis: compare fees for derivatives that displace the original''s market against fees for derivatives in unrelated markets.',
    'Fees decoupled from harm indicate a rent component dominating the commercial-side levy, supporting a higher epsilon assessment; harm-tracking fees support the market-clearing framing and lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_fee_vs_substitution_harm, empirical, 'Whether clearance pricing reflects harm or input monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(deri_tr_t0, observed).
narrative_ontology:measurement(deri_tr_t6, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(deri_tr_t6, observed).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(deri_tr_t12, observed).
narrative_ontology:measurement(deri_tr_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement_basis(deri_tr_t18, observed).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(deri_tr_t24, observed).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(deri_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(deri_be_t0, observed).
narrative_ontology:measurement(deri_be_t6, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(deri_be_t6, observed).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(deri_be_t12, observed).
narrative_ontology:measurement(deri_be_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement_basis(deri_be_t18, observed).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement_basis(deri_be_t24, observed).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(deri_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement_basis(deri_su_t0, observed).
narrative_ontology:measurement(deri_su_t6, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement_basis(deri_su_t6, observed).
narrative_ontology:measurement(deri_su_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(deri_su_t12, observed).
narrative_ontology:measurement(deri_su_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(deri_su_t18, observed).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(deri_su_t24, observed).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(deri_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the derivative work boundary' conflates three structurally distinct claims about where the authorization line sits — universality (enclosure_reading), incorporation (coordination_reading), and commerciality (this reading). Per the epsilon-invariance principle each is authored as its own story with its own epsilon, beneficiaries, and victims; they form a constraint family linked through affects_constraints. Genealogically, enclosure_reading is the formalist baseline from which the other two depart; coordination_reading supplied the transformativeness vocabulary this reading absorbs into its non-commercial lane; this reading is the operative middle that both siblings attack.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
