% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Derivative Work Boundary — Commercial Exploitation Carve-Out Reading
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   Under this reading, the derivative-work right operates as a
 *   commerciality-keyed boundary: transformative use without commercial
 *   purpose proceeds without permission, while any commercial exploitation of
 *   a transformative character requires authorization from the rights holder.
 *   The interval indexes approximately 1994–2024, from the transformative-use
 *   consolidation in case law through the platform-monetization era. The ε
 *   referent is the standing carve-out arrangement itself — the licensing
 *   regime as it actually operates over commercial transformers and the
 *   exempt non-commercial zone — assessed by this reading's own lights, never
 *   the arrangement a sibling reading would install. Claim and metrics are
 *   authored independently: the claimed type states the structure believed
 *   true (genuine coordination function plus categorical asymmetric
 *   extraction), and the metric values describe observed operation without
 *   being tuned to any predicted engine verdict. KEY AGENTS (by structural
 *   relationship): - copyright_holders: Primary beneficiary and enforcement
 *   principal ([institutional]/[arbitrage]) — collects licensing fees,
 *   chooses litigation targets - major_licensees: Large payer with
 *   cross-licensing offset ([powerful]/[constrained]) — pays fees but
 *   collects them too - independent_commercial_creators: Squeezed payer
 *   ([moderate]/[trapped]) — bears licensing costs without leverage -
 *   non_commercial_creators: Exempt-zone beneficiary ([moderate]/[mobile]) —
 *   creates without transactions - licensing_intermediaries: Secondary
 *   beneficiary ([institutional]/[arbitrage]) — commissions on volume -
 *   downstream_content_consumers: Near-symmetric dual-position seat
 *   ([organized]/[constrained]) — receives adaptations, carries pass-through
 *   costs - global_south_creators: Excluded voice ([powerless]/[trapped]) —
 *   outside the licensing tables - legislatures_and_courts: Agenda setter
 *   ([institutional]/[analytical]) — defines and polices the line -
 *   copyright_law_academics: Analytical observer — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.6).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary — Commercial Exploitation Carve-Out Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, 'a351af75-499f-45bc-9305-391ac948c3cf').
narrative_ontology:cs_kernel_codification('a351af75-499f-45bc-9305-391ac948c3cf', formalized).
narrative_ontology:cs_authority_grounding('a351af75-499f-45bc-9305-391ac948c3cf', lineage).
narrative_ontology:cs_interpretation_layer_present('a351af75-499f-45bc-9305-391ac948c3cf').
narrative_ontology:cs_reading_relation('a351af75-499f-45bc-9305-391ac948c3cf', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('a351af75-499f-45bc-9305-391ac948c3cf', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('a351af75-499f-45bc-9305-391ac948c3cf', foundational, commercial_exploitation_triggers_authorization).
narrative_ontology:cs_axiom_status(commercial_exploitation_triggers_authorization, holdable).
narrative_ontology:cs_axiom_grounding('a351af75-499f-45bc-9305-391ac948c3cf', commercial_exploitation_triggers_authorization, conventional).
narrative_ontology:cs_axiom('a351af75-499f-45bc-9305-391ac948c3cf', foundational, noncommercial_transformation_free).
narrative_ontology:cs_axiom_status(noncommercial_transformation_free, holdable).
narrative_ontology:cs_axiom_grounding('a351af75-499f-45bc-9305-391ac948c3cf', noncommercial_transformation_free, deontological).
narrative_ontology:cs_reference_frame('a351af75-499f-45bc-9305-391ac948c3cf', commercial_exploitation_threshold).
narrative_ontology:cs_drift_state('a351af75-499f-45bc-9305-391ac948c3cf', platform_monetization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a351af75-499f-45bc-9305-391ac948c3cf', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, licensing_intermediaries).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, major_licensees).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, independent_commercial_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, downstream_content_consumers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, downstream_content_consumers).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, transformative_use_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_incentive_theory_of_copyright).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own catalogs of protected expression. Set licensing terms for commercial adaptations, decide which unauthorized commercial uses to pursue in court, and collect fees and settlements. Can dual-license, release selected works openly, or withhold rights entirely; catalog revenue persists across decades.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, agenda_setter).

% Clearance houses, agencies, and collectives that broker adaptation permissions between rights holders and commercial creators, retaining a percentage of each transaction. Volume of commercial adaptation activity determines their throughput; they can reposition across media markets as it shifts.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, licensing_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Fan-fiction writers, academic remixers, hobbyist modders, and nonprofit video essayists whose transformative work falls inside the exempt zone. They create without negotiating permissions and can move freely among subjects and communities; exposure arises mainly when a project later seeks revenue.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_creators, beneficiary,
    moderate, biographical, mobile, global).

% Studios, publishers, and franchise holders that pay for adaptation, sequel, and translation rights at scale. They simultaneously hold large catalogs that others license from, so fee flows cross in both directions; in-house legal departments treat clearance as routine production cost.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, major_licensees, payer,
    powerful, generational, constrained, global).

% Small-press authors, indie game developers, and monetized fan creators who need permissions for commercial releases but lack bargaining leverage. License quotes and litigation exposure weigh heavily against thin margins; declining the terms usually means abandoning the project outright.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, independent_commercial_creators, payer,
    moderate, biographical, trapped, regional).

% Readers, viewers, and players who receive authorized adaptations through the licensing system. They enjoy the steady flow of sequels, translations, and remakes the system produces, and carry licensing costs indirectly where prices pass them through.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, downstream_content_consumers, beneficiary,
    organized, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, downstream_content_consumers, payer).

% Statutory drafters and judges who define and police where the commercial line sits — interpreting what counts as transformative, what counts as commercial, and which uses require authorization. Treaty commitments constrain how far the line can move in either direction.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Creators in regions where licensing markets and collecting infrastructure barely reach. Formal permission routes are effectively closed to them; commercial-scale production proceeds informally or not at all, and no negotiation table represents their interests in rates or boundary-drawing.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, global_south_creators, excluded,
    powerless, biographical, trapped, continental).

% Scholars and commentators who map how the boundary operates across cases, document its drift, and supply the doctrinal analysis courts and legislators cite. They hold no position in fee flows and can describe the whole structure from outside it.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_law_academics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the authorization-routing problem for derivative creation: a single categorical rule tells creators and rights holders which transformations require a transaction (commercial ones) and which do not (non-commercial ones), sustaining a functioning licensing market for commercial adaptation while keeping non-commercial transformation outside the transaction system entirely.
% TRANSFER_FUNCTION: Moves licensing fees, advances, and negotiated royalties from commercial developers of derivative works to copyright holders, with intermediaries retaining commissions on volume; moves legal risk asymmetrically onto small commercial actors who cannot price or predict enforcement.
% ABSENT_VOICES: Creators in licensing-sparse regions and informal remix economies never sit at licensing tables — their commercial-scale cultural production is either chilled or pushed informal, and no seat represents them in rate-setting or boundary-drawing. Audiences likewise have no voice in how licensing costs pass through to prices.
% DISAPPEARANCE_RATIONALE: If the commercial carve-out vanished overnight, one of the rival boundary criteria would fill the vacuum: either all transformative use comes to require authorization (the non-commercial commons closes and fan and academic remixing must be litigated into existence) or the line redraws around degree of incorporation (the commercial licensing market collapses to literal recastings). Either successor rearranges publishing pipelines, platform monetization policy, and fan-economy practice.
% FOUNDING_PROBLEM: Nineteenth-century unauthorized commercial exploitation of authors' works — stage dramatizations and foreign translations profiting publishers while authors received nothing — which the derivative-work right was built to stop by giving authors control over commercial recastings of their expression.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: pre-Berne historical records of unauthorized dramatization and translation markets, modern infringement dockets documenting continued unauthorized commercial adaptation, and independent economic scholarship measuring licensing-market volumes — none of these sources depends on licensing revenue. No credible source attests the founding problem dead.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.58 at interval end): commercial transformers pay real fees that are decoupled from the marginal cost of clearing rights, but the exempt zone preserves a large non-chargeable space, capping total extraction below enclosure-style regimes. Suppression (0.60) reflects the active machinery needed to hold the commercial line — statutory damages, takedown regimes, platform-level automated filtering — while leaving the non-commercial zone formally open; suppression is authored as a raw structural property and is not scaled by power or scope, unlike extractiveness. Theater ratio (0.32) is moderate-low: licensing transactions remain functional, but anti-piracy campaigns and automated content-matching sweeps contribute a growing performative component with substantial false-positive volume. Accessibility collapse (0.45) is well below mountain territory: alternatives persist everywhere — original creation, open-licensed and public-domain source material, jurisdictional shopping — so understanding the boundary does not close the option set. Resistance (0.55) is sustained: fair-use advocacy, free-culture litigation, and platform policy fights continuously contest the line's reach. All three tracked series run on one shared six-point grid; the common drivers are platform monetization expanding the chargeable surface (extraction accumulation) and automated enforcement scaling up (rising suppression requirement, with theater rising via indiscriminate sweeps). The dispersed independent-creator seat resists coalition formation — competitors, anonymous, jurisdictionally scattered — which keeps that seat weak despite its numbers.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the copyright holder's seat the arrangement is a functioning market it administers and staffs; from the independent commercial creator's seat it is a toll gate with unpredictable pricing and existential downside. Major licensees straddle the divide: their catalogs make them simultaneous payers and collectors, so their experienced burden sits far below what the payer role alone suggests — the divergence is captured by the directionality override documented below. Non-commercial creators experience near-zero imposition until a monetization ambition trips the line, at which point their seat collapses toward the independent-payer position. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (copyright_holders, licensing_intermediaries, non_commercial_creators) derive directionality near the subsidized end; payers (major_licensees, independent_commercial_creators) derive near the full-target end; consumers sit near symmetric via their dual declaration. One override is authored: power_atom 'powerful' → d 0.6. The derivation from the payer declaration alone would place major_licensees near full-target, but the story's structural fact is that they simultaneously operate as major licensors — cross-licensing pools and catalog ownership mean fee flows cross in both directions and their net extraction exposure is materially below the derived value. The 'moderate' atom is deliberately left untouched because two opposite-position agents share it (non_commercial_creators as beneficiary, independent_commercial_creators as payer) and their derived directionalities are correct; the monetization-margin ambiguity for non-commercial creators is routed to the commerciality_margin_blur omega rather than distorted through a shared-atom override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards both mislabeling directions. Reading the carve-out as pure coordination ignores the categorical asymmetry — costs concentrate on the commercial side and fees float above clearance cost — so the extraction component must stay visible in the type. Reading it as pure extraction ignores the genuine coordination: a working authorization market, a preserved non-commercial commons, and persistent alternatives for every payer short of abolition. The founding problem (unpaid commercial exploitation of authors' expression) remains live and independently corroborated, so no mandatrophy resolution is declared; the arrangement still performs its founding function while carrying a growing extraction load on its commercial half.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint is one reading of kernel derivative_work_statutory_boundary (reading: hybrid_carveout_reading); what would each sibling reading change structurally?',
    'Compare the compiled sibling stories derivative_work_statutory_boundary__enclosure_reading and derivative_work_statutory_boundary__coordination_reading: the enclosure reading deletes the non-commercial exemption (victim set expands to all reusers, ε rises sharply); the coordination reading deletes the commercial trigger (licensing shrinks to substantial incorporations, ε falls).',
    'The disagreement is located in the boundary criterion itself — commerciality versus incorporation extent versus universality. Which criterion governs redistributes the victim set and moves ε across the rope/tangled_rope/snare range for the same statutory text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer structure: this story is the hybrid carve-out reading; siblings instantiate different constraints over the same kernel.').

omega_variable(
    commerciality_margin_blur,
    'Does the commercial/non-commercial criterion remain classifiable at the margin under platform monetization — ad-shared fan videos, crowdfunded novelizations, tip-jar fan artists?',
    'Track enforcement outcomes and platform policy for monetized-amateur cases; if tribunals increasingly treat any monetization as commercial per se, the exempt class contracts in practice.',
    'If the margin collapses, the carve-out degrades toward the enclosure reading for monetized amateurs: the effective victim set grows beyond the categorical design and the authored ε understates realized extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commerciality_margin_blur, conceptual, 'Whether the reading''s load-bearing commerciality line survives monetization-driven blurring.').

omega_variable(
    licensing_fee_cost_basis,
    'Do commercial-use licensing fees track the cost of clearing and administering rights, or do they extract rents above it?',
    'Independent economic analysis comparing observed license rates against comparable clearance and administration costs; disclosure of intermediary commission structures.',
    'A wide gap establishes the commercial side as rent collection riding on a real coordination function; a narrow gap supports treating most measured extraction as coordination cost and softens the tangled_rope reading toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_fee_cost_basis, empirical, 'Whether the fee level reflects service cost or market power over adaptation rights.').

omega_variable(
    chilling_spillover_internalization,
    'How much of the arrangement''s suppressive force on non-commercial creation is structural (enforcement exposure) versus internalized (self-censorship from classification anxiety that would persist even absent enforcement)?',
    'Natural experiments where carve-out enforcement relaxes in a domain: compare non-commercial output before and after; survey creator behavior under stated legal certainty.',
    'If internalized chilling dominates, effective suppression exceeds the structural measure and the exempt zone is smaller in practice than on paper — weakening the coordination half of the hybrid and pushing the computed type toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_spillover_internalization, empirical, 'Structural versus internalized suppression mechanism in the exempt zone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(deri_tr_t6, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(deri_tr_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(deri_be_t6, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 6, 0.49).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(deri_be_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(deri_su_t6, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement(deri_su_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(deri_su_t18, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the derivative work boundary' decomposes into three structurally distinct readings of one statutory kernel, per the ε-invariance principle: enclosure_reading (universal derivativity — highest ε, maximal victim set), coordination_reading (incorporation-extent line — lowest ε, minimal licensing surface), and this hybrid_carveout_reading (commerciality-keyed line — intermediate ε with a categorical beneficiary/victim split). Each is authored as its own ε-invariant story with its own metrics and stakeholders. This file links both siblings because the readings compete to govern the same statutory text and cite one another's cases; the upstream coordination reading supplies the transformative-use doctrine this reading's exempt zone rests on, while the enclosure reading supplies the enforcement-maximalist pressure this reading's commercial half absorbs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
