% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant as Shared-Infrastructure Coordination Compact
 *   domain: economic/political
 *
 * SUMMARY:
 *   This story authors the coordination reading of the hoa_covenant_scope
 *   kernel as a clean, epsilon-invariant constraint: a recorded declaration
 *   under which lot owners pool assessments to maintain genuinely shared
 *   infrastructure (private roads, stormwater drainage, common areas) and
 *   resolve objective nuisances between adjacent lots. Enforcement is a
 *   narrow backstop — late fees and liens against withheld assessments —
 *   invoked episodically against a small fraction of households, while the
 *   load-bearing structure is the net benefit every owner receives from
 *   maintained shared assets. KEY AGENTS (by structural relationship): -
 *   all_homeowners: Symmetric primary beneficiary (organized/constrained) —
 *   pays assessments, receives maintained shared assets - hoa_board:
 *   Agenda-setter (organized/constrained) — administers contracts and budget,
 *   captures no personal gain - assessment_delinquent_owners: Episodic
 *   cost-bearer (moderate/constrained) — bears fees and lien risk during
 *   collection episodes - municipal_infrastructure_authority: Institutional
 *   observer (institutional/analytical) — tracks whether private coordination
 *   holds - prospective_homebuyers: Excluded voice (moderate/mobile) —
 *   inherits the covenant without negotiating it. This file is one member of
 *   a three-story constraint family; the sibling readings are separate
 *   constraints linked in network.affects_constraints, and the cross-reading
 *   comparison lives in commentary.kernel_context and the omega variables,
 *   not in this constraint's body.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.18).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.22).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant as Shared-Infrastructure Coordination Compact").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "economic/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, 'a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39').
narrative_ontology:cs_kernel_codification('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', fixed_text).
narrative_ontology:cs_authority_grounding('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', lineage).
narrative_ontology:cs_interpretation_layer_present('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39').
narrative_ontology:cs_reading_relation('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', foundational, assessment_authority_bounded_by_shared_cost).
narrative_ontology:cs_axiom_status(assessment_authority_bounded_by_shared_cost, holdable).
narrative_ontology:cs_axiom_grounding('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', assessment_authority_bounded_by_shared_cost, deontological).
narrative_ontology:cs_axiom('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', foundational, use_restrictions_require_demonstrable_externality).
narrative_ontology:cs_axiom_status(use_restrictions_require_demonstrable_externality, holdable).
narrative_ontology:cs_axiom_grounding('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', use_restrictions_require_demonstrable_externality, instrumental).
narrative_ontology:cs_reference_frame('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', shared_infrastructure_cost_pooling_compact).
narrative_ontology:cs_drift_state('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', contemporary_association_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1f0ffb2-80b8-41f4-a8a2-39ee3917fc39', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, assessment_delinquent_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own lots in the recorded subdivision, pay the annual assessment, and receive in return maintained roads, functioning stormwater drainage, and kept common areas. Vote on budgets and board seats at the annual meeting. Leaving means selling the lot, which transfers the covenant obligation to the buyer along with the maintained asset.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, biographical, constrained, local).

% Elected volunteer owners who solicit maintenance bids, sign contractor contracts, set the annual budget within the limits stated in the recorded declaration, and pursue delinquent accounts through the lien process the instrument specifies. Serve fixed terms and can be voted out; receive no compensation beyond their own standing as owners.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    organized, biographical, constrained, local).

% Owners who fall behind on assessments during job loss, illness, or billing disputes. Accumulate late fees and face lien filings on their lots under the collection policy; a small number lose lots to foreclosure. Bear the covenant's enforcement consequences, concentrated in a few households at any given time.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, assessment_delinquent_owners, payer,
    moderate, immediate, constrained, local).

% County and city public-works and planning staff who declined to accept the subdivision's private infrastructure into public maintenance, interact with the association on stormwater discharge compliance and emergency access, and watch whether private coordination keeps the roads and drainage off the public maintenance rolls.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, municipal_infrastructure_authority, observer,
    institutional, generational, analytical, regional).

% Households shopping in the area who encounter the declaration at contract and closing. They choose whether to enter the community but take no part in drafting or amending its rules; after purchase they stand in the same position as existing owners.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, prospective_homebuyers, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools periodic assessments to fund maintenance, repair, and replacement of genuinely shared assets — private roads, stormwater drainage, common-area landscaping, shared roofs and walls in attached products — that no individual owner can maintain alone, and adjudicates boundary and nuisance disputes between adjacent lots.
% TRANSFER_FUNCTION: Moves money: recurring assessments from each lot owner into a common operating and reserve fund, disbursed to maintenance contractors; secondarily moves collection costs (late fees, lien filings) onto owners who withhold their assessed share.
% ABSENT_VOICES: Renters occupying homes in the community use the commons and live under the rules but hold no vote; prospective buyers inherit amended covenants they never negotiated; future owners will absorb today's reserve-underfunding decisions. All sit outside the board meeting where scope is actually set.
% DISAPPEARANCE_RATIONALE: Without the pooled fund and maintenance schedule, private roads go unrepaired, stormwater systems fail and flood lower lots, common areas degrade, and each owner faces the classic underprovision problem individually — the coordination problem the covenant solves reasserts itself immediately, and lot values adjust downward.
% FOUNDING_PROBLEM: Developer-built subdivisions containing infrastructure the municipality refused to accept into public maintenance (private roads, stormwater systems) needed a durable vehicle to fund upkeep and stop cross-lot externalities from degrading the shared asset base after the developer sold out.
% FOUNDING_PROBLEM_CORROBORATION: County public-works refusal letters declining to accept the private roads, third-party engineering reserve studies documenting continuing capital needs, and state planned-community statutes all attest, from outside the benefiting parties, that the underlying maintenance-funding problem remains real; no source outside the beneficiary set attests that the problem is solved.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__coordination_reading_tests).
:- end_tests(hoa_covenant_scope__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the assessment is calibrated to budgeted maintenance and reserve costs and the proceeds buy services owners cannot cheaply procure individually; the gentle upward drift in the measurement series reflects fee creep and reserve accumulation, not rent design. Suppression is low (0.22) and deliberately NOT scaled here — it is a raw structural property — because the enforcement machinery is a rarely-fired backstop rather than the load-bearing wall: compliance rests on net benefit, which is why requires_active_enforcement is authored false even though lien machinery exists. Theater ratio is low (0.15): budget hearings, bid solicitation, and reserve studies are functional activity; a thin layer of procedural ritual accumulates as boards mature. Accessibility collapse is moderate (0.45): alternatives (municipal absorption of roads, dissolution votes, informal neighbor agreements) remain workable but carry real friction, and post-purchase exit means selling the home. Resistance is low-moderate (0.30): assessment disputes and amendment fights occur but do not threaten the arrangement. The claimed_type (rope) is authored from my structural belief about this reading; the metrics are authored from what I judge descriptively true — the two were composed independently. All temporal series share one grid (t=0,6,12,18,24,30) so every tracked metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the median owner's position the arrangement is a service exchange: money out, maintained assets back, vote included. From the delinquent owner's position the same structure presents as fees compounding on a lot they cannot easily liquidate — the entire coercive surface of the covenant concentrates there. From the board's position it is stewardship under fiduciary constraint. The engine computes these per-seat classifications from the structural data (power, exit, role); this commentary explains why they diverge without adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are declared beneficiaries and sit symmetrically near the beneficiary end of directionality — the covenant subsidizes each of them with maintained shared assets in proportion to what they pay in. The board derives near-beneficiary directionality as well: it administers the flow but captures none of it personally, and its members are themselves paying owners. Assessment-delinquent owners are intentionally NOT declared victims: under this reading their cost-bearing is episodic recovery of a genuinely owed share, not structural extraction, so the derivation correctly leaves them near-neutral rather than pushing them to the target end. The municipality and prospective buyers are peripheral seats with negligible directional pull. If the assessment-cost-coverage audit (see omegas) found systematic surpluses, delinquent owners would acquire victim status and the whole directional structure would tilt.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — municipally refused private infrastructure needing a funding vehicle — is still live, so mandatrophy is not resolved and no sunset applies. The classification matters in both directions here: ignoring the coordination function would let the lien machinery masquerade as snare evidence, while ignoring the episodic harm to delinquent households would let the rope label launder real coercive incidents. The assessment-cost-coverage omega is the tripwire: if assessments systematically outrun costs, the mandate persists but the arrangement has begun extracting around it, and the classification should migrate toward tangled_rope with delinquent owners as victims. The slow rise in base_extractiveness across the interval is exactly the signal that omega watches.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hoa_kernel_reading_instantiation,
    'This constraint instantiates the coordination_reading of the hoa_covenant_scope kernel; what structural differences would govern if either sibling reading (behavioral_control_reading, extraction_reading) were the operative constraint instead?',
    'Cross-read the three sibling stories'' epsilon, victim sets, enforcement scope, and gain_flow: behavioral_control_reading widens enforcement to subjective aesthetics and creates dissenting_owner victims with elevated suppression; extraction_reading concentrates gains in the board and fine apparatus (named-seat gain_flow, snare-flavored profile).',
    'If a sibling reading governs, the claimed type shifts (tangled_rope or snare candidates), victim sets appear where this reading declares none, and gain_flow moves from diffuse to a captured seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hoa_kernel_reading_instantiation, conceptual, 'Committer structure: one reading of the hoa_covenant_scope kernel; sibling readings are separate constraints, not parts of this one.').

omega_variable(
    enforcement_scope_disagreement_location,
    'Where exactly do the three readings disagree — is the dispute located in enforcement scope (objective nuisance versus subjective aesthetics versus discretionary fines), in beneficiary symmetry, or in gain concentration?',
    'Clause-by-clause textual analysis of recorded declarations and amendment histories, mapped against doctrine on judicial deference to association rulemaking, assigning each contested clause to the reading whose scope it serves.',
    'If the dispute lives in enforcement scope, narrowing architectural-review authority collapses the sibling readings back toward this one; if it lives in gain structure, the dispute survives any scope narrowing and the extraction reading remains independently operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_scope_disagreement_location, conceptual, 'Locates the inter-reading disagreement in specific structural elements of the instrument.').

omega_variable(
    genuine_externality_boundary,
    'Which restrictions under this reading correct genuine externalities affecting shared assets, and which quietly impose majority taste (lawn standards, paint palettes, parking aesthetics) — and how much of the measured extractiveness is hidden preference extraction?',
    'Hedonic pricing studies isolating the market-value effect of specific restriction classes, controlling for community age, location, and housing-stock quality.',
    'If taste-only restrictions show no value protection or impose value penalties, effective extraction rises above the authored 0.18 and the rope tilts toward tangled_rope with dissenting owners as victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_externality_boundary, empirical, 'Boundary between externality correction and preference imposition inside the covenant''s restriction set.').

omega_variable(
    assessment_cost_coverage_audit,
    'Do assessments track actual maintenance and reserve costs, or do systematic surpluses accumulate as fee creep — and are delinquent owners therefore shirkers being legitimately recovered from, or partial victims of over-assessment?',
    'Independent audit comparing multi-year assessment revenue to contracted maintenance spend and third-party reserve-funding requirements.',
    'Systematic surplus would raise effective extraction on all owners and recast delinquent owners as victims, migrating the classification toward tangled_rope; tight cost coverage confirms the rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assessment_cost_coverage_audit, empirical, 'Whether the assessment level is cost-reflective or rent-bearing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hoa__tr_t6, hoa_covenant_scope__coordination_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__coordination_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(hoa__tr_t18, hoa_covenant_scope__coordination_reading, theater_ratio, 18, 0.12).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__coordination_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__coordination_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hoa__be_t6, hoa_covenant_scope__coordination_reading, base_extractiveness, 6, 0.12).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__coordination_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement(hoa__be_t18, hoa_covenant_scope__coordination_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__coordination_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__coordination_reading, base_extractiveness, 30, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hoa_covenant_scope__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'HOA covenant' decomposes into three structurally distinct claims per the epsilon-invariance principle, forming the hoa_covenant_scope constraint family. This file authors the coordination reading: lowest epsilon (~0.18), symmetric beneficiaries, no declared victims, narrow enforcement. The behavioral_control_reading authors the same instrument as an aesthetic-uniformity regime (higher suppression, dissenting owners as victims); the extraction_reading authors it as a revenue-and-power apparatus (concentrated gains, fine proliferation, selective enforcement). The coordination function is the historical substrate: the assessment and lien machinery built for maintenance is the platform on which behavioral rules and fine schedules were later layered, which is why this story sits upstream of both siblings in the network. Each story carries its own epsilon, stakeholders, and classification; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
