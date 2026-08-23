% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: Common-Interest Covenant as Shared-Infrastructure Cost-Sharing Compact
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   A recorded declaration binds every lot in a common-interest residential
 *   development to pooled funding of shared physical components and to
 *   objective cross-boundary standards. Under this reading the instrument's
 *   operation is dominated by reserve-funded roof, street, and drainage work,
 *   equal-share assessments, and enforcement confined to documented cost
 *   recovery and verifiable nuisance. The claim and the metrics are authored
 *   independently: claimed_type states the structure this reading holds true,
 *   and the metrics state descriptive operation as this reading measures it -
 *   the engine computes per-seat types, and any divergence is the datum the
 *   corpus exists to take. Family note: this story is one of three
 *   epsilon-invariant decompositions of the hoa_covenant_scope kernel; the
 *   siblings (hoa_covenant_scope__behavioral_control_reading,
 *   hoa_covenant_scope__extraction_reading) author different epsilon values
 *   over the same standing arrangement and are linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship):
 *   hoa_board_of_directors: agenda-setter (organized/constrained) -
 *   administers the compact and enforces its narrow scope while paying into
 *   it identically; all_hoa_homeowners: symmetric beneficiary-payer
 *   (moderate/constrained) - the collective principal;
 *   future_homebuyers_in_development: prospective beneficiary
 *   (moderate/arbitrage) - consents at priced entry;
 *   nonconforming_property_owner: concentrated enforcement target
 *   (moderate/constrained) - bears corrective costs;
 *   renter_residents_without_vote: excluded voice (powerless/mobile) -
 *   subject to rules without a vote; municipal_service_providers: secondary
 *   beneficiary (institutional/mobile) - receives privately financed
 *   infrastructure; urban_planning_analysts: analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.18).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.22).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "Common-Interest Covenant as Shared-Infrastructure Cost-Sharing Compact").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '3d2e0bcf-4016-410b-9f01-9102e9781f8b').
narrative_ontology:cs_kernel_codification('3d2e0bcf-4016-410b-9f01-9102e9781f8b', formalized).
narrative_ontology:cs_authority_grounding('3d2e0bcf-4016-410b-9f01-9102e9781f8b', lineage).
narrative_ontology:cs_interpretation_layer_present('3d2e0bcf-4016-410b-9f01-9102e9781f8b').
narrative_ontology:cs_reading_relation('3d2e0bcf-4016-410b-9f01-9102e9781f8b', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d2e0bcf-4016-410b-9f01-9102e9781f8b', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('3d2e0bcf-4016-410b-9f01-9102e9781f8b', foundational, covenants_legitimate_only_for_indivisible_shared_costs).
narrative_ontology:cs_axiom_status(covenants_legitimate_only_for_indivisible_shared_costs, holdable).
narrative_ontology:cs_axiom_grounding('3d2e0bcf-4016-410b-9f01-9102e9781f8b', covenants_legitimate_only_for_indivisible_shared_costs, deontological).
narrative_ontology:cs_axiom('3d2e0bcf-4016-410b-9f01-9102e9781f8b', secondary, objective_standards_suffice_for_enforcement).
narrative_ontology:cs_axiom_status(objective_standards_suffice_for_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('3d2e0bcf-4016-410b-9f01-9102e9781f8b', objective_standards_suffice_for_enforcement, instrumental).
narrative_ontology:cs_reference_frame('3d2e0bcf-4016-410b-9f01-9102e9781f8b', common_interest_cost_sharing_compact).
narrative_ontology:cs_drift_state('3d2e0bcf-4016-410b-9f01-9102e9781f8b', contemporary_enforcement_record, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('3d2e0bcf-4016-410b-9f01-9102e9781f8b', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_hoa_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, future_homebuyers_in_development).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, municipal_service_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, hoa_board_of_directors).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, all_hoa_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, nonconforming_property_owner).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, compulsory_cost_sharing_for_indivisible_common_elements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own lots encumbered by the recorded declaration. Pay periodic assessments into a common reserve and receive roof replacement, private-street resurfacing, stormwater upkeep, and common-area care that no single owner could procure alone. Vote in annual elections and can propose amendment or termination of the declaration through the supermajority threshold the document specifies. Selling the home ends the obligation, but nearly every comparable attached-unit purchase nearby carries a similar instrument.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_hoa_homeowners, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_hoa_homeowners, payer).

% Receive statutory disclosure of covenants, reserve balances, and fee schedules before closing and price them into the purchase decision. They acquire maintained shared components below the cost of procuring them privately; their cheapest exit is declining the purchase entirely, which makes their acceptance the lowest-friction consent in the arrangement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, future_homebuyers_in_development, beneficiary,
    moderate, biographical, arbitrage, national).

% Elected volunteer owners who obtain contractor bids, set the annual budget, collect assessments, and administer enforcement confined to shared-component repair funding and documented nuisances. They levy the charges on themselves equally with their neighbors, receive no compensation, and control no revenue stream beyond the reserve fund. Leaving the seat means selling their home like any other owner.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board_of_directors, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, hoa_board_of_directors, beneficiary).

% An owner cited for an objectively documented condition - a failed retaining wall discharging onto neighboring lots, uncontained refuse, an unpermitted structural modification spanning shared elements - who bears the concentrated cost of correction, contractor invoices, and possible lien placement until the condition is cured.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, nonconforming_property_owner, payer,
    moderate, biographical, constrained, local).

% Occupy homes inside the association boundaries under the same rules on noise, parking, refuse, and common-area use, but hold no vote in elections and no independent standing at meetings except through their landlord. Their exit is a lease cycle rather than a property sale.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, renter_residents_without_vote, excluded,
    powerless, immediate, mobile, national).

% The city or county whose crews do not maintain the development's private streets, sidewalks, and detention basins because the association funds them. They collect full property taxes on the parcels while the common elements remain privately financed, and they absorb or condemn failed private infrastructure when an association dissolves.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, municipal_service_providers, beneficiary,
    institutional, generational, mobile, regional).

% Researchers studying common-interest communities' fiscal structure, governance quality, and maintenance outcomes. They observe reserve adequacy, enforcement composition, and dissolution events from outside the arrangement and bear none of its assessments.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, urban_planning_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools each owner's share of costs for physically indivisible shared components - party-wall roofs, private streets, stormwater detention, common landscaping - that no owner can repair or replace alone, and settles cross-lot externalities such as drainage discharge, noise transmission, and refuse containment that bilateral bargaining between adjacent owners does not reliably resolve.
% TRANSFER_FUNCTION: Moves scheduled assessments from every lot owner into a common reserve and moves spending authority over shared components to an elected board that contracts the work; targeted enforcement moves corrective costs to owners whose documented conditions generate the externalities.
% ABSENT_VOICES: Renters inside the boundaries live under the rules but vote nowhere; where mixed-use units exist, their tenants likewise lack standing. Minority owners repeatedly outvoted on budget priorities speak only at annual meetings. Future owners were absent when the declaration was drafted but consented at purchase under disclosure rules.
% DISAPPEARANCE_RATIONALE: Without pooled reserves, attached-unit roofs, private streets, and retention systems fail on staggered individual timelines with no procurement mechanism to replace them; drainage and refuse externalities revert to neighbor-on-neighbor litigation; insurers and lenders reprice or withdraw from the stock; municipalities inherit or condemn failed private infrastructure.
% FOUNDING_PROBLEM: When tract developers exited completed phases, owners of attached and common-element housing were left jointly responsible for components no individual household could maintain: roofs spanning multiple lots, private streets dedicated to neither municipality nor sole owner, shared drainage - plus recurring cross-lot nuisances with no arbitration mechanism between neighbors.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: municipal public-works departments attest the problem persists, tracking failed private-street takeovers and condemned associations; state legislatures maintain enabling statutes and reserve-disclosure laws on the same premise; court opinions upholding assessment liens cite the recorded maintenance obligation as the operative justification; and federal housing-finance eligibility standards treat functioning association reserves as underwriting requirements.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness sits low (0.18) because the dominant money flow recycles into indivisible shared goods the payers jointly own: the transfer buys roofs and street surfaces rather than accumulating in a captor's account, and the mild upward drift across the interval reflects reserve strain and administrative overhead rather than rent layering. Suppression (0.22) is real but narrow - liens for unpaid shares and citations for documented nuisances - and represents the coercive minimum any compulsory cost pool requires, not barrier construction; as a raw structural property it is authored unscaled, with directionality and scope amplification owned by the engine. Theater ratio is low (0.12): budget meetings, competitive bids, and inspections correspond to physical work orders, with slow growth from procedural formalization. Accessibility collapse (0.35) is partial: entry is priced openly under disclosure law, sale exits exist, and supermajority amendment remains legally available, so understanding the covenant does not strand participants. Resistance (0.30) appears as assessment disputes, recall petitions, and amendment campaigns rather than systemic opposition. All tracked metrics share one seven-point grid (T=0 through T=36); suppression_requirement is deliberately untracked because enforcement capacity is flat by design across the interval - the scalar carries that picture, and authoring a flat redundant series would add nothing.
 *
 * PERSPECTIVAL GAP:
 *   From the board seat the instrument is stewardship - budgets balanced, assets preserved, charges levied on the leviers themselves. From the nonconforming owner's seat it is concentrated corrective cost landing on one household for a condition the community documents and demands cured. From the renter's seat it is rule-subjection without voice. The engine computes divergent per-seat classifications from these structural positions and their differing exit options; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive the derivation: all_hoa_homeowners sit near-symmetric (they pay in and collectively receive the good), future_homebuyers_in_development sit near the beneficiary pole with arbitrage-priced entry consent, and municipal_service_providers are near-pure subsidy recipients. One directionality override corrects the board seat: the structural derivation reads administrators through their enforcement function and would place the agenda-setter well off the beneficiary pole, but this board levies charges on itself, accepts no compensation, and controls no revenue stream beyond the jointly owned reserve - d=0.25 pins it just off the beneficiary pole, and the story contains no other organized-power agent the override could distort. The nonconforming_property_owner derives a high d honestly: they concentrate the corrective burden even though the burden is fair-share in design, and their constrained exit keeps them near the target end despite the coordination frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live wherever attached housing stands, so no obsolescence gap opens: founding_problem_status=live paired with disappearance_verdict=world_rearranges keeps the mismatch flag dark and mandatrophy unresolved. The classification performs symmetrical protective work here: computing this as a snare would erase a genuine coordination good that resolves maintenance deadlocks bilateral bargaining cannot; computing it as an unquestionable natural fixture would blind the corpus to scope creep - fine proliferation, discretionary aesthetic expansion - which the omega variables track as this reading's actual characteristic failure mode rather than presuming it absent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexical_epsilon,
    'Is the low extractiveness authored here a property of the standing arrangement itself, or an artifact of the coordination lens applied to it?',
    'Read all three kernel-family stories against the same enforcement-action ledger: classify recorded actions by function (cost recovery / conformity / fine revenue) and compare each reading''s authored extractiveness against the observed functional composition.',
    'If ledger shares match the extraction_reading profile, this file''s epsilon misdescribes the world despite internal coherence and classification authority shifts to whichever reading tracks observed functional composition; if shares match this profile, the siblings'' higher epsilon values index lenses rather than the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_indexical_epsilon, conceptual, 'Reading-indexed epsilon versus arrangement-intrinsic epsilon for the hoa_covenant_scope kernel.').

omega_variable(
    enforcement_scope_stability,
    'Can enforcement remain confined to infrastructure cost recovery and objective nuisance over the long run, or does scope creep toward discretionary conformity occur structurally?',
    'Time-series audit of enforcement-action categories and architectural-committee citation counts across the interval, benchmarked against the narrow-scope baseline this reading assumes.',
    'Systematic creep relocates the effective constraint toward the behavioral reading''s territory and raises effective extractiveness; durable confinement confirms the rope profile and validates the narrow-scope axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_scope_stability, empirical, 'Whether the narrow enforcement scope this reading depends on holds under board incentive pressure.').

omega_variable(
    free_rider_or_rights_bearer,
    'Are owners resisting assessments correctly modeled as free riders being corrected, or as rights-bearers bearing imposed obligations?',
    'Test each contested assessment category against component indivisibility: obligations tied to genuinely unshareable physical components support the free-rider model; obligations detached from any physical interdependence between lots indicate imposed terms.',
    'A nontrivial imposed-obligation class would populate the victim set, breaking the empty-victim structure this reading rests on and pushing classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_or_rights_bearer, conceptual, 'Status of the assessment-resisting owner under the coordination frame.').

omega_variable(
    minority_amendment_access,
    'Does the supermajority amendment threshold give persistent minorities a usable channel, such that low measured resistance reflects genuine consent rather than contained dissent?',
    'Compare pass rates of minority-initiated amendment and board-recall motions against majority-initiated equivalents across the interval.',
    'Systematically blocked minority channels mean measured suppression understates dissent containment; effective suppression rises and the consent basis beneath the rope classification weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_amendment_access, empirical, 'Whether low resistance is consent or blocked-channel artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t6, hoa_covenant_scope__coordination_reading, theater_ratio, 6, 0.06).
narrative_ontology:measurement_basis(hoa__tr_t6, observed).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__coordination_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement_basis(hoa__tr_t12, observed).
narrative_ontology:measurement(hoa__tr_t18, hoa_covenant_scope__coordination_reading, theater_ratio, 18, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t18, observed).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__coordination_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement_basis(hoa__tr_t24, observed).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__coordination_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement_basis(hoa__tr_t30, observed).
narrative_ontology:measurement(hoa__tr_t36, hoa_covenant_scope__coordination_reading, theater_ratio, 36, 0.12).
narrative_ontology:measurement_basis(hoa__tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t6, hoa_covenant_scope__coordination_reading, base_extractiveness, 6, 0.13).
narrative_ontology:measurement_basis(hoa__be_t6, observed).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__coordination_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement_basis(hoa__be_t12, observed).
narrative_ontology:measurement(hoa__be_t18, hoa_covenant_scope__coordination_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement_basis(hoa__be_t18, observed).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__coordination_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement_basis(hoa__be_t24, observed).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__coordination_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement_basis(hoa__be_t30, observed).
narrative_ontology:measurement(hoa__be_t36, hoa_covenant_scope__coordination_reading, base_extractiveness, 36, 0.18).
narrative_ontology:measurement_basis(hoa__be_t36, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hoa_covenant_scope__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'HOA covenant' conflates three structurally distinct claims over one recorded instrument (epsilon-invariance decomposition following the BGS pattern). This file holds the coordination claim (low epsilon, symmetric benefit, narrow enforcement scope); behavioral_control_reading holds the aesthetic-conformity claim; extraction_reading holds the revenue-and-power-consolidation claim. Directionality within the family runs from this reading outward: the coordination claim is the historically prior, lender-validated core from which the other two readings borrow their cover legitimacy, so this story links to both siblings as dependents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__coordination_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
