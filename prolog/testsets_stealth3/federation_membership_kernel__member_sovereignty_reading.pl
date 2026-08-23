% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member-State Sovereignty Reading: Welfare-Capacity Bounds on Free Movement
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   Treaty-guaranteed free movement is bounded in practice by nationally
 *   administered residence conditions: after a short unconditional window,
 *   continued residence requires sufficient resources, comprehensive sickness
 *   insurance, or worker or student status, and those who fail the tests
 *   become removable. This story instantiates ONE reading — the
 *   member_sovereignty_reading — of the contested kernel
 *   federation_membership_kernel, treated as a clean epsilon-invariant
 *   constraint: the standing arrangement under contest is the existing regime
 *   of bounded movement, and the integration_reading and
 *   welfare_coordination_reading are separate stories, not folded into this
 *   one. KEY AGENTS (by structural relationship): receiving_state_governments
 *   — agenda setter (institutional/constrained), administers and defends the
 *   conditions; receiving_state_welfare_institutions — primary beneficiary
 *   (institutional/constrained), collects the protected contribution base;
 *   receiving_state_taxpayers and domestic_low_wage_workers — secondary
 *   beneficiaries (moderate-organized/constrained), diffuse service
 *   protection and wage shielding; economically_inactive_migrants — primary
 *   target (powerless/trapped), fully excluded beyond the initial window;
 *   resource_constrained_jobseekers — target with a conversion path
 *   (powerless/constrained); sending_state_governments — paying side
 *   (institutional/constrained), bear selective demographic loss;
 *   european_commission and migrant_rights_litigators — observers
 *   (institutional-analytical), policing and litigating the boundary's
 *   legality; sending_state_civic_organizations — excluded voice
 *   (moderate/constrained).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.6).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.6).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member-State Sovereignty Reading: Welfare-Capacity Bounds on Free Movement").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'b3aa7e72-01c9-456c-941f-ff30a43c4e8a').
narrative_ontology:cs_kernel_codification('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', fixed_text).
narrative_ontology:cs_authority_grounding('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', lineage).
narrative_ontology:cs_interpretation_layer_present('b3aa7e72-01c9-456c-941f-ff30a43c4e8a').
narrative_ontology:cs_reading_relation('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', foundational, compatriot_priority_in_social_obligations).
narrative_ontology:cs_axiom_status(compatriot_priority_in_social_obligations, holdable).
narrative_ontology:cs_axiom_grounding('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', compatriot_priority_in_social_obligations, deontological).
narrative_ontology:cs_axiom('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', secondary, contributory_boundary_preserves_pool_solvency).
narrative_ontology:cs_axiom_status(contributory_boundary_preserves_pool_solvency, holdable).
narrative_ontology:cs_axiom_grounding('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', contributory_boundary_preserves_pool_solvency, empirically_contingent).
narrative_ontology:cs_reference_frame('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', nationally_bounded_solidarity_settlement).
narrative_ontology:cs_drift_state('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', contemporary_welfare_nationalist_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b3aa7e72-01c9-456c-941f-ff30a43c4e8a', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_taxpayers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, domestic_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, resource_constrained_jobseekers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and revise residence conditions — sufficient-resource thresholds, sickness-insurance requirements, registration and removal procedures — and defend them publicly as protection of national welfare systems. Bound by the treaty framework and court review: they can tighten conditions at the margin but cannot withdraw the underlying mobility guarantee. Answer electorally to publics that reward visible bordering of welfare access.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_governments, agenda_setter,
    institutional, biographical, constrained, national).

% Administer benefit systems whose budgets and caseloads the residence conditions shield. Run the eligibility checks that operationalize the conditions and process removal referrals. Collect the preserved contribution base and reduced contingent liability; bear the administrative cost of operating the gate.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_institutions, beneficiary,
    institutional, generational, constrained, national).

% Fund national welfare systems through taxation and contributions. The residence conditions limit the pool of potential claimants who have not contributed, which taxpayers experience as protection of services they finance. They carry no direct administrative burden and cannot individually opt out of either the taxes or the arrangement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_taxpayers, beneficiary,
    moderate, biographical, constrained, national).

% Work in labor segments where mobile workers compete. Labor-market protection elements — equal-treatment carve-outs, transitional controls during enlargements, enforcement against undeclared agency work — shield their wages and conditions. Their stake runs through sectoral bargaining structures rather than through the residence rules directly.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, domestic_low_wage_workers, beneficiary,
    organized, biographical, constrained, national).

% Reside or wish to reside in another member state without employment, student status, or documented self-sufficiency. Beyond an initial short window they lose lawful residence and become removable; access to benefits is barred. Realistic paths are returning to the state of nationality, accumulating qualifying resources, or remaining irregularly. They have no channel to contest the thresholds applied to them.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, continental).

% Move to seek work but arrive without savings that satisfy sufficient-resource tests. They may remain while showing a genuine chance of employment, but face verification, time limits, and benefit ineligibility during the search. Finding work converts their status and unlocks residence; failing returns them to the excluded category.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, resource_constrained_jobseekers, payer,
    powerless, immediate, constrained, continental).

% Govern states whose working-age residents emigrate under the arrangement's selective terms. Bear fiscal and demographic losses — depleted workforces, remittance dependence, regional depopulation — and hold little leverage over receiving-state residence rules beyond infringement procedures and council argument. Their own welfare systems lose contributors to the gatekeeping states.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_governments, payer,
    institutional, generational, constrained, national).

% Monitor member-state application of the mobility directive, open infringement proceedings where restrictions exceed what the treaty and directive permit, and publish citizenship reports assessing the balance between mobility rights and national competence. Hold no residence-rule pen of their own; operate through legal procedure and reporting.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, european_commission, observer,
    institutional, generational, analytical, continental).

% Bring test cases challenging expulsions, benefit bars, and discriminatory application of residence conditions before national courts and the union court. Funded by foundations and memberships; shape the doctrine through which conditions are judged proportionate or not, without holding any administrative role in the system they litigate.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, migrant_rights_litigators, observer,
    organized, biographical, analytical, continental).

% Regional associations, municipalities, and civil-society groups in high-emigration areas observing working-age depopulation and service decline. Petition national governments and union institutions for voice in mobility policy but hold no formal seat in the directive's governance; their objection registers mainly through the sending state's domestic politics.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_civic_organizations, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_institutions).
narrative_ontology:fixing_cost_class(federation_membership_kernel__member_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the actuarial integrity of nationally financed, largely contributory welfare pools by conditioning residence beyond a short initial window on sufficient resources, comprehensive sickness insurance, or worker or student status; dampens downward competitive pressure among member states with differently generous benefit systems.
% TRANSFER_FUNCTION: Moves residence security and welfare-pool access away from resource-poor mobile citizens toward receiving-state contributor bases; concentrates practical mobility among those who can document self-sufficiency; shifts the fiscal and demographic cost of immobility onto sending states.
% ABSENT_VOICES: Economically inactive migrants and rejected jobseekers have no seat in the council working groups and national ministries that draft residence conditions; sending-state civic organizations affected by selective depopulation lack standing in the directive's implementation; the judgment of what constitutes an undue burden on the host system is made by the very systems that would bear it, with the burdened party absent from the measurement.
% DISAPPEARANCE_RATIONALE: If residence conditions and removal powers vanished overnight, receiving-state welfare systems would face immediate accession claims from previously excluded residents, national politics would convulse around emergency fiscal measures, intra-union settlement patterns would redistribute toward the most generous systems, and sending states would see accelerated out-migration of precisely the currently gated populations.
% FOUNDING_PROBLEM: Reconcile a single market guaranteeing free movement with welfare states that are nationally financed and largely contributory: the 1970s abuse-of-free-movement debates and the safeguards attached to the 2004 enlargement both sought to stop mobile citizens from drawing on solidarity pools they had never paid into, without dismantling the mobility guarantee itself.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: union-level citizenship reports and the court's own case docket treat the mobility-solidarity tension as unresolved; comparative welfare-state scholarship documents the contributory-boundary trilemma independent of any member-state government; sending-state governments and migrant-rights litigators attest the asymmetry from the paying side. No party disputes that the founding problem exists — the dispute is over its solution, not its reality.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial but not total (0.60): the arrangement curtails a guaranteed right and excludes a defined population outright, yet most affected movers retain conditional access and a conversion path through employment, so the hardest exclusion concentrates on the inactive minority. Suppression (0.60) reflects the enforcement machinery — registration systems, resource verification, removal directives, benefit ineligibility — that the arrangement's persistence actively requires; it is a raw structural property, unscaled by power or scope, while the engine scales only extractiveness. Theater (0.32) is moderate-low but rising: the gatekeeping function is real, while a growing share of enforcement activity and public justification defends against an 'abuse' whose measured incidence has repeatedly come in below the rhetoric. Accessibility collapse is mid-range (0.45): understanding the conditions does not eliminate alternatives — converting to worker status, accumulating resources, or returning remain available — so this is not a natural-law profile. Resistance (0.55) is sustained and organized: litigation, infringement proceedings, and sending-state objection meet the arrangement continuously. All three tracked series share one six-point grid (t=0..30, roughly Maastricht-era to present), so every metric is authored at every examined time point; the trajectories show extraction accumulation and enforcement hardening alongside rhetorical inflation, with no oscillation requiring cycle modeling.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter and beneficiary seats the arrangement presents as stewardship: a necessary boundary that keeps contributory promises fundable and shields domestic labor segments. From the payer seats the identical structure presents as a gated right: a guarantee suspended at the point of need, administered by the party that benefits from its strictness. The engine derives these per-seat classifications from the structural data — power, exit, and declared position — and the divergence between them is the measurement this story exists to take; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the receiving-side seats: welfare institutions collect the preserved base directly, taxpayers and low-wage workers benefit diffusely through protected services and wages. Victim declarations drive high directionality for the paying seats: economically inactive migrants sit nearest the full-target end (trapped exit, no contest channel), jobseekers slightly less far (constrained exit via employment conversion), and sending-state governments far toward the target end despite institutional power, because their loss — contributors and working-age population — has no exit at all. The derivation chain handles these relationships from the declarations plus exit options; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — reconciling single-market mobility with nationally financed contributory solidarity — remains live, attested by every seat including the paying ones. The hybrid classification earns its keep by resisting two symmetric mislabels. Reading the arrangement as pure coordination ignores that its costs fall asymmetrically on a defined, voiceless population and that its persistence requires active enforcement against them. Reading it as pure extraction denies the genuine solvency problem that contributory welfare systems face under unconditional access — a problem corroborated outside the benefiting parties. The tangled-rope structure keeps both halves on the table and lets the omega variables (fiscal magnitude, gate breadth) arbitrate which half dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_bounding_authority_location,
    'This story instantiates the member_sovereignty_reading of the federation_membership_kernel: would the integration_reading or the welfare_coordination_reading relocate the authority to bound movement such that this arrangement''s victim set and enforcement structure change?',
    'Author the sibling readings as separate stories and compare computed classifications across the kernel family; observe treaty-amendment proposals and court doctrinal shifts to see which reading captures the operative framework.',
    'Under the integration_reading the excluded-migrant victim set contracts sharply and member-state discretion itself becomes the deviation; under the welfare_coordination_reading exclusion softens into floor-coordinated access and measured extraction falls.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_bounding_authority_location, conceptual, 'Kernel-level ambiguity over which reading of the mobility commitment governs the bounding of movement.').

omega_variable(
    welfare_migration_fiscal_magnitude,
    'Is cross-border welfare claiming by mobile union citizens large enough to threaten receiving-state pool solvency, or is the bounding apparatus disproportionate to the fiscal risk?',
    'Administrative microdata linking residence histories to benefit receipts and contribution records across member states; natural experiments from transitional-control expirations after enlargements.',
    'A negligible fiscal threat thins the coordination half and pushes the arrangement toward pure exclusion; a material threat substantiates the pool-protection function and stabilizes the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_migration_fiscal_magnitude, empirical, 'Whether the solvency-protection coordination problem the arrangement solves is materially real.').

omega_variable(
    resources_gate_selectivity_breadth,
    'Does the sufficient-resources gate exclude only the economically inactive, or does it filter by wealth across the economically active as well?',
    'Compare rejection and removal statistics by economic status and asset holdings; track conversion rates of jobseekers to worker status under the genuine-chance test.',
    'If filtering is wealth-wide, the victim set broadens beyond the inactive and effective extraction rises above the authored estimate; if narrow, the current victim declarations stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resources_gate_selectivity_breadth, empirical, 'Breadth of the population actually excluded by the resource and insurance conditions.').

omega_variable(
    selective_drain_intensification,
    'Does bounding movement to the resource-holding intensify sending-state skill depletion relative to an unbounded regime, as the expected structural delta for this reading asserts?',
    'Occupational and educational composition of migration flows under restriction versus pre-restriction baselines in high-emission sending regions.',
    'Confirmed selectivity adds a sending-state harm dimension captured here only through the sending-government seat; disconfirmation would soften that edge of the victim structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_drain_intensification, empirical, 'Whether restriction skews emigration toward the skilled and deepens sending-state losses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t6, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(fede_tr_t12, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(fede_tr_t18, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(fede_tr_t24, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(fede_tr_t30, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t6, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(fede_be_t12, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(fede_be_t18, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(fede_be_t24, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(fede_be_t30, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fede_su_t6, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(fede_su_t12, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(fede_su_t18, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 18, 0.56).
narrative_ontology:measurement(fede_su_t24, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(fede_su_t30, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'free movement in the federation.' The label conflates three structurally distinct claims with different epsilon values, victim sets, and enforcement loci: (1) integration_reading — mobility as constitutive right, expansively interpreted; (2) member_sovereignty_reading (this story) — mobility bounded by national welfare capacity, with the excluded-migrant and sending-state victim structure; (3) welfare_coordination_reading — mobility managed through coordinated floors rather than unilateral exclusion. This reading sits upstream of the coordination reading in practice: unilateral restrictionism generates the demand for coordination instruments. Each story links the others via network edges; epsilon is stable within each because each refers to a distinct standing arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
