% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Rights Overlay on UNCLOS EEZ Boundaries
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates the historical-rights reading of the UNCLOS
 *   sovereignty kernel: the claim that a state's pre-treaty pattern of usage
 *   and occupation generates sovereign rights that predate and override the
 *   exclusive economic zone entitlements UNCLOS assigns to coastal states.
 *   The reading is authored from its own internal logic — continuity of
 *   historical use as the ground of sovereignty — assessed at the standing
 *   arrangement it currently produces: patrol-enforced, licensing-backed
 *   control over waters that overlap neighboring states' undisputed UNCLOS
 *   EEZs. This is not a story about whether the claim is legally correct
 *   (arbitral bodies have ruled it is not); it is a story about the
 *   constraint the claim's assertion and enforcement actually produce for the
 *   parties on the water. Sibling readings — strict_eez_reading (EEZ
 *   boundaries as exclusive per UNCLOS Article 57) and
 *   non_ratifier_enforcement_reading (freedom of navigation as customary law
 *   enforced by naval presence) — are separate constraints with separate ε
 *   values, linked via network.affects_constraints, not folded into this one.
 *
 * KEY AGENTS:
 *   - expansive_claimant_state: agenda_setter (institutional/arbitrage) — asserts and enforces the historical claim
 *   - claimant_state_fishing_fleets and resource_extraction_firms: beneficiaries (organized/mobile) — gain access licensed under the claim
 *   - adjacent_coastal_states: payer (institutional/constrained) — lose exclusive UNCLOS-conforming control
 *   - coastal_state_artisanal_fishing_communities: payer (powerless/trapped) — bear the sharpest immediate cost
 *   - regional_shipping_and_navigation_interests: payer (powerful/constrained) — face elevated transit friction
 *   - international_arbitral_and_treaty_bodies: excluded (institutional/analytical) — ruled against the claim but cannot enforce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.71).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.62).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Overlay on UNCLOS EEZ Boundaries").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1').
narrative_ontology:cs_kernel_codification('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', fixed_text).
narrative_ontology:cs_authority_grounding('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', distributed).
narrative_ontology:cs_reading_relation('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', foundational, prior_occupation_generates_sovereign_title).
narrative_ontology:cs_axiom_status(prior_occupation_generates_sovereign_title, holdable).
narrative_ontology:cs_axiom_grounding('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', prior_occupation_generates_sovereign_title, conventional).
narrative_ontology:cs_axiom('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', secondary, treaty_codification_cannot_extinguish_pre_existing_title).
narrative_ontology:cs_axiom_status(treaty_codification_cannot_extinguish_pre_existing_title, holdable).
narrative_ontology:cs_axiom_grounding('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', treaty_codification_cannot_extinguish_pre_existing_title, deontological).
narrative_ontology:cs_reference_frame('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', pre_unclos_customary_occupation_order).
narrative_ontology:cs_drift_state('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', post_2016_arbitration_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('87f6039f-a8e8-48fd-b6c7-fb4ef77b6aa1', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_fishing_fleets).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_resource_extraction_firms).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, adjacent_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, coastal_state_artisanal_fishing_communities).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, coastal_state_offshore_energy_developers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, regional_shipping_and_navigation_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts a historical usage and occupation claim (citing fishing grounds, historical maps, or pre-colonial administration) that overlaps and overrides neighboring states' UNCLOS-declared 200nm EEZs. Backs the claim with coast guard patrols, artificial island construction, and administrative acts (permits, licensing) inside the disputed zone. Frames the claim as a restoration of a pre-existing sovereign right rather than a new assertion, which lets it reject UNCLOS arbitral rulings as inapplicable to a right that predates the treaty.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Operate inside the contested zone under the claimant state's escort and licensing regime, gaining access to fishing grounds that would otherwise fall inside a neighboring state's exclusive zone. Their operations are the on-the-water instantiation of the historical claim and depend on continued state enforcement to keep the zone open to them.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_fishing_fleets, beneficiary,
    organized, biographical, mobile, regional).

% Hold or seek exploration and drilling licenses issued by the claimant state inside the overlapping zone, betting that state backing and prolonged occupation will convert into durable extraction rights regardless of arbitral outcomes.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_resource_extraction_firms, beneficiary,
    organized, generational, mobile, regional).

% Hold UNCLOS-conforming EEZ declarations over the same waters and have in some cases won favorable arbitral rulings, but lack the naval or economic power to compel the claimant state to withdraw. Exit options are limited to diplomatic protest, coalition-building with external powers, or accepting de facto loss of exclusive control over the resources and traffic in the overlapping zone.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, adjacent_coastal_states, payer,
    institutional, generational, constrained, regional).

% Depend on nearshore and shelf waters now patrolled and periodically closed by the claimant state's coast guard. Face harassment, confiscation of catch, or exclusion from traditional fishing grounds with no capacity to contest the claim directly; their livelihood is the most immediate transfer point of the arrangement.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, coastal_state_artisanal_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Hold licenses from the coastal state to explore or drill within its UNCLOS EEZ, but face vessel interdiction, cable-cutting, or survey-ship harassment inside the disputed overlap, which raises insurance costs and can halt projects outright despite the underlying legal entitlement being unambiguous under strict EEZ reading.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, coastal_state_offshore_energy_developers, payer,
    moderate, biographical, constrained, regional).

% Commercial shipping and naval transit rely on predictable, rules-based passage through the contested corridor. The historical-rights overlay increases friction: claimant-state patrol vessels intercept, warn off, or shadow transiting ships, raising transit risk and insurance premiums even though passage itself is not always physically blocked.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, regional_shipping_and_navigation_interests, payer,
    powerful, immediate, constrained, global).

% Have in relevant cases ruled that historical-rights claims of this kind have no legal basis under UNCLOS once a state has ratified the treaty, but possess no enforcement mechanism of their own. The claimant state's core move is to treat the ruling as advisory rather than binding on a right it holds to predate the treaty, effectively excluding the tribunal's authority from the operative dispute.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_arbitral_and_treaty_bodies, excluded,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_state).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its own terms, the reading coordinates continuity between pre-treaty patterns of use (fishing grounds, historical administration, ancestral navigation) and present-day sovereignty, protecting long-standing communities and practices from being erased by a treaty boundary drawn without reference to that history.
% TRANSFER_FUNCTION: Moves de facto control over fishing grounds, seabed resources, and transit conditions from the UNCLOS-declared coastal state and its населения (fishers, developers) to the expansive claimant state and the fleets/firms it licenses inside the overlapping zone.
% ABSENT_VOICES: International arbitral bodies have ruled on the legal question but have no seat at the water's edge where enforcement actually happens; artisanal fishing communities inside the disputed zone are rarely consulted by either state and bear the sharpest, most immediate costs.
% DISAPPEARANCE_RATIONALE: If the historical-rights claim were fully withdrawn, the overlapping zone would revert to the adjacent coastal states' undisputed UNCLOS EEZs: their fishing communities would regain uncontested access, offshore energy licenses would proceed without patrol interference, and shipping lanes would see materially reduced naval friction. The claimant state's fishing fleets and extraction firms would lose access they currently hold only by virtue of the claim and its enforcement.
% FOUNDING_PROBLEM: The claim was built to preserve or restore access and control that the claimant state asserts existed through historical fishing, navigation, and administrative practice prior to UNCLOS codifying 200nm EEZs in 1982 — the founding grievance is that a treaty boundary was drawn without regard to pre-existing patterns of use.
% FOUNDING_PROBLEM_CORROBORATION: The claimant state's own historical and cartographic institutes attest to pre-treaty usage; this is corroborated only partially and contestedly by independent historians and by the 2016 Permanent Court of Arbitration ruling (South China Sea Arbitration), which found no legal basis for historical rights claims superseding UNCLOS entitlements once a state is a treaty party — a corroboration from outside the claimant state that runs counter to, not in support of, the claimant's genealogy.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high-but-not-maximal (0.71 at interval end) because the claim does deliver a genuine, if contested, coordination good from the claimant's perspective — continuity of historically rooted access for its fishing and extraction sectors — while simultaneously transferring exclusive control away from the UNCLOS-declared coastal states. Suppression is substantial (0.62) and rising, tracking the escalation from cartographic assertion to coast guard patrols, licensing, and artificial-island construction — the claim's persistence depends on this enforcement infrastructure, not on voluntary recognition by the affected states. Theater ratio is moderate (0.4): a meaningful share of the enforcement activity (historical-institute publications, symbolic patrols, commemorative administrative acts) functions to perform continuity of claim for domestic and international audiences, while the harder infrastructure (coast guard interdiction, licensing enforcement) does the actual work of control.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant state's seat, the arrangement computes as legitimate restoration of sovereign continuity — a rope, in its own telling. From the adjacent coastal states' and artisanal fishing communities' seats, the same patrol-and-licensing structure computes as enforced extraction backed by an unrecognized legal theory. The engine should compute these divergently from the structural data (power, exit, beneficiary/victim declarations) without either side's self-description determining the verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The expansive claimant state is the structural agenda-setter and, through its licensed fleets and firms, the chief beneficiary — d sits near the beneficiary end because the arrangement subsidizes their access at others' expense. Adjacent coastal states and their fishing/energy sectors are targets: their UNCLOS entitlement is undisputed under the sibling strict_eez_reading, so from the historical-rights reading's own vantage they are the ones whose expected control is overridden, and their exit options are constrained (diplomatic and legal recourse without matching enforcement capacity). Artisanal fishing communities are trapped — the most powerless seat bearing the most concentrated cost. Shipping and navigation interests are powerful but constrained, since commercial and naval traffic cannot simply route around a claimed corridor without cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading asserts — that a treaty boundary erased pre-existing patterns of use — may have been genuinely live at the moment UNCLOS was concluded, when some claimant states were not treaty architects and had documented historical practices predating 1982. But the founding_problem_status is authored as contested rather than resolved: the corroboration comes from the claimant state's own historical institutes on one side, and from the Permanent Court of Arbitration's 2016 ruling on the other, which found the claim without legal basis for a ratifying state. This mismatch — a live self-asserted founding grievance against a dead-by-external-ruling legal status — is exactly the structure the R5 genealogy interview is designed to surface: the claim continues to be pressed and enforced well past the point where its own legal community (outside the claimant) recognizes it as settled against.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_continuity_evidentiary_basis,
    'Does the claimant state''s asserted historical usage and occupation meet the evidentiary bar international tribunals have applied to historical-rights claims (continuous, exclusive, and recognized exercise of authority), or is the historical narrative substantially retrospective construction layered onto a post-hoc strategic claim?',
    'Independent historical and archival review (not conducted by either the claimant or rival states) of pre-1982 fishing records, administrative acts, and third-party recognition; comparison against the evidentiary standard applied in prior arbitral rulings on historical rights.',
    'If the historical record is thin or contested, the reading''s coordination story (continuity of use) collapses to pure cover for a resource grab, pushing the classification toward snare; if the record is genuinely strong and was simply not anticipated by UNCLOS drafters, the coordination function is more substantial and the tangled_rope reading is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_continuity_evidentiary_basis, empirical, 'Whether the historical usage claim is evidentially well-founded or largely constructed after the fact.').

omega_variable(
    kernel_framing_which_reading_is_the_baseline,
    'Is the unclos_sovereignty_boundary kernel more accurately framed as UNCLOS-as-baseline (with historical rights as a contested overlay claim, this story''s framing) or as historical-occupation-as-baseline (with UNCLOS as a Western-drafted overlay that some states never fully consented to)?',
    'Examine ratification history, reservations filed at signature, and whether the claimant state was a treaty architect versus a non-participant or objector during UNCLOS negotiation (1973-1982).',
    'Under the UNCLOS-as-baseline framing (adopted here), the claimant state is the beneficiary and adjacent coastal states are victims. Under the alternative framing, the moral valence and possibly the beneficiary/victim assignment would need re-examination for this reading, though the structural extraction pattern (who controls the water) would remain the same regardless of which baseline is chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_which_reading_is_the_baseline, conceptual, 'Alternative framing of which legal order is the baseline against which the other appears as an overlay claim.').

omega_variable(
    enforcement_durability_vs_recognition,
    'Does sustained physical enforcement (patrols, artificial islands, licensing) without international legal recognition eventually convert into de facto sovereignty regardless of the arbitral ruling, or does the absence of recognition mean the arrangement remains permanently contested and reversible?',
    'Comparative study of historical territorial disputes where prolonged unrecognized occupation did or did not eventually gain international acceptance (effectivités doctrine in international law).',
    'If enforcement without recognition tends to solidify into accepted control over time, the current tangled_rope classification may drift toward a more stable extraction arrangement (approaching snare) as the interval extends; if recognition remains permanently withheld, the arrangement stays perpetually contested and reversible, supporting continued tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_durability_vs_recognition, empirical, 'Whether unrecognized but enforced occupation tends to solidify into accepted sovereignty over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uncl_tr_t8, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(uncl_tr_t16, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(uncl_tr_t32, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uncl_be_t8, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(uncl_be_t16, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(uncl_be_t32, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(uncl_su_t8, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(uncl_su_t16, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(uncl_su_t32, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__historical_rights_reading, 0.1).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the unclos_sovereignty_boundary kernel. strict_eez_reading inverts the beneficiary/victim structure (coastal state as beneficiary, claimant state's sectors as targets) and should show substantially lower extraction and suppression since it describes the treaty-conforming baseline rather than a contested overlay. non_ratifier_enforcement_reading shares this reading's departure from UNCLOS-as-sole-authority but grounds it in customary navigational law enforced by naval powers rather than historical occupation enforced by a claimant coastal state — its beneficiary set (blue-water naval powers) and enforcement mechanism (freedom-of-navigation operations) are structurally distinct from this reading's beneficiary set and enforcement mechanism (coast guard patrol and licensing). All three stories share ε-invariance individually but must not be averaged or reconciled into a single verdict on 'the UNCLOS dispute.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
