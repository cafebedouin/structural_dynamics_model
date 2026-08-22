% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Negotiated Collective Management of Shared Digital Infrastructure (Commons Reading)
 *   domain: political economy of technology / software engineering / intellectual property
 *
 * SUMMARY:
 *   This story instantiates the commons_reading of the
 *   software_control_legitimacy kernel: the claim that legitimate authority
 *   over shared digital infrastructure flows only from negotiated collective
 *   management — foundation charters, steering councils, RFC processes,
 *   contribution agreements — rather than from unilateral property or from an
 *   unconditional freedom claim. The standing arrangement under assessment is
 *   the existing commons-governance regime as this reading sees it, never the
 *   regime this reading would prefer. Per the epsilon-invariance principle,
 *   the kernel label decomposes into four structurally distinct constraints
 *   (one per reading); this file authors only the commons reading, and the
 *   sibling readings are separate files linked through
 *   network.affects_constraints. The claim/metric gap is deliberate:
 *   claimed_type is authored from structural analysis (genuine coordination
 *   function plus asymmetric extraction plus active enforcement), while the
 *   metrics are authored from descriptive observation of how the regime
 *   actually operates — the engine measures any divergence.
 *
 * KEY AGENTS:
 *   - - foundation_governance_bodies: agenda setter (institutional/constrained) — charters, hosts, and adjudicates the governance regime
 *   - - project_maintainers: enforcement layer with payer overlay (moderate/identity_locked) — daily merge authority, unpaid burden
 *   - - volunteer_contributor_communities: primary beneficiary with payer overlay (organized/identity_locked) — contribute labor, receive standing
 *   - - corporate_infrastructure_sponsors: beneficiary-payer hybrid (powerful/arbitrage) — fund and steer, retain exit
 *   - - downstream_users_and_integrators: diffuse beneficiary (organized/constrained) — consume stability they cannot cheaply produce
 *   - - proprietary_infrastructure_vendors: primary target (powerful/arbitrage) — denied unilateral control over shared code
 *   - - absolute_freedom_advocates: target with excluded voice (moderate/identity_locked) — premise ruled inadmissible inside governed projects
 *   - - governance_researchers: analytical observer — sees the full structure without a stake in any project
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.48).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.36).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Negotiated Collective Management of Shared Digital Infrastructure (Commons Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "political economy of technology / software engineering / intellectual property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '0be4956b-0000-4fd6-bc00-c992d389108f').
narrative_ontology:cs_kernel_codification('0be4956b-0000-4fd6-bc00-c992d389108f', distributed).
narrative_ontology:cs_authority_grounding('0be4956b-0000-4fd6-bc00-c992d389108f', practice).
narrative_ontology:cs_interpretation_layer_present('0be4956b-0000-4fd6-bc00-c992d389108f').
narrative_ontology:cs_reading_relation('0be4956b-0000-4fd6-bc00-c992d389108f', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('0be4956b-0000-4fd6-bc00-c992d389108f', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('0be4956b-0000-4fd6-bc00-c992d389108f', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('0be4956b-0000-4fd6-bc00-c992d389108f', foundational, control_requires_negotiated_mandate).
narrative_ontology:cs_axiom_status(control_requires_negotiated_mandate, holdable).
narrative_ontology:cs_axiom_grounding('0be4956b-0000-4fd6-bc00-c992d389108f', control_requires_negotiated_mandate, conventional).
narrative_ontology:cs_axiom('0be4956b-0000-4fd6-bc00-c992d389108f', secondary, shared_infrastructure_denies_unilateral_disposition).
narrative_ontology:cs_axiom_status(shared_infrastructure_denies_unilateral_disposition, holdable).
narrative_ontology:cs_axiom_grounding('0be4956b-0000-4fd6-bc00-c992d389108f', shared_infrastructure_denies_unilateral_disposition, deontological).
narrative_ontology:cs_reference_frame('0be4956b-0000-4fd6-bc00-c992d389108f', negotiated_multistakeholder_stewardship).
narrative_ontology:cs_drift_state('0be4956b-0000-4fd6-bc00-c992d389108f', platform_capture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0be4956b-0000-4fd6-bc00-c992d389108f', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, volunteer_contributor_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, corporate_infrastructure_sponsors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, downstream_users_and_integrators).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, proprietary_infrastructure_vendors).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_freedom_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, project_maintainers).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, volunteer_contributor_communities).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, corporate_infrastructure_sponsors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the umbrella foundations and steering councils that host shared infrastructure projects: they charter projects, adopt governance policy, hold trademarks and hosting infrastructure, and adjudicate disputes between contributors and sponsors. Their authority rests on charters the participating parties have accepted. Stepping aside would mean transferring stewardship to another body or dissolving the host.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, foundation_governance_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Hold merge authority on specific projects day to day: review contributions, set roadmaps within charter limits, enforce contribution and conduct rules, and carry responsibility for security response. Most are unpaid or underpaid for this work; stepping back means recruiting a successor or watching the project decay, and long tenure fuses personal reputation with the project's fate.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, project_maintainers, agenda_setter,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, project_maintainers, payer).

% Contribute code, documentation, review, and issue triage without payment, gaining a voice in project direction through the governance process. Some sign contribution agreements that assign or license their rights to the host foundation. Leaving usually means leaving a community and a reputation they spent years building.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, volunteer_contributor_communities, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, volunteer_contributor_communities, payer).

% Fund development and staff engineers onto shared projects because dependable common infrastructure is cheaper than building alone. They gain roadmap influence through governance seats, sponsored maintainer employment, and attendance capacity that smaller participants lack. They bear participation costs they do not fully control, and they can redirect engineers, run internal branches, or withdraw if decisions go against them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, corporate_infrastructure_sponsors, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, corporate_infrastructure_sponsors, payer).

% Build products and services on the governed infrastructure, receiving stability, coordinated security response, and compatibility they could not cheaply produce individually. They shape direction mainly through sponsorship or issue participation; switching away means rewriting integrations and re-auditing dependencies.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, downstream_users_and_integrators, beneficiary,
    organized, biographical, constrained, global).

% Sell software or hosted services and prefer sole discretion over the code they create or depend on: closed licensing, unilateral feature control, no obligation to share improvements. Regimes that require negotiated decision-making and shared improvements deny them that discretion. Their practical escape routes are building parallel closed stacks, withdrawing from governed projects, or relocating activity to jurisdictions and licenses outside the governance perimeter.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, proprietary_infrastructure_vendors, payer,
    powerful, generational, arbitrage, global).

% Hold that no one may legitimately restrict how software is run, studied, modified, or shared, and object to governance rules, obligation-bearing licenses, and contribution agreements as restrictions in themselves. They campaign, publish, occasionally litigate, and maintain their own uncompromising projects. Inside governed projects their premise is not admissible in decision-making, however much they participate in discussion.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_freedom_advocates, payer,
    moderate, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, absolute_freedom_advocates, excluded).

% Study how software governance actually operates: who attends meetings, who merges, whose objections alter outcomes, how sponsorship maps to influence. They publish analyses, advise foundations on charter design, and hold no stake in any particular project's direction.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, governance_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__commons_reading, corporate_infrastructure_sponsors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages shared digital infrastructure that no single party can safely own outright: merge rights, roadmap priorities, security response, and compatibility standards are decided through a negotiated process so that many unrelated parties can build on common code without fragmenting it or holding each other hostage.
% TRANSFER_FUNCTION: Moves decision authority over shared code away from any single actor — vendor or individual — and into negotiated bodies; moves contribution labor and governance attention from contributors and maintainers into process overhead; and moves durable influence toward participants with the resources to staff governance continuously.
% ABSENT_VOICES: Proprietary vendors and freedom absolutists are present but overruled or structurally inadmissible — their objections register as positions the process exists to channel, not as vetoes. Genuinely absent: future maintainers who will inherit today's decisions, non-participating users affected by roadmap changes, and contributors in regions without the time-zone, language, or employer backing to attend governance reliably.
% DISAPPEARANCE_RATIONALE: If negotiated governance vanished overnight, shared infrastructure would split between proprietary silos seized by whoever could fund continued maintenance and ungoverned codebases nobody could safely depend on; downstream industries would rearrange around duplicated effort, incompatible forks, and unpatched vulnerabilities within months.
% FOUNDING_PROBLEM: Shared software lacked any legitimate decision mechanism: single-vendor control made common code unsafe to depend on, while ownerless code had no one empowered to respond to security incidents or settle design disputes. The commons reading was built to answer who may decide, and by what process, when infrastructure belongs to everyone.
% FOUNDING_PROBLEM_CORROBORATION: Foundation charters and multi-stakeholder governance literature attest the founding problem from outside any single beneficiary; tellingly, proprietary vendors attest it too — they participate in governance they dislike rather than exiting entirely, which corroborates that the coordination problem is real even while they dispute the mandated solution. Academic political-economy studies of digital infrastructure provide independent corroboration.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48 reflects a regime that genuinely coordinates but steadily transfers value upward: contributor labor is converted into infrastructure that sponsors monetize, contribution agreements shift rights toward host foundations, and governance attendance capacity tracks corporate payroll rather than community size. The value is deliberately mid-range because the reading itself predicts variable epsilon depending on commons rules — trust-based charters sit lower, copyright-assignment regimes higher. Suppression 0.36 is moderate: forking remains a real exit, which caps coercive force, but codes-of-conduct enforcement, CLA terms, and maintainer gatekeeping coerce within projects, and the suppression series shows enforcement machinery hardening monotonically from informal 1998-era norms to formalized 2020s compliance infrastructure. Theater_ratio 0.32 captures a growing performative share — community consultations that ratify pre-negotiated corporate positions, diversity statements unaccompanied by seat redistribution — while core merge-review and security-response functions remain real. Accessibility_collapse 0.40: alternatives persist (forks, proprietary stacks, new projects), so the regime closes off options only partially. Resistance 0.50: sustained pushback arrives from both flanks — vendors lobbying against obligation-bearing governance and absolutists campaigning against governance as such. All three series run on one shared eight-point grid (1998–2026, four-year spacing) so no metric borrows another's endpoints; trajectories are monotonic rather than cyclical, driven by institutionalization and platform-era sponsorship rather than oscillating external shocks. End-state values match base_properties by construction.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute divergent types from identical structural data. From the foundation and maintainer seats the regime reads as legitimate stewardship they personally sustain; from the vendor seat it reads as expropriation of investment dressed as process; from the absolutist seat it reads as restriction illegitimate in kind, not degree; from the sponsor seat it reads as a useful framework worth funding precisely because it can be steered. The volunteer seat is the sharpest test: it collects genuine standing (beneficiary) while paying participation labor and rights assignment (payer overlay) — the engine's per-seat computation should surface this dual position rather than averaging it away.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for volunteer contributors, downstream users, and corporate sponsors; victim declarations drive high directionality for proprietary vendors and freedom absolutists. Exit modulation then differentiates within roles: sponsors' arbitrage-grade exit damps their effective extraction despite deep involvement, while the identity_locked exits of maintainers, volunteers, and absolutists amplify theirs — a maintainer cannot leave without abandoning a reputation fused with the project, and an absolutist cannot exit the ideology that constitutes their position. Two same-power seats deserve explicit note: corporate sponsors and proprietary vendors both hold powerful power atoms and both hold arbitrage exit, yet occupy opposite structural relationships — the derivation distinguishes them through role declarations, which is precisely why no directionality_overrides are authored here: an override keys on the power atom alone and would conflate the two seats into a single d value, destroying the distinction the structural data already encodes correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. Reading the regime as pure rope ignores the documented asymmetry: gains concentrate (receipt traced to the sponsor seat) while costs spread across unpaid maintainers and bound vendors. Reading it as a snare ignores the real coordination function — fragmentation prevention and coordinated security response that every seat, including the victims, continues to rely on — and the absence of a single capturing seat: sponsors capture as a class through parallel investment, not through administration of the regime itself. The R5 interview finds the founding problem live, so no mandatrophy declaration is authored; the mismatch consumer should nonetheless watch the (status x disappearance_verdict) cell, since a future finding that the founding problem is dead while the world still rearranges would flag zombie governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_design_epsilon_variance,
    'Is the authored epsilon of roughly 0.48 representative of commons governance generally, or does it swing widely with governance design (trust-based foundation charters versus copyright-assignment contribution agreements versus corporate-dominated steering councils)?',
    'Cross-project comparison: measure contributor attrition, decision-latency, and rights-assignment terms across differently designed governance regimes and correlate with participant-reported burden.',
    'If epsilon varies strongly by design, the classification is per-regime rather than per-reading, and poorly designed commons regimes would compute as snares while well-designed ones compute near rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_design_epsilon_variance, empirical, 'Whether the reading''s extraction load is a constant of commons governance or an artifact of particular governance designs.').

omega_variable(
    capture_drift_reversibility,
    'Is corporate concentration of de facto authority inside commons governance a correctable drift within the reading''s own ideals, or evidence that the negotiated-mandate ideal is unrealizable at infrastructure scale?',
    'Track whether governance reforms (seat caps, maintainer independence funding, asynchronous participation channels) measurably redistribute decision influence over successive charter revisions.',
    'If the drift is irreversible, this reading collapses toward the pragmatic-openness sibling (governance as managed method rather than legitimate mandate) and the victim set shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_drift_reversibility, conceptual, 'Whether the gap between the stewardship ideal and platform-era practice is reformable or structural.').

omega_variable(
    fork_exit_effectiveness,
    'How effective is forking as an exit for the parties this regime binds — particularly vendors denied unilateral control and communities rejecting governance decisions?',
    'Census of prominent forks: survival rates, ecosystem adoption, and whether the forked-away party retained the community or merely the code.',
    'If network effects make forks non-viable in practice, suppression is understated at 0.36 and the regime trends toward enforced extraction for its targets; if forks routinely succeed, the current suppression figure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fork_exit_effectiveness, empirical, 'Whether the exit option that keeps suppression moderate is real or nominal.').

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel software_control_legitimacy — the commons_reading. How would the structural picture change under the sibling readings (freedom_imperative_reading, pragmatic_openness_reading, property_rights_reading), and where exactly is the disagreement located?',
    'Author each sibling as its own constraint story; compare victim sets, beneficiary sets, and epsilon across the family. The disagreement is located in the source of legitimate control authority: negotiated mandate (this reading) versus user freedom versus development-methodology outcomes versus creator property right.',
    'Under the property-rights sibling, proprietary vendors move from victim set to beneficiary set and this reading''s governance mandates become the extraction; under the freedom-imperative sibling, governance itself is the target. Classification is reading-indexed; no single verdict spans the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame record: this story instantiates one reading of a four-way contested kernel; sibling readings are separate constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swc_commons_tr_t1998, software_control_legitimacy__commons_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(swc_commons_tr_t2002, software_control_legitimacy__commons_reading, theater_ratio, 2002, 0.16).
narrative_ontology:measurement(swc_commons_tr_t2006, software_control_legitimacy__commons_reading, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(swc_commons_tr_t2010, software_control_legitimacy__commons_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(swc_commons_tr_t2014, software_control_legitimacy__commons_reading, theater_ratio, 2014, 0.27).
narrative_ontology:measurement(swc_commons_tr_t2018, software_control_legitimacy__commons_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement(swc_commons_tr_t2022, software_control_legitimacy__commons_reading, theater_ratio, 2022, 0.31).
narrative_ontology:measurement(swc_commons_tr_t2026, software_control_legitimacy__commons_reading, theater_ratio, 2026, 0.32).

% Extraction over time
narrative_ontology:measurement(swc_commons_be_t1998, software_control_legitimacy__commons_reading, base_extractiveness, 1998, 0.3).
narrative_ontology:measurement(swc_commons_be_t2002, software_control_legitimacy__commons_reading, base_extractiveness, 2002, 0.33).
narrative_ontology:measurement(swc_commons_be_t2006, software_control_legitimacy__commons_reading, base_extractiveness, 2006, 0.37).
narrative_ontology:measurement(swc_commons_be_t2010, software_control_legitimacy__commons_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(swc_commons_be_t2014, software_control_legitimacy__commons_reading, base_extractiveness, 2014, 0.44).
narrative_ontology:measurement(swc_commons_be_t2018, software_control_legitimacy__commons_reading, base_extractiveness, 2018, 0.46).
narrative_ontology:measurement(swc_commons_be_t2022, software_control_legitimacy__commons_reading, base_extractiveness, 2022, 0.48).
narrative_ontology:measurement(swc_commons_be_t2026, software_control_legitimacy__commons_reading, base_extractiveness, 2026, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(swc_commons_su_t1998, software_control_legitimacy__commons_reading, suppression_requirement, 1998, 0.14).
narrative_ontology:measurement(swc_commons_su_t2002, software_control_legitimacy__commons_reading, suppression_requirement, 2002, 0.18).
narrative_ontology:measurement(swc_commons_su_t2006, software_control_legitimacy__commons_reading, suppression_requirement, 2006, 0.23).
narrative_ontology:measurement(swc_commons_su_t2010, software_control_legitimacy__commons_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(swc_commons_su_t2014, software_control_legitimacy__commons_reading, suppression_requirement, 2014, 0.31).
narrative_ontology:measurement(swc_commons_su_t2018, software_control_legitimacy__commons_reading, suppression_requirement, 2018, 0.34).
narrative_ontology:measurement(swc_commons_su_t2022, software_control_legitimacy__commons_reading, suppression_requirement, 2022, 0.36).
narrative_ontology:measurement(swc_commons_su_t2026, software_control_legitimacy__commons_reading, suppression_requirement, 2026, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'software control legitimacy' conflates four structurally distinct claims and is decomposed per the epsilon-invariance principle into a four-story constraint family sharing the kernel software_control_legitimacy. This file authors the commons_reading only: the standing arrangement under assessment is the existing negotiated-governance regime as the commons reading sees it, with its own epsilon (0.48), its own victim set (both absolutist flanks, denied governance participation), and its own beneficiary set (stakeholder communities). The pragmatic_openness sibling historically fed this reading's institutionalization (open-source pragmatism built the foundations), and this reading's mature governance institutions now exert downstream pressure back on the methodology-choice framing. Sibling files must reciprocate these edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
