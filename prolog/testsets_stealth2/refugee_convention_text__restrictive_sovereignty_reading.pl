% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention as Minimum Floor Permitting Maximum Sovereign Discretion (Restrictive Sovereignty Reading)
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The 1951 Convention and 1967 Protocol text functions as a contested
 *   kernel, and this story instantiates ONE reading of it: the restrictive
 *   sovereignty reading, under which the Convention is a minimum floor of
 *   state obligation beneath which lies a wide zone of reserved sovereign
 *   discretion. On this reading, well-founded fear is satisfied only by
 *   individualized proof of persecutor intent and targeting; particular
 *   social group reaches only immutable characteristics of which the state is
 *   aware; admissibility screening may occur far from territory; offshore
 *   processing is permissible; generalized violence and non-state persecution
 *   fall outside the threshold entirely. The epsilon referent is the standing
 *   arrangement this reading governs, the floor-plus-discretion adjudication
 *   regime as it actually operates, assessed by the reading's own lights: the
 *   reading registers the floor as binding and the discretion zone as
 *   legitimate reserve, so its reading-indexed epsilon is moderate even
 *   though the structural costs borne by excluded claimants are severe. The
 *   sibling readings (expansive humanitarian, procedural integrity) are
 *   separate constraints with their own epsilon values over the same
 *   referent; they are linked in the network, not folded into this story. KEY
 *   AGENTS (by structural relationship): destination_states (agenda-setter
 *   and principal beneficiary, institutional/arbitrage);
 *   immigration_enforcement_bureaucracies (enforcer and secondary
 *   beneficiary, institutional/constrained);
 *   restrictionist_political_movements (beneficiary, powerful/mobile);
 *   asylum_seekers_without_individualized_proof,
 *   civilians_fleeing_generalized_violence, targets_of_non_state_persecution
 *   (primary targets, powerless/trapped); host_transit_states (payer with
 *   partial offsets, moderate/constrained); unhcr_supervisory_mission and
 *   international_human_rights_bodies (observers, institutional/analytical);
 *   protected_refugee_diaspora (floor beneficiary, organized/constrained).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.42).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.7).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention as Minimum Floor Permitting Maximum Sovereign Discretion (Restrictive Sovereignty Reading)").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'c955bd9d-1a16-4ea3-b85d-d07e255bd5f0').
narrative_ontology:cs_kernel_codification('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', fixed_text).
narrative_ontology:cs_authority_grounding('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', lineage).
narrative_ontology:cs_interpretation_layer_present('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0').
narrative_ontology:cs_reading_relation('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', foundational, sovereign_discretion_above_minimum_floor).
narrative_ontology:cs_axiom_status(sovereign_discretion_above_minimum_floor, holdable).
narrative_ontology:cs_axiom_grounding('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', sovereign_discretion_above_minimum_floor, conventional).
narrative_ontology:cs_axiom('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', foundational, individualized_proof_gates_protection).
narrative_ontology:cs_axiom_status(individualized_proof_gates_protection, holdable).
narrative_ontology:cs_axiom_grounding('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', individualized_proof_gates_protection, conventional).
narrative_ontology:cs_reference_frame('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', postwar_bounded_reciprocity_compact).
narrative_ontology:cs_drift_state('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', contemporary_mass_displacement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c955bd9d-1a16-4ea3-b85d-d07e255bd5f0', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, destination_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, immigration_enforcement_bureaucracies).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, restrictionist_political_movements).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_without_individualized_proof).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, civilians_fleeing_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, targets_of_non_state_persecution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, protected_refugee_diaspora).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, host_transit_states).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, sovereign_discretion_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, individualized_persecution_threshold).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, psg_immutability_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified the Convention and its Protocol and administers the regime through domestic asylum statutes, admissibility rules, and case law. Sets how narrowly well-founded fear and particular social group are construed, where screening happens, and whether transfers to third countries occur. Keeps the wide band of policy above the treaty floor under national control: intake levels, detention, externalized processing. Bears the floor's binding core, since it cannot return a person who clears the individualized bar to the persecuting state. Leaving the treaty outright is diplomatically costly; adjusting interpretation, externalizing, and deterring arrival are cheap and always available.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, destination_states, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, destination_states, beneficiary).

% Operates interdiction at sea, airport liaison networks, carrier-sanction administration, detention estates, and accelerated admissibility screening. Budgets, staffing, and statutory powers have grown with each tightening of the arrival route. Career paths, promotion criteria, and institutional memory are built around interdiction and removal volume; a mandate reframed toward facilitating claims would dissolve those structures. Funding arrives through the same appropriations the enforcement perimeter justifies.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, immigration_enforcement_bureaucracies, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, immigration_enforcement_bureaucracies, beneficiary).

% Campaign on arrival numbers, boat incidents, and the perceived leniency of protection systems. Each salient incident converts into votes and donations; each tightening of admissibility is credited to the movement. The issue can be dropped and picked up as electoral salience shifts, so commitment to any particular instrument is shallow.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, restrictionist_political_movements, beneficiary,
    powerful, immediate, mobile, national).

% Fled targeted harm but cannot meet the standard of proof demanded: no documents, no named persecutor, testimony discounted in credibility findings. Rejected at admissibility or refused after abbreviated interview. Faces removal to the country fled, prolonged detention, or years of unresolved status. No alternative legal channel, whether labor, study, or family route, is reachable from their position.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_without_individualized_proof, payer,
    powerless, immediate, trapped, regional).

% Escaped civil war, siege, cartel and gang control, or indiscriminate bombardment. Under the individualized-persecution threshold their case is categorically barred however grave the danger, because no single persecutor targeted them personally. Encounters pushback at borders, offshore transfer, or return to the violence they fled.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, civilians_fleeing_generalized_violence, payer,
    powerless, immediate, trapped, regional).

% Persecuted by clans, militias, trafficking networks, or families in states unable or unwilling to protect them. The social-group category as construed here reaches only immutable characteristics the state is aware of, excluding gender-role, occupational, and kinship targeting. Their claims fail on group definition before individual circumstances are weighed.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, targets_of_non_state_persecution, payer,
    powerless, immediate, trapped, regional).

% Neighbors the crises and hosts protracted displaced populations for decades. Absorbs arrivals deflected by destination-state screening and receives readmissions under externalization arrangements. Receives humanitarian and development financing conditioned on containment. Cannot seal borders without destabilizing frontier regions, and the treaty floor shields the populations already hosted from forced return.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, host_transit_states, payer,
    moderate, generational, constrained, regional).

% Carries the treaty's supervisory mandate: publishes interpretive guidelines favoring broader group definitions and inclusion of generalized-violence flight, intervenes in individual determinations and offshore arrangements, reports on state practice. Has voice in doctrine but no vote in any national system and depends on voluntary state funding.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr_supervisory_mission, observer,
    institutional, generational, analytical, global).

% Regional courts and UN treaty bodies review individual cases and state practice, issuing findings that refoulement occurred or detention breached safeguards. Their conclusions press on the reading's edges, particularly offshore arrangements and chain-refoulement, but execution runs through state consent and varies by jurisdiction.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Cleared the individualized bar in an earlier period and holds status the floor protects from forced return. Organizes advocacy for broader readings and family reunification. Its own security depends on the floor remaining binding even as it contests everything above the floor.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, protected_refugee_diaspora, beneficiary,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, destination_states).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem among states: a common refugee definition and a binding non-return floor prevent a race to the bottom in which each state competes to deflect protection costs onto others, and give the smaller set of claimants who clear the individualized bar a durable, portable status. Even under this reading, the floor is real and reciprocally binding.
% TRANSFER_FUNCTION: Moves protection obligations and their costs away from destination states onto the claimants themselves (rejection, detention, return risk, years of limbo) and onto transit and host states (concentrated hosting burdens, readmission intake), while moving discretionary control over intake and doctrine to destination-state executives and enforcement agencies.
% ABSENT_VOICES: The excluded claimants themselves have no seat anywhere: not in treaty governance, not in domestic rule-making, not in admissibility design. UNHCR holds consultative voice without decisional standing; refugees in protracted host-state camps are rarely consulted on the doctrines that determine who joins them. The unanimity of restrictive doctrine among administering states partly reflects that no paying seat was ever in the room.
% DISAPPEARANCE_RATIONALE: If the floor-plus-discretion arrangement vanished overnight, border regimes would reorganize immediately: interdiction and offshore contracts would lapse or be replaced, externalization partners would renegotiate, camp systems and resettlement pipelines would reshuffle, and the contest between the sibling readings would move from adjudication to open treaty politics. Arrangements across at least four continents depend on it.
% FOUNDING_PROBLEM: Post-WWII mass displacement and the collapse of earlier passport and Nansen-regime arrangements left states needing a bounded, reciprocal commitment: protect the individually persecuted, defined narrowly and originally tied to pre-1951 European events, without accepting open-ended obligation toward everyone displaced.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the travaux preparatoires record and contemporaneous diplomatic correspondence, by historical scholarship on the convention's drafting (Hathaway, Betts, and successors), and by UNHCR Global Trends displacement reporting showing the underlying problem at record scale. Destination-state assertions that the founding bargain still governs are the interested claim; the historical and statistical record independently attests both the problem's origin and its persistence.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).
:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.42 as a reading-indexed value over the fixed referent: this reading concedes the floor binds (states cannot return those who clear the individualized bar), registers floor-breaches and burden asymmetry as violations, but treats the discretion zone as legitimate, so only part of the arrangement's operation counts as extractive from this seat. Suppression is 0.70 as a raw structural property, deliberately unscaled: interdiction fleets, carrier sanctions, visa regimes, safe-third-country rules, and offshore detention exist precisely to close the route into the protection system, and suppression is never multiplied by power or scope in the engine's arithmetic. Theater ratio 0.41 reflects adjudicatory activity increasingly structured as screening-out performance (accelerated and manifestly-unfounded tracks, credibility findings at admissibility) alongside genuine individual determination for screened-in cases. Accessibility collapse 0.58: alternative legal channels (sponsorship, humanitarian visas, labor routes) are thin but not zero. Resistance 0.62: sustained strategic litigation, UNHCR public objection, treaty-body findings, and civil mobilization. Claim and metrics are independent facts: claimed_type tangled_rope follows from structure (a real reciprocal floor plus asymmetric extraction plus active enforcement), while the metric values describe observed operation; the engine computes per-seat classifications and any divergence from the claim is the datum. The temporal series run on one shared grid (points 0, 8, 16, 24, 32, 40, mapping approximately to 1985, 1993, 2001, 2009, 2017, 2025) with all three metrics authored at every point. The trajectory is a monotonic ratchet, not a cycle: enforcement intensifies stepwise after salient incidents (interdiction episodes, the 2001 offshore turn, the 2015-2016 externalization wave) with no relaxation phase, so intermittent reinforcement is not the mechanism here.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes materially different types per seat from this structural data. From the destination-state seat the arrangement is self-authored coordination: the state wrote the admissibility rules, collects the discretion dividend, and bears only the floor's narrow core, so it computes near the beneficiary end with low effective burden. From the three payer seats the same structure operates as enforced exclusion with amplified effective burden, since they are trapped, powerless, and declared victims. Host_transit_states show same-level divergence: nominally peer state actors with destination states, they are differentiated by exit options (constrained versus arbitrage) and by role, bearing concentrated hosting costs without agenda control. The taxonomy itself fragments the target class: proof-failures, generalized-violence fleeers, and non-state-persecution targets are separated by category, which impedes coalition formation among the powerless despite their shared structural position; a coalition keyed to the shared exclusion mechanism rather than the claim categories is the main countervailing possibility. Identity-lock dynamics concentrate in immigration_enforcement_bureaucracies: institutional identity fusion, where the organization has become its interdiction function, so reform reframing the mandate toward claim facilitation threatens budget lines and career structures from inside the state apparatus, not merely from politicians.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: destination_states, enforcement bureaucracies, and restrictionist movements sit near the beneficiary end (arbitrage-grade exit pulls destination states furthest); the three trapped, powerless victim groups sit near the full-target end, with trapped exit pushing them further than mobile targets would sit. One override is declared: the moderate power atom carries d_value 0.60 for host_transit_states, because the structural derivation would read declared-victim plus constrained-exit as near-full-target, while the actual relationship includes containment-linked financing and the floor's shielding of already-hosted populations, which partially subsidizes the seat. The override is unambiguous in this story because host_transit_states is the only moderate-power seat. Scope effects are left to the engine: the regime's continental-to-global reach makes verification of floor compliance harder, which scales effective burden upward for targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline guards against both mislabels. Calling this a pure extraction structure would erase the genuine coordination function: the common definition and the non-return floor solve a real collective-action problem, prevent competitive defection, and give cleared claimants durable status, which is why the floor survives contestation. Calling it pure coordination would erase the asymmetric extraction: the discretion zone's costs land entirely on seats with no voice, sustained by an enforcement perimeter that grew every decade of the interval. On the genealogy interview, the founding problem (bounded reciprocal handling of mass flight) is live, corroborated by record-scale displacement statistics, and the disappearance verdict is world_rearranges, so the status-times-verdict pair shows no mismatch and no zombie flag: this is an active, contested bargain, not theatrical maintenance of a dead mandate. The piton failure mode would arrive only if displacement ceased while the adjudicatory machinery persisted; the measurement signature would be theater_ratio overtaking functional activity while extraction decays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates one reading (restrictive_sovereignty_reading) of the refugee_convention_text kernel; is that reading one live position among several rather than the settled meaning, and what would each sibling reading change structurally if it prevailed?',
    'Comparative tracking of which reading commands adherence across destination-state doctrine, UNHCR guideline uptake, and regional instrument convergence; the sibling files (expansive_humanitarian_reading, procedural_integrity_reading) carry the counterfactual structures as separate constraints.',
    'If the expansive reading prevails, the victim set widens to generalized-violence and non-state-persecution fleeers and epsilon re-authors high over the same referent; if the procedural reading prevails, thresholds flexibilize but process strictures bind outcomes less predictably.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraint files, not parts of this one.').

omega_variable(
    floor_naturality_vs_constructed_bargain,
    'Is the minimum-floor character of the regime a natural feature of treaty-consent logic (states rationally accept only bounded obligations) or a constructed allocation that identifiable actors benefit from presenting as inevitable?',
    'Travaux preparatoires analysis and counterfactual drafting history: did drafters face viable broader-mandate proposals that specific state coalitions defeated?',
    'If constructed, the floor''s apparent fixity is false-summit material and the discretion zone reads as negotiated advantage rather than structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(floor_naturality_vs_constructed_bargain, conceptual, 'Whether the floor/discretion split is natural limit or negotiated construct.').

omega_variable(
    externalization_dependence,
    'How much of the arrangement''s stability rests on shifting protection costs onto transit and host states rather than on genuine floor reciprocity among destination states?',
    'Burden-sharing accounting: compare destination-state protection deliveries against transit-state hosted populations and externalized-processing volumes across the interval.',
    'High externalization dependence concentrates costs on the weakest seats and signals drift from the current hybrid structure toward pure extraction even under this reading''s own standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_dependence, empirical, 'Dependence of regime stability on cost-shifting to transit states.').

omega_variable(
    immutability_criterion_fit,
    'Does the immutable-characteristics-with-state-awareness criterion for particular social group track the actual targeting patterns that drive flight, or does it systematically miss mutable-association persecution (gender roles, occupation, kinship) that states observe but disclaim?',
    'Cross-jurisdictional grant-rate comparison for social-group claims keyed to mutable versus immutable characteristics, checked against country-of-origin information on targeting patterns.',
    'Systematic misfit would mean the reading''s own threshold fails its stated filtering purpose, creating an internal-validity crisis that pressures reinterpretation from inside the reading rather than from its rivals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_criterion_fit, empirical, 'Empirical adequacy of the immutability criterion against real persecution patterns.').

omega_variable(
    offshore_below_floor_question,
    'Do offshore processing and interdiction regimes operate within the floor this reading endorses, or beneath it, denying even the individualized assessment the reading itself requires?',
    'Audit of offshore arrangements against the reading''s own procedural minimum: whether individualized status determination occurs before transfer or removal in each operating arrangement.',
    'If beneath the floor, the reading''s own commitments condemn prevailing practice, and pressure toward the pure-extraction classification arises from this seat''s own standards rather than its rivals''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_below_floor_question, empirical, 'Whether offshore practice complies with the reading''s own floor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(refu_tr_t0, observed).
narrative_ontology:measurement(refu_tr_t8, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(refu_tr_t8, observed).
narrative_ontology:measurement(refu_tr_t16, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement_basis(refu_tr_t16, observed).
narrative_ontology:measurement(refu_tr_t24, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(refu_tr_t24, observed).
narrative_ontology:measurement(refu_tr_t32, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement_basis(refu_tr_t32, observed).
narrative_ontology:measurement(refu_tr_t40, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(refu_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(refu_be_t0, observed).
narrative_ontology:measurement(refu_be_t8, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement_basis(refu_be_t8, observed).
narrative_ontology:measurement(refu_be_t16, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement_basis(refu_be_t16, observed).
narrative_ontology:measurement(refu_be_t24, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(refu_be_t24, observed).
narrative_ontology:measurement(refu_be_t32, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement_basis(refu_be_t32, observed).
narrative_ontology:measurement(refu_be_t40, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(refu_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(refu_su_t0, observed).
narrative_ontology:measurement(refu_su_t8, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(refu_su_t8, observed).
narrative_ontology:measurement(refu_su_t16, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement_basis(refu_su_t16, observed).
narrative_ontology:measurement(refu_su_t24, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(refu_su_t24, observed).
narrative_ontology:measurement(refu_su_t32, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement_basis(refu_su_t32, observed).
narrative_ontology:measurement(refu_su_t40, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(refu_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Refugee Convention' covers three structurally distinct constraints (per the epsilon-invariance principle): this restrictive sovereignty reading (narrow victim set, high admissibility screening, offshore permissible, moderate reading-indexed epsilon over the standing arrangement), the expansive humanitarian reading (wide victim set including generalized violence and non-state persecution, high epsilon over the same referent), and the procedural integrity reading (flexible threshold, non-negotiable process). They form one constraint family linked by affects_constraints. The expansive reading is upstream in rhetorical citation (its authorities are invoked against this reading), while this reading is upstream in institutional practice (its settled doctrine shapes the procedural reading's operating environment, hence the influences edge).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__restrictive_sovereignty_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
