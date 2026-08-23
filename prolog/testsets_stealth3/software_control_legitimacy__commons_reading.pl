% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Negotiated Collective Governance of Shared Digital Infrastructure (Commons Reading)
 *   domain: economic/political/technological
 *
 * SUMMARY:
 *   This story instantiates the commons reading of the
 *   software-control-legitimacy kernel: the claim that control over shared
 *   digital infrastructure is neither an individual-liberty matter nor a
 *   creator-property matter but a governance question settled by negotiated
 *   collective management. The standing arrangement under assessment is the
 *   actual institutional fabric of that settlement - foundation stewardship
 *   of licenses and trademarks, technical steering committees and RFC
 *   processes, contribution agreements and codes of conduct - through which
 *   most of the internet's software substrate is now governed. Its
 *   coordination achievement is real: merge discipline, security response,
 *   and license custody are produced once, centrally, instead of per-dispute.
 *   Its asymmetry is equally real: the labor sustaining critical components
 *   comes disproportionately from unpaid or underpaid maintainers while the
 *   largest consumers capture most of the surplus, and the two absolutist
 *   camps - free-software purists and proprietary maximalists - hold
 *   positions the negotiated regime admits only as outvoted voices. Three
 *   sibling readings are separate constraint files linked through the network
 *   section; this file authors only the commons reading, with its own stable
 *   epsilon. KEY AGENTS (by structural relationship):
 *   foundation_governance_bodies: agenda setter (institutional/constrained) -
 *   administers custody and merge authority;
 *   stakeholder_developer_communities: primary beneficiary
 *   (organized/constrained); commercial_infrastructure_consumers: dual
 *   beneficiary-payer (powerful/arbitrage) - captures most surplus;
 *   uncompensated_core_maintainers: primary target
 *   (moderate/identity_locked); freedom_maximalists: target by exclusion
 *   (organized/constrained); proprietary_rights_holders: target by exclusion
 *   (powerful/arbitrage); ordinary_end_users: diffuse beneficiary-payer
 *   (powerless/constrained); peripheral_excluded_contributors: excluded voice
 *   (moderate/constrained); digital_governance_analysts: analytical observer
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.54).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.44).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Negotiated Collective Governance of Shared Digital Infrastructure (Commons Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "economic/political/technological").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '8303754f-8292-47a9-ad5d-cb1ee9cdfaa1').
narrative_ontology:cs_kernel_codification('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', distributed).
narrative_ontology:cs_authority_grounding('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', practice).
narrative_ontology:cs_interpretation_layer_present('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1').
narrative_ontology:cs_reading_relation('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_axiom('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', foundational, no_absolute_software_entitlements).
narrative_ontology:cs_axiom_status(no_absolute_software_entitlements, holdable).
narrative_ontology:cs_axiom_grounding('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', no_absolute_software_entitlements, deontological).
narrative_ontology:cs_axiom('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', foundational, shared_infrastructure_requires_collective_custody).
narrative_ontology:cs_axiom_status(shared_infrastructure_requires_collective_custody, holdable).
narrative_ontology:cs_axiom_grounding('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', shared_infrastructure_requires_collective_custody, empirically_contingent).
narrative_ontology:cs_reference_frame('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', negotiated_polycentric_stewardship).
narrative_ontology:cs_drift_state('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8303754f-8292-47a9-ad5d-cb1ee9cdfaa1', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_developer_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, commercial_infrastructure_consumers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, ordinary_end_users).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, uncompensated_core_maintainers).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, freedom_maximalists).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, proprietary_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, commercial_infrastructure_consumers).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, ordinary_end_users).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, negotiated_collective_management_doctrine).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, ostrom_polycentric_governance_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold licenses, trademarks, and project assets in trust for their communities; run elections, RFC votes, and release processes; enforce contribution agreements and codes of conduct; decide which patches merge and which subprojects graduate. Their authority derives from charters their member communities ratified. Exiting means dissolving or transferring assets to another steward, which charters and legal obligations make slow.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, foundation_governance_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Contribute code, review, documentation, and dispute-resolution labor to shared projects; receive working infrastructure, peer standing, and a voice in governance proportional to their engagement. Leaving means forking or joining another project, forfeiting accumulated reputation and commit history.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_developer_communities, beneficiary,
    organized, biographical, constrained, global).

% Build products and services on commons-hosted infrastructure; pay membership dues, contribute engineer-hours, and accept license obligations such as sharing modifications where copyleft applies. In return they obtain dependable components at a fraction of bespoke development cost. They can take the code and walk - hiring maintainers, hard-forking, or relicensing their own stacks - and some periodically do.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, commercial_infrastructure_consumers, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, commercial_infrastructure_consumers, payer).

% Carry merge responsibility, security response, and release duty for widely deployed packages, usually alongside day jobs or without pay; their decisions keep banks, hospitals, and cloud services running. Stepping away means abandoning users who depend on them and, for many, abandoning a project that has become central to their professional identity and self-description.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, uncompensated_core_maintainers, payer,
    moderate, biographical, identity_locked, global).

% Hold that computing users must control their own software and treat compromise with proprietary interests as betrayal. Inside negotiated regimes their position is permanently outvoted: governance charts courses that trade purity for participation, admitting them as voices but never as a decisive bloc. They can retreat to enclaves running entirely free stacks, at the cost of influence and compatibility with the mainstream.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, freedom_maximalists, payer,
    organized, generational, constrained, global).

% Firms and creators asserting exclusive authority over their code - restricting use, modification, or redistribution to protect investment. The negotiated settlement treats that assertion as one claimant voice among many rather than a trump card. Exercising full control means leaving the commons and paying the network and reputational costs of exit, as several infrastructure vendors have done.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, proprietary_rights_holders, payer,
    powerful, biographical, arbitrage, global).

% Run the resulting infrastructure as internet users, employees, and citizens; they receive reliable services and pay indirectly through the security incidents and outages that under-maintained components produce. They hold no seat in any steering committee.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, ordinary_end_users, beneficiary,
    powerless, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, ordinary_end_users, payer).

% Would-be contributors from regions and constituencies underrepresented in governance forums; language barriers, time-zone-hostile meeting schedules, and reputation systems seeded elsewhere keep them at the margins of decision-making even when their patches are accepted.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, peripheral_excluded_contributors, excluded,
    moderate, biographical, constrained, global).

% Researchers and policy analysts studying how software commons are governed; they observe all seats, publish comparisons, and advise funders and regulators, but hold no vote in any project.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, digital_governance_analysts, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__commons_reading, commercial_infrastructure_consumers).
narrative_ontology:fixing_cost_class(software_control_legitimacy__commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of sustaining shared digital infrastructure: merge authority, conflicting-contribution resolution, license and trademark custody, security response, and critical-component funding are decided once through negotiated process instead of per-dispute warfare among claimants.
% TRANSFER_FUNCTION: Moves decision authority and maintenance labor from individual contributors and absolutist claimants into collective institutions; moves corporate dues and contributed engineer-hours into common pools; delivers the finished infrastructure to all participants, with the largest per-unit benefit accruing to commercial consumers who contribute least relative to what they draw.
% ABSENT_VOICES: Peripheral contributors and end users most exposed to governance outcomes rarely sit on steering committees; the absolutist camps participate only to the extent they accept negotiation, so their objections are structurally muted rather than answered. Both groups would insist that legitimacy requires seats, not invitations to comment.
% DISAPPEARANCE_RATIONALE: If negotiated governance vanished overnight, the software substrate would fragment into warring proprietary enclosures and purity enclaves; critical maintenance would collapse as merge discipline and security response lost their institutional homes, multiplying Heartbleed- and Log4Shell-class failures; corporate consumers would scramble to internalize or bid for maintainer labor; the internet's software layer would reorganize around whichever maximalism seized the most assets first.
% FOUNDING_PROBLEM: The fragmentation and enclosure crises of the 1980s-90s: incompatible proprietary Unix variants, formerly shared academic software locked behind licenses, license wars, and unmanaged shared code decaying through under-maintenance - the problem of sustaining common infrastructure without either central ownership or anarchic neglect.
% FOUNDING_PROBLEM_CORROBORATION: Independent security postmortems (Heartbleed 2014, Log4Shell 2021) authored by researchers outside any benefiting party attest that maintenance fragility persists; academic commons scholarship in the Ostrom lineage and philanthropic infrastructure assessments (e.g., Sloan-funded state-of-the-commons reviews) corroborate the funding-gap diagnosis from outside the beneficiary set. Foundation staff also attest the problem is live, but the external attestation exists independently of them.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate and rising (0.34 to 0.54 across the interval) because the arrangement's financing is asymmetric: merge discipline and security response are produced largely by maintainer labor whose compensation lags the commercial value drawn from it, while the absolutist camps bear a continuing legitimacy cost of denied participation. Suppression (0.44) tracks enforcement formalization - the suppression_requirement series records the shift from informal trust norms to contractual and procedural machinery (contribution agreements, code-of-conduct enforcement, platform terms, trademark policing) - but stays below snare territory because forking remains a live exit valve that caps how far enforcement can reach. Theater (0.18 to 0.41) grows with openness-washing: corporate marketing of community virtue, RFC processes where outcomes are prearranged, and governance rituals unaccompanied by changed decisions; core review, release, and security functions remain genuinely performed, keeping the ratio under the Goodhart threshold. Accessibility collapse is low-moderate (0.40): alternatives demonstrably persist - forks, purity enclaves, proprietary islands - so understanding the arrangement does not close the option space. Resistance (0.48) is real: license-flip fights, capture attempts contested by communities, and permanent absolutist critique. All three series run on one shared eight-point grid (1998-2024); the small extraction dip after 2019 reflects funding responses (sponsorship programs, foundation maintenance funds) partially catching up to the subsidy, not a structural reversal. Receipt surface: the gains land demonstrably with commercial consumers as underpriced inputs, so gain_flow names that seat rather than asserting diffuse; fixing the imbalance is prohibitive because rebalancing requires solving a public-goods funding problem across thousands of projects that no single administrator bears alone.
 *
 * PERSPECTIVAL GAP:
 *   Four seats compute different constraint characters from the same structure. From the maintainer seat the arrangement operates as open-ended claims on their labor, held in place by identity fusion with their projects. From the absolutist seats it operates as a permanent silencing - a table they may sit at only by surrendering the premise that brought them. From the commercial-consumer seat it is the best available deal in the history of software input markets: dependable components, managed license risk, and a voice proportionate to dues. From the foundation seat it is a life's work functioning as designed. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Developer communities and end users sit near the beneficiary pole: the arrangement subsidizes them with infrastructure and voice at low marginal cost. Commercial consumers are genuinely dual-positioned - encoded as beneficiary with secondary payer - because obligations (dues, copyleft compliance, contributed hours) pull them toward symmetry while their disproportionate draw pulls them toward the beneficiary pole; they are also the seat the gains demonstrably accrue to, which is why gain_flow names them. Maintainers sit near the full-target pole, amplified by identity lock: their exit is not merely costly but self-dissolving. The absolutist camps register high directionalities through the victim declarations - their cost is denial of governance participation, an exclusion the derivation reads as bearing the arrangement's imposed terms. Foundation bodies sit near symmetric as stewards who collect standing rather than surplus, with capture risk (see omega governance_capture_trajectory) as the mechanism that would push them toward the consumer pole. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the correct relationships, and the dual positions are carried by secondary_role rather than overridden scalars.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite errors. Reading the arrangement as pure coordination would erase the measurable maintainer subsidy and the silenced absolutist camps - the coordination function is real but asymmetrically financed, which is precisely the hybrid signature. Reading it as pure extraction would ignore that enforcement is consensual-contractual, that forking keeps exits open, and that participants are net beneficiaries on balance. On mandatrophy proper: the founding problem (unmanaged commons decay) is live and externally corroborated, so no mandate has outlived its function and no sunset applies. The forward risk is different: the theater trajectory (0.18 to 0.41) marks accumulating performative openness; if governance rituals continue substituting for maintenance funding and participation reform, marketing-heavy corners of the ecosystem could drift toward inertial performance while the load-bearing subsidy persists underneath.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_rule_variability,
    'Does the measured epsilon describe the modal commons arrangement or the distribution across commons rule-sets (copyleft vs permissive licensing, CLA vs DCO contribution terms, funded vs unfunded projects)?',
    'Stratified measurement across foundation-hosted, corporate-stewarded, and orphaned projects; compare extraction indicators within each stratum before aggregating.',
    'The permissive-license, unfunded corner computes as near-pure free-riding on maintainer labor; the funded copyleft corner approaches balanced coordination. A single scalar epsilon conceals whether the arrangement''s character is set by its dominant stratum or its worst.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_rule_variability, empirical, 'Epsilon varies systematically with which commons rules a given project runs.').

omega_variable(
    absolutist_cost_nature,
    'Is the cost borne by the freedom-maximalist and proprietary-maximalist camps material extraction, or legitimacy exclusion (denied governance participation) that carries little material burden?',
    'Compare welfare trajectories of absolutist camps inside versus outside negotiated regimes; analyze purity-enclave viability (fully free stacks, fully proprietary islands) and the actual losses their members incur from compromised participation.',
    'If the cost is exclusion rather than material transfer, the absolutist seats'' effective extraction falls and the arrangement drifts toward balanced coordination; if material, the hybrid classification stands with the absolutists counted among its targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_cost_nature, conceptual, 'Whether denial of governance participation constitutes extraction or mere exclusion.').

omega_variable(
    maintainer_subsidy_magnitude,
    'How large is the uncompensated-maintainer-to-commercial-consumer transfer for critically deployed packages?',
    'Census of maintainer compensation against downstream commercial revenue attributable to critical dependencies (postmortem audits of Heartbleed, Log4Shell, xz-class events; dependency-economic studies).',
    'A large persistent transfer pushes epsilon toward the pure-extraction boundary and strengthens the case for compulsory funding mechanisms; a small transfer supports reading the arrangement as net-beneficial coordination with friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_subsidy_magnitude, empirical, 'Size of the volunteer-labor subsidy flowing to commercial users of commons infrastructure.').

omega_variable(
    governance_capture_trajectory,
    'Are foundation governance bodies converging toward serving their largest corporate members, or retaining polycentric independence?',
    'Longitudinal tracking of board composition, contribution-agreement holder concentration, and decision outcomes weighted by member class.',
    'Capture converts the stewardship seat into an instrument of its largest funders, shifting the arrangement''s center of gravity toward enforced extraction maintained against community resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_capture_trajectory, empirical, 'Whether commons stewardship institutions are being captured by major corporate members.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'This constraint is one reading (commons_reading) of the software_control_legitimacy kernel; the sibling readings (freedom_imperative_reading, pragmatic_openness_reading, property_rights_reading) locate legitimate control authority elsewhere - in individual user liberty, in engineering-outcome selection, or in creator title. Which structural element do the readings actually disagree on, and what would instantiating a sibling change?',
    'Track adoption patterns: migration of governance participation toward individual-user veto mechanisms favors the freedom reading; re-legitimation of exclusive licensing favors the property reading; consolidation of outcome-measured methodology choice favors the pragmatic reading.',
    'Instantiating a sibling replaces the victim and beneficiary sets wholesale - under the property reading, commons participants become the wronged party and creators the entitled seat; under the freedom reading, every compromise with proprietary adjacency becomes the harm. Cross-reading comparison is valid only at the kernel level, never by averaging these files'' metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Committer structure: the readings disagree on the locus of legitimate control authority, producing disjoint constraint instantiations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swc_commons_tr_t1998, software_control_legitimacy__commons_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(swc_commons_tr_t2003, software_control_legitimacy__commons_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(swc_commons_tr_t2008, software_control_legitimacy__commons_reading, theater_ratio, 2008, 0.23).
narrative_ontology:measurement(swc_commons_tr_t2012, software_control_legitimacy__commons_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement(swc_commons_tr_t2016, software_control_legitimacy__commons_reading, theater_ratio, 2016, 0.31).
narrative_ontology:measurement(swc_commons_tr_t2019, software_control_legitimacy__commons_reading, theater_ratio, 2019, 0.36).
narrative_ontology:measurement(swc_commons_tr_t2021, software_control_legitimacy__commons_reading, theater_ratio, 2021, 0.39).
narrative_ontology:measurement(swc_commons_tr_t2024, software_control_legitimacy__commons_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(swc_commons_be_t1998, software_control_legitimacy__commons_reading, base_extractiveness, 1998, 0.34).
narrative_ontology:measurement(swc_commons_be_t2003, software_control_legitimacy__commons_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement(swc_commons_be_t2008, software_control_legitimacy__commons_reading, base_extractiveness, 2008, 0.42).
narrative_ontology:measurement(swc_commons_be_t2012, software_control_legitimacy__commons_reading, base_extractiveness, 2012, 0.47).
narrative_ontology:measurement(swc_commons_be_t2016, software_control_legitimacy__commons_reading, base_extractiveness, 2016, 0.53).
narrative_ontology:measurement(swc_commons_be_t2019, software_control_legitimacy__commons_reading, base_extractiveness, 2019, 0.57).
narrative_ontology:measurement(swc_commons_be_t2021, software_control_legitimacy__commons_reading, base_extractiveness, 2021, 0.56).
narrative_ontology:measurement(swc_commons_be_t2024, software_control_legitimacy__commons_reading, base_extractiveness, 2024, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(swc_commons_su_t1998, software_control_legitimacy__commons_reading, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement(swc_commons_su_t2003, software_control_legitimacy__commons_reading, suppression_requirement, 2003, 0.24).
narrative_ontology:measurement(swc_commons_su_t2008, software_control_legitimacy__commons_reading, suppression_requirement, 2008, 0.29).
narrative_ontology:measurement(swc_commons_su_t2012, software_control_legitimacy__commons_reading, suppression_requirement, 2012, 0.33).
narrative_ontology:measurement(swc_commons_su_t2016, software_control_legitimacy__commons_reading, suppression_requirement, 2016, 0.37).
narrative_ontology:measurement(swc_commons_su_t2019, software_control_legitimacy__commons_reading, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement(swc_commons_su_t2021, software_control_legitimacy__commons_reading, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement(swc_commons_su_t2024, software_control_legitimacy__commons_reading, suppression_requirement, 2024, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who may legitimately control software' decomposes into four structurally distinct claims, each with its own epsilon, beneficiary/victim structure, and classification. This file instantiates the commons reading only. The freedom-imperative and property-rights readings negate this reading's foundational anti-absolutism premise outright (foreclosure edges in cs_structure); the pragmatic-openness reading coexists, treating the same governance machinery as simply the methodology winner's institutional form. The upstream/downstream citation flow runs from the pragmatic reading (widest industrial adoption) into this one, and from this one into the absolutist readings as the compromise they define themselves against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
