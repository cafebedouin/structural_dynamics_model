% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Openness Commitment — Capture Substrate Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   The IETF openness commitment — anyone may participate, decisions by rough
 *   consensus — operates as the coordination substrate for internet protocol
 *   standardization. This story instantiates the capture_substrate_reading:
 *   the process genuinely coordinates (multi-vendor interoperability is
 *   produced and would be missed), AND resource advantage translates into
 *   encoded gatekeeping — professionalized delegations from a handful of
 *   platform operators dominate editorship, agenda formation, and deployment
 *   timing, so specification text and mandatory-to-implement feature sets
 *   skew toward sponsor product surfaces while costs fall on small
 *   implementers, veteran volunteers, and end users. The epsilon referent is
 *   the standing arrangement — the standards process as it actually operates
 *   — assessed by this reading's own lights; it is NOT the reformed or
 *   stewardship arrangement critics would build. Claim and metrics are
 *   independent authored facts: claimed_type tangled_rope is stated from
 *   structural belief (genuine coordination function plus asymmetric
 *   extraction, actively enforced through procedural gates), and the metrics
 *   describe observed operation without tuning toward any predicted engine
 *   verdict. This story is one member of a three-story constraint family; the
 *   siblings are separate files with their own epsilon values over the same
 *   standing arrangement.
 *
 * KEY AGENTS:
 *   - hyperscale_platform_operators: structural beneficiary with arbitrage-grade exit (institutional/global) — collects specification-shape and deployment-timing gains; can relocate work outside the process and return it finished
 *   - incumbent_network_equipment_vendors: secondary beneficiary (institutional/constrained) — feature sets and patent positions ride into mandatory-to-implement text
 *   - ietf_leadership_bodies: agenda_setter (institutional/identity_locked) — administers approval gates and appeals; institutionally fused
 *   - small_independent_implementers: primary target (moderate/constrained) — bears participation costs and meets finished feature sets as review items
 *   - veteran_volunteer_contributors: target with identity-fused exit (moderate/identity_locked) — supplies reviewer labor while absorbing crowding-out
 *   - end_user_communities: diffuse target (powerless/trapped) — receives thinned universality, holds no seat
 *   - alternative_standards_bodies: excluded competitor (institutional/mobile) — hosts rival processes outside the conversation
 *   - academic_protocol_researchers: analytical observer (moderate/analytical) — documents the resource-to-influence translation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.58).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.42).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment — Capture Substrate Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, 'b79ed6db-18e7-464d-910f-fa4bb0891030').
narrative_ontology:cs_kernel_codification('b79ed6db-18e7-464d-910f-fa4bb0891030', formalized).
narrative_ontology:cs_authority_grounding('b79ed6db-18e7-464d-910f-fa4bb0891030', practice).
narrative_ontology:cs_interpretation_layer_present('b79ed6db-18e7-464d-910f-fa4bb0891030').
narrative_ontology:cs_reading_relation('b79ed6db-18e7-464d-910f-fa4bb0891030', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('b79ed6db-18e7-464d-910f-fa4bb0891030', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('b79ed6db-18e7-464d-910f-fa4bb0891030', foundational, formal_openness_cannot_offset_resource_asymmetry).
narrative_ontology:cs_axiom_status(formal_openness_cannot_offset_resource_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('b79ed6db-18e7-464d-910f-fa4bb0891030', formal_openness_cannot_offset_resource_asymmetry, empirically_contingent).
narrative_ontology:cs_axiom('b79ed6db-18e7-464d-910f-fa4bb0891030', secondary, specification_text_encodes_sponsor_advantage).
narrative_ontology:cs_axiom_status(specification_text_encodes_sponsor_advantage, holdable).
narrative_ontology:cs_axiom_grounding('b79ed6db-18e7-464d-910f-fa4bb0891030', specification_text_encodes_sponsor_advantage, empirically_contingent).
narrative_ontology:cs_reference_frame('b79ed6db-18e7-464d-910f-fa4bb0891030', formal_openness_procedural_parity).
narrative_ontology:cs_drift_state('b79ed6db-18e7-464d-910f-fa4bb0891030', platform_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b79ed6db-18e7-464d-910f-fa4bb0891030', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, hyperscale_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, incumbent_network_equipment_vendors).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_independent_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, veteran_volunteer_contributors).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_user_communities).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, rough_consensus_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, openness_confers_neutrality_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate global network services and client platforms whose traffic dominates the protocols under specification. They send large professional delegations, fund editorship and chair roles, run pre-standardization deployments that confront the process with finished systems, and hold patent and copyright positions over their contributions. When the process moves slower than their product cycle they have taken the work outside and returned it as a completed draft; leaving permanently would cost them the legitimacy of a neutral venue, so they stay and shape the text.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, hyperscale_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, hyperscale_platform_operators, agenda_setter).

% Sell routers, switches, and carrier equipment whose feature lists are built from the mandatory-to-implement sections of the specifications. They maintain standing delegations and patent portfolios positioned around working group output; a specification that omits their features or invalidates their installed base is a direct revenue event. Abandoning the process is thin option for them because their customers procure against these specifications.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, incumbent_network_equipment_vendors, beneficiary,
    institutional, biographical, constrained, global).

% Build clients, libraries, and niche products on the protocols. Participation costs — travel, billable time, the expertise to follow hundreds of concurrent threads — scale poorly against corporate delegations, and they typically encounter finished feature sets as review items rather than as authors. Walking away means accepting whatever specification ships and building to it anyway, since the market they sell into is defined by it.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_independent_implementers, payer,
    moderate, biographical, constrained, global).

% Have contributed for ten to thirty years, many having written earlier generations of the same specifications. Their standing rests on accumulated technical credit inside the community, and their sense of themselves is bound up with the venue's self-image as a neutral commons; several have declined paid positions elsewhere to keep contributing. Costs arrive as crowding-out — agendas set elsewhere, review burdens without resources — and leaving would mean walking away from a community that constitutes much of their professional identity.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, veteran_volunteer_contributors, payer,
    moderate, biographical, identity_locked, global).

% Run mail servers, browsers, messaging clients, and connected devices built on the specifications. They experience the output as nominally universal protocols whose practical implementation burden and extension behavior favor the largest software estates; when a protocol profile works mainly between the biggest implementations, the promised universality thins. They hold no seat in the process and no realistic exit from the protocol ecosystem their infrastructure runs on.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_user_communities, payer,
    powerless, generational, trapped, global).

% Area directors, working group chairs, and the steering and architecture boards approve specifications, resolve disputes, and hear appeals. They administer the process on volunteered time atop day jobs, hold their roles through community esteem, and understand their task as protecting the venue's legitimacy. Stepping down returns them to ordinary contributor status; the role and the institution are difficult to separate from their standing in the field.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_leadership_bodies, agenda_setter,
    institutional, biographical, identity_locked, global).

% Consortia, treaty-sector unions, and web-focused bodies that host competing specification processes. Work that stays in the IETF leaves them outside that conversation; they respond by courting sponsors with faster processes or narrower scopes. Their position is mobile in principle — they can charter new work — but the network effects of deployed internet infrastructure concentrate gravity on the incumbent venue.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, alternative_standards_bodies, excluded,
    institutional, generational, mobile, global).

% Study the process itself — participation data, specification text evolution, deployment outcomes — and publish analyses of how resources translate into influence. They attend meetings sporadically, hold no decision roles, and their exit is analytical: they can redirect attention to other institutions without career cost.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, academic_protocol_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, hyperscale_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converges competing implementers on shared wire-level specifications: a venue where rivals draft, review, and adopt common protocol text so that mail, transport, routing, and web infrastructure interoperate across vendors instead of fragmenting into incompatible private networks.
% TRANSFER_FUNCTION: Moves specification-shaping power toward the best-resourced delegations: their engineering priorities become mandatory-to-implement feature sets, their intellectual-property positions ride into the text, and their deployment schedules set the cadence; the offsetting costs land as participation burdens on smaller implementers and as thinner practical universality for end users.
% ABSENT_VOICES: End users have no seat anywhere in the process; implementers from low-resource firms and regions are absent in proportion to travel and time costs; independent security researchers appear thinly. They would object that universality claims are made without them; they stand outside the room, voiced only by proxy through sympathetic participants.
% DISAPPEARANCE_RATIONALE: Protocol work would scatter to consortia and bilateral de facto specifications overnight; interoperability would degrade until new venues stabilized, the largest operators would lose the legitimacy subsidy of a neutral venue and pay for private standard-setting instead, and the volunteer culture that supplies reviewer labor would dissipate without a home.
% FOUNDING_PROBLEM: Vendor protocol fragmentation in the 1970s–80s: proprietary networking suites did not interoperate, and the research community needed a vendor-neutral venue to converge on host-to-host and internetworking protocols that no single seller controlled.
% FOUNDING_PROBLEM_CORROBORATION: The networking-history literature and the documented pre-common-protocol incompatibility record corroborate the founding problem from outside the current beneficiary set; working engineers across competing firms continue to attest the live interoperability problem whenever new protocol layers need multi-vendor agreement. No party seriously disputes that the original problem existed; the dispute is over whether today's process still serves it evenly.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 is moderate-high: real coordination is delivered alongside a skewed distribution of specification-shaping power, matching the expected structural delta for this reading. Suppression 0.42 is structural rather than coercive — no legal force compels participation; exclusion operates through priced participation, procedural volume, and installed-base gravity, so exit exists but is expensive. Theater 0.38: last-call comment periods and the 'anyone can participate' framing perform inclusivity beyond what agenda formation delivers, while core technical review remains functional. Accessibility_collapse 0.45: alternatives exist (consortia, de facto specifications, rival venues) but installed-base gravity limits them for core protocols. Resistance 0.50: periodic organized pushback — formal appeals, rival venues, academic critique — without rupture. The measurement series run on ONE shared grid {0, 8, 16, 24, 32, 40} (1986–2026) with every tracked metric authored at every point. suppression_requirement is included deliberately: the story specifically tracks the hardening of the process's structural-exclusion machinery — professionalization raising the participation floor and procedural volume serving as defense — which is a changing suppressive requirement, not a static picture. The trajectory is monotonic, not cyclical: commercialization ratcheted stakes upward without reversal phases.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats the arrangement computes as coordination they sustain and legitimately profit from; from the payer seats the same procedural structure operates as priced exclusion. The engine computes per-seat classifications from the structural data, and divergence between seats is the expected signal, not noise. The sharpest contrast is the veteran volunteer seat: nominally the same community membership as the beneficiaries, structurally opposite position — supplying the unpaid review labor that legitimizes the output while absorbing the crowding-out the beneficiaries' delegations produce.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (hyperscale_platform_operators, incumbent_network_equipment_vendors) derive low directionality — the constraint subsidizes them. Declared victims (small_independent_implementers, veteran_volunteer_contributors, end_user_communities) derive high directionality, amplified by their exit atoms: trapped end users and identity_locked veterans sit nearer the full-target end than the merely constrained small implementers. The hyperscalers' arbitrage-grade exit pushes them toward the beneficiary pole despite their agenda-setting secondary role — they can credibly leave, which is precisely why the process accommodates them. Leadership derives a near-symmetric position: it administers and absorbs drift without capturing the material gains. No directionality overrides are needed; the derivation from beneficiary/victim declarations plus exit atoms reproduces the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabels. Reading the process as pure coordination (rope) erases the encoded-gatekeeping transfer and adopts the venue's self-description at face value; reading it as pure extraction (snare) erases the genuine multi-vendor interoperability good that would vanish with the venue — the founding problem is corroborated as live from outside the beneficiary set. No mandatrophy declaration is authored: the mandate has not outlived its function; the defect is distributional, not obsolescent. The failure mode to watch is drift toward piton: if protocol coordination ever completed its migration to de facto deployment and private consortia, the process would persist as theatrical maintenance of a coordination form whose substance had moved elsewhere — the rising theater_ratio series is the early indicator of that trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_position_within_openness_kernel,
    'This story instantiates the capture_substrate_reading of the ietf_openness_commitment kernel: is the standing arrangement''s defining structure resource-advantage-encoded gatekeeping riding a real coordination substrate (this reading), successful public-infrastructure stewardship preserving interoperability for all implementers (commons_stewardship_reading), or a rough-consensus mechanism whose legitimacy is eroding under organized pressure regardless of outcome distribution (legitimacy_erosion_reading)?',
    'Cross-reading comparison over the shared referent: estimate epsilon and victim sets independently under each reading''s own lights, then test which reading''s predictions about outcome skew, participation attrition, and specification-text asymmetry best fit the observational record.',
    'Under the commons_stewardship_reading epsilon falls toward coordination-cost levels and the type moves toward rope; under the legitimacy_erosion_reading the defect relocates from outcome distribution to the consensus mechanism itself, changing which interventions are relevant. The disagreement is located in whether formal openness neutralizes resource asymmetry — a question each reading answers differently over the same standing arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_position_within_openness_kernel, conceptual, 'Kernel-indexical position: this constraint is one of three readings of the IETF openness commitment, differing on where the defect sits.').

omega_variable(
    merit_gatekeeping_boundary,
    'Which specification provisions reflect technical merit and which encode sponsor advantage? Ex ante the two are often indistinguishable — a mandatory-to-implement feature may be essential engineering or a competitive moat.',
    'Retrospective analysis of deployed specifications: compare provision survival rates, implementation-cost distribution, and competitive outcomes for provisions sponsored by dominant delegations versus others, controlling for technical necessity.',
    'If most contested provisions survive merit scrutiny, measured epsilon overstates extraction and the arrangement sits nearer the coordination end; if a substantial fraction fails scrutiny, the encoded-gatekeeping share of epsilon is confirmed and rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merit_gatekeeping_boundary, empirical, 'Where legitimate sponsor contribution ends and encoded gatekeeping begins in specification text.').

omega_variable(
    suppression_structural_or_identity_internalized,
    'Is the low defection rate among veteran volunteer contributors structural (review labor and community goods exist only inside the venue) or internalized (professional identity fused with the venue''s self-image makes exit unthinkable)?',
    'Post-exit trajectory study of contributors who left: if criticism and disengagement persist after departure, part of the suppression was carried internally; if leavers report relief and continued technical work elsewhere, suppression was structural.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — the venue retains critic-labor absorption capacity that pure participation-cost accounting misses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_identity_internalized, empirical, 'Structural versus internalized suppression mechanism for the identity-locked contributor seat.').

omega_variable(
    arbitrage_exit_discipline_question,
    'Does the largest operators'' demonstrated ability to move work outside the process and return it as finished drafts discipline the process toward accommodation, and is that accommodation channel the primary transfer mechanism?',
    'Compare specification trajectories for work initiated inside the process versus imported from outside: adoption speed, feature-set provenance, and whose deployments define conformance in practice.',
    'If imported work systematically arrives pre-shaped to sponsor products, the transfer function concentrates further and epsilon trends upward; if the process meaningfully reshapes imports, the coordination function is stronger than this reading assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbitrage_exit_discipline_question, empirical, 'Whether outside-option threat is the load-bearing extraction channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(ietf_tr_t0, observed).
narrative_ontology:measurement(ietf_tr_t8, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(ietf_tr_t8, observed).
narrative_ontology:measurement(ietf_tr_t16, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(ietf_tr_t16, observed).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(ietf_tr_t24, observed).
narrative_ontology:measurement(ietf_tr_t32, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement_basis(ietf_tr_t32, observed).
narrative_ontology:measurement(ietf_tr_t40, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(ietf_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(ietf_be_t0, observed).
narrative_ontology:measurement(ietf_be_t8, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement_basis(ietf_be_t8, observed).
narrative_ontology:measurement(ietf_be_t16, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(ietf_be_t16, observed).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement_basis(ietf_be_t24, observed).
narrative_ontology:measurement(ietf_be_t32, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement_basis(ietf_be_t32, observed).
narrative_ontology:measurement(ietf_be_t40, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(ietf_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(ietf_su_t0, observed).
narrative_ontology:measurement(ietf_su_t8, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 8, 0.27).
narrative_ontology:measurement_basis(ietf_su_t8, observed).
narrative_ontology:measurement(ietf_su_t16, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 16, 0.32).
narrative_ontology:measurement_basis(ietf_su_t16, observed).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement_basis(ietf_su_t24, observed).
narrative_ontology:measurement(ietf_su_t32, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement_basis(ietf_su_t32, observed).
narrative_ontology:measurement(ietf_su_t40, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(ietf_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'IETF openness' decomposes into three structurally distinct claims per the epsilon-invariance principle: (1) the process produces multi-vendor interoperability (commons_stewardship_reading — upstream, highest empirical confidence, cited as both evidence and cover by the other two), (2) resource advantage translates into encoded gatekeeping within that process (this story — moderate epsilon, outcome-distribution defect), (3) the rough-consensus mechanism's legitimacy is eroding under organized pressure (legitimacy_erosion_reading — downstream, mechanism-level defect). Each carries its own epsilon, beneficiaries, and victims; edges run upstream-to-downstream because the stewardship claim is what the capture dynamics parasitize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
