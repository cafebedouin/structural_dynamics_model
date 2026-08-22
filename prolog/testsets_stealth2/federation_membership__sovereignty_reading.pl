% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Conditional-Treaty Federation Membership: Free Movement as Negotiated Policy
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   A federation of sovereign states operates under a membership treaty that
 *   its governments treat as conditional and revisable: each member retains
 *   full authority over its borders, its labor market, and the terms on which
 *   other member-states' citizens may enter, and movement between members
 *   proceeds through recurring negotiated categories — quotas, permit
 *   classes, family-reunification thresholds — rather than through any
 *   entrenched entitlement. The arrangement is presented as the price of
 *   democratic self-government: welfare states built on bounded solidarity
 *   require that the community control its own composition. The people who
 *   move, or would move, bear the arrangement's costs directly in delayed
 *   careers, separated families, and deterred departures, while established
 *   workers in sheltered sectors collect the wage protection it produces.
 *
 * KEY AGENTS:
 *   - national_governments: Agenda-setting beneficiary (institutional/arbitrage) — writes and revises the mobility terms, collects the electoral returns
 *   - local_labor_insiders: Primary beneficiary (organized/constrained) — collects the wage protection the restriction produces
 *   - mobile_citizens: Primary target (moderate/constrained) — bears restricted movement, queued careers, tied permits
 *   - transnational_households: Secondary target (moderate/trapped) — bears family-separation risk with anchored lives
 *   - cross_border_employers: Dual-positioned payer (powerful/mobile) — bears compliance costs while gaining permit-tied retention leverage
 *   - sending_state_governments: Excluded voice (moderate/trapped) — negotiates mobility channels without levers over destination-state rules
 *   - prospective_migrants: Excluded voice (powerless/trapped) — deterred before entering any procedure
 *   - supranational_arbitration_panel: Analytical observer (institutional/analytical) — sees the cross-jurisdiction case record, commands no border posts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.76).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.68).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Conditional-Treaty Federation Membership: Free Movement as Negotiated Policy").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '71612e57-b029-424b-b802-9d58e3fa607c').
narrative_ontology:cs_kernel_codification('71612e57-b029-424b-b802-9d58e3fa607c', formalized).
narrative_ontology:cs_authority_grounding('71612e57-b029-424b-b802-9d58e3fa607c', self_enforcing).
narrative_ontology:cs_reading_relation('71612e57-b029-424b-b802-9d58e3fa607c', federation_membership__integration_reading, forecloses).
narrative_ontology:cs_axiom('71612e57-b029-424b-b802-9d58e3fa607c', foundational, national_gatekeeping_authority_legitimate).
narrative_ontology:cs_axiom_status(national_gatekeeping_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('71612e57-b029-424b-b802-9d58e3fa607c', national_gatekeeping_authority_legitimate, deontological).
narrative_ontology:cs_axiom('71612e57-b029-424b-b802-9d58e3fa607c', foundational, free_movement_negotiable_not_entrenched).
narrative_ontology:cs_axiom_status(free_movement_negotiable_not_entrenched, holdable).
narrative_ontology:cs_axiom_grounding('71612e57-b029-424b-b802-9d58e3fa607c', free_movement_negotiable_not_entrenched, conventional).
narrative_ontology:cs_reference_frame('71612e57-b029-424b-b802-9d58e3fa607c', intergovernmental_conditional_membership).
narrative_ontology:cs_drift_state('71612e57-b029-424b-b802-9d58e3fa607c', contemporary_post_enlargement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('71612e57-b029-424b-b802-9d58e3fa607c', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_insiders).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, cross_border_employers).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, transnational_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, cross_border_employers).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, bounded_solidarity_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, national_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and revise the terms on which citizens of other member states may enter, work, and reside: quota levels, permit categories, family-reunification thresholds, enforcement priorities. Justify each restriction publicly before their electorates and collect the resulting political credit when restrictions prove popular. They can reopen the treaty text, negotiate opt-outs, or threaten withdrawal, and they face no comparable restriction on their own officials' movement. Their costs are limited to diplomatic friction with sending states and arbitration rulings they can contest or distinguish away.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, national_governments, beneficiary).

% Workers already established in sheltered sectors of member-state labor markets — construction trades, transport, public administration, licensed professions — face fewer wage competitors than an open border would admit. Their unions and professional bodies press for quota ceilings and credential-recognition hurdles, and the wage premium from reduced competition flows to them directly. Their stake is place-bound: housing, seniority, language, and pension accrual tie them to the very labor market they are protecting.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_insiders, beneficiary,
    organized, biographical, constrained, national).

% Citizens of member states who seek work, residence, or family life in another member state. What they may do depends on permit categories, quota timing, and bilateral arrangements that can change between application and decision. Many spend years in procedural queues or accept work below their qualification level under permits tied to a single employer; some relocate outside the federation entirely at high personal cost. Their careers, partnerships, and children's schooling are scheduled around rules they did not write and cannot vote on.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, constrained, continental).

% Families whose members hold different member-state nationalities or residence statuses. Spouses navigate reunification income floors, waiting periods, and evidentiary demands; children grow up across jurisdictional lines that determine which parent they may live with and where they may attend school. Their lives are anchored — enrollment records, property, custody orders — so relocating wholesale to escape the rules is rarely feasible.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, transnational_households, payer,
    moderate, biographical, trapped, continental).

% Firms operating across member-state lines that need to staff sites in several jurisdictions. They absorb recruitment lead times, permit fees, and posting-rule compliance, and they lose candidates to faster hiring channels outside the federation. At the same time, permit systems that tie a worker's right to stay to a specific employer hand them retention leverage over that worker that an open labor market would remove — a benefit they rarely state publicly.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, cross_border_employers, payer,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, cross_border_employers, beneficiary).

% Governments of member states that export labor. They lose workers in shortage occupations and gain remittance inflows and reduced unemployment pressure; they argue for wider mobility channels in negotiations but hold few levers over destination-state border rules, since admission policy sits entirely with the receiving state. Their nationals bear the restrictions regardless of what they negotiate.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, sending_state_governments, excluded,
    moderate, generational, trapped, regional).

% People in member states who would move for work or family if channels were open, but who read the queue lengths, refusal rates, and income thresholds and never apply. They appear nowhere in the procedures — no file, no hearing, no statistics — yet the deterrent effect of the rules shapes their working lives as surely as a refusal letter would.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, prospective_migrants, excluded,
    powerless, biographical, trapped, continental).

% A treaty body that hears disputes between member states, and between individuals and member states, over mobility rules, issuing rulings that governments comply with, distinguish away, or publicly contest. It compiles the case record across jurisdictions and can identify systematic patterns no single national procedure reveals, but it commands no border posts and sets no quotas.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_arbitration_panel, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, local_labor_insiders).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lets each member polity calibrate labor inflows to what its housing stock, welfare financing, and public services can absorb on its own timetable, and keeps the composition of the national community a question its own electorate answers — solving the collective problem of integrating economies while preserving separately accountable welfare states.
% TRANSFER_FUNCTION: Moves access to cross-border work, residence, and family life out of the category of entitlement and into the category of recurring political decision, transferring bargaining power over millions of life plans from individual movers to national majorities; transfers wage protection to established workers in sheltered sectors.
% ABSENT_VOICES: Prospective migrants deterred before filing, sending-state communities that lose their working-age members, and future cohorts who will inherit whatever mobility terms are locked in now — none holds a seat in the quota negotiations; their interests arrive only filtered through destination-state electorates.
% DISAPPEARANCE_RATIONALE: If border discretion vanished overnight and movement became unconditional, wage structures in sheltered sectors would rebalance within years, welfare contribution bases and service loads would shift, permit bureaucracies and carrier-sanction systems would demobilize, and sending regions would lose a share of remittance income while their residents gained outside options — the federation's political economy would reorganize around open internal mobility.
% FOUNDING_PROBLEM: Rebuilding economic cooperation among sovereign states after war without asking any electorate to surrender control over who enters its territory: pooling trade and security while each government kept the gate to its own labor market and welfare system.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the published record of the founding intergovernmental conferences shows the gatekeeping reservation was demanded by small and large states alike as the price of signature; comparative federal scholarship documents the same reservation in every voluntary federation that survived; and migrant-rights litigation archives show courts treating the reservation as the settled baseline they argue within. No attestation rests solely on the governments that collect the political credit.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76 at interval end) because the arrangement prices movement itself: the value a mobile citizen can realize from a cross-border life is set by quota timing and permit category rather than by anything the citizen controls, and the pool bearing that price grew across the interval as enlargement multiplied the number of member-state citizens with reasons to move. Suppression (0.68) reflects the enforcement machinery — databases, carrier sanctions, externalized checks, removal operations — the arrangement requires in order to hold; it is authored as a raw structural property and is deliberately not scaled by power or scope, unlike extractiveness, which the engine scales by directionality and spatial scope. Theater is low-to-moderate (0.28): the checks are real and the queues are real, though a growing share of public justification leans on emergency and fairness language that outlives the specific conditions cited. Accessibility collapse is moderate (0.48) because negotiated channels genuinely exist — skilled-worker routes, family provisions, quota windows — so alternatives narrow but do not vanish. Resistance (0.55) is sustained: litigation by mobile citizens, employer lobbying, sending-state objections, and periodic electoral shocks. Claim and metrics are authored independently: I claim tangled_rope because the arrangement solves a real coordination problem (calibrating inflows to absorptive capacity while keeping welfare communities democratically answerable) AND charges a specific, identifiable population for it under active enforcement; the engine computes each seat's type from the structural data. The measurement series runs on one shared six-point grid so every tracked metric is authored at every examined time point; the trajectories rise together as enforcement hardened and the mobile population expanded.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently from the same structure. From the government seat the arrangement is ordinary self-government: every restriction was enacted by a majority answerable at the ballot box, and the arbitration panel's counter-moves look like overreach. From the mobile-citizen seat the same rules operate as a price on life plans levied by a polity the mover cannot vote in; from the household seat it is a standing threat to family integrity. Same-level differentiation matters too: cross-border employers sit at the same nominal exposure as mobile citizens but hold mobile exit (relocation, remote staffing) and a hidden second position — permit-tied workers are easier to retain — so their computed relationship to the arrangement diverges from the workers'. Coalition potential among the payer seats exists on paper (movers, employers, sending states) but divides at every concrete proposal: employers want selective openness, sending states want emigration outlets, movers want unconditional entry, so the coalition has never held.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real positions: local_labor_insiders collect the wage premium the restriction produces and are place-bound, so their derived directionality sits near the beneficiary pole; national_governments both administer the terms and collect the electoral returns, sitting near the beneficiary pole despite holding the pen. Victim declarations map likewise: mobile_citizens and transnational_households bear the restriction with constrained or trapped exit, placing them near the target pole; cross_border_employers derive high target-directionality from their declared victim position, tempered by their mobile exit and their unstated retention benefit. Sending-state governments and prospective migrants are excluded rather than coordinated — their exclusion is part of what the enforcement maintains. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. Reading the arrangement as pure coordination would erase the identifiable population charged for it — mobile citizens whose movement is a bargaining chip rather than a right — and would launder the wage-protection transfer as neutral overhead. Reading it as pure extraction would erase the genuine coordination function: welfare states do face real absorptive limits, and the founding bargain's gatekeeping reservation is corroborated from outside the benefiting parties as the price of signature. The founding problem remains live and the disappearance verdict is world_rearranges, so the mismatch consumer finds no dead-mandate signal: the arrangement persists because the problem it manages persists, not because its function has atrophied. Mandatrophy would attach only if absorptive-capacity justifications collapsed empirically while the machinery persisted — the trajectory the omega variables watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_location,
    'This constraint is one reading of the federation_membership kernel — the sovereignty_reading. What would change structurally if the sibling integration_reading were adopted instead?',
    'Adoption signals: treaty amendment entrenching movement rights beyond ordinary revision, court doctrine subjecting mobility terms to an entrenched-rights standard, or member-state practice converging on unconditional internal movement. Any of these migrates the governing constraint to the sibling file.',
    'Under the sibling reading the victim set widens to every refused or deterred mover, epsilon is indexed to denied rights rather than priced concessions, and the same border machinery computes as rights-infringement rather than conditional-treaty operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_location, conceptual, 'Committer structure: which reading of the membership kernel governs.').

omega_variable(
    bounded_demos_necessity,
    'Is nationally bounded membership a structural requirement of democratic welfare states, or a constructed arrangement maintainable only by continuous enforcement?',
    'Compare welfare-state solvency and democratic-legitimacy indicators across federations that constitutionalized internal mobility against those that retained gatekeeping, controlling for size and fiscal structure.',
    'If structural, part of the measured extraction is the unavoidable price of bounded democracy and the effective coordination floor rises; if constructed, the restriction is ordinary policy and the full extraction counts as overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bounded_demos_necessity, conceptual, 'Whether the coordination function is a hard requirement or a chosen design.').

omega_variable(
    identity_frame_cover_risk,
    'Does the identity-coordination framing (boundary maintenance for a national community) describe genuine coordination, or does it cover restriction concentrated on low-power movers?',
    'Test restriction incidence across power strata: if skilled movers clear the channels while low-wage movers absorb nearly all refusals and tied-permit dependency, the boundary function is operating selectively as labor-market segmentation.',
    'If the framing covers segmentation, the excess above the identity-coordination floor is extractive overhead and the computed classification shifts toward the extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_frame_cover_risk, empirical, 'Whether identity framing justifies coupling that actually concentrates burdens.').

omega_variable(
    absorption_capacity_empirics,
    'Are the absorptive-capacity justifications (housing, welfare financing, service load) empirically grounded at the levels actually restricted, or rhetorical?',
    'Natural experiments from abrupt openings and closings — enlargement waves, sudden channel suspensions — measuring wage, fiscal, and service-load effects against the stated thresholds.',
    'Grounded thresholds support the genuine-coordination half of the tangled_rope claim; rhetorical thresholds strip the coordination function and leave enforcement defending a transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_capacity_empirics, empirical, 'Empirical status of the coordination justification.').

omega_variable(
    deterrence_internalization,
    'How much of the measured suppression is structural (queues, permits, checks) versus internalized (deterrence beliefs that persist after channels open)?',
    'Post-liberalization flow data: if movement stays below the pre-restriction trend after channels widen, a deterrence residue is being carried by the population itself.',
    'Internalized deterrence raises effective suppression above the structural measure and slows any recomputation after reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_internalization, empirical, 'Structural versus internalized component of suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t6, federation_membership__sovereignty_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(fede_tr_t6, observed).
narrative_ontology:measurement(fede_tr_t12, federation_membership__sovereignty_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(fede_tr_t12, observed).
narrative_ontology:measurement(fede_tr_t18, federation_membership__sovereignty_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(fede_tr_t18, observed).
narrative_ontology:measurement(fede_tr_t24, federation_membership__sovereignty_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(fede_tr_t24, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership__sovereignty_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(fede_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t6, federation_membership__sovereignty_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(fede_be_t6, observed).
narrative_ontology:measurement(fede_be_t12, federation_membership__sovereignty_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(fede_be_t12, observed).
narrative_ontology:measurement(fede_be_t18, federation_membership__sovereignty_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement_basis(fede_be_t18, observed).
narrative_ontology:measurement(fede_be_t24, federation_membership__sovereignty_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement_basis(fede_be_t24, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership__sovereignty_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement_basis(fede_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t6, federation_membership__sovereignty_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement_basis(fede_su_t6, observed).
narrative_ontology:measurement(fede_su_t12, federation_membership__sovereignty_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(fede_su_t12, observed).
narrative_ontology:measurement(fede_su_t18, federation_membership__sovereignty_reading, suppression_requirement, 18, 0.63).
narrative_ontology:measurement_basis(fede_su_t18, observed).
narrative_ontology:measurement(fede_su_t24, federation_membership__sovereignty_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(fede_su_t24, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership__sovereignty_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(fede_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the federation_membership kernel per the epsilon-invariance principle: the colloquial label 'federation membership' conflates two structurally distinct arrangements. This file (sovereignty_reading) authors membership-as-conditional-treaty with mobility as revisable policy — epsilon indexed to the standing arrangement of negotiated movement, referent fixed, value reading-indexed. The sibling file (integration_reading) authors membership-as-irreversible-integration with movement as entrenched right — different victim set, different epsilon, different failure modes. The edge runs from this reading to the sibling: sovereignty reservations are cited as the treaty's original meaning against integrationist jurisprudence, so this reading's persistence constrains the sibling's operating environment. Each file links the other; neither hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
