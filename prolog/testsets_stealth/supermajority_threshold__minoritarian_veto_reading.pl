% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Amendment Threshold as Minoritarian Veto (Minoritarian Reading)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   A long-lived federal constitution requires supermajorities at two stages
 *   of formal amendment — two-thirds of each legislative chamber to propose,
 *   three-quarters of constituent units to ratify — and has been amended
 *   rarely relative to the volume of proposed change. This story instantiates
 *   ONE reading of the contested supermajority-threshold kernel: the
 *   minoritarian-veto reading, on which the threshold converts any durable
 *   minority above the blocking fraction into a veto player, and — where that
 *   minority's position tracks historically accumulated advantage (unit-equal
 *   apportionment, protected resource regimes, legacy regulatory settlements)
 *   — converts historical privilege into a permanent veto against
 *   contemporary majorities. Per the epsilon-invariance rule, epsilon here
 *   refers to the standing arrangement (the threshold as it operates),
 *   assessed by this reading's own lights; the sibling readings
 *   (consensus_safeguard_reading, adaptive_gradient_reading) are separate
 *   constraints with their own epsilon, linked through
 *   network.affects_constraints. The claimed type (snare) is authored from
 *   this reading's structural judgment; the metrics are authored as
 *   descriptive truths about the arrangement's operation; the engine computes
 *   per-seat classifications from the structural data, and any divergence
 *   between claim and computation is the datum the corpus exists to take.
 *
 * KEY AGENTS:
 *   - entrenched_minority_blocs: Primary beneficiary (powerful/constrained) — durable bloc above the blocking fraction; collects the veto value of every blocked reform
 *   - sparsely_populated_state_voters: Amplified-weight beneficiary (organized/constrained) — unit-equal ratification multiplies their voice at amendment moments
 *   - incumbent_officeholders: Secondary beneficiary and administrator (institutional/arbitrage) — staff the chambers and courts that operate the threshold; careers survive any amendment outcome
 *   - contemporary_majorities: Primary target (organized/trapped) — win elections but cannot clear the amendment bar; bear the blocked-reform costs
 *   - blocked_reform_movements: Target (moderate/constrained) — movements whose object requires the locked channel
 *   - legislative_majority_leaders: Nominal-power payer (powerful/constrained) — command majorities that die at the threshold; absorb blame for undelivered platforms
 *   - constitutional_courts: Administrator (institutional/analytical) — police ratification validity, deadlines, and counting; keep the channel formally intact
 *   - future_generations: Excluded (powerless/trapped) — bound by bargains they cannot join
 *   - comparative_constitutionalists: Analytical observer (analytical/analytical) — document the rigidity-duration correlation across systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.66).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Amendment Threshold as Minoritarian Veto (Minoritarian Reading)").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "political/constitutional").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, 'ed954553-9049-4810-8169-85742e0742cf').
narrative_ontology:cs_kernel_codification('ed954553-9049-4810-8169-85742e0742cf', fixed_text).
narrative_ontology:cs_authority_grounding('ed954553-9049-4810-8169-85742e0742cf', extraction).
narrative_ontology:cs_interpretation_layer_present('ed954553-9049-4810-8169-85742e0742cf').
narrative_ontology:cs_reading_relation('ed954553-9049-4810-8169-85742e0742cf', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed954553-9049-4810-8169-85742e0742cf', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('ed954553-9049-4810-8169-85742e0742cf', foundational, no_permanent_minority_veto_legitimate).
narrative_ontology:cs_axiom_status(no_permanent_minority_veto_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ed954553-9049-4810-8169-85742e0742cf', no_permanent_minority_veto_legitimate, deontological).
narrative_ontology:cs_axiom('ed954553-9049-4810-8169-85742e0742cf', secondary, entrenchment_destabilizes_constitutional_order).
narrative_ontology:cs_axiom_status(entrenchment_destabilizes_constitutional_order, holdable).
narrative_ontology:cs_axiom_grounding('ed954553-9049-4810-8169-85742e0742cf', entrenchment_destabilizes_constitutional_order, empirically_contingent).
narrative_ontology:cs_reference_frame('ed954553-9049-4810-8169-85742e0742cf', popular_sovereignty_amendment_accessibility).
narrative_ontology:cs_drift_state('ed954553-9049-4810-8169-85742e0742cf', contemporary_entrenchment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed954553-9049-4810-8169-85742e0742cf', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_minority_blocs).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, sparsely_populated_state_voters).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, incumbent_officeholders).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, blocked_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, legislative_majority_leaders).
narrative_ontology:constraint_vindicates(supermajority_threshold__minoritarian_veto_reading, rigidity_mortality_correlation).
narrative_ontology:constraint_vindicates(supermajority_threshold__minoritarian_veto_reading, extraconstitutional_channel_pressure_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A durable coalition of legislators and ratifying-unit delegations that reliably exceeds the blocking fraction on the issues that matter to it. Its regions and member interests depend on arrangements — apportionment formulas, resource regimes, regulatory settlements — that sitting majorities periodically seek to revise. Because the amendment default is the status quo, blocking costs its members nothing but attendance: they need only stay united, not win. Leaving the bloc would expose its members' protected arrangements to majority revision, so cohesion is maintained by the shared stake.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_minority_blocs, beneficiary,
    powerful, generational, constrained, national).

% Voters in low-population constituent units whose ratification weight is multiplied by unit-equal counting: an amendment must carry three-quarters of the units regardless of how few people live in each. At amendment moments their per-capita voice exceeds that of large-unit voters severalfold. They did not build the rule and do not run it; they collect the amplified voice it grants, and their daily politics is otherwise ordinary.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, sparsely_populated_state_voters, beneficiary,
    organized, biographical, constrained, national).

% Legislators, executives, clerks, and judges who staff the institutions the amendment rule operates through. Whatever the outcome of any amendment fight, their offices, seniority, and jurisdiction persist — the frozen constitution is the furniture of their careers. They also administer the rule day to day: presiding officers count votes, clerks certify ratifications, courts police the channel's boundaries.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, incumbent_officeholders, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, incumbent_officeholders, agenda_setter).

% Electoral majorities that win elections and pass ordinary statutes but cannot clear the two-thirds-plus-three-quarters bar for the constitutional changes they were elected to pursue. Their preferred reforms accumulate as platform promises that die at the threshold; their alternatives are limited to persuading members of the blocking bloc, waiting for demographic drift, or leaving the formal channel altogether. Exit from the arrangement would mean exiting the polity.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    organized, biographical, trapped, national).

% Organized movements — suffrage-era successors, labor, fiscal, and governance reform campaigns — whose objectives are written into the constitution's text or structure and therefore cannot be achieved by statute. Each generation of the movement inherits the same wall; membership turns over faster than the wall moves. Persisting outside the formal channel is possible; achieving the movement's object through it is not, short of flipping the blocking bloc.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, blocked_reform_movements, payer,
    moderate, biographical, constrained, national).

% Leaders of majority parties who campaign on constitutional reform, win mandates, and then watch their agendas stop at the threshold. They bear the blame for undelivered platforms while the blocking bloc absorbs no comparable accountability. The arrangement also shelters them: promising reforms they cannot deliver is electorally cheaper than delivering unpopular ones, and the threshold supplies a standing excuse.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, legislative_majority_leaders, payer,
    powerful, immediate, constrained, national).

% Courts adjudicate what counts as a valid proposal, a valid ratification, a rescission, a deadline. They keep the amendment channel formally intact and policed — which is what makes the blocking veto reliable rather than chaotic. Their own authority rests on the constitution they decline to revise; they have no institutional interest in lowering the bar they administer.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% People not yet born or not yet enfranchised who will live under whatever the freeze preserves. Ratification bargains bind them; they cast no vote in them. Their interests enter the process only through the proxy of whoever happens to hold office, and their objections cannot register until they are already governed by the outcome.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, future_generations, excluded,
    powerless, civilizational, trapped, national).

% Scholars comparing amendment rules across national constitutions: measuring amendment rates, constitutional lifespans, and the frequency of extra-constitutional replacement. They hold no stake in any single constitution's arrangement and publish the cross-national data that outside observers use to evaluate what the threshold does over time.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, comparative_constitutionalists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, entrenched_minority_blocs).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Raises the cost of fundamental constitutional change above ordinary-politics reach: temporary electoral swings cannot rewrite foundational rules, constituent units receive a guaranteed ratification voice, and a focal line separates ordinary legislation from constitutional revision.
% TRANSFER_FUNCTION: Transfers constitutional decision-rights from sitting electoral majorities to whichever durable minority exceeds the blocking fraction; each blocked reform moves the full value of the unmade change to the holders of the protected status quo, and moves the cost of adaptation onto those who sought the change.
% ABSENT_VOICES: Future generations bound by ratification bargains they cannot join; residents of high-population jurisdictions whose per-capita ratification weight is diluted by unit-equal counting; and movements whose object is constitutional change itself, who therefore have no voice inside the channel the threshold governs — their objections register only as extra-constitutional pressure.
% DISAPPEARANCE_RATIONALE: If the threshold vanished overnight, amendment would proceed by simple majority: the backlog of durable-majority reforms would move immediately, blocking minorities would lose their veto and compete for ordinary legislative influence like everyone else, and constitutional politics would merge into ordinary politics — the entire protected position of the status-quo beneficiaries evaporates, so the world rearranges around the loss.
% FOUNDING_PROBLEM: At founding, the rule answered a real problem: protecting the new constitutional order from transient majorities and factional capture, and reassuring smaller constituent units that union would not dissolve their guaranteed voice — a stability-and-inclusion bargain among the founding generation's factions.
% FOUNDING_PROBLEM_CORROBORATION: Defenders inside the benefiting bloc attest the transient-passion problem is live (the consensus-safeguard tradition). Outside the beneficiary set: comparative constitutional scholarship — cross-national duration data linking amendment rigidity to shorter constitutional lifespans and extra-constitutional replacement — attests that the barrier now correlates with instability rather than consensus quality; and the historical record of the barrier yielding only when blocking coalitions dissolved or were dissolved by force (readmission-conditioned ratification) corroborates that its operative function is coalition protection. No fully disinterested domestic corroborator exists; the strongest external attestation is comparative-empirical.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 at interval end) because the transfer is large and decoupled from service: every durable-majority reform stopped at the threshold moves the full value of the unmade change to whoever holds the protected status quo, while the threshold renders no compensating good to those it stops. Suppression (0.66) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's arithmetic; only extractiveness is scaled. It reflects foreclosure of the formal reform channel: ordinary legislation cannot reach constitutional objects, judicial reinterpretation reaches only partway, and the remaining exits (a convention call under the same article, or extra-constitutional replacement) are prohibitively costly. Theater (0.38) tracks the growing share of threshold-defense activity that is rhetorical — deliberation and anti-passion invocations — relative to the arrangement's operative blocking function. Accessibility collapse (0.62) and resistance (0.58) sit in constructed-constraint ranges: alternatives partly survive (statutes, courts, state experimentation) and the arrangement meets sustained, organized opposition rather than acquiescence. The measurement series share one time grid (nine points, 0-240) across all three tracked metrics; trajectories rise monotonically rather than cyclically, modeling accumulation: extraction and enforcement harden as demographic drift widens the gap between the blocking bloc's interests and the median voter's. The suppression_requirement series is authored because the story specifically traces enforcement-capacity change — counting-rule hardening, deadline imposition, judicial gatekeeping — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the entrenched bloc's seat the arrangement is a guarantee: their constituents' settled arrangements simply persist, at zero marginal cost. From the contemporary-majority seat the identical rule is a wall: elections are won and nothing constitutional follows. Same-level divergence is the sharpest datum: legislative_majority_leaders and entrenched_minority_blocs hold comparable formal power, yet one experiences the threshold as paralysis and the other as protection — differentiation comes entirely from position relative to the blocking fraction, not from global standing. Constitutional_courts experience a functioning, policed procedure; blocked_reform_movements experience the same procedure as closure. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: entrenched_minority_blocs collect the veto value directly; sparsely_populated_state_voters collect amplified ratification voice without administering anything; incumbent_officeholders collect career persistence and are additionally damped toward the beneficiary end by arbitrage-grade exit — their offices survive any amendment outcome, so the arrangement's fall costs them little. Victim declarations drive high directionality: contemporary_majorities are trapped (no alternative formal channel), and blocked_reform_movements are constrained (persistence outside the channel is possible; achieving the object through it is not). No directionality_overrides are authored: the role-plus-exit declarations already differentiate the two powerful seats (bloc versus majority leadership), and an override keyed only to a power atom would misstate one of them. One qualitative asymmetry is noted for the record: majority leaders also derive a blame-shelter benefit from the threshold (campaigning on undeliverable reform is cheaper than delivering it), which would pull their effective directionality somewhat below the pure-payer value; the structural arrays do not encode this, and it is flagged here rather than forced through an override that would distort the bloc's seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — filtering transient majoritarian passion while guaranteeing constituent-unit voice — is authored as contested, not dead: the parties genuinely dispute whether that problem persists or has been inverted into minority entrenchment. Because the status is contested rather than dead, the mismatch consumer finds no zombie flag here, correctly: this is not an atrophied shell. The veto operates at full force; nothing about the arrangement is inertial or theatrical in its core mechanism. Mandatrophy analysis nonetheless matters for classification hygiene: the genuine coordination residue (stability, unit voice, a focal constitutional-politics boundary) is real, and naming it prevents overcorrection — the claim is not that coordination is absent but that it is outrun by asymmetric transfer with identifiable collectors. A scaffold reading (transition tool awaiting sunset) fails structurally: the rule carries no sunset and was not built to expire. A piton reading fails: the function has not atrophied — blocking is the function, performed continuously. The snare claim stands on the combination the schema requires: named victims, named beneficiaries, active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the minoritarian_veto_reading of the supermajority_threshold kernel; which structural facts would change if the consensus_safeguard_reading or adaptive_gradient_reading were instantiated instead?',
    'Cross-reading comparison on the shared referent: re-author epsilon, beneficiaries, and victims under each sibling reading and compare computed classifications; convergence on functional-dominance evidence resolves the indexical split.',
    'Under consensus_safeguard_reading the same threshold computes as a protective coordination device with diffuse beneficiaries; under adaptive_gradient_reading as a conditionally legitimate tunable instrument; the snare classification with named victims is specific to this reading''s function attribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings emit different constraints from the same text.').

omega_variable(
    operative_function_dominance,
    'Is the threshold''s dominant operative function consensus-filtering (as the sibling reading claims) or minority-veto entrenchment (as this reading claims)?',
    'Classify the population of constitutionally blocked reforms by counterfactual durability: would each have passed under a simple-majority rule and remained stable after passage? Dominance of durable-majority blocks confirms this reading; dominance of transient blocks supports the sibling.',
    'If transient blocks dominate, epsilon drops toward tangled_rope range and the snare claim fails; if durable blocks dominate, the snare classification is confirmed and the safeguard framing is exposed as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_function_dominance, empirical, 'Which function dominates the threshold''s actual operation determines which reading''s classification is correct.').

omega_variable(
    blocking_coalition_identity_stability,
    'Is the exercising blocking coalition a stable, identity-defined bloc (indicating privilege conversion) or a rotating issue-by-issue coalition (indicating ordinary supermajority friction)?',
    'Longitudinal analysis of amendment votes and ratification patterns: track whether the same units and blocs recur across unrelated blocked reforms over decades.',
    'Stable recurring blocs confirm the historical-privilege mechanism and sustain high directionality for the beneficiary seat; rotation downgrades the constraint toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocking_coalition_identity_stability, empirical, 'Whether the veto is exercised by a fixed privileged coalition or by shifting majorities-of-minorities.').

omega_variable(
    informal_adaptation_offset,
    'To what extent does informal adaptation (judicial reinterpretation, statutory workarounds, interstate competition) substitute for blocked formal amendment, offsetting the harms this reading attributes to the threshold?',
    'Compare policy outcomes in rigid-amendment systems against matched flexible-amendment systems on the blocked reform domains.',
    'High substitution reduces realized harm and pulls effective extraction down despite formal blockage; low substitution confirms the veto''s full incidence on contemporary majorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_adaptation_offset, empirical, 'Whether informal channels absorb the change pressure the formal channel refuses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(minoritarian_veto_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(minoritarian_veto_tr_t30, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(minoritarian_veto_tr_t60, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(minoritarian_veto_tr_t90, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 90, 0.22).
narrative_ontology:measurement(minoritarian_veto_tr_t120, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement(minoritarian_veto_tr_t150, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 150, 0.28).
narrative_ontology:measurement(minoritarian_veto_tr_t180, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 180, 0.31).
narrative_ontology:measurement(minoritarian_veto_tr_t210, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 210, 0.35).
narrative_ontology:measurement(minoritarian_veto_tr_t240, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 240, 0.38).

% Extraction over time
narrative_ontology:measurement(minoritarian_veto_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(minoritarian_veto_be_t30, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(minoritarian_veto_be_t60, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(minoritarian_veto_be_t90, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 90, 0.59).
narrative_ontology:measurement(minoritarian_veto_be_t120, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 120, 0.63).
narrative_ontology:measurement(minoritarian_veto_be_t150, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 150, 0.67).
narrative_ontology:measurement(minoritarian_veto_be_t180, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 180, 0.71).
narrative_ontology:measurement(minoritarian_veto_be_t210, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 210, 0.75).
narrative_ontology:measurement(minoritarian_veto_be_t240, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 240, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(minoritarian_veto_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(minoritarian_veto_su_t30, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(minoritarian_veto_su_t60, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(minoritarian_veto_su_t90, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 90, 0.43).
narrative_ontology:measurement(minoritarian_veto_su_t120, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 120, 0.48).
narrative_ontology:measurement(minoritarian_veto_su_t150, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 150, 0.53).
narrative_ontology:measurement(minoritarian_veto_su_t180, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 180, 0.57).
narrative_ontology:measurement(minoritarian_veto_su_t210, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 210, 0.62).
narrative_ontology:measurement(minoritarian_veto_su_t240, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 240, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, adaptive_gradient_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, judicial_interpretive_expansion).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'supermajority threshold' (epsilon-invariance rule): one label, three structurally distinct claims. This file is the minoritarian-veto member (high epsilon, named beneficiaries and victims, snare). consensus_safeguard_reading is the deep-consensus-filter member (diffuse benefit, low extraction, protective). adaptive_gradient_reading is the calibration-instrument member (conditional, evidence-indexed legitimacy). Upstream/downstream: the consensus-safeguard reading supplies the public justification this reading identifies as the veto's cover, so the safeguard story functions as this one's upstream legitimation source; the adaptive-gradient reading sits downstream of both, recasting the contest as a tuning problem. All three link through affects_constraints; each carries its own epsilon and must not average across readings. The third edge records that this reading's lock-in pushes change pressure onto judicial interpretation, feeding the separate judicial_interpretive_expansion constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
