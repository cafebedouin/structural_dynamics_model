% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause Boundary — Originalist Narrow Reading (Border-Crossing Trade Only)
 *   domain: constitutional law/federalism/commerce regulation
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the contested kernel
 *   commerce_clause_text: the originalist narrow reading, under which federal
 *   regulatory authority reaches only trade that physically crosses state
 *   lines and the instrumentalities (vessels, vehicles, channels) of
 *   interstate movement, while all intrastate economic activity remains
 *   governed exclusively by state police power. Per the ε-invariance
 *   principle, the sibling readings — expansive_federal_reading and
 *   substantial_effects_limited_reading — are separate constraint files with
 *   their own ε values, beneficiary/victim sets, and classifications; this
 *   story authors ε for the narrow-boundary arrangement as the originalist
 *   reading itself assesses it: a genuine coordination achievement (clean
 *   jurisdictional allocation, locally accountable governance) that
 *   nonetheless concentrates real costs on identifiable seats, which the
 *   reading counts as constitutionally mandated allocation rather than
 *   extraction — landing reading-indexed ε moderate rather than low. The
 *   three family members are linked through network.affects_constraints and
 *   the decomposition is documented in network.dual_formulation_note. KEY
 *   AGENTS (by structural relationship): - state_governments: Primary
 *   beneficiary (institutional/constrained) — collects exclusive intrastate
 *   regulatory authority - anti_federal_consolidation_advocates: Secondary
 *   beneficiary (organized/identity_locked) — ideological constituency fused
 *   with the boundary - cross_border_externality_bearers: Primary target
 *   (powerless/trapped) — absorb spillovers the boundary leaves unmanaged -
 *   multistate_businesses: Primary target (powerful/constrained) — bear
 *   fifty-regime compliance fragmentation - federal_congressional_majorities:
 *   Institutional target (institutional/constrained) — denied the federal
 *   regulatory instrument - supreme_court_originalist_majority: Agenda setter
 *   (institutional/identity_locked) — polices the boundary by invalidating
 *   statutes - national_solution_advocates: Excluded seat (organized/mobile)
 *   — rerouted to weakened state venues -
 *   comparative_constitutional_scholars: Analytical observer — sees the full
 *   structure, collects nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.48).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.55).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause Boundary — Originalist Narrow Reading (Border-Crossing Trade Only)").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional law/federalism/commerce regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, 'b489f463-3c50-4170-8797-ad4646278faf').
narrative_ontology:cs_kernel_codification('b489f463-3c50-4170-8797-ad4646278faf', fixed_text).
narrative_ontology:cs_authority_grounding('b489f463-3c50-4170-8797-ad4646278faf', lineage).
narrative_ontology:cs_interpretation_layer_present('b489f463-3c50-4170-8797-ad4646278faf').
narrative_ontology:cs_reading_relation('b489f463-3c50-4170-8797-ad4646278faf', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('b489f463-3c50-4170-8797-ad4646278faf', commerce_clause_text__substantial_effects_limited_reading, forecloses).
narrative_ontology:cs_axiom('b489f463-3c50-4170-8797-ad4646278faf', foundational, commerce_power_reaches_only_border_crossing_trade_and_instrumentalities).
narrative_ontology:cs_axiom_status(commerce_power_reaches_only_border_crossing_trade_and_instrumentalities, holdable).
narrative_ontology:cs_axiom_grounding('b489f463-3c50-4170-8797-ad4646278faf', commerce_power_reaches_only_border_crossing_trade_and_instrumentalities, conventional).
narrative_ontology:cs_axiom('b489f463-3c50-4170-8797-ad4646278faf', foundational, intrastate_police_power_exclusively_state_retained).
narrative_ontology:cs_axiom_status(intrastate_police_power_exclusively_state_retained, holdable).
narrative_ontology:cs_axiom_grounding('b489f463-3c50-4170-8797-ad4646278faf', intrastate_police_power_exclusively_state_retained, conventional).
narrative_ontology:cs_reference_frame('b489f463-3c50-4170-8797-ad4646278faf', original_public_meaning_commerce_scope).
narrative_ontology:cs_drift_state('b489f463-3c50-4170-8797-ad4646278faf', contemporary_post_new_deal_doctrine, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b489f463-3c50-4170-8797-ad4646278faf', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, cross_border_externality_bearers).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, multistate_businesses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, federal_congressional_majorities).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, enumerated_powers_limitation).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, dual_sovereignty_allocation).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, state_police_power_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislate labor, safety, environmental, and commercial rules for activity inside their borders; under this boundary no federal statute may reach that intrastate activity, so their regulatory authority within it is exclusive. They cannot reach activity in other states whose effects spill across their own borders except through interstate compacts that require the source state's consent. Their gain is the retained authority itself; their loss is any instrument for governing what enters from next door.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, constrained, regional).

% A political and intellectual movement whose organizing commitment is a federal government of enumerated powers. The border-crossing boundary is the practical achievement they defend in litigation, scholarship, and judicial-confirmation politics. Conceding the boundary would dissolve the movement's defining project, so stepping away from defending it is not a live option for its core members; their political identity is constituted through maintaining this line.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, generational, identity_locked, national).

% Residents and downstream jurisdictions that absorb pollution plumes, disease spread, aquifer depletion, or financial distress originating in other states' intrastate activity. The federal remedial instrument is unavailable to them under this boundary; their recourse runs through the source state's domestic politics or through compacts the source state must agree to. Moving away from the spillover is costly and partial, and the harm regenerates at the border.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, cross_border_externality_bearers, payer,
    powerless, biographical, trapped, regional).

% Firms selling into all fifty states must track and satisfy divergent state regimes governing the intrastate portions of their operations — labeling, employment terms, product standards, licensing. Uniform national rules and federal preemption are unavailable under this boundary, so compliance overhead scales with state count. They lobby continuously for harmonization the boundary forbids Congress from delivering, and they cannot serve the national market without accepting the fragmentation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, multistate_businesses, payer,
    powerful, biographical, constrained, continental).

% Elected federal legislators who periodically seek to address problems whose causes sit inside state borders — workplace safety, product hazards, environmental spillovers — and find the relevant bills unconstitutional as drawn under this boundary. Their substitutes are narrower bills, spending conditions attached to grants, or constitutional amendment; each is slower, weaker, or more remote than direct regulation, and amendment is practically out of reach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_congressional_majorities, payer,
    institutional, biographical, constrained, national).

% The judicial coalition that polices the boundary by reviewing federal statutes and invalidating those that reach past border-crossing trade and the instrumentalities that carry it. Its authority claim rests on applying the founding text's fixed meaning rather than updating it; abandoning the policing role would unsettle the institutional self-conception that distinguishes it from an ordinary policy-making body, so the role sustains itself through the coalition's own commitments.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, supreme_court_originalist_majority, agenda_setter,
    institutional, generational, identity_locked, national).

% Consumer-safety, environmental, and labor coalitions whose preferred instrument is a single federal standard. Under this boundary their proposals fail as drafted, so they reroute through fifty state legislatures, ballot initiatives, and compact negotiations — campaigns that multiply fifty-fold and dilute their resources. They are present in public argument but absent from the operative framework their arguments target.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_solution_advocates, excluded,
    organized, biographical, mobile, national).

% Academic analysts who compare this boundary with other federations' allocations of regulatory authority and trace its enforcement record across two centuries. They publish assessments of where the line has held, migrated, or collapsed, but hold no enforcement or legislative seat and collect nothing from the arrangement's operation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_text__originalist_narrow_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory jurisdiction between two levels of government along a fixed textual line: trade crossing state borders and the vessels, vehicles, and channels of interstate movement belong to Congress; everything else belongs to state police power. This solves duplication and jurisdictional conflict, keeps policy accountable to the governed locality, and preserves competitive federalism among the states.
% TRANSFER_FUNCTION: Moves regulatory authority — and the discretionary rents attached to it — from the federal government to state governments; moves the costs of unmanaged cross-border spillovers onto residents of receiving states and the costs of regulatory fragmentation onto multistate firms; moves the federal legislative instrument out of the hands of congressional majorities confronting intrastate-origin problems.
% ABSENT_VOICES: Cross-border externality bearers have no seat: the boundary was drawn without them, and the remedy they would ask for — federal management of spillovers — is precisely what the line forecloses. Future generations accumulating slow spillovers (aquifer drawdown, climate-relevant emissions) are absent entirely. National-solution advocates are present only as litigants and lobbyists outside the enforcement coalition; their objections are heard and then routed to venues the boundary renders weak.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, federal regulatory authority would expand into intrastate economic activity, state-exclusive domains would face preemption in overlapping fields, the litigation and scholarly industry organized around policing the line would lose its object, and multistate compliance structures would reorganize around a single national regime. State governments would lose their most consequential retained power; nothing about the surrounding economy would hold the previous shape.
% FOUNDING_PROBLEM: After independence, states discriminated against each other's goods, levied rival tariffs, and issued paper money that destabilized neighboring economies, while the Continental Congress possessed no commerce power at all; the Philadelphia Convention created one to secure a common market. This reading fixes that power at its founding reach — trade that crosses state lines and the instrumentalities that carry it — leaving all else to the states that held it before the Convention met.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside any benefiting party corroborate the founding problem's reality: the documentary record of state trade discrimination and the Annapolis Convention's call is not authored by the boundary's defenders. Whether the problem remains live divides on beneficiary lines — state-aligned federalism scholars attest the allocative problem is perennial, while national-solution advocates and externality-bearer representatives attest the original problem (state trade barriers) was solved long ago and the line now only blocks responses to newer problems. No disinterested seat attests that the founding problem is still unsolved as originally stated; that absence is itself signal.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48, reading-indexed: the originalist lights register the blocked national solutions and the unmanaged spillovers as real costs borne by real seats, but discount them as constitutionally mandated division rather than extraction, holding ε moderate; the temporal series shows that cost base growing with economic integration (0.20 in 1790, when markets were local and spillovers small, to 0.48 today, when supply chains, capital, and externalities routinely cross borders). Suppression is 0.55: the operative force is judicial invalidation aimed at legislative instruments rather than persons, and state-action and compact alternatives remain open by design — but for spillover bearers the federal remedy is wholly closed, which is a structural closure, not an internalized one. Theater is 0.20: when enforced, the boundary does decisive work (statutes fall), with a rhetorical surplus around confirmation politics; the series spikes near the 1937 collapse, when invocation of the line became largely ceremonial, and falls back as post-1995 enforcement resumed real operation. Accessibility collapse is low (0.28) because alternatives are preserved by the arrangement's own design — state police power and consensual compacts remain live venues; this is the opposite profile of a natural law, which closes alternatives completely. Resistance is high (0.68): two centuries of congressional testing, national-advocacy litigation, and a mid-century doctrinal defeat the reading never accepted. The enforcement series is cyclical rather than monotonic — near-continuous policing before 1937, roughly four decades of dormancy, revival after 1995 — and the cycle is driven by judicial personnel and economic crisis, not by intermittent reinforcement as an extraction mechanism; the base_properties scalars were measured at the current revival phase (2026 endpoint of the shared grid). Claim and metrics are independent: claimed_type is tangled_rope because the structure holds both a genuine coordination function (jurisdictional allocation nobody else performs) and asymmetric extraction through the same line (bearer and firm seats pay; state and movement seats collect), sustained by active judicial enforcement — while every metric was authored from the descriptive record without reference to that classification.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same line. From state_governments the boundary is the guarantee of their authority — coordination they administer and profit from; from cross_border_externality_bearers the identical line is the removal of their remedy; from federal_congressional_majorities it is a capacity ceiling encountered bill by bill; from the Court it presents as neutral arbitration rather than allocation at all. Two identity locks stabilize the structure: the anti-consolidation movement is ideologically fused (its worldview makes concession unthinkable — if the frame broke, its members would migrate toward the nearest live alternative that preserves a limiting function, the substantial-effects-limited reading, rather than the expansive one), and the Court is institutionally fused (it has become its boundary-policing function; abandoning the role would dissolve its claim to stand apart from politics). Coalition capacity is sharply asymmetric across the payer side: externality bearers are diffuse, geographically separated, and rarely coordinate across state lines, while multistate businesses coordinate effectively through trade associations — so the better-resourced payer seat wins partial accommodations (safe harbors, tailored preemption requests) that the trapped seat never obtains, even though both sit on the same side of the line.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. state_governments are named beneficiaries whose gain is the direct flow of the arrangement (retained authority), placing them near the beneficiary pole; anti_federal_consolidation_advocates sit nearby, with identity lock stabilizing their subsidized position rather than amplifying cost. cross_border_externality_bearers are named victims with trapped exit, placing them near the full-target end — trapped targets sit nearer full-target than mobile ones. multistate_businesses are named victims with constrained (not trapped) exit and sufficient power to win partial relief, sitting high but below the bearers. federal_congressional_majorities bear a denied-instrument cost through their constrained-exit payer seat. The Court's seat is administrative and near-symmetric. No directionality_overrides are authored: the override surface is keyed by power atom, and this story's three institutional seats (state governments, Congress, the Court) occupy genuinely different structural positions that a single power-atom override could not separate without corrupting the other two — the structural declarations plus exit options already yield the correct ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem has two halves that aged differently. The trade-barrier half (states taxing each other's goods) is solved — no modern state erects tariffs, and the dormant-commerce doctrine polices the residue — while the allocative half (which level of government governs what) is perennially live. Hence founding_problem_status: contested, paired with disappearance_verdict: world_rearranges; the mismatch consumer reads status x verdict, and contested-status with world-rearranges does not trip the dead-problem zombie flag — correctly, because the arrangement persists on the strength of its live half, not as inertial performance. The tangled_rope classification is what prevents mislabeling in both directions: a pure-extraction reading would erase the real jurisdiction-coordination function that no other structure performs, and a pure-coordination reading would erase the bearer and firm seats that demonstrably pay through the same line. Fixing the arrangement — moving the line — requires either constitutional amendment or a sustained judicial supermajority realignment, a cost class far above the benefit of any single adjustment, hence fixing_cost: prohibitive; meanwhile the arrangement's operative gains demonstrably accrue to the state-government seat, hence gain_flow naming that seat rather than diffuse. Receipt and cost are recorded independently, on their own evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_scope_line,
    'This constraint is one reading of the commerce_clause_text kernel. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Doctrinal cartography: locate the scope line each reading draws (border-crossing transactions only, versus substantial aggregate effects, versus effects gated by jurisdictional nexus and non-pretext) and enumerate which intrastate activities each places inside federal power.',
    'Adopting a sibling changes the victim set (externality bearers become federally reachable or not), flips payer-seat directionalities, and shifts the classification profile between the hybrid and pure-coordination shapes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_scope_line, conceptual, 'Committer structure: one of three live readings of one textual grant; the disagreement is located at the scope line.').

omega_variable(
    spillover_magnitude_under_boundary,
    'How large are the cross-border externalities (air and water pollution, disease spread, aquifer depletion, financial contagion) that the boundary leaves to source-state discretion, relative to what state action and consensual compacts can actually manage?',
    'Compare spillover outcomes across periods and federations operating under narrow versus expanded central commerce authority; measure compact formation rates, coverage gaps, and failure modes.',
    'Large, compact-unmanageable spillovers raise effective extraction on the bearer seat and push the arrangement toward a snare-flavored profile; manageable ones support the coordination reading and hold the hybrid balanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spillover_magnitude_under_boundary, empirical, 'Magnitude of unmanaged cross-border externalities relative to sub-federal management capacity.').

omega_variable(
    fragmentation_cost_magnitude,
    'What do multistate actors actually pay in duplicated compliance across fifty divergent intrastate regimes, relative to a harmonized counterfactual?',
    'Economic studies quantifying regulatory-divergence compliance costs (labeling, employment rules, product standards, licensing) for multistate firms, with harmonized-jurisdiction baselines.',
    'High fragmentation costs raise the multistate-business seat''s effective extraction and strengthen the extraction half of the hybrid; low costs support treating the boundary as near-pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_cost_magnitude, empirical, 'Compliance-fragmentation burden on multistate actors versus a uniform-regime counterfactual.').

omega_variable(
    stable_line_drawability,
    'In an economy where supply chains, information flows, and capital routinely cross state lines, can any stable line be drawn between ''crossing borders'' and ''affecting commerce,'' or does the boundary inevitably migrate under interpretive pressure?',
    'Doctrinal history of line-drawing attempts — navigation-era definitions, stream-of-commerce theories, modern nexus rules — and their stability under successive waves of economic integration.',
    'If no stable line exists, the constraint is inherently transitional regardless of its founders'' intent — a scaffold-flavored trajectory — and its enforcement will oscillate indefinitely between revival and collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stable_line_drawability, conceptual, 'Whether a durable border-crossing/effects line is drawable in an integrated economy.').

omega_variable(
    enforcement_volatility_driver,
    'What drives the enforcement cycle visible in the record — near-continuous policing before 1937, roughly four decades of dormancy, revival after 1995: judicial personnel, economic crisis, or instability internal to the doctrine?',
    'Judicial-politics analysis of the switching episodes (the 1937 transformation, the post-1995 revival) controlling for bench composition and crisis timing.',
    'Personnel-driven swings mean the arrangement''s persistence depends on sustaining a judicial coalition — an enforcement-maintenance profile characteristic of the hybrid; doctrine-internal instability means the scope line itself cannot hold, feeding the stable_line_drawability omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_volatility_driver, empirical, 'Driver of the historical enforcement collapse-and-revival cycle.').

omega_variable(
    epsilon_ledger_for_blocked_solutions,
    'Does this reading''s own accounting place boundary-blocked national solutions (uniform standards, federal externality management) on the extraction ledger, or allocate them away as constitutionally mandated division that generates no extraction claim?',
    'Internal consistency test within the reading''s normative framework: examine whether the reading concedes offsetting arrangements (compacts, cooperative programs, state-level innovation) that compensate the blocked solutions, and whether it treats the blocked instruments as costs at all.',
    'Counting them raises reading-indexed ε toward 0.6 and strengthens the extraction half of the hybrid; allocating them away holds ε near 0.45 and keeps the classification balanced between coordination and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_ledger_for_blocked_solutions, conceptual, 'Whether the reading''s own lights book blocked national solutions as extraction or as mandated allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1790, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1790, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1790, 0.08).
narrative_ontology:measurement_basis(comm_tr_t1790, observed).
narrative_ontology:measurement(comm_tr_t1860, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement_basis(comm_tr_t1860, observed).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.26).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement_basis(comm_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1790, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1790, 0.2).
narrative_ontology:measurement_basis(comm_be_t1790, observed).
narrative_ontology:measurement(comm_be_t1860, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1860, 0.3).
narrative_ontology:measurement_basis(comm_be_t1860, observed).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.44).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.46).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2026, 0.48).
narrative_ontology:measurement_basis(comm_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1790, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1790, 0.38).
narrative_ontology:measurement_basis(comm_su_t1790, observed).
narrative_ontology:measurement(comm_su_t1860, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1860, 0.46).
narrative_ontology:measurement_basis(comm_su_t1860, observed).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.14).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement_basis(comm_su_t1995, observed).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(comm_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Commerce Clause' conflates three structurally distinct authority-scopes, decomposed per the ε-invariance principle into a three-story constraint family sharing the kernel commerce_clause_text. This file (originalist_narrow_reading) authors ε for the border-crossing-only arrangement as the originalist lights assess it — moderate, with costs booked as mandated allocation. expansive_federal_reading authors ε for the substantial-effects arrangement from its own lights; substantial_effects_limited_reading authors an intermediate arrangement gated by nexus and non-pretext. Each member carries its own ε, beneficiary/victim sets, and classification; edges here assert logical exclusivity within a single doctrinal framework, not predictions about which reading prevails. The upstream/downstream resource dynamic runs the other way historically: the expansive reading's mid-century dominance is what this reading's revival pressures react against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
