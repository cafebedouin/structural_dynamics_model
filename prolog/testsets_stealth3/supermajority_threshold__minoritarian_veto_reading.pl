% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Amendment Barrier as Minoritarian Veto
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the supermajority-threshold
 *   kernel: the minoritarian-veto reading, under which a constitutional
 *   amendment rule requiring more-than-majority assent (two-thirds of each
 *   chamber plus three-quarters of constituent units, in the canonical
 *   federal form) operates as a standing veto held by whichever minority can
 *   assemble the blocking fraction. On this reading the barrier's
 *   deliberative justification is cover: what it durably does is convert
 *   founding-era privileges — unit weighting, property protections,
 *   entrenched office — into protection no election can touch, while
 *   contemporary majorities pay in forgone self-government. The epsilon
 *   referent is the standing amendment-barrier arrangement, assessed by this
 *   reading's own lights (hence high); it is never the safeguard arrangement
 *   the sibling readings would defend. Sibling readings —
 *   consensus_safeguard_reading and adaptive_gradient_reading — are separate
 *   constraint files linked through network.affects_constraints; their
 *   epsilon values differ because epsilon is reading-indexed over a shared
 *   referent, not because the barrier changed.
 *
 * KEY AGENTS:
 *   - - entrenched_blocking_minorities: Primary beneficiary (powerful/arbitrage) — wields the standing veto the threshold confers; needs only the blocking fraction, never a majority
 *   - - incumbent_privilege_holders: Secondary beneficiary (powerful/arbitrage) — collects preserved rents from the frozen status quo without administering anything
 *   - - contemporary_majorities: Primary target (organized/trapped) — wins elections but cannot convert them into fundamental change; pays in forgone self-government
 *   - - reform_advocacy_coalitions: Secondary target (moderate/constrained) — bears repeated failed-campaign costs chasing reforms parked behind the barrier
 *   - - constitutional_guardian_institutions: Agenda setter (institutional/identity_locked) — administers and judicially defends the amendment process; identity fused with textual guardianship
 *   - - future_generations: Excluded party (powerless/trapped) — inherits the freeze with no seat in amendment politics
 *   - - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — documents which reforms stalled at which thresholds and who retained advantage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.82).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Amendment Barrier as Minoritarian Veto").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "political/constitutional").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, 'bc698804-19a2-4bb9-881b-d98171c3671a').
narrative_ontology:cs_kernel_codification('bc698804-19a2-4bb9-881b-d98171c3671a', fixed_text).
narrative_ontology:cs_authority_grounding('bc698804-19a2-4bb9-881b-d98171c3671a', lineage).
narrative_ontology:cs_interpretation_layer_present('bc698804-19a2-4bb9-881b-d98171c3671a').
narrative_ontology:cs_reading_relation('bc698804-19a2-4bb9-881b-d98171c3671a', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc698804-19a2-4bb9-881b-d98171c3671a', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('bc698804-19a2-4bb9-881b-d98171c3671a', foundational, entrenchment_converts_privilege_into_veto).
narrative_ontology:cs_axiom_status(entrenchment_converts_privilege_into_veto, holdable).
narrative_ontology:cs_axiom_grounding('bc698804-19a2-4bb9-881b-d98171c3671a', entrenchment_converts_privilege_into_veto, empirically_contingent).
narrative_ontology:cs_axiom('bc698804-19a2-4bb9-881b-d98171c3671a', foundational, majority_self_government_presumption).
narrative_ontology:cs_axiom_status(majority_self_government_presumption, holdable).
narrative_ontology:cs_axiom_grounding('bc698804-19a2-4bb9-881b-d98171c3671a', majority_self_government_presumption, deontological).
narrative_ontology:cs_reference_frame('bc698804-19a2-4bb9-881b-d98171c3671a', founding_bargain_privilege_protection).
narrative_ontology:cs_drift_state('bc698804-19a2-4bb9-881b-d98171c3671a', contemporary_mass_democracy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bc698804-19a2-4bb9-881b-d98171c3671a', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_blocking_minorities).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, incumbent_privilege_holders).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_advocacy_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, constitutional_guardian_institutions).
narrative_ontology:constraint_vindicates(supermajority_threshold__minoritarian_veto_reading, founder_authority_doctrine).
narrative_ontology:constraint_vindicates(supermajority_threshold__minoritarian_veto_reading, constitutional_immutability_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of constituent units and factional blocs whose combined weight falls short of a majority but exceeds the blocking fraction. They need not win elections to prevail on fundamental questions: assembling enough seats, states, or ratification votes defeats any amendment they oppose. Their disproportionate weight in the chamber-and-state architecture — a product of founding-era bargains — is what the threshold converts into standing veto power. Exit is unnecessary: the arrangement insures them against losing, and their members can shift among veto points (chambers, units, courts) as political winds move.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_blocking_minorities, beneficiary,
    powerful, generational, arbitrage, national).

% Economic and social elites whose asset values, market positions, and customary prerogatives depend on the legal status quo remaining frozen. They run nothing and need only fund defense of the barrier; every blocked reform preserves their position another cycle. Their exposure is indirect — they hold no institutional veto of their own — but their returns track the barrier's durability.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, incumbent_privilege_holders, beneficiary,
    powerful, generational, arbitrage, national).

% The living electorate, which can win ordinary elections yet cannot translate electoral victory into fundamental change. Each generation inherits rules it never consented to and finds the amendment door barred by fractions it did not choose. Exit means emigration, which is costly and partial; staying means paying in forgone self-government — suffrage extensions delayed, apportionment distortions uncorrected, fiscal and rights reforms stalled.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    organized, biographical, trapped, national).

% Organized movements that campaign for specific fundamental reforms. They bear the costs of repeated failed campaigns — litigation, convention-application drives, referendum efforts that clear every hurdle except the last fraction. Their exit is constrained: they can redirect energy toward courts and ordinary legislation, but the objects they exist to pursue sit behind the barrier.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_advocacy_coalitions, payer,
    moderate, biographical, constrained, national).

% Courts, presiding officers, and chamber clerks who administer the amendment process: certifying proposals, judging ratification regularity, voiding shortcuts. Their authority and self-concept are fused with textual guardianship — treating the founding text as binding is what makes them custodians rather than mere officials. The freeze also enlarges them: when formal amendment stalls, interpretive authority migrates to their benches, a benefit they did not seek and rarely name.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_guardian_institutions, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, constitutional_guardian_institutions, beneficiary).

% People not yet born who will inherit the frozen rules and the blocked-reform queue. They hold no seat in any chamber and no vote in any ratification; the barrier was set before they existed and operates whether or not they would consent. Their objection — that each generation should not be governed by another's lock — is structurally uncastable.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, future_generations, excluded,
    powerless, generational, trapped, global).

% Analysts who compare amendment rules across orders and eras, documenting which reforms stalled at which thresholds and who retained advantage. They collect testimony from every seat, publish the comparative record, and take no side in amendment politics; their seat exists to see the whole structure.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, entrenched_blocking_minorities).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real commitment problem: constituent units and minority factions will not bind into (or remain in) a shared fundamental-law framework that a transient majority could rewrite each cycle; the threshold gives every faction assurance that fundamental rules move slowly, stabilizing long-horizon investment and inter-unit bargains.
% TRANSFER_FUNCTION: Moves veto power over fundamental law from numerical majorities to whatever minority coalition can assemble the blocking fraction, and thereby preserves the offices, asset values, and customary prerogatives of those favored by the frozen status quo — paid for by the forgone self-governance of contemporary majorities and the blocked-reform queue handed to future generations.
% ABSENT_VOICES: Those bound by the founding bargain without consent — the enslaved, disenfranchised women, colonized populations at founding — and everyone born since: future generations inherit the freeze with no seat in any ratification. They are absent because the threshold predates their standing and the freeze keeps them out; the people most burdened by the barrier are precisely those with no vote on it. Pairs with the future_generations excluded seat.
% DISAPPEARANCE_RATIONALE: If the barrier vanished overnight, amendment would proceed by simple majority: the blocked-reform queue (suffrage extensions, apportionment correction, fiscal and rights revisions) would begin moving immediately, entrenched minorities would lose standing veto rents, privilege holders would face repricing of protected positions, and guardian institutions would lose the interpretive windfall — the constitutional order would reorganize around majoritarian responsiveness within a few electoral cycles.
% FOUNDING_PROBLEM: At founding, sovereign constituent units and rival factions would not join (or remain in) a union whose fundamental rules a transient majority could rewrite: the threshold was built to buy their consent by guaranteeing that specific founding-era arrangements — unit weighting, property protections, and in several orders chattel slavery — could not be amended away by ordinary majorities.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting coalition: ratification-era convention records and founder correspondence (public archives) show the threshold was traded for unit consent and specific privilege protections; comparative-politics scholarship documents the same trade recurring across federal orders. The benefiting parties attest the minority-protection problem is still live; civil-society reform coalitions and empirical amendment studies attest the founding-specific protections are obsolete while the barrier persists. No external source settles the dispute — hence contested.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.78 at interval end) because the barrier decouples fundamental-law outcomes from electoral preference: a durable minority holding the blocking fraction defeats any reform indefinitely, so the transfer of governing capacity runs one way, every cycle, compounding. Suppression (0.82) is authored separately and remains a raw structural property — the engine scales only extractiveness (by directionality and spatial scope); the barrier's persistence depends on actively maintained procedural exclusion (certification refusals, ratification-deadline maneuvers, court doctrine voiding irregular amendment paths), not participant preference. Theater ratio (0.40) captures the growing share of maintenance activity that is performative — anniversary pageantry, civic pedagogy teaching that amendment difficulty is itself a virtue, gravitas rituals — defending the veto rather than conducting deliberation. Accessibility_collapse (0.50) reflects partial survival of alternatives (ordinary legislation, judicial interpretation, subnational experimentation) alongside collapse of the fundamental-amendment route once understood; resistance (0.62) reflects sustained reform movements, convention-application drives, and scholarly delegitimation. The measurement series run on one shared seven-point grid (every tracked metric authored at every point); trajectories are monotonic — no oscillation, so no intermittent-reinforcement mechanism is claimed. Suppression's structural/internalized split is handled narratively here and by the internalized_barrier_reverence omega. Claim and metrics are independent authored facts: claimed_type snare states this reading's structural assessment; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural inputs. From the guardian-institution seat the barrier is constitutional craft — the source of custodial authority and, unstated, of enlarged interpretive power when formal amendment stalls; that seat computes far less extractive than the payer seats. From the contemporary-majority seat the same text is a locked door: electoral victory without conversion. Entrenched minorities experience insurance — the rare seat for which the barrier is purely subsidizing. Identity-lock dynamics bind the guardian seat: its self-concept as custodian depends on treating the founding text as binding rather than as one negotiable layer; if that frame broke — if courts acknowledged they function as the de facto amendment mechanism — the enforcement story would change and the seat's computed extraction would rise sharply. The divergence is the measurement; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real positions: entrenched_blocking_minorities and incumbent_privilege_holders sit near the beneficiary end (d approximately 0.05-0.15) — the barrier subsidizes them, amplified by their arbitrage-grade mobility across veto points. contemporary_majorities and reform_advocacy_coalitions sit near the target end (d approximately 0.85-0.95), pushed toward full-target by trapped exit: emigration is the only exit and it is costly and partial. constitutional_guardian_institutions derive mid-range directionality — they administer rather than collect, though the secondary beneficiary role (interpretive-authority windfall) pulls them off pure neutrality. future_generations carry high directionality with zero voice — the clearest case of extraction without representation. National spatial scope moderately amplifies effective extraction for targets, since verification of necessity claims is jurisdiction-wide and contestable.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Without the minoritarian reading, the barrier's coordination cover — stability, deliberation, minority assurance — could carry it into rope territory despite the one-directional transfer; the reading forces the victim set into the open, which the canonical classifier requires before any hybrid or extractive verdict. Conversely, the analysis distinguishes snare from piton: concentrated beneficiaries exist (the blocking minorities demonstrably capture the veto), so this is not an inertial vestige nobody profits from; and it distinguishes snare from tangled_rope on this reading's lights — the coordination is assurance one side purchases from the other at the other's expense, with identifiable victims, so the genuine-net-benefit half of the hybrid gate fails here. Founding-problem status is authored contested, not resolved: the R5 mismatch consumer watches status times disappearance_verdict, and no zombie flag is asserted. fixing_cost is prohibitive because the barrier protects itself — amending the amendment rule requires meeting the threshold it imposes — which is the structural fact that makes the veto permanent rather than merely durable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of kernel supermajority_threshold (minoritarian_veto_reading). How would instantiating a sibling reading instead change the structural picture?',
    'Cross-reading comparison on the shared referent (the standing amendment barrier): compile the sibling files and compare epsilon, victim sets, and computed types seat by seat.',
    'Under consensus_safeguard_reading the victim set dissolves (blocked changes reframed as lacking deep consensus) and epsilon drops toward rope range; under adaptive_gradient_reading victims are reframed as calibration casualties, moving the barrier toward tangled_rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    reading_disagreement_location,
    'Where exactly do the three readings disagree — on what structural element of the same barrier?',
    'Locate the disputed element: whether identifiable victims exist (this reading''s claim) versus whether blocked changes reflect unripe preference (consensus sibling) or miscalibration (gradient sibling); adjudicate by examining who bears the costs of specific blocked reforms.',
    'If the disagreement locates in victim-set existence, the readings are indexically distinct constraints (as authored); if it collapses to degree, the family may merge into one calibrated constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_disagreement_location, conceptual, 'The specific structural element the readings differ on: victim-set existence.').

omega_variable(
    necessary_reform_classification,
    'Which blocked fundamental reforms count as necessary reform owed to contemporary majorities, versus contestable preference disputes?',
    'Counterfactual welfare analysis of the blocked-reform queue: estimate effects of stalled suffrage, apportionment, fiscal, and rights reforms on those denied them.',
    'If many blocked reforms were elite-preference contests rather than necessary self-government, the victim set shrinks and epsilon falls materially; if most were necessity-class, the snare reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessary_reform_classification, empirical, 'Empirical basis of the victim claim: necessity classification of blocked reforms.').

omega_variable(
    majority_coordination_capacity,
    'Can contemporary majorities solve their collective-action problem and assemble the winning fraction — making the veto contingent rather than permanent?',
    'Comparative study of amendment surges: episodes where broad coalitions cleared supermajority bars (post-crisis reconstructions, wartime consolidations) versus persistent minority blocks.',
    'If majorities can coordinate when stakes concentrate, the permanent-veto framing overstates entrenchment and epsilon falls; if coordination reliably fails below the bar, the snare reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_coordination_capacity, empirical, 'Coalition-power check on the permanence claim.').

omega_variable(
    self_referential_entrenchment,
    'Is the amendment rule''s self-protection (changing the rule requires meeting the rule) a designed lock or incidental inheritance?',
    'Drafting-history analysis: did framers deliberately apply the threshold to amendment of the threshold itself, or did the recursion emerge from general application?',
    'Designed self-lock supports intentional entrenchment and the prohibitive fixing-cost cell; incidental recursion suggests the lock is an artifact, weakening the intentional-extraction reading and possibly opening procedural workarounds that lower fixing cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_referential_entrenchment, conceptual, 'Origin of the self-referential lock that makes fixing prohibitive.').

omega_variable(
    internalized_barrier_reverence,
    'Is part of the barrier''s hold internalized — do citizens treat amendment difficulty as democratic virtue rather than imposed cost?',
    'Cross-jurisdiction attitude surveys where thresholds differ, plus tracking of resistance levels after civic-curriculum changes.',
    'If internalized, effective suppression exceeds the structural measure and payer-seat resistance is further dampened; the engine''s payer-seat extraction computation would understate lived extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_barrier_reverence, empirical, 'Internalized component of suppression in an institutional constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(supe_tr_t40, observed).
narrative_ontology:measurement(supe_tr_t80, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(supe_tr_t80, observed).
narrative_ontology:measurement(supe_tr_t120, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 120, 0.26).
narrative_ontology:measurement_basis(supe_tr_t120, observed).
narrative_ontology:measurement(supe_tr_t160, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 160, 0.31).
narrative_ontology:measurement_basis(supe_tr_t160, observed).
narrative_ontology:measurement(supe_tr_t200, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 200, 0.36).
narrative_ontology:measurement_basis(supe_tr_t200, observed).
narrative_ontology:measurement(supe_tr_t240, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 240, 0.4).
narrative_ontology:measurement_basis(supe_tr_t240, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement_basis(supe_be_t40, observed).
narrative_ontology:measurement(supe_be_t80, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 80, 0.49).
narrative_ontology:measurement_basis(supe_be_t80, observed).
narrative_ontology:measurement(supe_be_t120, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 120, 0.57).
narrative_ontology:measurement_basis(supe_be_t120, observed).
narrative_ontology:measurement(supe_be_t160, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 160, 0.65).
narrative_ontology:measurement_basis(supe_be_t160, observed).
narrative_ontology:measurement(supe_be_t200, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 200, 0.72).
narrative_ontology:measurement_basis(supe_be_t200, observed).
narrative_ontology:measurement(supe_be_t240, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 240, 0.78).
narrative_ontology:measurement_basis(supe_be_t240, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement_basis(supe_su_t40, observed).
narrative_ontology:measurement(supe_su_t80, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 80, 0.59).
narrative_ontology:measurement_basis(supe_su_t80, observed).
narrative_ontology:measurement(supe_su_t120, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 120, 0.66).
narrative_ontology:measurement_basis(supe_su_t120, observed).
narrative_ontology:measurement(supe_su_t160, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 160, 0.73).
narrative_ontology:measurement_basis(supe_su_t160, observed).
narrative_ontology:measurement(supe_su_t200, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 200, 0.78).
narrative_ontology:measurement_basis(supe_su_t200, observed).
narrative_ontology:measurement(supe_su_t240, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 240, 0.82).
narrative_ontology:measurement_basis(supe_su_t240, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% One colloquial label — 'the supermajority threshold' — covers three structurally distinct claims about the same amendment barrier. This file instantiates the minoritarian-veto reading: the barrier as standing veto converting historical privilege into permanent minority rule, with epsilon authored high (0.78) against the standing arrangement. The consensus-safeguard sibling authors the same barrier as a deliberative filter (low extraction, rope-side); the adaptive-gradient sibling authors it as a miscalibrated instrument (contested, tunable). The readings share the referent (the amendment barrier) and differ in epsilon because epsilon is reading-indexed over a fixed referent; they are linked here so contamination and foreclosure analysis can traverse the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
