% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Mutual-Vulnerability Deterrence Equilibrium over Residual Total-War Reachability (Dropping Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the dropping_reading of the total-war
 *   reachability kernel: total war's probability has fallen dramatically but
 *   remains nonzero, and the fall is explained by a maintained coordination
 *   equilibrium — deterrence — rather than by the disappearance of
 *   reachability itself. The standing arrangement under contest is mutual
 *   vulnerability: each side's cities are held hostage to the other's
 *   survivable second strike, and the arrangement continuously extracts
 *   existential risk-bearing from non-consenting populations while delivering
 *   war-prevention, status, and leverage. The claim and the metrics are
 *   independent authored facts: the reading CLAIMS tangled_rope (genuine
 *   coordination with asymmetric extraction), and the authored metrics
 *   describe substantially extractive, actively enforced operation — the
 *   engine measures any divergence. KEY AGENTS (by structural relationship):
 *   - nuclear_armed_states: agenda-setter and principal beneficiary
 *   (institutional/constrained) — runs the equilibrium and collects its
 *   returns - defense_intellectual_establishment: identity-locked beneficiary
 *   (organized/identity_locked) — careers fused to managing the arsenal -
 *   extended_deterrence_allies: dual-positioned beneficiary-payer
 *   (powerful/constrained) — protected below arsenal cost, hosting delivery
 *   infrastructure - urban_hostage_populations: primary target
 *   (powerless/trapped) — bears the existential mechanism, holds no seat -
 *   nuclear_frontline_societies: secondary target (moderate/constrained) —
 *   stationing burdens and crisis exposure - anti_nuclear_movements and
 *   nonaligned_states_bloc: excluded voices — abolition petitions and
 *   deferred timelines - nuclear_policy_historians: analytical observer —
 *   sees the full record Sibling readings of the same kernel
 *   (contraction_reading, contingent_reachability_reading) are separate
 *   constraint files linked via network edges; their epsilon values differ
 *   because their referents differ, and this file authors only its own
 *   reading's structure.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: agenda-setting possessor governments — administer the equilibrium and collect security, status, and leverage returns
 *   - defense_intellectual_establishment: beneficiary-analysts and operators with identity-fused careers bound to managing the arsenal
 *   - extended_deterrence_allies: protected non-weapon states collecting security below arsenal cost while hosting delivery infrastructure
 *   - urban_hostage_populations: primary targets — city residents whose vulnerability is the operative mechanism, holding no seat
 *   - nuclear_frontline_societies: secondary targets — societies hosting deployments or bordering rivals, bearing stationing and crisis exposure
 *   - anti_nuclear_movements: excluded transnational campaigners pressing abolition from outside the councils
 *   - nonaligned_states_bloc: excluded majority bloc whose disarmament-timeline demands were indefinitely deferred
 *   - nuclear_policy_historians: analytical observers reconstructing the full record from declassified archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.66).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.6).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Mutual-Vulnerability Deterrence Equilibrium over Residual Total-War Reachability (Dropping Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '244de176-eb11-4113-a36f-d81620014b0a').
narrative_ontology:cs_kernel_codification('244de176-eb11-4113-a36f-d81620014b0a', distributed).
narrative_ontology:cs_authority_grounding('244de176-eb11-4113-a36f-d81620014b0a', distributed).
narrative_ontology:cs_reading_relation('244de176-eb11-4113-a36f-d81620014b0a', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('244de176-eb11-4113-a36f-d81620014b0a', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('244de176-eb11-4113-a36f-d81620014b0a', foundational, deterrence_is_coordination_equilibrium).
narrative_ontology:cs_axiom_status(deterrence_is_coordination_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('244de176-eb11-4113-a36f-d81620014b0a', deterrence_is_coordination_equilibrium, empirically_contingent).
narrative_ontology:cs_axiom('244de176-eb11-4113-a36f-d81620014b0a', foundational, total_war_remains_probabilistically_reachable).
narrative_ontology:cs_axiom_status(total_war_remains_probabilistically_reachable, holdable).
narrative_ontology:cs_axiom_grounding('244de176-eb11-4113-a36f-d81620014b0a', total_war_remains_probabilistically_reachable, empirically_contingent).
narrative_ontology:cs_axiom('244de176-eb11-4113-a36f-d81620014b0a', secondary, credible_threat_requires_survivable_second_strike).
narrative_ontology:cs_axiom_status(credible_threat_requires_survivable_second_strike, holdable).
narrative_ontology:cs_axiom_grounding('244de176-eb11-4113-a36f-d81620014b0a', credible_threat_requires_survivable_second_strike, instrumental).
narrative_ontology:cs_reference_frame('244de176-eb11-4113-a36f-d81620014b0a', managed_bipolar_coordination_equilibrium).
narrative_ontology:cs_drift_state('244de176-eb11-4113-a36f-d81620014b0a', contemporary_multipolar_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('244de176-eb11-4113-a36f-d81620014b0a', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, defense_intellectual_establishment).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, urban_hostage_populations).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, nuclear_frontline_societies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, rational_actor_deterrence_models).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, stability_instability_paradox).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possessor governments that maintain arsenals, doctrinal posture, and signaling cycles, and decide force structure and employment policy. They collect security, status, and coercive leverage from the credibility of their threats. They cannot unilaterally stand down without accepting vulnerability to rivals who retain forces, and verified multilateral disarmament requires steps they do not control.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_armed_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, nuclear_armed_states, beneficiary).

% Strategists, weapons laboratories, strategic commands, and contractor workforces whose budgets, careers, and professional prestige are bound to analyzing and operating the arsenal. They produce the assessments that justify posture choices. Leaving the field means professional obsolescence; the analytic tradition they inhabit constitutes their standing.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, defense_intellectual_establishment, beneficiary,
    organized, biographical, identity_locked, national).

% Non-weapon states under a protector's security guarantee. They host delivery systems and basing on their territory and receive protection without owning warheads, gaining security below the cost of independent arsenals. Their territory is nonetheless coupled to the protector's confrontation schedule, and treaty commitments narrow independent weaponization.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies, payer).

% Residents of large population centers whose destruction is the operative mechanism by which threats are made credible. They never consented to the arrangement and hold no seat in its councils. Evacuating cities is impossible at scale, and the threat follows population concentration wherever it exists.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, urban_hostage_populations, payer,
    powerless, immediate, trapped, global).

% Societies hosting forward-deployed systems or bordering nuclear-armed rivals: Cold War Central Europe, the Korean peninsula, the Baltic flank, the Taiwan Strait region. They bear stationing burdens, exercise disruption, and crisis exposure. They hold partial voice through democratic politics, but alliance dependence narrows their independent options.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_frontline_societies, payer,
    moderate, generational, constrained, regional).

% Transnational campaigns pressing for abolition: test-ban and freeze movements historically, the humanitarian-consequences coalition and its treaty process recently. They are marginalized from possessor-state councils; their proposals enter the conversation only as petitions the weapon states decline.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, anti_nuclear_movements, excluded,
    organized, biographical, constrained, global).

% The majority bloc of states without nuclear weapons, which accepted non-proliferation obligations in exchange for promised disarmament timelines. Their requested timetables were indefinitely deferred at review conferences. They remain inside the treaty structure but outside the decision circle.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nonaligned_states_bloc, excluded,
    organized, generational, constrained, global).

% Scholars working from declassified archives across the whole interval. They hold no stake in posture debates; they reconstruct how crises resolved, how near-misses occurred, and how the arrangement's costs and achievements compare.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_policy_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, nuclear_armed_states).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power war under conditions where war-fighting would be catastrophic and intentions are unverifiable: mutually survivable second-strike capability makes attack irrational for each side simultaneously, converting war avoidance into a self-enforcing equilibrium. Tacit bargaining norms — signaling grammar, hotlines, exercise cycles — coordinate crisis conduct without prior negotiation.
% TRANSFER_FUNCTION: Moves existential risk onto civilian populations concentrated in cities, and fiscal resources into arsenal maintenance, delivery systems, and signaling exercises; moves status, security, and coercive leverage toward nuclear-armed states, and deference away from non-weapon states.
% ABSENT_VOICES: Urban hostage populations have no seat anywhere in the structure. The nonaligned majority's disarmament-timeline demands are indefinitely deferred. Abolition campaigners are excluded from possessor-state councils. They stand outside the P5 process, the alliance planning cells, and the Washington-Moscow-Beijing channels — present only as petitioners.
% DISAPPEARANCE_RATIONALE: Alliance architectures, arsenal budgets, strategic doctrines, and the non-proliferation bargain all presuppose the equilibrium. Overnight removal would force either rapid verified disarmament or a scramble to rebuild forces, and the tacit crisis-bargaining grammar would vanish precisely when it is most needed — every seated actor's arrangements would reorganize.
% FOUNDING_PROBLEM: After 1945: prevent a recurrence of industrial-scale total war between great powers whose next conflict would open with atomic exchange — to stabilize a rivalry that could not be settled by war and could not be dissolved by trust.
% FOUNDING_PROBLEM_CORROBORATION: Declassified 1945-1950 planning archives corroborate the founding problem's origin. Non-weapon states' own security statements and neutral states' civil-defense histories attest that the war-prevention problem remains real — while those same non-beneficiary sources dispute that this arrangement is the only or best remedy for it.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.66 at interval end) because the arrangement's operative mechanism is the deliberate exposure of non-consenting civilian populations, layered on top of a real war-prevention service; suppression is comparable (0.60) because persistence depends on actively maintained enforcement — arsenal readiness, signaling discipline, and the structural punishment of any attempt to escape mutual vulnerability — not on participant preference. Theater is moderate (0.42): parades, excess exercises, and declaratory ritual are real but ride on functioning machinery. Accessibility collapse is moderate (0.5): understood mutual vulnerability collapses unilateral exit completely, but alternative postures (minimum deterrence, no-first-use, arms-control variants, abolition) remain articulable. Resistance is moderate (0.5): test-ban, freeze, and abolition movements mounted sustained opposition that shaped outcomes at the margins without displacing the arrangement. The temporal series run on ONE shared grid (1947, 1955, 1962, 1972, 1983, 1991, 2005, 2026) with every tracked metric authored at every point. The series show two full cycles — buildup to near-miss (1962), institutional learning (détente), re-accumulation to second peak (1983), relaxation (1991-2005), and a third accumulation underway (2026). The oscillation is partly an extraction mechanism: each scare re-legitimates budget peaks (intermittent reinforcement of domestic audiences), not merely noise. Base properties are measured at the 2026 point, on the rising phase of the third cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the possessor seat the arrangement reads as a jointly maintained peace mechanism it operates and pays for; from the hostage seat the identical structure reads as uncompensated existential exposure enforced by geography of population concentration; the establishment seat's identity lock amplifies perceived necessity and dampens perceived extraction; the ally seat experiences genuine protection and genuine target-coupling simultaneously. The engine computes per-seat classifications from the structural data — the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Nuclear_armed_states sit nearest the beneficiary pole (agenda-setter plus collector; constrained exit keeps them from full arbitrage). Defense_intellectual_establishment derive low d amplified by identity_locked exit — they cannot cheaply reprice their position. Extended_deterrence_allies derive intermediate d: declared beneficiaries whose secondary payer position (hosted targets) pulls them toward symmetric. Urban_hostage_populations derive near-full-target d: declared victims, powerless, trapped, global scope. Nuclear_frontline_societies derive high-but-not-maximal d: victims with partial political voice and constrained exit. Global spatial scope modestly amplifies effective extraction for the target seats (verification of restraint is harder at planetary scale). Suppression enters the computation as a raw structural property, unscaled by power or scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — great-power war prevention — is live, so no mandatrophy resolution is declared. The classification guards against both mislabels: a pure-rope reading would erase the hostage structure (who pays, and that they never consented); a pure-snare reading would erase the demonstrated war-prevention service the arrangement delivers and that non-beneficiary historians corroborate. The theater_ratio trajectory flags the atrophy SYMPTOM without function loss: the 2005 peak (0.56) marks the unipolar lull, when maintenance continued past the intensity of the function it served — a piton-drift warning that reversed as great-power rivalry returned. Watching theater_ratio on the next relaxation phase is the standing test for whether the equilibrium decays toward performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_reachability_boundary,
    'This story is one reading of the kernel total_war_reachability_boundary: does residual total-war probability reflect a maintained coordination equilibrium (this reading), a zero-feasibility boundary (contraction_reading), or a technology-dependent state (contingent_reachability_reading) — and which reading''s epsilon referent governs?',
    'Cross-reading comparison on shared evidence: crisis behavior under degraded communications, proliferation events, and whether arms-control collapse changes observed war probability.',
    'Under contraction_reading the arrangement loses its protective function and drifts piton-ward; under contingent_reachability_reading epsilon becomes technology-indexed and reversible; this reading''s tangled_rope classification holds only if maintained coordination explains the observed low probability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_indexicality_of_reachability_boundary, conceptual, 'Which reading of the reachability kernel this epsilon belongs to.').

omega_variable(
    accident_vs_design_defection_source,
    'Is residual reachability carried by rational miscalculation, technical accident, or deliberate escalation — and does the mix change the equilibrium''s robustness?',
    'Declassified incident records (1962, 1973, 1983, 1995 false alarms; crisis case files) coded for initiation channel.',
    'Accident-dominated reachability means the coordination equilibrium is thinner than rationalist framing assumes and effective extraction per unit of protection is higher; design-dominated reachability supports the equilibrium reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accident_vs_design_defection_source, empirical, 'Source channel of residual defection risk in the deterrence equilibrium.').

omega_variable(
    hostage_population_consent_status,
    'Does democratic acquiescence of urban populations constitute consent that lowers the extraction they experience, or is the arrangement non-consensual regardless of expressed preference?',
    'Deliberative polling and referendum-grade surveys on nuclear posture conducted outside election cycles.',
    'Measured informed consent would damp the payer-seat extraction; informed opposition would raise it and strengthen abolitionist standing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hostage_population_consent_status, preference, 'Consent status of the populations whose exposure is the arrangement''s operative mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1947, total_war_reachability_boundary__dropping_reading, theater_ratio, 1947, 0.18).
narrative_ontology:measurement_basis(tota_tr_t1947, observed).
narrative_ontology:measurement(tota_tr_t1955, total_war_reachability_boundary__dropping_reading, theater_ratio, 1955, 0.36).
narrative_ontology:measurement_basis(tota_tr_t1955, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.24).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1972, total_war_reachability_boundary__dropping_reading, theater_ratio, 1972, 0.41).
narrative_ontology:measurement_basis(tota_tr_t1972, observed).
narrative_ontology:measurement(tota_tr_t1983, total_war_reachability_boundary__dropping_reading, theater_ratio, 1983, 0.33).
narrative_ontology:measurement_basis(tota_tr_t1983, observed).
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__dropping_reading, theater_ratio, 1991, 0.46).
narrative_ontology:measurement_basis(tota_tr_t1991, observed).
narrative_ontology:measurement(tota_tr_t2005, total_war_reachability_boundary__dropping_reading, theater_ratio, 2005, 0.56).
narrative_ontology:measurement_basis(tota_tr_t2005, observed).
narrative_ontology:measurement(tota_tr_t2026, total_war_reachability_boundary__dropping_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(tota_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1947, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1947, 0.34).
narrative_ontology:measurement_basis(tota_be_t1947, observed).
narrative_ontology:measurement(tota_be_t1955, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1955, 0.52).
narrative_ontology:measurement_basis(tota_be_t1955, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.72).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1972, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1972, 0.62).
narrative_ontology:measurement_basis(tota_be_t1972, observed).
narrative_ontology:measurement(tota_be_t1983, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1983, 0.75).
narrative_ontology:measurement_basis(tota_be_t1983, observed).
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1991, 0.5).
narrative_ontology:measurement_basis(tota_be_t1991, observed).
narrative_ontology:measurement(tota_be_t2005, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement_basis(tota_be_t2005, observed).
narrative_ontology:measurement(tota_be_t2026, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2026, 0.66).
narrative_ontology:measurement_basis(tota_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1947, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1947, 0.22).
narrative_ontology:measurement_basis(tota_su_t1947, observed).
narrative_ontology:measurement(tota_su_t1955, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1955, 0.58).
narrative_ontology:measurement_basis(tota_su_t1955, observed).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.66).
narrative_ontology:measurement_basis(tota_su_t1962, observed).
narrative_ontology:measurement(tota_su_t1972, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1972, 0.49).
narrative_ontology:measurement_basis(tota_su_t1972, observed).
narrative_ontology:measurement(tota_su_t1983, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1983, 0.69).
narrative_ontology:measurement_basis(tota_su_t1983, observed).
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1991, 0.38).
narrative_ontology:measurement_basis(tota_su_t1991, observed).
narrative_ontology:measurement(tota_su_t2005, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement_basis(tota_su_t2005, observed).
narrative_ontology:measurement(tota_su_t2026, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2026, 0.6).
narrative_ontology:measurement_basis(tota_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'total war reachability' decomposes into three structurally distinct claims sharing one kernel. This file (dropping_reading) authors epsilon 0.66 over the standing mutual-vulnerability arrangement as a maintained coordination equilibrium. contraction_reading authors its epsilon over a world where winnable total war has left the feasible set — the arrangement there trends vestigial. contingent_reachability_reading authors epsilon as technology-indexed and reversible. The upstream/downstream pressure runs from this reading outward: its coordination account is what the other two readings accept or deny. All three files link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
