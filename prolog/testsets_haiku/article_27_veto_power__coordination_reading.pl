% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: Article 27 P5 Veto—Coordination Reading: Prevention of Great-Power War via Unanimity Gate
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   Article 27 of the UN Charter grants each of the five permanent Security
 *   Council members an absolute veto over any substantive resolution. Under
 *   the coordination reading instantiated in this story, the veto serves as a
 *   mechanism to prevent inadvertent great-power war by ensuring that no P5
 *   member can be compelled via majority Security Council vote into military
 *   confrontation it rejects. The reading asserts that the veto's primary
 *   function is coordination (solving the collective-action problem of
 *   great-power coexistence in a single binding institution) rather than
 *   oligarchic entrenchment (extracting authority rents while blocking
 *   institutional evolution) or sovereignty instantiation (operationalizing
 *   the Westphalian principle that no state consents without agreement). This
 *   story authors the veto under the coordination frame. The other readings
 *   (oligarchy, sovereignty) are separate constraint stories with different ε
 *   values, different beneficiary/victim structures, and different
 *   classifications.
 *
 * KEY AGENTS:
 *   - Permanent Security Council members (US, Russia, China, UK, France): institutional agenda-setters; hold the veto mechanism itself; trapped in the system (leaving the UN carries enormous cost); in this reading, beneficiaries of the war-prevention guarantee
 *   - Non-permanent Security Council members: can propose resolutions but cannot veto; benefit from the P5 veto's constraint on unilateral great-power enforcement (their own interests are protected by the fact that no binding military action can occur without P5 consent)
 *   - General Assembly delegations (all other UN members): benefit from the implicit constraint on P5 unilateralism; the veto prevents the UN from becoming an instrument of great-power coercion masked as collective action
 *   - International legal system (non-agent; the corpus of law grounding state consent): vindicated by the veto's instantiation of the unanimity principle
 *   - Collective-security doctrine (non-agent; the institutional theory that grounds UN legitimacy): vindicated by the veto's preservation of voluntary participation for great powers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.12).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.05).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "Article 27 P5 Veto—Coordination Reading: Prevention of Great-Power War via Unanimity Gate").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '759e2110-eb27-4d66-8802-bd3632d9fa81').
narrative_ontology:cs_kernel_codification('759e2110-eb27-4d66-8802-bd3632d9fa81', formalized).
narrative_ontology:cs_authority_grounding('759e2110-eb27-4d66-8802-bd3632d9fa81', expertise).
narrative_ontology:cs_interpretation_layer_present('759e2110-eb27-4d66-8802-bd3632d9fa81').
narrative_ontology:cs_reading_relation('759e2110-eb27-4d66-8802-bd3632d9fa81', article_27_veto_power__oligarchy_reading, coexists_with).
narrative_ontology:cs_reading_relation('759e2110-eb27-4d66-8802-bd3632d9fa81', article_27_veto_power__sovereignty_reading, influences).
narrative_ontology:cs_axiom('759e2110-eb27-4d66-8802-bd3632d9fa81', foundational, great_power_war_prevention_via_unanimity).
narrative_ontology:cs_axiom_status(great_power_war_prevention_via_unanimity, holdable).
narrative_ontology:cs_axiom_grounding('759e2110-eb27-4d66-8802-bd3632d9fa81', great_power_war_prevention_via_unanimity, instrumental).
narrative_ontology:cs_axiom('759e2110-eb27-4d66-8802-bd3632d9fa81', secondary, collective_security_requires_voluntary_participation).
narrative_ontology:cs_axiom_status(collective_security_requires_voluntary_participation, holdable).
narrative_ontology:cs_axiom_grounding('759e2110-eb27-4d66-8802-bd3632d9fa81', collective_security_requires_voluntary_participation, deontological).
narrative_ontology:cs_reference_frame('759e2110-eb27-4d66-8802-bd3632d9fa81', great_power_consensus_based_security_governance).
narrative_ontology:cs_drift_state('759e2110-eb27-4d66-8802-bd3632d9fa81', contemporary_geopolitical_fragmentation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('759e2110-eb27-4d66-8802-bd3632d9fa81', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, all_un_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_permanent_security_council_members).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, general_assembly_delegations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five nuclear-armed great powers (US, Russia, China, UK, France) hold exclusive veto over binding Security Council resolutions. The veto gives each the capacity to prevent any military authorization or binding enforcement action that would compel it into armed confrontation. They are the formal gatekeepers of the mechanism and the primary beneficiaries of the war-prevention guarantee.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, permanent_security_council_members, agenda_setter,
    institutional, civilizational, trapped, universal).

% Participate in deliberations but cannot block resolutions. They benefit from the veto's constraint on P5 unilateralism (a veto'd resolution means no binding enforcement action, which protects smaller powers from being compelled into wars they reject). Their exit is constrained: leaving the Security Council or UN is theoretically possible but practically comes at severe cost.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_permanent_security_council_members, beneficiary,
    powerful, generational, constrained, universal).

% Benefit from the veto's implicit constraint on P5 enforcement autonomy: a veto'd Security Council resolution prevents unilateral great-power military action in the name of the UN, which preserves the collective-action fiction and protects non-aligned and smaller states from being conscripted into wars they reject.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, general_assembly_delegations, beneficiary,
    organized, generational, constrained, universal).

% The veto instantiates a foundational principle of international law: no state can be bound by a binding decision without its consent (in this reading, translated as 'no great power can be compelled into war without its veto'). The constraint vindicates this principle by operationalizing it.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_legal_system, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, international_legal_system).

% The veto prevents the UN's collective-security mandate from imposing binding military obligations on any permanent member, which ensures that collective security operations remain voluntary for great powers and thus maintain their feasibility (if the veto were absent, any P5 member facing mandatory war participation would face an intolerable choice: bind itself or leave the system).
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, collective_security_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, collective_security_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of preventing inadvertent great-power war: without a unanimity gate, a Security Council majority could authorize military action that one great power regards as forced escalation, triggering either unilateral defection or global war. The veto ensures that no P5 member can be compelled into military confrontation it rejects, preserving the condition for stable coexistence.
% TRANSFER_FUNCTION: Does not move material resources from one party to another. Instead, it allocates authority: it grants each P5 member the capacity to block any binding enforcement resolution, which transfers decision-making power away from majoritarian procedures and toward unanimous consent among great powers.
% ABSENT_VOICES: Non-P5 states have a structural absence: they participate in Security Council deliberations and voting but hold no veto. They would argue (from within the system) for weighted majority voting or supermajority thresholds that dilute P5 power; they are present in the room but structurally excluded from the veto mechanism itself. Potential alternative power arrangements (e.g., a weighted supermajority system, regional security mechanisms outside the UN) are absent from the negotiating table because the P5 veto over Charter amendment makes them unamendable by non-P5 action.
% DISAPPEARANCE_RATIONALE: If the veto disappeared overnight, the incentive structure for P5 membership in the Security Council would invert: great powers would face potential majority-imposed war participation, which would either force withdrawal from the collective-security system or create strong incentives to act unilaterally and outside the UN framework. The world would reorganize around either a P5 withdrawal from collective-security norms or a reversion to pre-UN great-power spheres-of-influence arrangements.
% FOUNDING_PROBLEM: After World War II, a critical coordination problem existed: could great powers coexist in a single collective-security institution without triggering inadvertent escalation? The veto was designed to solve this by ensuring that no great power would be involuntarily conscripted into a military action it viewed as compelled war, which would preserve the cooperative basis of the UN while allowing collective action against non-great-power aggressors.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international law scholars from outside the P5 governments (e.g., primary sources from the San Francisco Conference, academic analyses by non-aligned scholars) attest that the founding problem was live: great-power coordination was fragile, and the addition of a binding collectivity rule without unanimity was seen as war-prone. The coordination reading draws on these sources. However, the reading is contested: oligarchy and sovereignty readings, held by observers and scholars aligned with different framings, dispute whether preventing great-power war was the primary purpose or a secondary legitimating narrative for a mechanism designed to entrench great-power dominance.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12 at interval end) because the veto, under this reading, coordinates rather than extracts: it distributes the same war-prevention benefit to all P5 members equally (each gets protected from compelled war by every other's veto), and the benefit extends to non-P5 members as well (they are protected from being forced into great-power wars they reject). Suppression is minimal (0.05) because the coordination is sustained by structural incentive (great powers prefer non-coercion to coercion), not by active enforcement against resistance. Theater is low (0.08) and stable: there is genuine coordination function throughout the interval; the increase to ~0.08 reflects the accumulation of instances where the veto is used without substantive deliberation, suggesting ceremonial operation in some cases, but the core function remains real. Accessibility collapse is high (0.78): once a great power commits to the UN, the unanimity gate forecloses alternatives (unilateral action outside the UN carries reputational cost; creating a rival security mechanism is impractical; withdrawing from the UN is a one-time irreversible choice with severe costs). Resistance is low (0.22): the veto is not heavily contested on coordination grounds (even non-P5 states accept that forcing great-power war participation would be counterproductive); the resistance that does exist is the oligarchy and sovereignty readings, which contest the framing rather than the mechanism's effectiveness. The measurements track the interval from 1945 (founding) to 2026 (present). The temporal pattern shows slight drift upward in extractiveness and theater (as geopolitical context evolves and great powers use the veto for strategic interests beyond war prevention), but the drift is shallow because the core coordination function remains intact. Theater dips in 2026 as institutional focus has returned to substantive coordination following post-2015 confidence-building.
 *
 * PERSPECTIVAL GAP:
 *   The P5 member seat computes as full beneficiary (d near 0.0) under this reading: they receive the exclusive benefit of the war-prevention guarantee and bear minimal suppression. The non-P5 seats compute as secondary beneficiaries (d near 0.3): they benefit from the constraint but do not directly control it and experience it as an external rule. The international legal system and collective-security doctrine compute as non-agent beneficiaries (d is not assigned; they are analytical seats). The engine's per-seat classification should reflect this asymmetry: all seats should compute to Rope (coordination), but directionality varies significantly.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the coordination reading, there are no victims—all states benefit from avoided great-power war. The beneficiary class is explicitly universal: 'all UN member states' and 'international system stability'. The P5 members are the primary beneficiaries (they hold the mechanism and receive the explicit war-prevention guarantee). Non-P5 states are secondary beneficiaries (they benefit from the constraint on great-power coercion without controlling it). No state pays a net cost for the war-prevention benefit—the cost (each P5 member's inability to compel others into war via Security Council majority) is borne equally by all five and is the price of their own war-prevention guarantee. The directionality computation should reflect this: P5 members have d near 0.0 (beneficiaries), non-P5 members have d near 0.2–0.3 (secondary beneficiaries with constrained agency), and the system as a whole has d near 0.0 (beneficiary of stability).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing inadvertent great-power war through consensus governance) was live in 1945 and remains live in 2026 under this reading. The mandatrophy question would arise if great-power war became structurally impossible (e.g., via irreversible nuclear deterrence or global resource interdependence) such that the veto's war-prevention function became obsolete while the constraint persisted. The measurements and temporal analysis do not show mandatrophy: the veto has been used to block resolutions that would have military consequences (Russian veto in Syria, Chinese veto on humanitarian intervention), indicating that the mechanism remains functionally relevant to preventing great-power coercion. Theater has drifted upward slightly (to 0.08), suggesting some ceremonial use, but not to the point of full degradation. The constraint would qualify as Piton if extractiveness were high, beneficiary claims were absent, and the mechanism persisted by institutional inertia rather than active coordination. Under this reading, neither condition holds: extraction is low, beneficiaries are explicit (international stability), and the mechanism is actively maintained because it solves a live coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prevention_counterfactual_validity,
    'Has the veto actually prevented great-power war, or would great-power war have been prevented anyway by nuclear deterrence, geographic isolation, or structural bipolarity?',
    'Counterfactual historical analysis: comparing P5 conflict trajectories under the veto-protected regime to comparable periods without unanimity gates; examining near-miss crises (Cuban Missile Crisis, Cold War standoffs) to assess whether veto-less Security Council authorization would have escalated them.',
    'If the veto''s prevention function is a secondary effect of deeper deterrence structures, the constraint reclassifies as theater or coordination-by-side-effect rather than as primary prevention mechanism. If the veto demonstrably prevented escalations that deterrence alone would not have blocked, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prevention_counterfactual_validity, empirical, 'Whether the veto causally prevents great-power war or merely rides on independent deterrence mechanisms.').

omega_variable(
    unanimity_gate_vs_oligarchy_boundary,
    'Is the veto''s unanimity gate distinguishable from oligarchic entrenchment? If the veto prevents great-power war by preserving great-power consent, does the preservation of consent constitute extraction (rent-seeking via institutional immutability)?',
    'Structural analysis: a veto that prevents war (coordination reading) is analytically separable from a veto that blocks institutional evolution that would diminish P5 authority (oligarchy reading) only if the two functions can be decoupled. A test case: would a P5 member accept a modified veto (e.g., temporary suspension for humanitarian crises, supermajority override, regional security arrangements) that preserved the war-prevention function while allowing institutional evolution?',
    'If the coordination function can be preserved under modified veto rules, the oligarchy reading and coordination reading are genuinely separable constraints. If the P5 insists on the current veto as essential to war prevention, the boundary collapses and the constraint reclassifies as tangled rope (coordination + oligarchic extraction bundled inseparably).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unanimity_gate_vs_oligarchy_boundary, conceptual, 'Whether the war-prevention function is separable from oligarchic entrenchment or structurally inseparable.').

omega_variable(
    reading_kernel_contest,
    'Which reading of the Article 27 kernel is structurally true: coordination (prevents inadvertent war by unanimity), oligarchy (entrenches great-power dominance while blocking evolution), or sovereignty (instantiates consent principle in Westphalian form)?',
    'The kernel is the Charter text and its institutional history. The three readings constitute three interpretive frames laid over the same artifact. Committer analysis: what does the founding-problem corroboration actually establish? Who attests the problem, and whose framing dominates the historical record? Structural analysis: do the three readings produce different policy predictions? (E.g., if unanimity prevents war, then weakening the veto should increase war risk; if oligarchy drives the constraint, then weakening the veto should redirect institutional competition; if sovereignty is the principle, then unanimity is non-negotiable regardless of war prevention.)',
    'Classification of the constraint depends on which reading is adopted. This constraint story (coordination_reading) classifies as Rope under the coordination frame; the oligarchy_reading classifies as Snare; the sovereignty_reading classifies as Mountain. The three are different constraints with different beneficiaries, different victim sets, and different ε values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'The three-reading kernel contest: coordination vs. oligarchy vs. sovereignty framing of the same Article 27 text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement_basis(arti_tr_t1945, observed).
narrative_ontology:measurement(arti_tr_t1962, article_27_veto_power__coordination_reading, theater_ratio, 1962, 0.06).
narrative_ontology:measurement_basis(arti_tr_t1962, observed).
narrative_ontology:measurement(arti_tr_t1980, article_27_veto_power__coordination_reading, theater_ratio, 1980, 0.07).
narrative_ontology:measurement_basis(arti_tr_t1980, observed).
narrative_ontology:measurement(arti_tr_t2000, article_27_veto_power__coordination_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(arti_tr_t2000, observed).
narrative_ontology:measurement(arti_tr_t2015, article_27_veto_power__coordination_reading, theater_ratio, 2015, 0.09).
narrative_ontology:measurement_basis(arti_tr_t2015, observed).
narrative_ontology:measurement(arti_tr_t2026, article_27_veto_power__coordination_reading, theater_ratio, 2026, 0.08).
narrative_ontology:measurement_basis(arti_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement_basis(arti_be_t1945, observed).
narrative_ontology:measurement(arti_be_t1962, article_27_veto_power__coordination_reading, base_extractiveness, 1962, 0.1).
narrative_ontology:measurement_basis(arti_be_t1962, observed).
narrative_ontology:measurement(arti_be_t1980, article_27_veto_power__coordination_reading, base_extractiveness, 1980, 0.11).
narrative_ontology:measurement_basis(arti_be_t1980, observed).
narrative_ontology:measurement(arti_be_t2000, article_27_veto_power__coordination_reading, base_extractiveness, 2000, 0.13).
narrative_ontology:measurement_basis(arti_be_t2000, observed).
narrative_ontology:measurement(arti_be_t2015, article_27_veto_power__coordination_reading, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement_basis(arti_be_t2015, observed).
narrative_ontology:measurement(arti_be_t2026, article_27_veto_power__coordination_reading, base_extractiveness, 2026, 0.12).
narrative_ontology:measurement_basis(arti_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__coordination_reading, suppression_requirement, 1945, 0.02).
narrative_ontology:measurement_basis(arti_su_t1945, observed).
narrative_ontology:measurement(arti_su_t1962, article_27_veto_power__coordination_reading, suppression_requirement, 1962, 0.04).
narrative_ontology:measurement_basis(arti_su_t1962, observed).
narrative_ontology:measurement(arti_su_t1980, article_27_veto_power__coordination_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement_basis(arti_su_t1980, observed).
narrative_ontology:measurement(arti_su_t2000, article_27_veto_power__coordination_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement_basis(arti_su_t2000, observed).
narrative_ontology:measurement(arti_su_t2015, article_27_veto_power__coordination_reading, suppression_requirement, 2015, 0.06).
narrative_ontology:measurement_basis(arti_su_t2015, observed).
narrative_ontology:measurement(arti_su_t2026, article_27_veto_power__coordination_reading, suppression_requirement, 2026, 0.05).
narrative_ontology:measurement_basis(arti_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__coordination_reading, 0.06).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligarchy_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% The Article 27 veto is a contested kernel yielding three structurally distinct constraints: (1) coordination_reading (this file) classifies as Rope; (2) oligarchy_reading classifies as Snare, with beneficiaries being the P5 members and victims being non-P5 states blocked from institutional evolution; (3) sovereignty_reading classifies as Mountain, with the veto as an instantiation of the Westphalian consent principle treated as a natural law of interstate relations. Each reading has its own ε, beneficiary/victim set, and stakeholder frame. They are linked via network.affects_constraints because the adoption of one reading constrains the plausibility of the others (oligarchy and coordination readings in particular are mutually constraining: if the veto is doing high-value coordination work, oligarchic rent extraction is harder to justify; if it is extracting rents, coordination framing becomes a cover story). The three stories together comprise the Article 27 constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
