% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Deterrence Coordination Equilibrium over Reachable Total War (Dropping Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This file authors ONE reading of the total_war_reachability_boundary
 *   kernel: the dropping_reading, on which nuclear weapons moved total war
 *   from a routine instrument of great-power competition to a low-probability
 *   residual possibility — dropped, not eliminated. On this reading the
 *   standing arrangement (mutual vulnerability plus continuously maintained
 *   threat credibility) solves a real collective-action problem: it converts
 *   recurring historical great-power war into a managed standoff. But the
 *   same structure extracts continuously — fiscal transfers to arsenals,
 *   unconsented existential risk imposed on urban populations, and security
 *   deference drawn from weaker states — and it persists only through active
 *   maintenance: force readiness, signaling, treaty verification, crisis
 *   management. Genuine coordination plus asymmetric extraction plus enforced
 *   persistence is why this reading classifies the arrangement as a tangled
 *   rope rather than a mountain: the stability is constructed, not natural
 *   law. The ε referent is the standing deterrence arrangement itself,
 *   assessed by this reading's lights — never the abolitionist alternative.
 *   The sibling readings (contraction_reading,
 *   contingent_reachability_reading) are separate constraint files linked
 *   through the network; the contest between them is carried in the omegas,
 *   not averaged into this file. KEY AGENTS (by structural relationship): -
 *   nuclear_powers_defense_establishments: agenda-setter and primary
 *   beneficiary (institutional/identity_locked) — administers posture,
 *   collects budget share and institutional continuity -
 *   extended_deterrence_allies: secondary beneficiary (powerful/constrained)
 *   — protected by the guarantee and simultaneously exposed as its hostage -
 *   defense_industrial_base: beneficiary (organized/mobile) — receives the
 *   procurement flows - urban_populations_under_nuclear_threat: primary
 *   target (powerless/trapped) — bears existential risk without consent -
 *   nuclear_state_taxpayers: target (moderate/trapped) — funds the
 *   arrangement without procedural leverage - anti_nuclear_movements:
 *   excluded (organized/constrained) — abolition alternative kept outside the
 *   operative conversation - strategic_studies_community: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.58).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.62).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Deterrence Coordination Equilibrium over Reachable Total War (Dropping Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, 'd90d524b-59f5-4170-9b48-483ad88438fd').
narrative_ontology:cs_kernel_codification('d90d524b-59f5-4170-9b48-483ad88438fd', distributed).
narrative_ontology:cs_authority_grounding('d90d524b-59f5-4170-9b48-483ad88438fd', distributed).
narrative_ontology:cs_reading_relation('d90d524b-59f5-4170-9b48-483ad88438fd', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('d90d524b-59f5-4170-9b48-483ad88438fd', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('d90d524b-59f5-4170-9b48-483ad88438fd', foundational, total_war_remains_reachable).
narrative_ontology:cs_axiom_status(total_war_remains_reachable, holdable).
narrative_ontology:cs_axiom_grounding('d90d524b-59f5-4170-9b48-483ad88438fd', total_war_remains_reachable, empirically_contingent).
narrative_ontology:cs_axiom('d90d524b-59f5-4170-9b48-483ad88438fd', foundational, deterrence_stability_requires_active_maintenance).
narrative_ontology:cs_axiom_status(deterrence_stability_requires_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('d90d524b-59f5-4170-9b48-483ad88438fd', deterrence_stability_requires_active_maintenance, instrumental).
narrative_ontology:cs_reference_frame('d90d524b-59f5-4170-9b48-483ad88438fd', managed_low_probability_deterrence_equilibrium).
narrative_ontology:cs_drift_state('d90d524b-59f5-4170-9b48-483ad88438fd', contemporary_rerivalry_phase, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d90d524b-59f5-4170-9b48-483ad88438fd', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_powers_defense_establishments).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, defense_industrial_base).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, urban_populations_under_nuclear_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, nuclear_state_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the strategic forces of the nuclear-armed states: they set posture and doctrine, run readiness cycles and signaling exercises, negotiate and sometimes abandon arms-control instruments, and brief political leadership on escalation options. Their budgets, career structures, and institutional self-conception formed around the deterrent mission; after the Cold War drawdown they partially diversified into conventional and counterterror roles but retained the core mission. Leaving the mission would mean dismantling the institutions themselves.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_powers_defense_establishments, agenda_setter,
    institutional, generational, identity_locked, global).

% States such as Japan, South Korea, and NATO Europe that rely on a patron's nuclear guarantee instead of fielding their own arsenals. They host forward-deployed forces, integrate into patron planning, and abstain from indigenous weapons programs. Their protection depends on the guarantee staying credible; abandoning it would force a choice between indigenous armament and living unprotected beside a nuclear-armed rival, and their own cities sit on the targeting lists of their patron's adversaries.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies, beneficiary,
    powerful, generational, constrained, regional).

% Contractors and laboratories that build delivery systems, warheads, and command networks. They receive multi-decade procurement and modernization funding whose scale follows threat-perception cycles, and they employ the engineering workforce that sustains the arsenal. They can and do pivot capacity toward conventional programs when strategic funding dips, though re-entry barriers are high.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, defense_industrial_base, beneficiary,
    organized, biographical, mobile, national).

% The residents of large cities in nuclear-armed and allied states, whose homes anchor every side's targeting plans. They bear the standing risk that crisis escalation would incinerate them, receive civil-defense guidance they cannot act on meaningfully, and have no procedural seat in the posture decisions taken on their behalf. Moving away from target zones means abandoning livelihoods, and no destination escapes the intercontinental reach of the forces involved.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, urban_populations_under_nuclear_threat, payer,
    powerless, biographical, trapped, global).

% General-taxpayer publics of the nuclear-armed states, whose revenue funds warhead life-extension, delivery-system modernization, and command-network upkeep regardless of their views. The line item survives electoral turnover because posture decisions are insulated from ordinary budget politics; emigration is the only individual opt-out and is practically unavailable at scale.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_state_taxpayers, payer,
    moderate, biographical, trapped, national).

% Transnational campaigns and the majority of non-nuclear-weapon states that pushed through the Treaty on the Prohibition of Nuclear Weapons. They argue the arrangement's risks outweigh its services and seek a negotiated abolition path; the nine nuclear-armed states and their allies boycott the ban process, keeping the movement's preferred alternative outside the operative conversation.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, anti_nuclear_movements, excluded,
    organized, generational, constrained, global).

% Academic and think-tank analysts who model escalation ladders, crisis bargaining, and failure modes such as the stability-instability paradox and accidental-launch pathways. They publish the near-miss reconstructions and base-rate estimates the other seats cite, advise governments intermittently, and hold no operational authority.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_studies_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, defense_industrial_base).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the great-power rivalry problem under mutual vulnerability: by making total war credibly suicidal for both sides, it converts a recurring historical outcome (hegemonic great-power war) into a managed standoff, coordinating reciprocal restraint through communicated red lines, survivable second-strike forces, and crisis-communication channels.
% TRANSFER_FUNCTION: Moves fiscal resources from the general taxpayer bases of nuclear-armed states to defense establishments and their contractors; moves existential risk onto the urban populations of all sides without their consent; moves security deference from allied and weaker states toward the nuclear patrons whose guarantees they accept.
% ABSENT_VOICES: Urban populations under threat have no seat in posture or employment decisions; the TPNW coalition of non-nuclear states is excluded from the P5's deterrence management; future generations who inherit both the arsenals and the tail risk are represented by no one in the room.
% DISAPPEARANCE_RATIONALE: If the equilibrium vanished overnight, extended-deterrence allies would face immediate proliferation pressure and most would arm within a decade; crisis bargaining between great powers would lose its stabilizing shadow and revert to conventional escalation logic; the alliance architecture built around the guarantees would renegotiate wholesale. The world would not stay as it is — it would reorganize around a new, more dangerous equilibrium or a race to rebuild the old one.
% FOUNDING_PROBLEM: After 1945, prevent the recurrence of great-power total war now that nuclear weapons made such a war potentially civilization-ending, while managing continuing rivalry between armed blocs below that threshold.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: declassified Truman- and Eisenhower-era planning records show the founding problem as contemporaneously understood; neutral and non-aligned state diplomacy in the UN First Committee attests it; and the abolitionist coalition (ICAN, TPNW sponsors) accepts the founding-problem description while disputing the arrangement — corroboration of the problem from the arrangement's sharpest critics, not merely its operators.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.58: the arrangement continuously transfers on the order of two to three percent of great-power output to strategic forces and imposes unconsented existential risk on urban populations, yet it also underwrites the longest great-power peace in the modern state system — substantial, bounded extraction, not predation. Suppression at 0.62 reflects active foreclosure of exits: unilateral disarmament is punished by first-strike incentives, abolition is blocked by verification distrust, and posture decisions are insulated from ordinary electoral politics; the coercion is structural rather than internalized. Theater at 0.35: signaling is load-bearing in deterrence — communication IS the mechanism — so most visible activity is functional; parade-scale ceremony and declaratory ritual are the minority share. Accessibility_collapse at 0.45: grasping the logic does not collapse the alternative set — minimum-deterrence postures, deeper arms control, and risk-reduction architectures remain partially viable, which is why resistance (0.50) is organized and persistent rather than futile. The temporal series run on one shared grid and are deliberately non-monotonic: extraction and enforcement climbed through the first Cold War, relaxed across the post-1991 drawdown, and are climbing again in the current re-rivalry; the base_properties scalars describe the current (rising) phase. The oscillation tracks rivalry intensity rather than functioning as intermittent reinforcement, though the current phase's periodic crisis scares do help sustain budget coalitions — noted, not classified. The claimed_type (tangled_rope) is asserted from structure; the metrics are descriptive and independently authored.
 *
 * PERSPECTIVAL GAP:
 *   From the establishment seat the arrangement presents as a working order its members operate and staff — coordination they administer, costs they justify in testimony and doctrine. From the trapped urban-population seat the identical structure presents as an unconsented imposition of existential risk with no procedural recourse. Same structure, opposite phenomenology; the engine computes the per-seat divergence from the power and exit asymmetries (institutional/identity_locked versus powerless/trapped), and that divergence is the datum this corpus exists to take — this file does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the establishments (agenda-setting, identity-locked persistence) derive near the beneficiary pole; the industrial base likewise, with mobile exit damping further. The allies derive low d from their beneficiary declaration, but the pure-beneficiary derivation undershoots their dual position — their cities sit on their patron's adversary's target lists, so they are simultaneously protected and exposed. An explicit override raises the powerful seat to d=0.30 to encode that hostage externality. Victim declarations drive the targets: urban populations (powerless, trapped) and taxpayers (moderate, trapped) derive near the full-target pole. Scope amplification applies at the global scope the arrangement operates at; suppression is authored unscaled as a raw structural property, per the engine's separation of the two.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — managing great-power rivalry below the total-war threshold — is live: rivalry has returned and the arsenals persist, so the arrangement cannot be dismissed as mandate-outlived. But its costs are real and continuous, so it is not a pure coordination device either; the tangled_rope claim holds both facts without averaging them. The classification is also reading-contingent in a way the omegas carry: under contraction_reading the coordination object vanishes and the same arrangement drifts toward piton (theatrical upkeep against an impossible event); under contingent_reachability_reading its classification indexes to technology trajectories. Mandatrophy resolution therefore belongs to the kernel contest, not to this file alone — which is why the sibling structure lives in the omegas and network edges rather than being resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the dropping_reading the correct instantiation of the total_war_reachability_boundary kernel, or does one of the sibling readings (contraction_reading, contingent_reachability_reading) better capture the boundary?',
    'Comparative classification across the three linked reading files, adjudicated by capability evidence: arsenal composition and counterforce accuracy trends, delivery-system survivability, and observed crisis behavior under stress.',
    'If contraction_reading is correct, this arrangement loses its coordination object and reclassifies toward piton (maintenance against an impossible event); if contingent_reachability_reading is correct, this file''s classification becomes technology-indexed and can reverse with capability change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the reachability kernel this arrangement instantiates.').

omega_variable(
    feasibility_probability_boundary,
    'Where exactly does the contested boundary sit: is total war improbable-but-feasible (this reading) or outside the feasible set entirely (contraction_reading) — and is the disagreement located at the feasibility line or at the probability estimate?',
    'Near-miss record analysis (Cuban Missile Crisis, Able Archer 83, the Petrov and Norwegian rocket incidents) combined with counterforce-capability assessment: episodes in which escalation turned on discretionary human intervention indicate positive feasibility.',
    'Resolving the location determines whether deterrence coordinates against a real possibility (the tangled_rope classification stands) or performs against a null set (the mandatrophy mismatch flag fires).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feasibility_probability_boundary, conceptual, 'Location of the contested feasibility/probability boundary between the readings.').

omega_variable(
    nuclear_escalation_base_rate,
    'What is the actual annualized probability of nuclear use under the current equilibrium?',
    'Bayesian synthesis of the documented near-miss record, crisis frequency, and structured expert elicitation, updated per crisis survived.',
    'A base rate materially above zero makes the imposed population risk the dominant term in the ledger and pushes effective extraction upward; a rate near zero supports treating the arrangement''s costs as coordination overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_escalation_base_rate, empirical, 'Annualized probability of nuclear escalation under the standing arrangement.').

omega_variable(
    stability_instability_extraction,
    'Does extended-deterrence credibility license lower-level aggression by protectors (the stability-instability paradox), so that part of the arrangement''s output is the very conflict it claims to prevent?',
    'Comparative crisis data: incidence of proxy and conventional aggression by nuclear patrons during the umbrella period versus matched historical baselines.',
    'Confirmation raises effective extraction (the coordination good is partly self-undermining) and strengthens the payer-side case that the arrangement''s costs exceed its services.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_instability_extraction, empirical, 'Whether the equilibrium generates sub-total-war conflict as a side effect.').

omega_variable(
    establishment_identity_lock_durability,
    'How durable is the defense establishments'' attachment to the deterrent mission — would they sustain the arrangement even if its necessity were credibly disproven?',
    'Post-1991 budget and mission behavior: the partial pivot toward conventional and counterterror portfolios indicates partial mobility; full identity lock-in would predict resistance to any diversification.',
    'High durability keeps the arrangement alive past its function (piton-drift risk); demonstrated mobility means the arrangement persists only while it delivers, supporting the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(establishment_identity_lock_durability, empirical, 'Durability of institutional identity fusion with the deterrent mission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1949, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1949, total_war_reachability_boundary__dropping_reading, theater_ratio, 1949, 0.28).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.34).
narrative_ontology:measurement(tota_tr_t1983, total_war_reachability_boundary__dropping_reading, theater_ratio, 1983, 0.4).
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__dropping_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(tota_tr_t2003, total_war_reachability_boundary__dropping_reading, theater_ratio, 2003, 0.26).
narrative_ontology:measurement(tota_tr_t2014, total_war_reachability_boundary__dropping_reading, theater_ratio, 2014, 0.29).
narrative_ontology:measurement(tota_tr_t2026, total_war_reachability_boundary__dropping_reading, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(tota_be_t1949, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1949, 0.68).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.74).
narrative_ontology:measurement(tota_be_t1983, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1983, 0.71).
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1991, 0.58).
narrative_ontology:measurement(tota_be_t2003, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2003, 0.47).
narrative_ontology:measurement(tota_be_t2014, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2014, 0.5).
narrative_ontology:measurement(tota_be_t2026, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1949, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1949, 0.62).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.76).
narrative_ontology:measurement(tota_su_t1983, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1983, 0.79).
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1991, 0.52).
narrative_ontology:measurement(tota_su_t2003, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2003, 0.44).
narrative_ontology:measurement(tota_su_t2014, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2014, 0.49).
narrative_ontology:measurement(tota_su_t2026, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'nuclear deterrence / the possibility of total war' decomposes into three structurally distinct claims (epsilon-invariance principle). This file authors the dropping_reading: total war improbable but feasible, deterrence a maintained coordination equilibrium (tangled_rope, epsilon 0.58). total_war_reachability_boundary__contraction_reading authors the claim that winnable total war left the feasible set, under which the same standing arrangement loses its coordination object and drifts toward piton/theatrical maintenance. total_war_reachability_boundary__contingent_reachability_reading authors technology-dependence, making classification index to capability trajectories. Each story carries its own epsilon, victims, and claimed type; the upstream empirical question (feasibility) conditions the downstream classification, so the family is network-linked rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
