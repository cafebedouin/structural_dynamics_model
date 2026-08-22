% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Lived-Catastrophe Necessity Doctrine of Competence Maintenance
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   Safety-critical organizations rehearse constantly — drills, tabletops,
 *   simulators — while real catastrophes arrive rarely and irregularly. The
 *   doctrine instantiated here holds that only actual catastrophe exercises
 *   the competence kernel proper: simulation is necessary rehearsal but not
 *   the thing itself, and judgment decays covertly between real activations.
 *   As a governing arrangement, the doctrine shapes what gets funded (live
 *   exercises and post-event investigation over simulation infrastructure and
 *   competency measurement), who advances (holders of real-event experience),
 *   and how failure is narrated afterward (an organization 'could not have
 *   known' what no real test had shown it). The claim/metric gap is
 *   deliberate: the doctrine is CLAIMED by its holders as hard-won
 *   professional wisdom about how competence works, while the authored
 *   metrics describe an actively enforced arrangement that transfers unpriced
 *   risk onto parties with no seat in its administration — the engine
 *   measures that divergence; do not reconcile the claim to the metrics. Per
 *   the epsilon-invariance principle, the colloquial label 'organizations
 *   need exercises to stay competent' decomposes into three structurally
 *   distinct claims; this file is one of them. KEY AGENTS (by structural
 *   relationship): - senior_operational_leadership: agenda-setter
 *   (institutional/arbitrage) — sets the training mix, owns qualification
 *   standards, invokes the doctrine after failures - crisis_exercise_vendors:
 *   beneficiary (organized/mobile) — sells live immersive exercises priced on
 *   the premise that only realism develops readiness -
 *   fenceline_communities_and_service_users: primary target
 *   (powerless/trapped) — bears the consequences of operator judgment never
 *   activated under real stakes - frontline_operators: target with incidental
 *   benefit (moderate/identity_locked) — receives the training mix, carries
 *   the consequences of untested decay, is named in post-failure
 *   investigations - early_career_responders: deferred-development target
 *   (powerless/constrained) — accumulates discounted simulator hours awaiting
 *   real events that may not arrive in their tenure -
 *   simulation_science_community: excluded voice (organized/mobile) —
 *   produces transfer-and-fidelity evidence the doctrine's holders treat as
 *   category-irrelevant - regulatory_safety_bodies: analytical observer
 *   (institutional/analytical) — audits drill compliance, investigates
 *   failures, adjudicates between readings
 *
 * KEY AGENTS:
 *   - senior_operational_leadership: agenda-setter (institutional/arbitrage) — administers the doctrine, holds the veteran authority premium, wields the post-failure exculpatory narrative
 *   - crisis_exercise_vendors: beneficiary (organized/mobile) — monetizes the doctrine's premise that only live realism develops readiness
 *   - fenceline_communities_and_service_users: primary target (powerless/trapped) — absorbs the gap between rehearsed and actual performance
 *   - frontline_operators: target with incidental benefit (moderate/identity_locked) — trained, exposed, and scapegoatable
 *   - early_career_responders: deferred-development target (powerless/constrained) — development gated on events outside their control
 *   - simulation_science_community: excluded voice (organized/mobile) — contests the categorical claim from outside the budget room
 *   - regulatory_safety_bodies: analytical observer (institutional/analytical) — audits, investigates, adjudicates between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.72).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Lived-Catastrophe Necessity Doctrine of Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'd3a1477e-6129-4c2c-adf4-2ac797e1b966').
narrative_ontology:cs_kernel_codification('d3a1477e-6129-4c2c-adf4-2ac797e1b966', distributed).
narrative_ontology:cs_authority_grounding('d3a1477e-6129-4c2c-adf4-2ac797e1b966', practice).
narrative_ontology:cs_interpretation_layer_present('d3a1477e-6129-4c2c-adf4-2ac797e1b966').
narrative_ontology:cs_reading_relation('d3a1477e-6129-4c2c-adf4-2ac797e1b966', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('d3a1477e-6129-4c2c-adf4-2ac797e1b966', exercise_as_competence_maintenance__hybrid_decay_reading, coexists_with).
narrative_ontology:cs_axiom('d3a1477e-6129-4c2c-adf4-2ac797e1b966', foundational, only_real_stakes_activate_competence_kernel).
narrative_ontology:cs_axiom_status(only_real_stakes_activate_competence_kernel, holdable).
narrative_ontology:cs_axiom_grounding('d3a1477e-6129-4c2c-adf4-2ac797e1b966', only_real_stakes_activate_competence_kernel, empirically_contingent).
narrative_ontology:cs_axiom('d3a1477e-6129-4c2c-adf4-2ac797e1b966', secondary, untested_competence_presumed_decayed).
narrative_ontology:cs_axiom_status(untested_competence_presumed_decayed, holdable).
narrative_ontology:cs_axiom_grounding('d3a1477e-6129-4c2c-adf4-2ac797e1b966', untested_competence_presumed_decayed, instrumental).
narrative_ontology:cs_reference_frame('d3a1477e-6129-4c2c-adf4-2ac797e1b966', real_event_exercise_standard).
narrative_ontology:cs_drift_state('d3a1477e-6129-4c2c-adf4-2ac797e1b966', contemporary_simulation_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d3a1477e-6129-4c2c-adf4-2ac797e1b966', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, senior_operational_leadership).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_exercise_vendors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, fenceline_communities_and_service_users).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, early_career_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, competence_decay_under_disuse_hypothesis).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, stress_inoculation_superiority_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the training-mix budget, defines what counts as qualification, and controls promotion criteria. Invokes the doctrine in budget defense before incidents ('there is no substitute for the real thing') and in investigation defense afterward ('no one could have known what we had never been tested on'). Accumulates authority from real-event experience that simulation hours cannot confer, and can move between organizations carrying that veteran status intact.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, senior_operational_leadership, agenda_setter,
    institutional, biographical, arbitrage, global).

% Sells live immersive exercises, stress-inoculation programs, and 'as-real-as-it-gets' training packages priced on the premise that only realism develops readiness. Revenue scales with the doctrine's acceptance; product lines can pivot as demand shifts between live exercises and simulation platforms.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_exercise_vendors, beneficiary,
    organized, immediate, mobile, global).

% Lives and works adjacent to plants, hospitals, and transport corridors operated by people whose judgment has never been activated under real stakes. Cannot opt out of the exposure and holds no seat in the decisions that set training mixes or qualification standards. Bears the consequences whenever rehearsed performance and actual performance diverge.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, fenceline_communities_and_service_users, payer,
    powerless, generational, trapped, local).

% Receives whatever training mix the organization funds, and in the moment of truth carries the consequences of capacities that were never activated under real conditions. Professional identity is fused with being ready, which makes acknowledging decay personally costly. When systems fail, investigations tend to arrive at their door before they arrive at the budget office's.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators, beneficiary).

% Enters the profession accumulating simulator hours that the prevailing doctrine discounts as rehearsal. Development toward full responsibility is gated on real events that may not occur during their tenure; career progression stalls on a scarcity they do not control and cannot manufacture.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, early_career_responders, payer,
    powerless, biographical, constrained, national).

% Produces research on transfer of training, simulator fidelity, and retention curves, arguing that the gap between rehearsal and reality is measurable and closable. Largely absent from the rooms where training budgets and qualification standards are set; findings are received by doctrine holders as category-irrelevant rather than as evidence to rebut.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_science_community, excluded,
    organized, generational, mobile, global).

% Audits drill compliance, mandates exercise frequency, investigates failures, and publishes findings that feed back into the doctrine's enforcement machinery. Adjudicates between competing accounts of what maintains competence, with authority to reshape the training mix through regulation.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, regulatory_safety_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, senior_operational_leadership).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps organizations investing in rehearsal (drills, simulators, tabletops) while reserving the term readiness for real-stakes performance; concentrates post-event effort into deep investigation because real events are the only occasions on which the organization observes what it can actually do; guards against the overconfidence that easy simulator success otherwise breeds.
% TRANSFER_FUNCTION: Moves unpriced risk — the gap between rehearsed and actual performance — from organizations and their leadership onto exposed publics and frontline staff; moves authority, promotion prospects, and post-event credibility toward holders of real-event experience; moves training budgets toward live exercises and their vendors, away from simulation infrastructure and competency-measurement programs.
% ABSENT_VOICES: Simulation-science and human-factors researchers (who would argue transfer is measurable and fidelity improvable) and the exposed publics themselves (who would price the risk they carry) are absent from the rooms where training mixes, qualification standards, and post-incident narratives are set.
% DISAPPEARANCE_RATIONALE: Training budgets would rebalance toward measurable competency programs, promotion criteria would lose the real-event premium, post-failure accountability could no longer be deflected onto the absence of real tests, and the live-exercise vendor market would contract; qualification regimes across safety-critical industries would reorganize around demonstrated rather than presumed decay.
% FOUNDING_PROBLEM: In the wake of disasters attributed to paper-perfect organizations — plans that passed every drill yet failed real events — the preparedness professions needed a way to insist that rehearsal performance does not certify crisis performance, and to justify treating rare real events as the decisive evidence of capability.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident-investigation boards and regulatory audit findings repeatedly document the drill-performance/reality gap the doctrine was built to name, corroborating the founding problem from outside the benefiting parties; the human-factors literature likewise attests that unchecked rehearsal breeds overconfidence. Whether the problem remains best answered by this doctrine rather than by validated simulation is disputed by the sibling readings and their research constituencies.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.72) because the arrangement's defining cost — the gap between rehearsed and actual performance — lands almost entirely on parties with no seat in its administration, and because the doctrine's exculpatory function converts predictable, budgetable decay into misfortune after the fact. Suppression (0.62) is authored as a raw structural property, unscaled by power or scope: the doctrine holds through budget gatekeeping, promotion criteria, and investigative framing rather than legal coercion, and it actively discounts the rival program (measured transfer via validated simulation) as category-irrelevant. Theater (0.45) reflects the compliance-drill layer: a substantial share of exercise activity exists to satisfy audit and documentation rather than to develop anything. Accessibility collapse (0.70): inside the doctrine's frame, alternatives collapse hard — if only real stakes exercise the kernel, simulation investment is rationally capped at rehearsal value and the remaining levers are waiting or buying realism. Resistance (0.55): the simulation-science community and parts of the regulatory apparatus actively contest the categorical claim, keeping the framework from total closure. Coordination type identity_coordination is declared because the doctrine's dominant function is boundary maintenance — deciding who counts as tested, and conferring authority on those who have crossed the boundary; the conservative floor applies, so excess extraction remains visible despite the complexity offset. All three temporal series run on one shared seven-point grid (t=0..45, mapped to 1980-2025): base_extractiveness creeps upward as safety gains lengthen the intervals between real activations in mature sectors, theater_ratio climbs with the formalization of compliance drilling, and suppression_requirement rises as certification and audit machinery matured, then plateaus.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat the doctrine reads as hard-won professional wisdom: anyone who has commanded through a real event knows the difference, and budgeting accordingly is prudence. From the trapped-public seat the same doctrine reads as an unpriced liability: their exposure is set by decisions made without them, justified by a claim no one has tested. From the identity-locked operator seat it is both creed and trap: professing it is professionally mandatory, and professing it means accepting that one's own readiness is unverifiable until the worst day. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them. The latent counterweight is a public-operator coalition — both seats bear the gap's costs — but identity lock and geographic dispersion keep it from forming.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place senior_operational_leadership and crisis_exercise_vendors near the subsidized end (low d): the doctrine preserves the leadership authority premium and the exculpatory shield, and sustains the live-exercise market. Victim declarations place fenceline_communities_and_service_users, frontline_operators, and early_career_responders near the full-target end (high d), amplified by exit structure: the public is trapped in place, operators are identity-locked (admitting decay threatens the self-concept their profession is built on), and junior staff are constrained by career dependence. Exit asymmetry does the differentiating work among actors of similar nominal standing, so no directionality overrides were needed — the derivation from beneficiary/victim declarations plus exit options reproduces the intended relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two opposite errors. Reading the doctrine as pure coordination misses the risk transfer: the arrangement's costs land on people who never sat in the budget meeting. Reading it as pure extraction misses the genuine epistemic service: rehearsal performance really does not certify crisis performance, and organizations that forget this fail catastrophically. The founding problem is authored contested rather than dead, so no obsolescence verdict is asserted. The watch-item is the reverse drift: if validated simulation ever closes the transfer gap, the doctrine's persistence would become inertia plus performance — annual drills kept for compliance, veteran authority sustained by scarcity of the very events that would refute it — the classic signature of a mandate outliving its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint instantiates the lived_catastrophe_necessity_reading of the exercise_as_competence_maintenance kernel; how would classification change under the sibling readings?',
    'Generate the sibling stories (simulation_sufficiency_reading, hybrid_decay_reading) and compare computed per-seat types, epsilon, and victim sets across the family.',
    'Under simulation_sufficiency, epsilon drops sharply (simulation delivers the exercise) and the victim set contracts to fidelity-gap cases; under hybrid_decay, values land intermediate with a split victim set. This file''s classification is conditional on its reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: one of three readings of a contested kernel; siblings are separate constraints.').

omega_variable(
    decay_law_vs_manageable_property,
    'Is competence decay without real-stakes activation an irreducible feature of skilled judgment, or a property that validated high-fidelity simulation and continuous measurement can mitigate?',
    'Longitudinal competency studies comparing cohorts with matched simulation dosage but differing real-activation histories, controlling for task type and measurement method.',
    'If decay is law-like, the doctrine''s core stands and part of the measured extraction is the price of an epistemic limit; if manageable, the doctrine operates as cover for underinvestment and epsilon rises further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_law_vs_manageable_property, empirical, 'Whether the doctrine''s factual core is a natural limit or a tractable engineering problem.').

omega_variable(
    caution_vs_exculpation_mix,
    'What fraction of the doctrine''s institutional persistence is genuine epistemic caution versus post-hoc accountability shielding?',
    'Compare doctrine-invocation contexts: budget-defense usage before incidents versus liability-defense usage in investigation testimony; discourse analysis of after-action records.',
    'A high exculpation share pushes the computed type toward the snare end; a high caution share supports the coordination-function reading and lowers effective extraction for the agenda-setting seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caution_vs_exculpation_mix, empirical, 'Decomposing the doctrine''s persistence into epistemic and self-protective components.').

omega_variable(
    victim_set_boundary,
    'Who counts as exposed? This reading counts everyone served by operators whose judgment has never been activated under real stakes; the sibling readings draw the boundary differently.',
    'Adopt each sibling reading and re-enumerate the victim set; compare aggregate exposure estimates across the family.',
    'Victim-set size drives aggregate extraction and can move the computed type across class thresholds; the boundary choice is load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, conceptual, 'Boundary of the harmed population depends on the reading adopted.').

omega_variable(
    validation_circularity,
    'Can simulation effectiveness ever be validated against real-stakes outcomes when real events are too rare to serve as a benchmark — does the doctrine''s core claim sit beyond empirical test?',
    'Pooled cross-industry event databases and natural experiments in which simulation-heavy teams encountered real events; meta-analysis across sectors to overcome single-domain rarity.',
    'If the claim is untestable as posed, the doctrine''s epistemic warrant weakens and its persistence must be explained by enforcement and interest rather than evidence, shifting classification toward extraction-maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(validation_circularity, empirical, 'Testability of the necessity claim given the rarity of real activations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(excomp_lived_cat_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(excomp_lived_cat_tr_t0, observed).
narrative_ontology:measurement(excomp_lived_cat_tr_t7, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement_basis(excomp_lived_cat_tr_t7, observed).
narrative_ontology:measurement(excomp_lived_cat_tr_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(excomp_lived_cat_tr_t15, observed).
narrative_ontology:measurement(excomp_lived_cat_tr_t22, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 22, 0.38).
narrative_ontology:measurement_basis(excomp_lived_cat_tr_t22, observed).
narrative_ontology:measurement(excomp_lived_cat_tr_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(excomp_lived_cat_tr_t30, observed).
narrative_ontology:measurement(excomp_lived_cat_tr_t37, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 37, 0.44).
narrative_ontology:measurement_basis(excomp_lived_cat_tr_t37, observed).
narrative_ontology:measurement(excomp_lived_cat_tr_t45, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement_basis(excomp_lived_cat_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(excomp_lived_cat_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.56).
narrative_ontology:measurement_basis(excomp_lived_cat_be_t0, observed).
narrative_ontology:measurement(excomp_lived_cat_be_t7, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 7, 0.6).
narrative_ontology:measurement_basis(excomp_lived_cat_be_t7, observed).
narrative_ontology:measurement(excomp_lived_cat_be_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(excomp_lived_cat_be_t15, observed).
narrative_ontology:measurement(excomp_lived_cat_be_t22, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 22, 0.66).
narrative_ontology:measurement_basis(excomp_lived_cat_be_t22, observed).
narrative_ontology:measurement(excomp_lived_cat_be_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 30, 0.69).
narrative_ontology:measurement_basis(excomp_lived_cat_be_t30, observed).
narrative_ontology:measurement(excomp_lived_cat_be_t37, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 37, 0.71).
narrative_ontology:measurement_basis(excomp_lived_cat_be_t37, observed).
narrative_ontology:measurement(excomp_lived_cat_be_t45, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 45, 0.72).
narrative_ontology:measurement_basis(excomp_lived_cat_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(excomp_lived_cat_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(excomp_lived_cat_su_t0, observed).
narrative_ontology:measurement(excomp_lived_cat_su_t7, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 7, 0.47).
narrative_ontology:measurement_basis(excomp_lived_cat_su_t7, observed).
narrative_ontology:measurement(excomp_lived_cat_su_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement_basis(excomp_lived_cat_su_t15, observed).
narrative_ontology:measurement(excomp_lived_cat_su_t22, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 22, 0.58).
narrative_ontology:measurement_basis(excomp_lived_cat_su_t22, observed).
narrative_ontology:measurement(excomp_lived_cat_su_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement_basis(excomp_lived_cat_su_t30, observed).
narrative_ontology:measurement(excomp_lived_cat_su_t37, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 37, 0.62).
narrative_ontology:measurement_basis(excomp_lived_cat_su_t37, observed).
narrative_ontology:measurement(excomp_lived_cat_su_t45, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement_basis(excomp_lived_cat_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, identity_coordination).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'organizations need exercises to stay competent' decomposes into three structurally distinct claims (epsilon-invariance principle): this lived-catastrophe necessity reading (epsilon 0.72; victims include all exposed to operators untested under real stakes), the simulation_sufficiency reading (epsilon far lower; simulation delivers the exercise itself), and the hybrid_decay reading (intermediate; split kernel with split exercise requirements). Each is authored as its own story with its own beneficiaries, victims, and claimed type; this file links both siblings via affects_constraints and the sibling files reciprocate. The lived-catastrophe reading is the traditional position from which the other two emerged as challenges, so its edges point at both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
