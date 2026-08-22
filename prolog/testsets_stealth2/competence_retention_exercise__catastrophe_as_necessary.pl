% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe-as-Necessary Doctrine in Organizational Competence Retention
 *   domain: safety engineering / organizational learning / high-reliability organizations
 *
 * SUMMARY:
 *   In high-stakes industries — aviation, nuclear power, chemical processing,
 *   offshore energy — a governing doctrine holds that only actual
 *   catastrophic events supply the organizational learning and visceral
 *   stakes that keep catastrophe-avoidance competence real; simulation is
 *   rehearsal, near-misses are shadows, and the disaster itself is the only
 *   authentic teacher. The doctrine is not merely believed: it is
 *   administered. It shapes training budgets, certification hour rules,
 *   post-incident inquiry framing, and the professional hierarchy separating
 *   operators who have 'been through the real thing' from those who have only
 *   rehearsed. Its genuine core is real — rare events do surface
 *   unknown-unknowns and encode stakes — but its operation also launders
 *   accountability (preventable failures reframed as systemic necessity),
 *   suppresses substitute technologies, and prices frontline and public harm
 *   as organizational tuition. KEY AGENTS (by structural relationship): -
 *   senior_operators_executives: agenda-setting beneficiary
 *   (institutional/arbitrage) — sets the doctrine, absorbs the accountability
 *   shield - frontline_operators: primary target (organized/identity_locked)
 *   — bears bodily risk priced as tuition; identity-fused enforcers -
 *   affected_public: primary target (powerless/trapped) — bears the
 *   catastrophe externality without consent - catastrophe_learning_industry:
 *   secondary beneficiary (organized/mobile) — monetizes each catastrophe's
 *   'lessons' - simulation_training_vendors: excluded challenger
 *   (powerful/arbitrage) — evidence defined as non-counting -
 *   safety_regulators: dual-positioned administrator
 *   (institutional/constrained) — enforced hour rules while crediting
 *   simulation - safety_science_community: analytical observer
 *   (institutional/analytical) — documents both genuine yield and laundering
 *   FAMILY NOTE: This file instantiates ONE reading of the
 *   competence_retention_exercise kernel; the sibling readings are separate
 *   constraints with separate ε. Shared referent (the standing
 *   catastrophe-as-tuition arrangement), reading-indexed values: this reading
 *   authors ε ≈ 0.60 — substantial laundering extraction bounded by a genuine
 *   epistemic core; the simulation_as_sufficient sibling authors lower ε for
 *   a simulation-centered regime (approaching the coordination-cost floor);
 *   near_miss_as_bridge sits between.
 *
 * KEY AGENTS:
 *   - senior_operators_executives: agenda-setting beneficiary (institutional/arbitrage) — sets learning doctrine, absorbs accountability shield, exposed to tail events
 *   - frontline_operators: primary target (organized/identity_locked) — bears the bodily risk the doctrine prices as tuition; identity fusion makes them simultaneous enforcers and casualties
 *   - affected_public: primary target (powerless/trapped) — bears catastrophe externality without consent or compensation; coalition potential via post-disaster advocacy
 *   - catastrophe_learning_industry: secondary beneficiary (organized/mobile) — collects from each catastrophe's framing as a fount of lessons
 *   - simulation_training_vendors: excluded challenger (powerful/arbitrage) — products adopted while their evidence is stipulated not to count
 *   - safety_regulators: dual-positioned administrator (institutional/constrained) — historically enforced real-hour minimums, progressively crediting simulation
 *   - safety_science_community: analytical observer (institutional/analytical) — attests both the genuine yield of real events and the laundering function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.6).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.44).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.6).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe-as-Necessary Doctrine in Organizational Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety engineering / organizational learning / high-reliability organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '08e7d9f4-877a-431f-8b2c-b9908a0119d8').
narrative_ontology:cs_kernel_codification('08e7d9f4-877a-431f-8b2c-b9908a0119d8', distributed).
narrative_ontology:cs_authority_grounding('08e7d9f4-877a-431f-8b2c-b9908a0119d8', practice).
narrative_ontology:cs_interpretation_layer_present('08e7d9f4-877a-431f-8b2c-b9908a0119d8').
narrative_ontology:cs_reading_relation('08e7d9f4-877a-431f-8b2c-b9908a0119d8', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('08e7d9f4-877a-431f-8b2c-b9908a0119d8', competence_retention_exercise__near_miss_as_bridge, forecloses).
narrative_ontology:cs_axiom('08e7d9f4-877a-431f-8b2c-b9908a0119d8', foundational, catastrophic_exposure_necessary_for_genuine_competence).
narrative_ontology:cs_axiom_status(catastrophic_exposure_necessary_for_genuine_competence, holdable).
narrative_ontology:cs_axiom_grounding('08e7d9f4-877a-431f-8b2c-b9908a0119d8', catastrophic_exposure_necessary_for_genuine_competence, empirically_contingent).
narrative_ontology:cs_axiom('08e7d9f4-877a-431f-8b2c-b9908a0119d8', secondary, simulated_stakes_cannot_substitute_for_real_consequence).
narrative_ontology:cs_axiom_status(simulated_stakes_cannot_substitute_for_real_consequence, holdable).
narrative_ontology:cs_axiom_grounding('08e7d9f4-877a-431f-8b2c-b9908a0119d8', simulated_stakes_cannot_substitute_for_real_consequence, empirically_contingent).
narrative_ontology:cs_reference_frame('08e7d9f4-877a-431f-8b2c-b9908a0119d8', real_event_experiential_standard).
narrative_ontology:cs_drift_state('08e7d9f4-877a-431f-8b2c-b9908a0119d8', contemporary_high_fidelity_simulation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('08e7d9f4-877a-431f-8b2c-b9908a0119d8', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, senior_operators_executives).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, catastrophe_learning_industry).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, affected_public).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, scar_tissue_competence_theory).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, visceral_stakes_epistemic_privilege).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, invisible_decay_during_incident_free_periods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the learning philosophy and training budgets for their organizations, chair post-incident reviews, and decide how a disaster is framed internally and publicly. Rarely present at the point of failure themselves, but exposed to tail consequences — hearings, litigation, reputation — when one occurs. Can move between firms and sectors, carrying the doctrine and its protections with them.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, senior_operators_executives, agenda_setter,
    institutional, biographical, arbitrage, global).

% Pilots, reactor operators, drill crews, refinery teams: the people physically present when systems fail. They absorb the bodily risk that the doctrine treats as the price of real competence, and many take pride in having 'been through the real thing,' which binds them to defend the standard their professional standing rests on. Unionized and licensed; leaving the industry means leaving the identity.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, frontline_operators, payer,
    organized, biographical, identity_locked, global).

% Communities living under flight paths, downstream of dams and plants, beside rail lines carrying hazardous freight. They bear the outside cost of rare failures without having chosen the exposure and without compensation; relocating away from infrastructure they depend on is rarely realistic. After disasters they organize ad hoc through advocacy groups and plaintiffs' coalitions.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, affected_public, payer,
    powerless, generational, trapped, regional).

% Investigation consultancies, post-incident training providers, speakers and authors who turn each disaster into curricula, conferences, and case libraries. Their revenue arrives only when catastrophes occur and grows largest when the disaster is framed as a fount of lessons. They do not set the doctrine; they collect from its operation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, catastrophe_learning_industry, beneficiary,
    organized, biographical, mobile, global).

% Build the full-motion simulators, digital twins, and scenario engines that the doctrine declares to be mere rehearsal. Locked out of the authority conversation — their evidence is stipulated not to count — even as regulators quietly accept more of their product toward certification. Their commercial strategy is arbitrage: sell hardest into the jurisdictions and domains where the doctrine's grip is weakest.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_training_vendors, excluded,
    powerful, biographical, arbitrage, global).

% Certify competence regimes and write the hour rules. For decades they enforced the doctrine's terms — minimum real-aircraft or real-plant hours — while progressively crediting simulator time as fidelity improved. Caught between veteran experiential authority and simulation evidence, and politically accountable after every visible disaster.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_regulators, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__catastrophe_as_necessary, safety_regulators, agenda_setter).

% Human-factors and high-reliability researchers who study both what real events teach and what simulation transfers. Their findings are cited selectively by every faction; they hold no enforcement power and sit outside the benefiting set.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_science_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, senior_operators_executives).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under rarity of competence-relevant events, the doctrine solves an epistemic-authority problem: it designates real catastrophic events as the authoritative test of catastrophe-avoidance competence, coordinating who counts as genuinely competent, how post-incident inquiry is resourced and framed, and how collective skepticism toward proxy exercises is organized. Stated without evaluation: whatever else it does, it coordinates the profession's answer to 'what counts as the real thing.'
% TRANSFER_FUNCTION: Moves the costs of rare system failures — death, injury, trauma, destroyed assets — onto frontline operators and exposed publics, while moving the informational yield upward into organizational memory and executive position; simultaneously moves training investment away from simulation infrastructure toward incident-response capacity, and moves blame away from decision-makers by recoding preventable failures as systemic necessity.
% ABSENT_VOICES: Simulation vendors and near-miss methodologists are excluded by definition — the doctrine stipulates their evidence does not count, so the challenge to it is structurally kept out of the authority conversation even as their products win adoption. Families of the dead are absent and cannot testify that their particular catastrophe was not necessary. Frontline operators in strong-doctrine cultures self-censor: questioning the doctrine marks an operator as un-blooded, so dissent exits through attrition rather than argument.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, training investment would reallocate toward simulation and near-miss analytics, certification regimes would drop real-hour minimums faster, post-incident inquiries would frame losses as preventable failures with assignable responsibility rather than tuition, the veteran authority hierarchy would dissolve, and the post-catastrophe learning economy would lose its raw material — the arrangements of high-stakes industries visibly depend on it.
% FOUNDING_PROBLEM: Before high-fidelity simulation existed, the events that tested catastrophe-avoidance competence were too rare to encounter in ordinary practice; real experience was the only teacher available, and the doctrine codified that scarcity into a principle of competence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of technology and regulatory dockets from the pre-simulator era attest the original scarcity of real feedback; the academic safety-science and human-factors literature attests both that the scarcity has been partially relieved by simulation and that a disputed residue remains. No party outside the doctrine's beneficiaries attests that the founding problem is fully dead; none inside it attests that simulation closes the residue — hence contested rather than live or dead.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.60 (interval end): the doctrine's operation mixes a genuine learning yield with a substantial laundering function — each catastrophe is absorbed into 'necessary tuition' framing that diffuses accountability, suppresses simulation investment, and externalizes harm onto seats with no exit. Suppression 0.44 reflects cultural-professional enforcement (gatekeeping, certification hour rules, ridicule of simulation reliance) rather than coercive force, and it is decaying. Theater 0.45 tracks the growing ceremonial share — anniversary lessons-learned rituals, scar-tissue mythology — as functional content migrates to simulation and near-miss systems; it sits just under the Goodhart line. Accessibility_collapse 0.42: alternatives are demonstrably viable (aviation's simulator-dominant training regime), so the doctrine does not fully collapse them. Resistance 0.60: sustained pressure from vendors, safety science, modernizing regulators, and victim advocacy. All three temporal series share one seven-point grid (1975–2025). Base_extractiveness oscillates around a declining trend: spikes are post-catastrophe framing wars in which the doctrine is re-legitimized precisely when accountability pressure peaks — the oscillation is itself partly an extraction mechanism (intermittent reinforcement), not noise; troughs are quiet-period drift toward substitution. The base_properties scalars are measured at the interval end, a quiet-phase trough. Suppression_requirement falls monotonically: the enforcement machinery (real-hour certification minimums, veteran gatekeeping) is eroding under simulation fidelity and regulatory modernization — enforcement decay, not ratchet.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement computes as the administered price of genuine competence — the executive funds incident response, frames losses as tuition, and experiences the doctrine as hard-won wisdom. From the payer seats the same structure computes as enforced extraction: the identity-locked operator cannot exit the industry that both employs and endangers them, and the trapped public bears the externality without consent. The excluded vendor seat experiences market foreclosure dressed as epistemics. The engine derives these divergent per-seat classifications from role, power, and exit data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map cleanly onto the derivation chain: senior_operators_executives (beneficiary, arbitrage exit) derive d near the beneficiary end — slightly raised above pure subsidy by tail-event exposure (hearings, litigation, reputation) that needs no override to express; catastrophe_learning_industry (beneficiary, mobile) likewise near-beneficiary. frontline_operators (victim, identity_locked) and affected_public (victim, trapped, powerless) derive d near the full-target end, with identity lock amplifying the operator seat toward full target. No directionality_overrides are authored: the derivation chain captures every seat's relationship from the declared structure, and adding overrides would second-guess data the chain already reads correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are prevented. Reading the doctrine as pure extraction erases its documented genuine yield — real events do surface unknown-unknowns and encode stakes that simulation demonstrably under-transfers; the coordination half is real, and the unknown_unknown_residue omega keeps that question open. Reading it as pure coordination launders the accountability-deflection function — the necessity frame systematically converts preventable failures into unblamable tuition, and the necessity_frame_accountability_share omega measures that share. Tangled rope holds both halves. On the R5 interview: founding_problem_status is contested, not dead — simulation relieved the original scarcity of real feedback without obviously closing the unknown-unknown residue — so the mismatch consumer finds contested × world_rearranges and correctly raises no zombie flag; the monitored transition is status drifting to dead while verdict stays world_rearranges, which would fire the capture/zombie flag against the computed theater path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the catastrophe_as_necessary reading of the competence_retention_exercise kernel — what structurally changes if a sibling reading is adopted instead?',
    'Cross-reading comparison of the three family files: instantiate each reading''s regime and compare authored ε, beneficiary/victim structure, and computed per-seat types.',
    'If simulation_as_sufficient is adopted, the arrangement''s ε falls toward the identity_coordination floor and the type drifts toward rope; if near_miss_as_bridge, an intermediate regime results; this reading''s foreclosure edges to both siblings are the formal statement of the trilemma.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption changes ε and type.').

omega_variable(
    simulation_transfer_validity,
    'Does high-fidelity simulation produce competence that transfers to real catastrophic conditions, or does the doctrine''s ''rehearsal is not the real thing'' claim identify a real transfer gap?',
    'Transfer-of-training meta-analyses; audits of real-event crew and plant performance attributable to simulator-trained versus experience-trained cohorts; natural experiments from jurisdictions that moved certification to simulation-dominant regimes.',
    'Strong transfer evidence collapses the doctrine''s empirical foundation, drives its foundational axiom toward overridden, and accelerates the authority_erosion drift already authored; persistent transfer gaps strengthen the genuine-coordination half and stabilize the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'The empirical crux: simulation-to-reality transfer of catastrophe-competence.').

omega_variable(
    necessity_frame_accountability_share,
    'When organizations invoke the necessity frame after a catastrophe, what share is genuine epistemic update versus accountability deflection?',
    'Comparative coding of post-incident reports across decades: did findings reassign responsibility and change incentive structures, or only diffuse blame into ''systemic lessons''? Concordance between invoked lessons and implemented changes.',
    'High deflection share pushes the computed type toward pure extraction and raises effective burden on the agenda-setter seat; low deflection share supports the hybrid reading and lowers ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_frame_accountability_share, empirical, 'Decomposing the necessity frame''s post-catastrophe use.').

omega_variable(
    unknown_unknown_residue,
    'Is there a residue of failure modes that can only be discovered by actual catastrophic occurrence — the doctrine''s strongest ground?',
    'Systematic taxonomy comparison: failure modes first revealed in real events versus those anticipated by simulation and near-miss analysis; bounds of near-miss extrapolation established by the near_miss_as_bridge sibling''s evidence base.',
    'A large residue anchors the genuine-coordination half and keeps the reading live despite simulation progress; a small residue removes the doctrine''s last empirical leg and tips the arrangement toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unknown_unknown_residue, empirical, 'Whether real catastrophes retain irreplaceable epistemic content.').

omega_variable(
    fresh_domain_importation,
    'The measured decline is traced in legacy high-reliability domains (aviation, nuclear); are emerging high-stakes domains (autonomous systems, AI deployment) importing the doctrine fresh, offsetting the aggregate decline?',
    'Track doctrine uptake — ''we need real-world incidents to learn'' framings in AI-lab and autonomy postmortems — and certification regimes in newly high-stakes domains.',
    'Importation would flatten or reverse the declining base_extractiveness series and raise scope-weighted extraction; the authored trajectory is domain-bound, not universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fresh_domain_importation, empirical, 'Counter-current to the measured enforcement decay.').

omega_variable(
    blooded_veteran_identity_lock,
    'Is veteran resistance to simulation substitution identity fusion — professional selfhood constituted by having ''been through the real thing'' — rather than evidence-tracking, and does that lock persist after the evidential case for substitution is settled?',
    'Longitudinal cohort attitude tracking as simulator fidelity rises; retirement-wave natural experiments comparing cohorts trained under real-hour versus simulation-dominant regimes.',
    'If identity-locked, enforcement decays slower than technology allows — the suppression floor is higher than the structural measure suggests, and the operator seat''s directionality stays pinned near full-target even as material conditions ease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blooded_veteran_identity_lock, empirical, 'Identity-lock mechanism sustaining the doctrine''s enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1975, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 1975, 0.25).
narrative_ontology:measurement_basis(comp_tr_t1975, observed).
narrative_ontology:measurement(comp_tr_t1983, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 1983, 0.28).
narrative_ontology:measurement_basis(comp_tr_t1983, observed).
narrative_ontology:measurement(comp_tr_t1991, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 1991, 0.31).
narrative_ontology:measurement_basis(comp_tr_t1991, observed).
narrative_ontology:measurement(comp_tr_t1999, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 1999, 0.33).
narrative_ontology:measurement_basis(comp_tr_t1999, observed).
narrative_ontology:measurement(comp_tr_t2007, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2007, 0.36).
narrative_ontology:measurement_basis(comp_tr_t2007, observed).
narrative_ontology:measurement(comp_tr_t2015, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(comp_tr_t2015, observed).
narrative_ontology:measurement(comp_tr_t2025, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(comp_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t1975, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 1975, 0.74).
narrative_ontology:measurement_basis(comp_be_t1975, observed).
narrative_ontology:measurement(comp_be_t1983, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 1983, 0.68).
narrative_ontology:measurement_basis(comp_be_t1983, observed).
narrative_ontology:measurement(comp_be_t1991, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 1991, 0.7).
narrative_ontology:measurement_basis(comp_be_t1991, observed).
narrative_ontology:measurement(comp_be_t1999, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 1999, 0.65).
narrative_ontology:measurement_basis(comp_be_t1999, observed).
narrative_ontology:measurement(comp_be_t2007, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2007, 0.67).
narrative_ontology:measurement_basis(comp_be_t2007, observed).
narrative_ontology:measurement(comp_be_t2015, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(comp_be_t2015, observed).
narrative_ontology:measurement(comp_be_t2025, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(comp_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1975, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement_basis(comp_su_t1975, observed).
narrative_ontology:measurement(comp_su_t1983, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 1983, 0.66).
narrative_ontology:measurement_basis(comp_su_t1983, observed).
narrative_ontology:measurement(comp_su_t1991, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 1991, 0.61).
narrative_ontology:measurement_basis(comp_su_t1991, observed).
narrative_ontology:measurement(comp_su_t1999, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 1999, 0.56).
narrative_ontology:measurement_basis(comp_su_t1999, observed).
narrative_ontology:measurement(comp_su_t2007, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2007, 0.52).
narrative_ontology:measurement_basis(comp_su_t2007, observed).
narrative_ontology:measurement(comp_su_t2015, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement_basis(comp_su_t2015, observed).
narrative_ontology:measurement(comp_su_t2025, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2025, 0.44).
narrative_ontology:measurement_basis(comp_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, identity_coordination).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how organizations maintain catastrophe-competence' decomposes, per the ε-invariance principle, into three structurally distinct claims about what suffices as real-world feedback — catastrophes only (this file, ε ≈ 0.60), near-misses sufficient (intermediate ε), simulation sufficient (ε near the coordination-cost floor). Each story carries its own ε, stakeholders, and claimed type; the upstream claim (real events teach) is cited as evidence by the downstream contested claims, so edges run from this reading to both siblings. Measuring the family through one story would conflate observables that yield different ε — the confusion is in the label, not the structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
