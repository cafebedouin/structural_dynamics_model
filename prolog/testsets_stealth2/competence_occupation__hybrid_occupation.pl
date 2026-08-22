% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Continuous Multi-Mechanism Competence Occupation Mandate
 *   domain: organizational/safety-regulatory
 *
 * SUMMARY:
 *   This story instantiates the hybrid_occupation reading of the
 *   competence_occupation kernel (see kernel_context below). The standing
 *   arrangement under contest: certificated high-reliability operators —
 *   airlines, nuclear licensees, complex hospital systems — must maintain
 *   safety-critical competence through a continuous stack of complementary
 *   mechanisms: full-mission simulation, classroom and computer-based
 *   refreshers, procedural reinforcement drills, and line observations/audits
 *   of real operations. No regulator, operator, or research body claims to
 *   know the optimal mix, so the requirement is administered as a permanent,
 *   untrimmable bundle. The bundle solves a real problem — skill decay in
 *   rarely exercised competencies is empirically documented and externally
 *   corroborated — while the no-consensus condition guarantees a permanent
 *   surplus of mandated activity above any demonstrable necessity, a surplus
 *   collected by simulator manufacturers and courseware vendors, by internal
 *   training bureaucracies whose scale tracks the number of required pillars,
 *   and by a research field whose funding depends on the question staying
 *   open. Per the epsilon-invariance principle, the sibling readings
 *   (simulation-only sufficiency; real-incident necessity) are separate
 *   constraints with their own epsilon, beneficiary structures, and
 *   classifications; they are linked through the network, not folded into
 *   this file. KEY AGENTS (by structural relationship): - safety_regulators:
 *   Agenda setter (institutional/constrained) — mandates the multi-mechanism
 *   stack and collects oversight authority and post-incident defensibility -
 *   training_vendors: Primary beneficiary (powerful/mobile) — sells the
 *   apparatus the mandate guarantees demand for -
 *   internal_training_departments: Secondary beneficiary
 *   (organized/identity_locked) — administers the stack; professional
 *   identity fused with its completeness - operator_organizations: Primary
 *   payer (powerful/constrained) — funds the full apparatus under certificate
 *   conditions it cannot renegotiate - frontline_operators: Payer with
 *   genuine incidental benefit (organized/constrained) — surrenders recurring
 *   duty days, receives maintained competence - human_factors_researchers:
 *   Beneficiary-analyst (organized/mobile) — careers funded by the unresolved
 *   configuration question - flying_public: Diffuse beneficiary-payer
 *   (powerless/constrained) — receives the safety margin, pays through prices
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.5).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.58).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.5).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Continuous Multi-Mechanism Competence Occupation Mandate").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "organizational/safety-regulatory").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '907bbca4-aec5-4127-bbdd-954cb04e18a0').
narrative_ontology:cs_kernel_codification('907bbca4-aec5-4127-bbdd-954cb04e18a0', formalized).
narrative_ontology:cs_authority_grounding('907bbca4-aec5-4127-bbdd-954cb04e18a0', expertise).
narrative_ontology:cs_interpretation_layer_present('907bbca4-aec5-4127-bbdd-954cb04e18a0').
narrative_ontology:cs_reading_relation('907bbca4-aec5-4127-bbdd-954cb04e18a0', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('907bbca4-aec5-4127-bbdd-954cb04e18a0', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_axiom('907bbca4-aec5-4127-bbdd-954cb04e18a0', foundational, no_single_mechanism_sufficient).
narrative_ontology:cs_axiom_status(no_single_mechanism_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('907bbca4-aec5-4127-bbdd-954cb04e18a0', no_single_mechanism_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('907bbca4-aec5-4127-bbdd-954cb04e18a0', foundational, precautionary_mechanism_breadth).
narrative_ontology:cs_axiom_status(precautionary_mechanism_breadth, holdable).
narrative_ontology:cs_axiom_grounding('907bbca4-aec5-4127-bbdd-954cb04e18a0', precautionary_mechanism_breadth, instrumental).
narrative_ontology:cs_reference_frame('907bbca4-aec5-4127-bbdd-954cb04e18a0', full_spectrum_occupation_standard).
narrative_ontology:cs_drift_state('907bbca4-aec5-4127-bbdd-954cb04e18a0', contemporary_cost_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('907bbca4-aec5-4127-bbdd-954cb04e18a0', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, internal_training_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, human_factors_researchers).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, flying_public).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operator_organizations).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, flying_public).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, skill_decay_hypothesis).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, defense_in_depth_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the certificate conditions that require operators to run recurrent training across every mechanism class — full-mission simulation, classroom and computer refreshers, procedural reinforcement drills, and line observations of real operations — and approves or rejects each operator's training program. The breadth of the mandate expands the regulator's inspection jurisdiction, its post-incident defensibility, and its standing with the public; the regulator also bears the administrative cost of reviewing and auditing the programs it requires. Stepping back from the mandate would mean ceding the safety portfolio itself, so the regulator's exit from the arrangement is effectively closed.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_regulators, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, safety_regulators, beneficiary).

% Manufactures and operates full-flight and full-scope simulators, builds courseware, and sells instructor and audit services against a demand curve guaranteed by the multi-mechanism requirement. Each additional required mechanism class opens a product line, and the absence of any agreed optimal configuration protects existing lines from consolidation into cheaper bundles. Capital and expertise redeploy readily to corporate, defense, and ed-tech training markets if the mandate ever thins.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Staffs the academies inside airlines, nuclear licensees, and hospital systems: scheduling, instructing, record-keeping, and curriculum maintenance for every mandated mechanism. Headcount and budget scale with the number of required pillars, and the department's professional self-concept is bound up with administering the complete apparatus — proposing to retire a pillar reads internally as dissolving the academy's own reason to exist, so the department defends the full stack irrespective of any particular pillar's measured yield.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, internal_training_departments, beneficiary,
    organized, biographical, identity_locked, national).

% Airlines, nuclear licensees, and hospital groups fund the entire apparatus — simulator purchase or lease hours, instructor payroll, crew and clinician downtime, documentation and audit overhead — under certificate conditions they cannot renegotiate unilaterally. Proposals to consolidate mechanisms stall for lack of admissible evidence that any configuration is safe to drop, and relocating to a lighter-touch jurisdiction forfeits market access, mutual-recognition agreements, and insurance terms.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operator_organizations, payer,
    powerful, generational, constrained, global).

% Pilots, reactor operators, and clinical staff surrender recurring duty days to the full mechanism stack and absorb the schedule disruption and fatigue around it, while drawing genuine maintenance of rarely exercised skills — especially for abnormal and emergency conditions they will mostly never meet on the line. Union representation negotiates the timing and burden of training but cannot touch the requirement itself; changing employers changes nothing because every certificated employer runs the same mandated stack, and leaving the profession forfeits accumulated seniority and licensure investment.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, frontline_operators, beneficiary).

% Studies skill decay curves, transfer of training, and mechanism interactions under a funding stream that persists precisely because the optimal configuration remains unsettled; publishes, advises regulators, and sits on the working groups that review training requirements. A settled consensus would collapse the open research agenda, though individual researchers retain mobility into applied industry and consultancy roles.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, human_factors_researchers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, human_factors_researchers, observer).

% Receives the safety margin the regime purchases — crews and operators who have recently rehearsed the failures that matter — and pays for it indirectly through fares, rates, and prices that carry the training bill. Individual members have no forum in the configuration debate and no practical alternative to the regulated services whose prices they pay.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, flying_public, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, flying_public, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, training_vendors).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains safety-critical competence against documented skill decay across heterogeneous components — handling of rare abnormal and emergency conditions, procedural fluency, knowledge retention, and real-line behavior — that no single exercise mechanism covers adequately on its own.
% TRANSFER_FUNCTION: Moves money and time: operating budget and frontline duty time flow from operator organizations and their staff to simulator and courseware vendors, to internal training bureaucracies, to regulators' oversight programs, and to the research field that studies the configuration question.
% ABSENT_VOICES: Lean-configuration advocates inside operator organizations — operations executives and training captains whose consolidation proposals die for lack of admissible evidence — and individual crew members who experience specific mechanisms as low-yield but have no standing to challenge the composite requirement. They are inside the system without a forum, because the no-consensus condition makes every 'this pillar costs more than it returns' claim unadjudicable at policy level.
% DISAPPEARANCE_RATIONALE: If the multi-mechanism mandate vanished overnight, the training market would contract sharply, regulators would lose their principal preventive oversight instrument and much of their post-incident defensibility, internal academies would shed most of their function, and competence maintenance would reorganize around whatever mechanisms individual operators chose to buy — with accident-investigation practice shifting from prevention assurance to forensic reconstruction.
% FOUNDING_PROBLEM: Post-accident investigations in aviation and nuclear operations showed that skills and knowledge for rarely exercised, safety-critical tasks decay measurably between uses, and that single-mechanism remediation after each disaster left other decay modes uncovered. The arrangement was built to occupy the competence kernel continuously, across all its components, before the next event rather than after it.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards (NTSB, BEA, ICAO annexes), the peer-reviewed human-factors literature on skill decay and transfer of training, and insurers' loss data all attest the founding problem independently of the vendors, training departments, and research programs that collect from the mandate. No corroborating source outside the benefiting parties attests that the problem is solved.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.50: the mandate's coordination half is real (decay prevention with external corroboration), but the no-consensus condition structurally guarantees a permanent surplus above any demonstrable necessity, and that surplus has identifiable collectors. Suppression is authored at 0.58 as a raw structural property — the mandate rides on certificate conditions, deviation invites enforcement action, and although no alternative is legally banned, the evidentiary standard for dropping a pillar is unreachable, which functionally closes the alternative. Suppression is NOT scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and scope. Theater ratio 0.42 reflects a real skill-maintenance core wrapped in a growing compliance-documentation shell: as the regime matured, record-keeping, checkbox completion, and audit-proofing absorbed a rising share of activity. Accessibility collapse is low (0.35) because the sibling readings keep genuine alternatives alive — understanding this constraint does not close exits, since the evidence underdetermines the configuration question. Resistance is moderate (0.45): periodic industry campaigns for simulator-credit expansion and burden relief, cost-driven lobbying, union negotiation over training time — persistent friction, no existential challenge. The claimed type (tangled_rope) is stated independently of these metrics: both a genuine coordination function and asymmetric extraction are present, and the arrangement requires active regulatory enforcement to hold. The temporal series run on one shared grid (t=0..36 in years since the modern post-accident reform wave, sampled every 6 years) with all three tracked metrics authored at every point; trajectories are monotonic — extraction, theater, and enforcement machinery all ratcheted upward as the regime professionalized — with no oscillatory cycle to document.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat (regulators) and the pure-beneficiary seat (vendors), the arrangement presents as coordination they built, administer, or supply — the mandate is the product. From the payer seats (operator organizations, frontline operators), the same structure operates as an untrimmable cost imposed under certificate threat, with the added insult that no one will tell them which parts matter. The internal-training-department seat is the sharpest divergence case: it collects budget from the mandate yet is identity-locked into defending it, so its experienced type is driven by institutional self-concept rather than by net benefit flow. The researcher seat experiences the open configuration question as opportunity rather than defect — the same indeterminacy that extracts surplus from operators funds the field. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Training vendors sit nearest the beneficiary pole (d near 0): they collect directly from every mandated hour with mobile exit and no offsetting burden. Safety regulators derive low-but-not-zero d: they collect authority and defensibility but also bear real administrative cost and cannot exit. Internal training departments derive low d from their beneficiary declaration, but their identity_locked exit means persistence does not depend on continued net benefit — they would defend the stack through a period of negative return, which is why the constraint survives cost-pressure eras. Operator organizations derive high d (full-target side): they fund everything under conditions they cannot renegotiate, with constrained exit. Frontline operators derive high d moderated by their secondary beneficiary position — genuine skill maintenance offsets part of the time extraction, but the industry-wide uniformity of the mandate leaves them no arbitrage. The flying public sits near symmetric: diffuse safety benefit, diffuse price pass-through, no forum. Human-factors researchers take a mild beneficiary tilt: they collect grants and careers from the open question while contributing the analysis that nominally serves resolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — skill decay causing accidents — is live and externally corroborated, so no mandatrophy resolution is declared and none should be inferred. The tangled-rope classification guards against two opposite errors. Reading the mandate as pure extraction would erase the documented decay-prevention function and the outside corroboration of the founding problem; reading it as pure coordination would launder the capture asymmetry that the no-consensus condition structurally guarantees — a surplus that cannot be trimmed because trimming is always unprovable, collected by parties whose revenue scales with the number of pillars. The classification keeps both halves on the table and specifies the migration paths: if a consensus configuration ever emerges and is adopted, the constraint migrates toward a standardizable coordination form (rope, or scaffold if the transition is sunsetted); if vendors and departments capture the standard-setting process outright and begin authoring the requirements they sell against, it drifts toward pure extraction. The rising theater_ratio series is the early-warning instrument for the second path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the hybrid multi-mechanism reading the correct instantiation of the competence_occupation kernel, or do the simulation_sufficiency or real_incident_necessity readings describe the binding constraint?',
    'Convergent transfer-of-training evidence: if controlled studies show simulation covers all decay-prone competence components at retention-equivalent durability, the simulation_sufficiency reading displaces this one; if line-performance data show rehearsal-free skills degrading in ways no synthetic mechanism reaches, the real_incident_necessity reading gains force.',
    'Under simulation_sufficiency the constraint collapses to a single-mechanism standard with materially lower cost and a tractable optimum, dissolving the permanent-surplus structure; under real_incident_necessity the entire preventive apparatus is recast as unable to occupy the kernel at all, and the mandate''s coordination claim fails wholesale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the competence-occupation kernel binds — multi-mechanism, simulation-only, or real-incident-only.').

omega_variable(
    optimal_configuration_discoverability,
    'Is there a discoverable optimal training configuration, or is the optimum irreducibly context-dependent, making the no-consensus condition a permanent structural feature rather than a temporary evidentiary gap?',
    'Multi-domain meta-analysis with accident and precursor endpoints, plus long-duration randomized variation in mechanism mixes across comparable operators under regulatory safe harbors.',
    'If discoverable, the mandate migrates toward a standardizable coordination form and the surplus above the optimum becomes removable; if irreducible, the untrimmable surplus is structural, the extractive component stabilizes permanently, and the tangled-rope classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_configuration_discoverability, empirical, 'Whether the no-consensus condition is temporary or structural.').

omega_variable(
    marginal_benefit_vs_capture,
    'How much of each mandated mechanism''s cost tracks demonstrated marginal safety benefit, and how much is budget capture by vendors, internal training departments, and research programs trading on the unresolved configuration?',
    'Per-mechanism cost-effectiveness audits against incident and precursor data, commissioned by parties outside the vendor-department-research triangle and with disclosure rights over vendor pricing and departmental cost structures.',
    'A high capture share would push the arrangement toward pure extraction riding a residual coordination core; a low share would support reading the surplus as the defensible price of precaution under irreducible uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginal_benefit_vs_capture, empirical, 'Split of mandate cost between demonstrated safety benefit and structural capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comp_tr_t6, competence_occupation__hybrid_occupation, theater_ratio, 6, 0.28).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__hybrid_occupation, theater_ratio, 12, 0.31).
narrative_ontology:measurement(comp_tr_t18, competence_occupation__hybrid_occupation, theater_ratio, 18, 0.34).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__hybrid_occupation, theater_ratio, 24, 0.37).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__hybrid_occupation, theater_ratio, 30, 0.4).
narrative_ontology:measurement(comp_tr_t36, competence_occupation__hybrid_occupation, theater_ratio, 36, 0.42).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comp_be_t6, competence_occupation__hybrid_occupation, base_extractiveness, 6, 0.41).
narrative_ontology:measurement(comp_be_t12, competence_occupation__hybrid_occupation, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(comp_be_t18, competence_occupation__hybrid_occupation, base_extractiveness, 18, 0.46).
narrative_ontology:measurement(comp_be_t24, competence_occupation__hybrid_occupation, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(comp_be_t30, competence_occupation__hybrid_occupation, base_extractiveness, 30, 0.49).
narrative_ontology:measurement(comp_be_t36, competence_occupation__hybrid_occupation, base_extractiveness, 36, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(comp_su_t6, competence_occupation__hybrid_occupation, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(comp_su_t12, competence_occupation__hybrid_occupation, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(comp_su_t18, competence_occupation__hybrid_occupation, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(comp_su_t24, competence_occupation__hybrid_occupation, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(comp_su_t30, competence_occupation__hybrid_occupation, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(comp_su_t36, competence_occupation__hybrid_occupation, suppression_requirement, 36, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__real_incident_necessity).

% DUAL FORMULATION NOTE:
% 'Competence occupation' is a colloquial label covering three structurally distinct claims about what occupies a safety-critical competence kernel, decomposed per the epsilon-invariance principle. This file authors the hybrid reading: a multi-mechanism continuous mandate with epsilon approximately 0.50 over the standing arrangement, beneficiaries spanning vendors, regulators, departments, researchers, and the public, and payers being operators and frontline staff. The simulation_sufficiency sibling authors a single-mechanism standard with lower cost, lower suppression, and a different (thinner) beneficiary set. The real_incident_necessity sibling authors a claim under which the entire preventive apparatus fails its purpose — a structurally different victim set and a coordination story that collapses. The upstream empirical layer (documented skill decay) is cited by all three readings as warrant; family links run through network.affects_constraints in each file, and no reading's epsilon is recoverable from another's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
