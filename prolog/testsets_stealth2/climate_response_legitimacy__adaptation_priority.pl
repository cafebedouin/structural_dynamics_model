% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Legitimacy Frame
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   International climate governance increasingly organizes around an
 *   adaptation-first legitimacy criterion: wealthy industrialized nations
 *   commit to resilience finance and protective infrastructure for exposed
 *   populations while declining binding emission reductions commensurate with
 *   historical responsibility, thereby preserving their existing development
 *   models. Low-income regions receive flows roughly an order of magnitude
 *   below independently assessed need (the ~$350B annual adaptation gap),
 *   bear the accelerating impacts of the accepted trajectory, and divert
 *   scarce fiscal space from development into self-protection. The frame
 *   presents this as realism and solidarity; its critics present it as
 *   laundering trajectory acceptance as care for the vulnerable. IMPORTANT
 *   SCOPE NOTE: this file instantiates ONLY the adaptation_priority reading
 *   of the climate_response_legitimacy kernel. The mitigation_priority and
 *   degrowth_transformation readings are separate constraints with their own
 *   epsilon values, victim sets, and classifications, linked through
 *   network.affects_constraints; their content is deliberately excluded from
 *   this story per the one-reading-one-constraint discipline. Epsilon here
 *   refers to the standing adaptation-priority arrangement as this reading's
 *   own lights assess it — never to the arrangements the rival readings would
 *   install.
 *
 * KEY AGENTS:
 *   - wealthy_industrialized_nations: agenda-setter (institutional/arbitrage) — sets finance terms and legitimacy language, preserves its development model, contributes below assessed need
 *   - low_income_regions: primary target (moderate/trapped) — bears the adaptation gap and the warming impacts of the accepted trajectory
 *   - small_island_states: existential-exposure target (moderate/trapped) — maximal stakes, minimal material leverage
 *   - frontline_vulnerable_communities: target with partial protection (powerless/trapped) — receives some resilience benefit, absorbs residual losses
 *   - future_generations: deferred-cost bearer (powerless/trapped) — inherits compounded warming, holds no seat
 *   - adaptation_infrastructure_contractors: beneficiary (powerful/mobile) — collects construction and engineering demand
 *   - climate_finance_intermediaries: beneficiary with agenda-setting power (institutional/arbitrage) — administers flows, takes overhead, shapes eligibility
 *   - climate_justice_movements: excluded objector (organized/constrained) — contests the frame from outside its decision rights
 *   - ipcc_impact_assessors: analytical observer (institutional/analytical) — documents the gap between pledged protection and physical need
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.65).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.55).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Adaptation-Priority Climate Legitimacy Frame").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '67e8b22a-6090-497d-a047-36781e14bf6c').
narrative_ontology:cs_kernel_codification('67e8b22a-6090-497d-a047-36781e14bf6c', distributed).
narrative_ontology:cs_authority_grounding('67e8b22a-6090-497d-a047-36781e14bf6c', practice).
narrative_ontology:cs_interpretation_layer_present('67e8b22a-6090-497d-a047-36781e14bf6c').
narrative_ontology:cs_reading_relation('67e8b22a-6090-497d-a047-36781e14bf6c', climate_response_legitimacy__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('67e8b22a-6090-497d-a047-36781e14bf6c', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('67e8b22a-6090-497d-a047-36781e14bf6c', foundational, adaptation_capacity_sufficient_at_accepted_warming).
narrative_ontology:cs_axiom_status(adaptation_capacity_sufficient_at_accepted_warming, holdable).
narrative_ontology:cs_axiom_grounding('67e8b22a-6090-497d-a047-36781e14bf6c', adaptation_capacity_sufficient_at_accepted_warming, empirically_contingent).
narrative_ontology:cs_axiom('67e8b22a-6090-497d-a047-36781e14bf6c', foundational, present_vulnerable_protection_has_moral_priority).
narrative_ontology:cs_axiom_status(present_vulnerable_protection_has_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('67e8b22a-6090-497d-a047-36781e14bf6c', present_vulnerable_protection_has_moral_priority, deontological).
narrative_ontology:cs_reference_frame('67e8b22a-6090-497d-a047-36781e14bf6c', trajectory_acceptance_with_protective_adaptation).
narrative_ontology:cs_drift_state('67e8b22a-6090-497d-a047-36781e14bf6c', contemporary_loss_and_damage_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('67e8b22a-6090-497d-a047-36781e14bf6c', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_industrialized_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_infrastructure_contractors).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, climate_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, small_island_states).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, frontline_vulnerable_communities).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, frontline_vulnerable_communities).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, adaptation_sufficiency_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, political_feasibility_pragmatism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominate the negotiating forums where climate finance volumes, eligibility rules, and legitimacy language are set. Under this frame they commit to resilience funding while avoiding binding emission reductions commensurate with historical responsibility, preserving their existing development and energy systems. Their finance contributions run far below independently assessed need, and they retain the option to reframe, redirect, or reduce commitments as domestic politics shift.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_industrialized_nations, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive adaptation finance flows roughly an order of magnitude below assessed need (an annual gap on the order of $350B), while bearing accelerating heat, drought, flood, and storm impacts from warming they contributed little to causing. Scarce fiscal space is diverted from schools, clinics, and industrialization into seawalls, early-warning systems, and drought-tolerant agriculture. They cannot exit the climate system, and whole-population relocation is not available; their leverage runs through negotiating blocs whose agenda power is limited.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_regions, payer,
    moderate, generational, trapped, continental).

% Live in exposed coastal, deltaic, and dryland settlements. Some receive protection from funded resilience projects; many receive none, or receive it after losses have already accumulated. When defenses fail or arrive late, they absorb the residual damage directly, and their exit route is distressed migration with attendant loss of livelihood and community.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, frontline_vulnerable_communities, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, frontline_vulnerable_communities, beneficiary).

% Face existential exposure to sea-level rise and intensifying cyclones. They organize as a vocal negotiating coalition and have won rhetorical recognition, including loss-and-damage language, but the material flows they receive remain small relative to territorial survival stakes. There is no exit: adaptation failure means uninhabitability.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, small_island_states, payer,
    moderate, generational, trapped, global).

% Inherit the compounded warming that trajectory acceptance locks in: higher seas, hotter extremes, and larger eventual stabilization burdens than a mitigation-first path would have produced. They hold no seat in any forum, receive none of the current protection spending, and cannot consent to or refuse the arrangement made on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Engineering and construction firms that design and build seawalls, resilient ports, water systems, and climate-proofed infrastructure. Their order books scale with adaptation finance volumes, and the frame's emphasis on built resilience directs demand toward them. They can move between markets and clients as funding streams shift.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_infrastructure_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Development banks and multilateral funds that administer adaptation finance: they take management overhead, shape project pipelines, define what counts as qualifying adaptation spending, and set disbursement conditions. Their programming choices determine which regions and project types access the money, giving them agenda-shaping power inside the frame they also draw income from.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_finance_intermediaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, climate_finance_intermediaries, agenda_setter).

% Transnational advocacy networks demanding loss-and-damage liability, mitigation urgency, and reparative finance. They hold protest presence and observer status at negotiating sessions but no agenda-setting vote. Their core objection is that the frame presents trajectory acceptance as solidarity with the vulnerable while shifting compounding costs onto those least able to carry them.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_justice_movements, excluded,
    organized, generational, constrained, global).

% Scientific bodies assessing climate impacts, adaptation limits, and finance needs. They document the widening distance between pledged adaptation support and assessed requirements, and identify warming levels at which adaptation options narrow sharply regardless of spending. They see the full structure: the protective function, the funding shortfall, and the physical limits of the accepted trajectory.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, ipcc_impact_assessors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, wealthy_industrialized_nations).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels resources toward protecting populations from climate impacts already locked into the system: pooled adaptation finance, shared vulnerability assessment, coordinated resilience infrastructure, and early-warning networks solve preparedness problems no individual vulnerable region can solve alone.
% TRANSFER_FUNCTION: Moves adaptation finance from wealthy-nation budgets through intermediary funds into resilience projects in exposed regions, at volumes far below assessed need; simultaneously moves the benefit of avoided economic transformation to wealthy economies and the cost of compounded warming onto low-income regions and future generations.
% ABSENT_VOICES: Mitigation-first ministries and degrowth economists sit outside this frame's legitimacy boundary and would dispute its core premise; future generations have no seat anywhere; loss-and-damage liability claimants hold observer status without agenda-setting power; frontline communities are consulted but do not set terms.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority legitimacy frame vanished overnight, the adaptation finance architecture, national adaptation plan processes, and the negotiating agenda built around resilience commitments would lose their organizing criterion; funding pipelines would stall pending re-legitimation, and the contest between the rival legitimacy criteria would reopen with different victor-dependent arrangements for who pays and who is protected.
% FOUNDING_PROBLEM: Provide a politically achievable answer to 'what must be done about climate change' at a moment when deep mitigation was blocked by wealthy-nation domestic politics: protect the already-exposed from impacts that could no longer be prevented, and call the result a legitimate response.
% FOUNDING_PROBLEM_CORROBORATION: IPCC impact assessments corroborate from outside the benefiting parties that the protective need is real, permanent, and underfunded; small island states and climate justice movements attest that the protective function is genuine but that the trajectory-acceptance component converts a protection mandate into permission for continued emissions; no party outside the benefiting set attests that trajectory acceptance itself is legitimate.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.65) because the arrangement's largest flows are negative for its targets: the finance delivered is far below assessed need, the preserved development model externalizes compounding warming costs onto those least able to absorb them, and the intergenerational bill grows with every year of deferred mitigation. Suppression (0.55) reflects the machinery required to hold the frame's legitimacy monopoly: conditionalities on finance, feasibility discourse that rules deeper mitigation out of order, and the procedural marginalization of loss-and-damage liability claims. Theater ratio (0.45) tracks the pledge-versus-disbursement gap — announcement cycles, replenishment conferences, and adaptation-goal rhetoric that routinely outrun delivered money. Accessibility collapse is low (0.35): the rival readings remain live, articulable alternatives that have not collapsed on contact with the frame. Resistance (0.6) is real and sustained: island-state coalitions, justice movements, and G77 finance demands continuously contest the arrangement. The temporal series run on one shared seven-point grid (all three metrics authored at every point) so no metric's end-state is silently substituted into earlier rows. The suppression_requirement series is authored deliberately: the story traces the maturation of legitimacy-enforcement capacity over the interval (hardening conditionalities, expanding feasibility policing), not merely a static suppression level, which alone would be carried by base_properties.suppression.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from identical facts. From the agenda-setter seat, the frame is responsible statecraft: protection for the exposed, feasible politics, finance honestly appropriated. From the trapped payer seats, the same structure is underfunded protection wrapped around permission to keep emitting — help that arrives late, in amounts an order of magnitude short, priced in compounded warming. From the contractor and intermediary seats it is a growing market and a durable administrative franchise. The engine derives these divergent per-seat classifications from the structural data (roles, power, exit options); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy industrialized nations derive a directionality near the beneficiary pole: they collect the frame's largest gain (preserved development model) and pay only the finance outlay they themselves set. Contractors and intermediaries sit similarly near the beneficiary pole with mobile or arbitrage exit. Low-income regions, small island states, and frontline communities derive directionality near the full-target pole, amplified by trapped exit — no arbitrage from the climate system exists — and by the continental-to-global scope at which verification of delivered protection is hardest. Future generations occupy the extreme: full-target directionality with permanently zero exit and zero agenda power. Frontline communities carry a secondary beneficiary position (some received protection) that moderates but does not reverse their net-target derivation. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the true structural relationships, so the derivation chain stands ungated.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what keeps both misreadings out. Reading the arrangement as pure extraction (snare) erases the genuine coordination function: pooled adaptation finance, shared vulnerability science, and coordinated resilience infrastructure solve real collective-action problems, and the protection actually delivered saves lives now. Reading it as pure coordination (rope) excuses the structural asymmetries: the $350B gap, the below-need contributions, and the intergenerational cost-deferral that compounds precisely because the frame legitimizes trajectory acceptance. The founding problem — protecting the already-exposed — is live and permanent, but the trajectory-acceptance component of the founding settlement is contested by parties outside the benefiting set; the R5 mismatch surface (contested status x world_rearranges verdict) flags exactly this partial obsolescence without collapsing the whole arrangement into a zombie designation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which legitimacy criterion should govern climate response — trajectory acceptance with protective adaptation (this reading), growth-preserving decoupling (mitigation_priority), or growth-dismantling transformation (degrowth_transformation) — and is the choice resolvable or irreducibly political?',
    'Not resolvable by data alone: the readings share the physical evidence and diverge on the normative weighting of present protection, future stabilization, and present consumption. Partial resolution tracks which criterion bargaining coalitions converge on in successive negotiation rounds.',
    'Switching readings relocates the victim set wholesale: this reading places low-income regions in it immediately via the adaptation gap; mitigation_priority defers victimhood to future generations; degrowth_transformation places wealthy-nation consumers in it now. Classification of the whole family moves with the selection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer-frame omega: this constraint is one of three rival readings of the climate_response_legitimacy kernel; the selection among them is a conceptual, not empirical, resolution.').

omega_variable(
    adaptation_adequacy_at_accepted_warming,
    'Can resilience infrastructure and adaptive capacity actually protect exposed populations adequately at the warming levels this reading accepts, and is the ~$350B annual finance gap closable within the frame?',
    'Compare delivered adaptation outcomes against IPCC-assessed residual damage at projected warming levels; track whether closed finance gaps reduce residual losses proportionally or reveal hard adaptation limits (sea-level rise, wet-bulb extremes) that spending cannot buy back.',
    'If adaptation saturates well below the accepted trajectory''s impacts, the frame''s foundational adequacy premise fails empirically and the arrangement''s legitimacy claim collapses toward pure cover; if the gap is closable and effective, a larger share of measured extraction is the price of real protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_adequacy_at_accepted_warming, empirical, 'Whether the protective promise of the frame is physically deliverable at the trajectory it accepts.').

omega_variable(
    intergenerational_cost_deferral_magnitude,
    'How much of the frame''s apparent present-day beneficence consists of costs shifted onto future generations through deferred mitigation, and under what discount-rate assumptions does the deferral look benign versus ruinous?',
    'Integrated assessment comparison of cumulative stabilization burdens and residual damages between the accepted trajectory and counterfactual mitigation-first paths, with explicit discount-rate sensitivity analysis.',
    'A large deferral component reweights the victim set toward future generations and raises effective extraction on the powerless/trapped pole; a small component supports the reading''s claim that present protection dominates the ethical ledger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_cost_deferral_magnitude, empirical, 'Size and ethical weight of the intergenerational transfer embedded in trajectory acceptance.').

omega_variable(
    net_position_of_low_income_regions,
    'Do low-income regions come out net ahead or net behind under the arrangement once adaptation inflows are counted against uncompensated warming damages, diverted fiscal space, and forgone development?',
    'Country-level accounting joining adaptation finance receipts, attributed climate damages, and counterfactual development paths; natural experiments where finance arrived at scale versus where it did not.',
    'Net-behind confirms their full-target directionality despite nominal beneficiary framing; net-ahead would force reclassification of their seat and soften the asymmetry finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_position_of_low_income_regions, empirical, 'Whether the nominal protection beneficiaries of the frame are in fact its net payers.').

omega_variable(
    suppression_mechanism_crowding_out,
    'Is the crowding-out of mitigation-first and degrowth alternatives structural (finance architecture, procedural rules, conditionalities) or internalized (feasibility discourse that makes deeper ambition unthinkable within negotiating cultures)?',
    'Post-frame-collapse thought experiment and comparative cases: where procedural barriers dropped (rump sessions, coalition breakaways), did mitigation ambition return quickly (structural) or stay suppressed (internalized)?',
    'If internalized, the frame''s suppressive force persists beyond its institutional shell and effective suppression exceeds the structural measure; if structural, removing the architecture restores the rival readings'' competitiveness immediately.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_crowding_out, conceptual, 'Structural versus internalized mechanism behind the frame''s suppression of alternative legitimacy criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t4, climate_response_legitimacy__adaptation_priority, theater_ratio, 4, 0.28).
narrative_ontology:measurement(clim_tr_t8, climate_response_legitimacy__adaptation_priority, theater_ratio, 8, 0.32).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__adaptation_priority, theater_ratio, 12, 0.36).
narrative_ontology:measurement(clim_tr_t16, climate_response_legitimacy__adaptation_priority, theater_ratio, 16, 0.4).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__adaptation_priority, theater_ratio, 20, 0.43).
narrative_ontology:measurement(clim_tr_t24, climate_response_legitimacy__adaptation_priority, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(clim_be_t4, climate_response_legitimacy__adaptation_priority, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(clim_be_t8, climate_response_legitimacy__adaptation_priority, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__adaptation_priority, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(clim_be_t16, climate_response_legitimacy__adaptation_priority, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(clim_be_t24, climate_response_legitimacy__adaptation_priority, base_extractiveness, 24, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t4, climate_response_legitimacy__adaptation_priority, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(clim_su_t8, climate_response_legitimacy__adaptation_priority, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(clim_su_t12, climate_response_legitimacy__adaptation_priority, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(clim_su_t16, climate_response_legitimacy__adaptation_priority, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(clim_su_t24, climate_response_legitimacy__adaptation_priority, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate climate response' decomposes into three structurally distinct constraints — adaptation_priority (this file), mitigation_priority, and degrowth_transformation — because the label conflates rival legitimacy criteria with different epsilon values and different victim sets. Per the epsilon-invariance principle, measuring 'legitimacy' by this reading's lights yields high extraction concentrated on low-income regions and future generations; the same label measured by the degrowth reading's lights yields extraction concentrated on wealthy-nation consumption. One story per reading, linked here. Upstream/downstream structure: this reading's adoption raises the mitigation burden the mitigation_priority reading must later discharge (every year of trajectory acceptance compounds the sibling's task), which is why the reading_relations edge to mitigation_priority is influences rather than coexists_with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
