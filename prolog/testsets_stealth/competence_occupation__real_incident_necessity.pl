% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real-Incident Necessity Doctrine for Competence Kernel Occupation
 *   domain: organizational/safety-science
 *
 * SUMMARY:
 *   Across high-hazard industries — nuclear power, commercial aviation,
 *   offshore drilling, firefighting, emergency medicine — a governing
 *   doctrine holds that the competence needed to handle catastrophic events
 *   can be authentically occupied only through actual catastrophic incidents.
 *   This story instantiates the real_incident_necessity reading of the
 *   competence_occupation kernel; the simulation_sufficiency and
 *   hybrid_occupation readings are separate constraints with their own files.
 *   Under this reading, rare real events become the only authoritative
 *   observable for crisis readiness: simulation investment is discounted,
 *   verification of readiness is deferred to the next catastrophe, and
 *   epistemic authority accrues to those who were present at past ones. The
 *   claim presents itself as a natural law of experiential learning ('you
 *   cannot train for the real thing'), and its epistemic core — fidelity
 *   limits, startle and stress effects — is genuinely corroborated outside
 *   the beneficiary set; but its operation also suppresses a live
 *   alternative, transfers risk to parties with no seat, and pays standing
 *   rents to an incumbent class. Claim and metrics are authored
 *   independently: claimed_type records the structure I judge true
 *   (tangled_rope — a real coordination core with asymmetric extraction
 *   riding on it); the metrics record what I judge descriptively accurate;
 *   the engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - incident_veteran_operators: primary beneficiary (organized/identity_locked) — holds instructional and investigative authority premised on having been present at real catastrophes
 *   - accident_investigation_community: secondary beneficiary (institutional/constrained) — produces the doctrine's privileged knowledge product
 *   - hro_frontline_workforce: primary target (organized/constrained) — bears unverifiable-preparation risk and receives doctrine-discounted training
 *   - fenceline_public: primary target (powerless/trapped) — bears catastrophe tail risk with no seat in training policy
 *   - safety_standards_bodies: agenda setter (institutional/constrained) — codifies what counts toward crisis qualification
 *   - hro_executive_management: dual-positioned (powerful/mobile) — banks training-budget savings between catastrophes, absorbs tail liability
 *   - simulation_training_specialists: excluded alternative (moderate/constrained) — their sufficiency claim is the doctrine's suppression object
 *   - organizational_scholars: analytical observer (analytical/analytical) — maps the contest without material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.62).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.68).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, tangled_rope).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident Necessity Doctrine for Competence Kernel Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety-science").

domain_priors:requires_active_enforcement(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '8fde6887-bb5c-416a-b2d5-baaa17ade7d3').
narrative_ontology:cs_kernel_codification('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', distributed).
narrative_ontology:cs_authority_grounding('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', practice).
narrative_ontology:cs_interpretation_layer_present('8fde6887-bb5c-416a-b2d5-baaa17ade7d3').
narrative_ontology:cs_reading_relation('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', competence_occupation__hybrid_occupation, coexists_with).
narrative_ontology:cs_axiom('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', foundational, authentic_catastrophe_conditions_irreducible).
narrative_ontology:cs_axiom_status(authentic_catastrophe_conditions_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', authentic_catastrophe_conditions_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', foundational, real_event_epistemic_primacy).
narrative_ontology:cs_axiom_status(real_event_epistemic_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', real_event_epistemic_primacy, conventional).
narrative_ontology:cs_reference_frame('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', real_event_gold_standard).
narrative_ontology:cs_drift_state('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', contemporary_post_fidelity_advances, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8fde6887-bb5c-416a-b2d5-baaa17ade7d3', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, incident_veteran_operators).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, accident_investigation_community).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, hro_frontline_workforce).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, fenceline_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, hro_executive_management).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, hro_executive_management).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, drill_reality_gap_observation).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, experiential_authenticity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior operators, pilots, engineers, and incident commanders who were present at major accidents. They serve as instructors, investigation witnesses, and committee members, and their accounts of real events carry a weight in hiring, promotion, and curriculum decisions that no simulated-hour record matches. Their standing is inseparable from the conviction that such experience cannot be replaced; they do not seek new disasters, but each new disaster adds a small cohort of peers and refreshes the premise of their authority. Recanting the conviction would dissolve the basis of their professional identity.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_veteran_operators, beneficiary,
    organized, biographical, identity_locked, global).

% Accident boards, regulatory investigators, and root-cause analysts. Each catastrophe generates their core product — findings, recommendations, and the authoritative narrative of what went wrong. The conviction that real events are the only authentic teacher keeps their output primary over laboratory and simulation evidence, and guarantees renewed demand for their expertise after every disaster. Much of the world's distributed knowledge about failure genuinely resides with them, which makes their position difficult to disentangle from the privilege it confers.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, accident_investigation_community, beneficiary,
    institutional, generational, constrained, global).

% Control-room crews, flight decks, drilling teams, fireground crews, and trauma teams operating hazardous systems. They spend thousands of hours in simulators and drills whose adequacy the doctrine officially discounts, then face real emergencies whose stresses the drills are said not to reproduce. They carry the injury exposure of both under-tested preparation and the rare real events from which the doctrine says competence must come. Leaving the industry forfeits accumulated career investment; staying means living with the gap.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, hro_frontline_workforce, payer,
    organized, biographical, constrained, global).

% Residents near nuclear stations, chemical complexes, refineries, and flight paths. They have no role in setting training policy, yet bear the tail risk that crystallizes when crisis competence turns out not to hold. Their exposure spans generations; moving away from a facility-linked economy is costly, and the relevant decisions are made in forums they do not attend.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, fenceline_public, payer,
    powerless, generational, trapped, regional).

% Nuclear regulators, aviation authorities, fire-service accrediting bodies, and professional boards. They write the hour requirements, accredit the simulators, and decide what counts toward crisis-command qualification. Current frameworks weight documented real-event experience above simulated hours for the most consequential roles. Re-weighting exposes them to blame if the next disaster touches a crew trained the new way, so revisions move slowly and trail the evidence.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_standards_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Executives and site directors who allocate training budgets. The doctrine gives them an evidentiary rationale to defer expensive simulation infrastructure — the argument that the real thing cannot be trained for — and they bank the savings between disasters. When a catastrophe arrives they absorb litigation, regulatory penalty, and reputational loss, and can depart for other firms before the tail risk matures.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, hro_executive_management, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, hro_executive_management, beneficiary).

% Simulator builders, scenario designers, and training scientists. They produce transfer-effectiveness studies and steadily improving fidelity, and they are consulted after every disaster. In steady state their budget requests are discounted on the ground that simulation cannot reproduce authentic conditions — the precise proposition their work challenges. Their access is episodic: invited after failures, marginal between them.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_training_specialists, excluded,
    moderate, biographical, constrained, global).

% Researchers of high-reliability organizing and safety science. They document the drill-reality gap, map the dispute over what closes it, and hold no material position in credentialing, budgeting, or operations. Their analyses circulate among the other seats without commanding any of them.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, organizational_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, incident_veteran_operators).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates expectations about which experiences confer crisis competence in high-hazard operations: it concentrates credentialing authority in incident-experienced personnel, gives standards bodies a common (if contested) criterion for qualifying crisis leaders, and directs investigative and memorial resources toward real events as the authoritative record of failure.
% TRANSFER_FUNCTION: Moves epistemic authority, career advancement, and instructional roles toward incident-experienced veterans and away from simulation-trained personnel; moves the cost of verifying crisis competence out of training budgets and into operational risk, where the verification debt is paid — when it is paid at all — by whoever is present at the next catastrophe.
% ABSENT_VOICES: Simulation and training scientists whose transfer-effectiveness evidence is discounted before evaluation; fenceline residents and passengers who bear the tail risk and hold no seat in training-policy deliberation; junior operators whose advancement depends on credentials the doctrine devalues. They are outside the investigation hearings, standards committees, and post-incident reviews where the doctrine is reaffirmed.
% DISAPPEARANCE_RATIONALE: Credentialing systems would re-weight simulated and scenario-based evidence within a review cycle; simulation infrastructure investment would surge; veteran instructional authority would erode to ordinary seniority; investigation findings would lose their privileged status over laboratory evidence. The underlying drill-reality gap would persist as a research problem, but the governing arrangement — who qualifies, on what evidence, at whose risk — would reorganize.
% FOUNDING_PROBLEM: After Three Mile Island and similar events, drill-proficient crews failed in real emergencies in ways their simulator performance did not predict; the doctrine was articulated to explain that gap and to prescribe where authentic crisis competence comes from: the real event itself.
% FOUNDING_PROBLEM_CORROBORATION: Accident-investigation reports (Three Mile Island, Challenger, Columbia, Macondo) and peer-reviewed human-factors literature corroborate the founding observation — drill performance systematically overpredicts real-event performance under startle, ambiguity, and degraded instrumentation — from outside the beneficiary set. The further inference that only real incidents occupy the kernel is attested mainly by incident veterans and portions of the investigation community, while simulation researchers publish transfer-effectiveness studies disputing it; no disinterested body currently certifies the strong claim.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the doctrine diverts resources from the cheapest available competence-maintenance path, transfers verification cost into operational risk borne by workforce and public, and discounts disconfirming evidence; it falls short of predatory levels because its epistemic core is real — drill performance does systematically overpredict real-event performance, and part of the doctrine's binding force tracks that fact. Suppression 0.68 (raw, unscaled structural property): the sufficiency of simulation is not merely doubted but institutionally discounted — simulator hours are weighted down in crisis-command qualification, transfer studies meet a-priori dismissal, and post-disaster simulation budgets revert once attention fades. Theater 0.30: post-incident lessons-learned rituals and nominally satisfied experience credits are real but secondary; the doctrine's enforcement runs through genuine credentialing decisions. Accessibility_collapse 0.60: inside doctrine-bound organizations the simulation-sufficiency option collapses from serious consideration, while hybrid configurations and improved-fidelity programs remain reachable at the margins. Resistance 0.58: sustained — transfer-effectiveness research, regulator movement toward scenario-based mandates, and the recurring post-disaster surge in simulation investment. Fixing is prohibitive: the decisive evidence for or against the doctrine requires the very catastrophe class it governs, credentialing systems would need wholesale reconstruction, and the identity-fused gatekeeping class controls the revision path. The temporal series share one nine-point grid (1979-2026) with all three metrics authored at every point. The trajectories oscillate on a disaster-ratchet cycle: extractiveness, theater, and enforcement intensity spike after Challenger (1986), Columbia (2003), and Macondo (2010), then partially decay. The oscillation is not noise — intermittent reinforcement is part of the retention mechanism: each catastrophe re-vindicates the doctrine, resets suppression, and adds a fresh veteran cohort, so the cycle itself helps the arrangement persist.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the veteran seat the doctrine is earned wisdom: the difference between having drilled a loss-of-coolant accident and having sat through one is the whole of crisis judgment, and proposals to substitute simulation read as credential inflation by the untested. From the simulation-specialist seat the same doctrine is unfalsifiable gatekeeping: any evidence of transfer is ruled inadmissible in advance, which is the signature of a boundary being maintained rather than a fact being tested. From the fenceline seat it is roulette conducted with their lives by institutions that cannot verify the competence they sell. From the executive seat it is two things at once — a budget rationale and a tail liability. The engine derives these per-seat classifications from power, exit, and directional position; the divergence, not any single seat's verdict, is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries: incident_veteran_operators (standing rents; identity-locked to the doctrine that pays them — they sit far toward the subsidized end despite locked exit, because the lock binds them to defense of the doctrine, not to exposure from it) and accident_investigation_community (primacy of their knowledge product; constrained exit into adjacent expertise markets). Declared victims: hro_frontline_workforce (pays in discounted preparation and residual exposure; organized but constrained) and fenceline_public (pays the tail risk outright; powerless, trapped, generational horizon — nearest the full-target end). Safety_standards_bodies administer the arrangement and sit mid-scale: they neither collect the standing rents nor bear the tail risk, but they own the revision decision and the blame exposure that discourages exercising it. hro_executive_management nets slightly toward the target side: continuous budget relief against rare catastrophic loss, with mobile exit that blunts even that exposure. simulation_training_specialists are the suppressed alternative rather than a paying seat — their exclusion is the enforcement object itself. Organizational scholars hold the analytical seat with no directional stake. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining why drill-proficient crews fail in real events — is contested but live: the drill-reality gap is corroborated outside the beneficiary set, while the strong inference drawn from it is disputed by the sibling readings. Classifying this as tangled_rope prevents two opposite errors. Reading it as a pure snare would erase the genuine epistemic core that keeps honest practitioners inside the doctrine; reading it as a mountain would launder a contested, enforcement-dependent, rent-paying arrangement as natural law. The R5 mismatch check returns no zombie flag: founding_problem_status=contested with disappearance_verdict=world_rearranges — the arrangement persists because a live, unresolved problem continues to generate conviction, not because a dead mandate is being theatrically maintained. If the sibling readings win — if hybrid configurations demonstrate kernel occupation at acceptable fidelity — the extraction half of this rope unravels first, and the doctrine survives only as one input among several.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_resolution,
    'Does authentic occupation of the crisis-competence kernel strictly require real catastrophic incidents (this reading), or do simulation-based or hybrid multi-mechanism regimens suffice (the sibling readings of the competence_occupation kernel)?',
    'Longitudinal transfer-effectiveness studies comparing real-event performance of simulation-intensive versus real-experience-intensive cohorts, pooled across nuclear, aviation, offshore, and emergency-medicine domains.',
    'If simulation_sufficiency wins, this constraint''s suppression of simulation investment was unwarranted and its extraction collapses; if hybrid_occupation wins, the exclusive claim fails but a residual authenticity premium survives; if this reading wins, the competence-maintenance problem is formally unresolvable at acceptable cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, empirical, 'Three-way contest over the competence_occupation kernel; this story is one reading of it.').

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the irreducibility claim a genuine structural feature of skill acquisition under extreme conditions, or a constructed doctrine whose persistence serves identifiable incumbents?',
    'Adversarial collaboration between simulation researchers and veteran practitioners; fidelity-frontier testing in which measured simulator-fidelity improvements are checked against closed-loop performance gaps on surrogate catastrophic scenarios.',
    'If constructed, the constraint reclassifies toward stronger extraction with clearer capture dynamics and the declared beneficiaries become structurally central; if genuine, the constraint approaches an epistemic limit and the beneficiaries are incidental collectors on an unresolvable problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether the doctrine is natural limit or defended construct.').

omega_variable(
    identity_lock_vs_evidence_gatekeeping,
    'Is the doctrine''s persistence driven primarily by genuine evidential caution about simulation fidelity, or by identity-protective cognition among incident veterans reinforced by credential gatekeeping?',
    'Blinded assessment protocols in which veteran and non-veteran evaluators rate identical transfer-effectiveness evidence; pre/post-retirement cohort tracking of doctrine endorsement.',
    'If persistence is substantially internalized, suppression outlives any structural barrier removed by policy and reform must address professional identity, not just standards text; if structural, standards revision alone would unwind the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_evidence_gatekeeping, empirical, 'Structural versus internalized mechanism sustaining the doctrine.').

omega_variable(
    beneficiary_structure_viability,
    'Do the standing rents accruing to incident veterans constitute viable capture of the arrangement, or are they epiphenomenal — authority that would persist under any resolution of the kernel contest?',
    'Counterfactual credentialing analysis under simulation_sufficiency governance: would documented real-event experience retain wage, promotion, and instructional premiums if simulated hours were weighted equally?',
    'If rents are epiphenomenal, the receipt surface shifts toward diffuse and the constraint reads as a genuine epistemic trap with no capturer; if durable, capture dynamics strengthen and the arrangement is more extractive than its epistemic core alone would imply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_viability, empirical, 'Whether the authority rents are structural capture or incidental.').

omega_variable(
    rare_event_observability_trap,
    'Can the kernel contest ever resolve at acceptable cost, given that decisive evidence for or against this reading requires observations from the catastrophe class itself?',
    'Validation of surrogate endpoints — physiological stress markers, induced startle response, time-pressure degradation curves measured in high-fidelity simulation — against historical incident-cohort performance data.',
    'Determines whether the disagreement is permanently live (perpetuating tangled-rope dynamics with periodic disaster-ratchet reinforcement) or resolvable through validated surrogates, which would allow the sibling readings to be tested without waiting for catastrophes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rare_event_observability_trap, empirical, 'Verification circularity: the observable is the disaster.').

omega_variable(
    learning_from_catastrophe_ethics,
    'If this reading is true, societies face a direct value conflict between authentic competence occupation and catastrophe prevention — is tolerating catastrophe risk as the price of authentic learning permissible?',
    'None by data; resolved only by explicit normative choice among risk-ethics frameworks (tolerable-risk doctrines, precautionary principles, tombstone-regulation practice).',
    'Shapes whether the constraint is treated as tragic necessity to be managed (approaching mountain-like acceptance) or as intolerable and overridden by mandated hybrid investment regardless of demonstrated sufficiency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(learning_from_catastrophe_ethics, preference, 'Normative residue: no empirical result settles the permissibility question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 1979, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_occ_rin_tr_t1979, competence_occupation__real_incident_necessity, theater_ratio, 1979, 0.2).
narrative_ontology:measurement_basis(comp_occ_rin_tr_t1979, observed).
narrative_ontology:measurement(comp_occ_rin_tr_t1986, competence_occupation__real_incident_necessity, theater_ratio, 1986, 0.28).
narrative_ontology:measurement_basis(comp_occ_rin_tr_t1986, observed).
narrative_ontology:measurement(comp_occ_rin_tr_t1988, competence_occupation__real_incident_necessity, theater_ratio, 1988, 0.24).
narrative_ontology:measurement_basis(comp_occ_rin_tr_t1988, observed).
narrative_ontology:measurement(comp_occ_rin_tr_t1996, competence_occupation__real_incident_necessity, theater_ratio, 1996, 0.22).
narrative_ontology:measurement_basis(comp_occ_rin_tr_t1996, observed).
narrative_ontology:measurement(comp_occ_rin_tr_t2003, competence_occupation__real_incident_necessity, theater_ratio, 2003, 0.32).
narrative_ontology:measurement_basis(comp_occ_rin_tr_t2003, observed).
narrative_ontology:measurement(comp_occ_rin_tr_t2005, competence_occupation__real_incident_necessity, theater_ratio, 2005, 0.28).
narrative_ontology:measurement_basis(comp_occ_rin_tr_t2005, observed).
narrative_ontology:measurement(comp_occ_rin_tr_t2010, competence_occupation__real_incident_necessity, theater_ratio, 2010, 0.34).
narrative_ontology:measurement_basis(comp_occ_rin_tr_t2010, observed).
narrative_ontology:measurement(comp_occ_rin_tr_t2018, competence_occupation__real_incident_necessity, theater_ratio, 2018, 0.31).
narrative_ontology:measurement_basis(comp_occ_rin_tr_t2018, observed).
narrative_ontology:measurement(comp_occ_rin_tr_t2026, competence_occupation__real_incident_necessity, theater_ratio, 2026, 0.3).
narrative_ontology:measurement_basis(comp_occ_rin_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(comp_occ_rin_be_t1979, competence_occupation__real_incident_necessity, base_extractiveness, 1979, 0.5).
narrative_ontology:measurement_basis(comp_occ_rin_be_t1979, observed).
narrative_ontology:measurement(comp_occ_rin_be_t1986, competence_occupation__real_incident_necessity, base_extractiveness, 1986, 0.56).
narrative_ontology:measurement_basis(comp_occ_rin_be_t1986, observed).
narrative_ontology:measurement(comp_occ_rin_be_t1988, competence_occupation__real_incident_necessity, base_extractiveness, 1988, 0.54).
narrative_ontology:measurement_basis(comp_occ_rin_be_t1988, observed).
narrative_ontology:measurement(comp_occ_rin_be_t1996, competence_occupation__real_incident_necessity, base_extractiveness, 1996, 0.52).
narrative_ontology:measurement_basis(comp_occ_rin_be_t1996, observed).
narrative_ontology:measurement(comp_occ_rin_be_t2003, competence_occupation__real_incident_necessity, base_extractiveness, 2003, 0.6).
narrative_ontology:measurement_basis(comp_occ_rin_be_t2003, observed).
narrative_ontology:measurement(comp_occ_rin_be_t2005, competence_occupation__real_incident_necessity, base_extractiveness, 2005, 0.57).
narrative_ontology:measurement_basis(comp_occ_rin_be_t2005, observed).
narrative_ontology:measurement(comp_occ_rin_be_t2010, competence_occupation__real_incident_necessity, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement_basis(comp_occ_rin_be_t2010, observed).
narrative_ontology:measurement(comp_occ_rin_be_t2018, competence_occupation__real_incident_necessity, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement_basis(comp_occ_rin_be_t2018, observed).
narrative_ontology:measurement(comp_occ_rin_be_t2026, competence_occupation__real_incident_necessity, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(comp_occ_rin_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_occ_rin_su_t1979, competence_occupation__real_incident_necessity, suppression_requirement, 1979, 0.55).
narrative_ontology:measurement_basis(comp_occ_rin_su_t1979, observed).
narrative_ontology:measurement(comp_occ_rin_su_t1986, competence_occupation__real_incident_necessity, suppression_requirement, 1986, 0.66).
narrative_ontology:measurement_basis(comp_occ_rin_su_t1986, observed).
narrative_ontology:measurement(comp_occ_rin_su_t1988, competence_occupation__real_incident_necessity, suppression_requirement, 1988, 0.62).
narrative_ontology:measurement_basis(comp_occ_rin_su_t1988, observed).
narrative_ontology:measurement(comp_occ_rin_su_t1996, competence_occupation__real_incident_necessity, suppression_requirement, 1996, 0.58).
narrative_ontology:measurement_basis(comp_occ_rin_su_t1996, observed).
narrative_ontology:measurement(comp_occ_rin_su_t2003, competence_occupation__real_incident_necessity, suppression_requirement, 2003, 0.7).
narrative_ontology:measurement_basis(comp_occ_rin_su_t2003, observed).
narrative_ontology:measurement(comp_occ_rin_su_t2005, competence_occupation__real_incident_necessity, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(comp_occ_rin_su_t2005, observed).
narrative_ontology:measurement(comp_occ_rin_su_t2010, competence_occupation__real_incident_necessity, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement_basis(comp_occ_rin_su_t2010, observed).
narrative_ontology:measurement(comp_occ_rin_su_t2018, competence_occupation__real_incident_necessity, suppression_requirement, 2018, 0.67).
narrative_ontology:measurement_basis(comp_occ_rin_su_t2018, observed).
narrative_ontology:measurement(comp_occ_rin_su_t2026, competence_occupation__real_incident_necessity, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement_basis(comp_occ_rin_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, identity_coordination).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% Constraint family: competence_occupation decomposes into three readings because the colloquial label 'competence maintenance' conflates structurally distinct claims with different epsilon referents (epsilon-invariance principle; BGS decomposition pattern). This member (real_incident_necessity) is the contested downstream claim: careers and standing ride on it, alternatives are suppressed, and its verification requires the catastrophe class itself — structurally analogous to bgs_eigenvector_thermalization in the worked family. Sibling stories: competence_occupation__simulation_sufficiency and competence_occupation__hybrid_occupation. The shared upstream observation (the drill-reality gap) feeds all three readings; edges here point to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
