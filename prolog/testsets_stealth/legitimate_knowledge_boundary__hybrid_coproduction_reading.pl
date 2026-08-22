% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Dual-Validation Co-Production Boundary on Legitimate Knowledge
 *   domain: epistemological/political
 *
 * SUMMARY:
 *   A boundary regime governing what counts as legitimate knowledge requires
 *   every knowledge claim to clear two gates: methodological rigor assessed
 *   by established inquiry standards, and experiential validity certified
 *   through structured co-production with people who live the conditions
 *   studied. Neither standard alone suffices; projects must invest in
 *   facilitated engagement infrastructure to pass. The arrangement solves a
 *   real integration problem — inquiry and lived experience genuinely need
 *   each other for reliable, usable, trusted knowledge — while routing
 *   validation authority, funding eligibility, and certification labor
 *   through a facilitation layer whose procedural standards both
 *   constituencies must accept on the facilitators' terms. Over the interval,
 *   compliance with the dual standard hardened from recommended practice into
 *   a funding precondition, and a growing share of co-production activity
 *   became documentation of participation rather than exercise of shared
 *   authority. KEY AGENTS (by structural relationship): -
 *   coproduction_facilitation_bodies: Agenda-setter (institutional/mobile) —
 *   designs protocols, certifies dual compliance, collects facilitation
 *   revenue - participatory_research_funders: Beneficiary
 *   (institutional/arbitrage) — conditions funding on dual validation, gains
 *   aligned portfolios - resourced_community_partners: Beneficiary-payer
 *   (organized/constrained) — holds seats and gains standing, pays
 *   professionalization costs - conventional_academic_researchers: Primary
 *   target (powerful/constrained) — bears engagement labor and lost agenda
 *   control - unresourced_grassroots_knowledge_holders: Primary target
 *   (powerless/trapped) — experience lacks standing without participation
 *   they cannot afford - independent_scholars_citizen_scientists: Excluded
 *   target (moderate/mobile) — fails both standards by construction, no seat
 *   in administration - science_policy_evaluators: Analytical observer
 *   (institutional/analytical) — assesses whether the dual standard
 *   outperforms single standards
 *
 * KEY AGENTS:
 *   - coproduction_facilitation_bodies: Agenda-setter (institutional/mobile) — designs co-production protocols, certifies dual compliance, administers the process, collects facilitation fees and overhead
 *   - participatory_research_funders: Beneficiary (institutional/arbitrage) — grant-makers conditioning awards on documented co-production; gain portfolios aligned with policy priorities and defensible legitimacy narratives
 *   - resourced_community_partners: Beneficiary with payer costs (organized/constrained) — well-staffed community organizations holding formal seats; gain funding and standing, pay professionalization and autonomy costs
 *   - conventional_academic_researchers: Primary target (powerful/constrained) — investigators who must add engagement labor and share agenda control to keep grants and publications
 *   - unresourced_grassroots_knowledge_holders: Primary target (powerless/trapped) — people with direct lived experience whose knowledge gains standing only through processes they cannot resource
 *   - independent_scholars_citizen_scientists: Excluded target (moderate/mobile) — knowledge producers outside both credential and organized-community channels; fail both standards by construction
 *   - science_policy_evaluators: Analytical observer (institutional/analytical) — evaluation offices assessing whether dual-validated knowledge outperforms single-standard knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.52).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Dual-Validation Co-Production Boundary on Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemological/political").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, '74516b7d-2887-45b4-9f4d-f929ae2b2de4').
narrative_ontology:cs_kernel_codification('74516b7d-2887-45b4-9f4d-f929ae2b2de4', distributed).
narrative_ontology:cs_authority_grounding('74516b7d-2887-45b4-9f4d-f929ae2b2de4', practice).
narrative_ontology:cs_interpretation_layer_present('74516b7d-2887-45b4-9f4d-f929ae2b2de4').
narrative_ontology:cs_reading_relation('74516b7d-2887-45b4-9f4d-f929ae2b2de4', legitimate_knowledge_boundary__credentialed_expertise_reading, forecloses).
narrative_ontology:cs_reading_relation('74516b7d-2887-45b4-9f4d-f929ae2b2de4', legitimate_knowledge_boundary__experiential_pluralism_reading, forecloses).
narrative_ontology:cs_axiom('74516b7d-2887-45b4-9f4d-f929ae2b2de4', foundational, dual_validation_jointly_necessary).
narrative_ontology:cs_axiom_status(dual_validation_jointly_necessary, holdable).
narrative_ontology:cs_axiom_grounding('74516b7d-2887-45b4-9f4d-f929ae2b2de4', dual_validation_jointly_necessary, instrumental).
narrative_ontology:cs_axiom('74516b7d-2887-45b4-9f4d-f929ae2b2de4', secondary, shared_validation_authority).
narrative_ontology:cs_axiom_status(shared_validation_authority, holdable).
narrative_ontology:cs_axiom_grounding('74516b7d-2887-45b4-9f4d-f929ae2b2de4', shared_validation_authority, deontological).
narrative_ontology:cs_reference_frame('74516b7d-2887-45b4-9f4d-f929ae2b2de4', dual_validation_coproduction_standard).
narrative_ontology:cs_drift_state('74516b7d-2887-45b4-9f4d-f929ae2b2de4', contemporary_participation_audit_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('74516b7d-2887-45b4-9f4d-f929ae2b2de4', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_facilitation_bodies).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, participatory_research_funders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, resourced_community_partners).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, conventional_academic_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, unresourced_grassroots_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, independent_scholars_citizen_scientists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, unresourced_grassroots_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, resourced_community_partners).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, knowledge_democratization_doctrine).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, epistemic_complementarity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the protocols that define adequate participation, certify that projects meet both the methodological and the experiential standard, train and accredit facilitators, and run compliance review on behalf of funders and journals. Staffing, overhead, consultancies, and training revenue flow to them, and they decide what counts as sufficient engagement. Their expertise transfers readily to adjacent governance niches if the arrangement changed.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_facilitation_bodies, agenda_setter,
    institutional, generational, mobile, global).

% Grant-making bodies and public agencies that condition awards on documented co-production. They gain research portfolios aligned with policy priorities and a defensible account of why their funded work deserves public trust. They can tighten, relax, or relocate the requirement across programs at will, and bear little of the compliance cost themselves.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, participatory_research_funders, beneficiary,
    institutional, generational, arbitrage, global).

% Well-staffed community organizations, patient groups, and advocacy bodies that hold formal seats in co-production panels. They gain funding, recognition, and real influence over research agendas. The price is professionalizing their members' knowledge into the formats the process accepts, committing staff time to facilitated cycles, and accepting agenda-framing negotiated with institutions. Declining to participate forfeits the standing they have accumulated.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, resourced_community_partners, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, resourced_community_partners, payer).

% Investigators trained in single-standard methodological work who must now build, document, and sustain community partnerships to keep grants and publish in mainstream venues. Engagement labor competes directly with laboratory and analysis time, and sharing agenda-setting with partners dilutes the individual credit their career structures reward. Mid-career exit from the research system is costly, so most absorb the added burden.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, conventional_academic_researchers, payer,
    powerful, biographical, constrained, global).

% People with direct lived experience of the conditions studied — tenants, patients, workers, residents — whose knowledge acquires official standing only when they join facilitated processes that demand time, travel, fluency in proposal formats, and sustained availability. Those without organizational backing contribute unpaid or not at all, and their experience carries no weight in formal venues either way. There is nowhere else for their knowledge to go.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, unresourced_grassroots_knowledge_holders, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, unresourced_grassroots_knowledge_holders, beneficiary).

% Researchers outside universities and outside organized community groups — retired specialists, amateur naturalists, autodidacts — who hold neither the credentials that satisfy the methodological standard nor the organizational membership that satisfies the experiential one. They would contest the dual requirement but have no seat in the bodies that administer it; they publish where they can and are cited rarely.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, independent_scholars_citizen_scientists, excluded,
    moderate, biographical, mobile, national).

% Evaluation offices and meta-research analysts who assess whether co-produced knowledge outperforms single-standard knowledge on reliability, uptake, and equity. They take testimony from every other seat, commission comparative studies, and publish findings that shape whether funders tighten or relax the dual requirement.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, science_policy_evaluators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_facilitation_bodies).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__hybrid_coproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of producing knowledge that crosses constituency boundaries: methodological validation alone produced findings that affected communities distrusted or could not use, while experiential accounts alone lacked the reliability screening that generalization requires. Co-production coordinates the two validation communities on shared artifacts — protocols, panels, jointly governed studies, co-authored outputs.
% TRANSFER_FUNCTION: Moves validation authority and research resources toward projects that invest in engagement infrastructure: funding eligibility and publication legitimacy flow to dual-validated work; time, labor, and agenda-setting concessions flow from researchers and community participants into facilitated processes; facilitation fees, training revenue, and overhead flow to the bodies administering the process.
% ABSENT_VOICES: Unresourced knowledge holders and independent scholars would object that the dual standard excludes exactly those without organizational capacity or credentials, yet neither group holds a seat in the bodies that set participation requirements. Methodological traditionalists who regard experiential validity as a category error likewise speak only from outside the process.
% DISAPPEARANCE_RATIONALE: If the dual-validation boundary vanished overnight, knowledge legitimation would reorganize around whichever single standard each venue already favored: funders would revert to peer-review-only criteria or drop validation requirements entirely, community partnerships would dissolve back into ad hoc consultation, and the facilitation sector built to administer the process would lose its function. Legitimacy flows, careers, and community standing currently routed through co-production would reroute within a few funding cycles.
% FOUNDING_PROBLEM: Expert-only knowledge production had lost credibility and usefulness: policy built on methodologically sound but context-blind research failed in deployment, publics increasingly distrusted expert institutions, and affected communities rejected findings made about them without them.
% FOUNDING_PROBLEM_CORROBORATION: Public-trust survey series and policy-implementation evaluations document the credibility and deployment failures from outside the co-production sector, and community advocacy literature attests the exclusion problem independently. The movement's own manifestos also attest the founding problem, but the external corroboration is what keeps the genealogy from being self-serving.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.58 because the dual standard decouples legitimacy from either standard's own merits: every project pays facilitation overhead and engagement labor regardless of whether integration improves the knowledge, and the rate is set by the bodies administering the process. Suppression is 0.52 and unscaled by scope or power — it is the raw structural fact that funders and journals actively refuse single-standard work, so persistence depends on continued refusal rather than participant preference; alternatives (preprint venues, community-run research) survive but carry discounted standing, hence accessibility_collapse at 0.45 rather than higher. Theater_ratio 0.42 reflects the growing share of co-production that is compliance documentation; resistance 0.58 reflects opposition arriving from both flanks — methodological traditionalists who reject the experiential requirement and grassroots groups who reject the professionalized form it takes. The claimed type is authored from the structural facts (a real integration function, named payers, active enforcement, identifiable beneficiaries); the metrics are authored descriptively and independently. The measurement series run on one shared time grid — every tracked metric is authored at every examined point — and the rising suppression_requirement series models enforcement hardening as dual validation moved from recommendation to funding precondition.
 *
 * PERSPECTIVAL GAP:
 *   The facilitator and funder seats should compute as sitting inside a coordination arrangement they designed, staff, and defend: from those positions the dual standard is the solution to fragmentation and distrust. The researcher seat computes the same structure as doubled labor under threat of funding loss; the grassroots seat computes it as a door that opens only for those already organized enough to walk through it; the independent-scholar seat computes it as exclusion from a conversation conducted about them without them. The engine derives these per-seat divergences from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Facilitation bodies and funders sit near the beneficiary end: they collect revenue, staffing, and portfolio alignment, and their exit options are strong. Conventional researchers sit near the target end: they bear the transfer of time, labor, and agenda control, with constrained exit mid-career. Unresourced grassroots holders sit nearest the full-target end — trapped exit amplifies their exposure, since their experience carries no standing anywhere else in formal venues. Independent scholars are targets despite nominal mobility: their mobility is mobility into irrelevance, since no venue honors their work under the dual standard. Resourced community partners are the genuinely dual-positioned seat: the derivation from their beneficiary declaration would place them near the beneficiary pole, but they also pay professionalization costs and surrender framing control, so a directionality override sets organized-power agents to d=0.32 rather than the near-zero a pure beneficiary reading would yield.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — expert-only knowledge losing credibility and usefulness — is still live, so this is not a resolved mandate drifting on inertia. The classification prevents mislabeling in both directions: reading the arrangement as pure coordination would erase the named payers and the enforcement dependence of the dual gate; reading it as pure extraction would erase the integration function that predates the facilitation layer and would survive its removal. The dangerous trajectory is forward, not backward: rising theater_ratio tracks the mechanism by which a functioning hybrid standard decays into compliance ritual — if the integration problems were ever solved by other means while the gate persisted as documentation, the arrangement would complete the drift toward inert theatrical maintenance. The temporal series is the tripwire for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the dual-validation co-production boundary the correct instantiation of the legitimate-knowledge kernel, or does the kernel under-determine between readings that place necessity on experiential validity (this reading), on credentialed review alone, or on community validation alone?',
    'Comparative outcome analysis across knowledge venues operating under different readings: if dual-validated work systematically outperforms single-standard work on reliability, uptake, and community-reported influence, this reading''s necessity claim strengthens; if not, the kernel resolves toward a sibling.',
    'Resolution redistributes the entire victim set: under the credentialed-expertise reading the payers are community knowers; under the experiential-pluralism reading they are methodologists; under this reading both pay the dual gate. Epsilon and classification differ sharply across resolutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the legitimate-knowledge kernel is instantiated changes the constraint''s victim set and epsilon.').

omega_variable(
    facilitation_layer_capture,
    'Does the facilitation layer genuinely integrate knowledge across constituencies, or has it become a self-perpetuating administrative stratum collecting overhead for compliance it mostly documents?',
    'Cost-and-outcome audit of co-production programs: compare facilitation spend and staffing against measured integration outcomes such as uptake, replication, and community-reported influence on research agendas.',
    'If capture dominates, effective extraction rises well above the authored 0.58 and the arrangement trends toward pure extraction administered by its own beneficiaries; if integration dominates, the coordination function is genuine and the hybrid characterization holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(facilitation_layer_capture, empirical, 'Whether the co-production intermediary layer integrates or captures.').

omega_variable(
    participation_theater_fraction,
    'What fraction of declared co-production activity constitutes substantive shared authority rather than performative consultation staged to satisfy the dual-validation requirement?',
    'Process tracing of funded projects: determine whether community input altered hypotheses, methods, or dissemination, or appears only in compliance documentation.',
    'A high theater fraction would push theater_ratio above 0.5, dating a Goodhart transition in which the proxy (documented participation) replaced the function (shared authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_theater_fraction, empirical, 'Share of co-production activity that is performative compliance rather than shared authority.').

omega_variable(
    experiential_validity_operationalization,
    'What counts as experiential validity is underspecified: who operationalizes it, and does the operationalization track lived-experience authority or the facilitator''s procedural preferences?',
    'Compare certification decisions across facilitation bodies presented with equivalent community inputs; divergent outcomes reveal operator-dependent standards.',
    'Different operationalizations move the experiential standard from a genuine second validation gate to a discretionary filter, changing who passes and shifting effective extraction upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(experiential_validity_operationalization, conceptual, 'Underspecification of the experiential-validity standard and its gatekeeping consequences.').

omega_variable(
    internalized_epistemic_deference,
    'For unresourced knowledge holders, is non-participation driven by structural barriers (time, travel, format literacy) or by internalized deference to methodological authority that persists even where participation is formally open?',
    'Post-removal participation trajectories: if holder participation stays flat after structural barriers are removed (stipends, childcare, plain-language formats), the residual gap is internalized.',
    'If internalized, effective suppression exceeds the structural measure: targets carry the exclusion with them after exit, and barrier-removal remedies will underperform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_epistemic_deference, empirical, 'Structural versus internalized mechanism behind community non-participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t6, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(legi_tr_t6, observed).
narrative_ontology:measurement(legi_tr_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(legi_tr_t12, observed).
narrative_ontology:measurement(legi_tr_t18, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement_basis(legi_tr_t18, observed).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(legi_tr_t24, observed).
narrative_ontology:measurement(legi_tr_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(legi_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t6, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 6, 0.43).
narrative_ontology:measurement_basis(legi_be_t6, observed).
narrative_ontology:measurement(legi_be_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement_basis(legi_be_t12, observed).
narrative_ontology:measurement(legi_be_t18, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 18, 0.51).
narrative_ontology:measurement_basis(legi_be_t18, observed).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement_basis(legi_be_t24, observed).
narrative_ontology:measurement(legi_be_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(legi_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t6, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(legi_su_t6, observed).
narrative_ontology:measurement(legi_su_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(legi_su_t12, observed).
narrative_ontology:measurement(legi_su_t18, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement_basis(legi_su_t18, observed).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(legi_su_t24, observed).
narrative_ontology:measurement(legi_su_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(legi_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who decides what counts as legitimate knowledge' decomposes into three structurally distinct constraints, one per reading of the legitimate_knowledge_boundary kernel. Each carries a distinct epsilon: this reading's dual gate generates facilitation-layer extraction absent from the credentialed reading's single gate and absent from the pluralism reading's community validation. Family links run through affects_constraints; downstream pressure runs through research funding policy, which cites whichever reading currently dominates when setting award conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
