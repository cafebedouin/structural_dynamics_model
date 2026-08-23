% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-production Boundary of Legitimate Knowledge
 *   domain: epistemology/science-and-technology-studies/political-theory
 *
 * SUMMARY:
 *   Across health research, environmental governance, and development
 *   practice, a boundary regime now holds that a knowledge claim counts as
 *   legitimate only when produced through processes integrating
 *   methodological rigor with experiential validation — co-produced with the
 *   people the knowledge concerns, certified by mixed panels, and documented
 *   in involvement statements that funders and journals require. The regime
 *   solves a real trust problem at the science-society interface while
 *   generating its own economy: brokerage institutions, delegate seats,
 *   participation labor, and compliance overhead. KEY AGENTS (by structural
 *   relationship): co_production_facilitation_bodies (agenda-setter,
 *   institutional/constrained) — administers dual validation and collects its
 *   proceeds; organized_community_delegates (beneficiary/payer,
 *   organized/constrained) — gain standing, supply labor;
 *   volunteer_knowledge_contributors (primary target, powerless/constrained)
 *   — supply testimony and interpretive labor; solo_academic_researchers
 *   (target, moderate/constrained) — bear double-gate compliance costs;
 *   research_universities (beneficiary/payer, institutional/mobile) — win
 *   participatory funding, cede gatekeeping;
 *   unrepresented_affected_populations (excluded, powerless/trapped) — live
 *   with outputs, hold no seat; sts_and_epistemic_justice_analysts (observer,
 *   analytical) — see the full structure. Claim/metric independence is
 *   preserved: the claimed type (tangled_rope) states my structural belief;
 *   the metrics state my descriptive beliefs about actual operation; the
 *   engine computes per-seat classifications and any divergence between claim
 *   and computation is the datum.
 *
 * KEY AGENTS:
 *   - co_production_facilitation_bodies: agenda-setter (institutional/constrained) — runs panels, writes standards, certifies dual validation, collects funding and authority
 *   - organized_community_delegates: beneficiary with payer costs (organized/constrained) — hold seats, gain influence, supply unpaid preparatory and emotional labor
 *   - volunteer_knowledge_contributors: primary target (powerless/constrained) — supply testimony that becomes raw material for outputs they do not control
 *   - solo_academic_researchers: target (moderate/constrained) — bear double-gate compliance costs without partnership leverage
 *   - research_universities: beneficiary with payer costs (institutional/mobile) — gain participatory funding streams, lose sole custody of quality judgment
 *   - unrepresented_affected_populations: excluded (powerless/trapped) — bear consequences of outputs with no seat in their making
 *   - sts_and_epistemic_justice_analysts: analytical observer — audits integration depth and tokenism from outside the funded circuit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.52).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-production Boundary of Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science-and-technology-studies/political-theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, '5be34f0b-3971-4882-b628-9b57188df0c3').
narrative_ontology:cs_kernel_codification('5be34f0b-3971-4882-b628-9b57188df0c3', distributed).
narrative_ontology:cs_authority_grounding('5be34f0b-3971-4882-b628-9b57188df0c3', practice).
narrative_ontology:cs_interpretation_layer_present('5be34f0b-3971-4882-b628-9b57188df0c3').
narrative_ontology:cs_reading_relation('5be34f0b-3971-4882-b628-9b57188df0c3', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('5be34f0b-3971-4882-b628-9b57188df0c3', legitimate_knowledge_boundary__experiential_pluralism_reading, influences).
narrative_ontology:cs_axiom('5be34f0b-3971-4882-b628-9b57188df0c3', foundational, dual_validation_necessary_for_legitimate_knowledge).
narrative_ontology:cs_axiom_status(dual_validation_necessary_for_legitimate_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('5be34f0b-3971-4882-b628-9b57188df0c3', dual_validation_necessary_for_legitimate_knowledge, instrumental).
narrative_ontology:cs_axiom('5be34f0b-3971-4882-b628-9b57188df0c3', foundational, methodological_rigor_not_substitutable_by_experience).
narrative_ontology:cs_axiom_status(methodological_rigor_not_substitutable_by_experience, holdable).
narrative_ontology:cs_axiom_grounding('5be34f0b-3971-4882-b628-9b57188df0c3', methodological_rigor_not_substitutable_by_experience, empirically_contingent).
narrative_ontology:cs_reference_frame('5be34f0b-3971-4882-b628-9b57188df0c3', integrated_coproduction_partnership).
narrative_ontology:cs_drift_state('5be34f0b-3971-4882-b628-9b57188df0c3', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5be34f0b-3971-4882-b628-9b57188df0c3', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, organized_community_delegates).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, research_universities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_facilitation_bodies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, volunteer_knowledge_contributors).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, solo_academic_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, organized_community_delegates).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, research_universities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the participatory apparatus: convene advisory panels, train facilitators, certify which studies meet both methodological and experiential criteria, and write the quality standards other institutions adopt. Dedicated funding streams, staff careers, and institutional standing depend on the continued centrality of facilitated co-production; pivoting back to conventional consultancy would forfeit accumulated legitimacy capital.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_facilitation_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Hold seats on co-production boards and panels as recognized community representatives. They gain agenda influence, public recognition, and occasional stipends, and they contribute extensive unpaid preparation and emotional labor translating community concerns into committee language. Stepping down forfeits a hard-won seat and hands representation to whoever replaces them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, organized_community_delegates, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, organized_community_delegates, payer).

% Are recruited to workshops, interviews, and priority-setting exercises because of lived experience — patients, residents, service users. They give testimony and interpretive labor, usually unpaid, that becomes raw material for publications and program designs they rarely control. Declining an invitation leaves their situation represented by others, or not represented at all.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, volunteer_knowledge_contributors, payer,
    powerless, immediate, constrained, local).

% Produce methodologically strong work without community partnerships. Funder mandates and journal expectations now treat such work as incomplete regardless of technical quality, so they must build collaborations, share framing control, and absorb slower timelines — or watch their findings go uncited by the policy and practice communities that once used them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, solo_academic_researchers, payer,
    moderate, biographical, constrained, continental).

% Host co-production centres and win dedicated participatory-program funding, gaining civic legitimacy for their brands. At the same time they absorb the administrative overhead of involvement requirements and cede sole custody of quality judgment to mixed panels that include non-academic members.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, research_universities, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, research_universities, payer).

% Live with the consequences of knowledge produced about people like them — clinical guidelines, housing plans, climate adaptations — but hold no delegate seat and fit no recruitment category. Their objections surface, when they surface at all, after the decisions have already been framed.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, unrepresented_affected_populations, excluded,
    powerless, generational, trapped, global).

% Study the boundary regime from outside: documenting where co-production deepens knowledge and where it launders pre-made decisions through participation theater, publishing audits that neither the facilitating bodies nor the funded programs commission.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, sts_and_epistemic_justice_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_facilitation_bodies).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__hybrid_coproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the trust-and-relevance problem at the science-society interface: knowledge that must clear both methodological review and experiential validation is more actionable, more trusted by affected communities, and less likely to fail on implementation than knowledge validated on either dimension alone.
% TRANSFER_FUNCTION: Moves epistemic authority and research resources toward community delegates and facilitating institutions; moves time, testimony, and unpaid interpretive labor from volunteer contributors into formal knowledge products; and moves certification power over what counts as legitimate to whichever bodies administer dual validation.
% ABSENT_VOICES: Unrepresented affected populations who match no recruitment category; knowledge holders whose experience does not fit workshop formats or official languages; researchers in resource-poor institutions who cannot absorb co-production overhead. None holds a seat; their objections enter only post hoc, through complaint or refusal to comply.
% DISAPPEARANCE_RATIONALE: If the dual-validation boundary vanished overnight, funders would drop involvement mandates, journals would stop requiring contribution statements, advisory panels would dissolve, delegates would lose their seats, and the channels through which volunteer testimony reaches formal knowledge would close. The legitimacy contest would snap back to its two sibling poles — pure credentialed expertise versus pure experiential pluralism — and the funding, careers, and infrastructure currently organized around brokerage would have to relocate.
% FOUNDING_PROBLEM: A twin credibility failure: top-down expert knowledge repeatedly ignored the situated understanding of affected communities, producing resistance, mistrust, and unusable findings, while purely experiential knowledge lacked the methodological standing to move institutions. The hybrid boundary was built so that neither pole could exclude the other from legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: longitudinal trust surveys showing persistent confidence gaps toward expert institutions among affected communities, published evaluations of failed top-down interventions (urban renewal, agricultural extension, public-health campaigns) that never involved their subjects, and STS analyses written without co-production sector funding. The facilitation bodies themselves also attest the problem is live, but that attestation is self-interested; the external sources above do not depend on the sector's continuation.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness 0.58 is moderate: real value flows back to contributing communities through influence and recognition, but the arrangement levies a double gate (rigor AND experience), monetizes mandatory complexity through brokerage, and draws substantial unpaid interpretive labor from precisely the people with least slack. Suppression 0.52 reflects epistemic gatekeeping enforced through funder mandates and review criteria rather than physical coercion — it is a raw structural property, unscaled by scope or power in the engine's computation, and it is what makes non-compliance costly for researchers and delegates alike. Accessibility collapse is 0.45: exits exist (community-led research outside formal channels, preprints, funders without involvement mandates) but they cost visibility, funding eligibility, and policy uptake, so they are degraded rather than closed. Resistance 0.55 is real on both flanks — researchers organizing against involvement bureaucracy, community groups refusing tokenistic invitation. Theater ratio 0.40 is the honest center of a documented spread: a large fraction of certified co-production functions as compliance performance (Arnstein-style audits repeatedly find consultation dressed as participation), while a smaller core demonstrably changes research questions and outcomes. The temporal series run on one shared eight-point grid (t=0..35, mapping roughly 1990-2025) with all three metrics authored at every point; all trajectories rise monotonically with institutionalization — mandates hardened, compliance formalized, and tokenistic share grew as the gate became a box-ticking requirement — with no oscillation, so no cyclical commentary is warranted. Identity-lock dynamics bind two seats: brokers' professional identity has fused with the participatory ethos (their organization has become its facilitation function), and delegates' representative identity makes resignation feel like betrayal of their community rather than a career move; if either frame broke, the broker seat's exit options would widen from constrained toward mobile and the delegate seat's extraction exposure would convert into open defection. On the receipt surface: gain_flow names co_production_facilitation_bodies because the story establishes that certification authority, dedicated funding streams, and career structures demonstrably accrue there — receipt, not merely benefit; fixing_cost is prohibitive because the dual-validation apparatus is embedded in funding instruments, review workflows, and explicit commitments to communities, so removal for whoever could effect it (funders, ministries) costs more relational and political capital than the burden it would relieve.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the broker seat, the arrangement is the hard-won correction to decades of technocratic failure — a coordination structure it built and legitimately staffs. From the solo researcher seat, the same structure is a double tax on rigor imposed by actors who control funding. From the volunteer seat, it is a harvest of testimony with uncertain return; from the delegate seat, a bargain — influence purchased with labor — struck from a position too weak to renegotiate terms. The universities straddle: winner of new revenue, loser of gatekeeping monopoly. The engine computes these divergent classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation and no overrides were needed. Co-production facilitation bodies derive a low d near the beneficiary end: they collect certification authority and funding and bear little of the gate's cost. Organized community delegates derive low-to-moderate d: they sit in the beneficiaries array, but their constrained exit (losing the seat) and their secondary payer position (unpaid labor) pull them off pure-beneficiary values toward symmetry — the derivation captures this through exit modulation rather than needing an override. Research universities derive low-moderate d for the same reason: net gainers with partial cost absorption and mobile exit keeping them nearer the beneficiary end. Volunteer knowledge contributors and solo academic researchers derive high d: both bear the gate's costs, both have constrained rather than arbitrage-grade exit, and the volunteers' powerlessness traps them nearest the full-target end. Unrepresented affected populations are excluded rather than exchanging — they register as absent voices, not as extraction targets. Scope amplification applies modestly at global scale, hardest on verification of integration quality, which is exactly where theater concentrates.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification discipline prevents two opposite errors. Reading this boundary as a pure rope would erase the asymmetric flows the same structure carries: unpaid participation labor, broker-captured certification rents, and compliance burdens landing on resource-poor researchers — the coordination story would become cover. Reading it as a pure snare would deny the demonstrated epistemic goods: co-produced knowledge is more trusted, more actionable, and has corrected genuine expert blind spots, and wholesale condemnation of participation would hand legitimacy back to the exclusions the founding problem documented. Tangled rope holds both facts: an enforceable dual standard coordinating trust across the expertise-experience divide, AND asymmetric extraction running through the identical gate. The founding problem is corroborated as live by sources outside the benefiting parties, so the mismatch consumer should find no dead-mandate/zombie flag here; the risk to monitor is forward drift — if theater_ratio continues climbing while integration depth stagnates, the arrangement migrates toward piton-like performance maintained by brokers whose function has hollowed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates only the hybrid_coproduction_reading of the legitimate_knowledge_boundary kernel; the credentialed_expertise_reading and experiential_pluralism_reading siblings instantiate different constraints with different epsilon values and different victim sets. Which necessity condition governs the boundary — is experiential validity necessary, sufficient, or neither?',
    'No empirical resolution: the readings disagree at the level of defining premises. Resolution arrives only through the corpus comparing all three stories'' computed classifications and observing which reading''s predicted failure modes actually materialize in funding, publication, and policy practice.',
    'Under the credentialed sibling, the victims named here (compliance-burdened researchers) disappear as a category and the broker seat becomes pure coordinator; under the experiential sibling, the methodological gate becomes the extraction object and the victim set inverts. The location of the disagreement is the necessity conditions, not the enforcement machinery all three share.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame omega recording that this constraint is one reading of a three-way kernel contest.').

omega_variable(
    tokenism_integration_depth,
    'How much of current co-production activity is substantive integration — participant input demonstrably altering research questions, methods, or conclusions — versus consultative theater that satisfies the dual-validation form without its function?',
    'Decision-traceability audits: for a sample of certified co-produced outputs, reconstruct whether and where participant contributions changed the final product, using project records independent of the facilitating bodies'' self-reports.',
    'If theater dominates, the theater_ratio is understated at 0.40, the coordination function degrades toward performance maintenance, and the arrangement drifts toward piton-like operation with brokers performing inclusion while capturing its funding; if integration is widespread, the rope component is stronger than the extraction component and effective extraction falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tokenism_integration_depth, empirical, 'Whether dual validation operates as genuine integration or compliance performance.').

omega_variable(
    participation_labor_distribution,
    'Who actually bears the participation burden the dual gate requires — paid professional delegates and institutionally supported partners, or uncompensated volunteers supplying testimony on top of existing obligations?',
    'Time-use and compensation audits across co-production sites, disaggregating contributor category against hours contributed and remuneration received.',
    'If burden concentrates on unpaid volunteers, base extractiveness understates the target-seat experience and the victim-side chi rises above what the aggregate 0.58 suggests; if burden is broadly compensated, the arrangement sits closer to ordinary collaborative labor markets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_labor_distribution, empirical, 'Distribution of unpaid interpretive labor across contributor categories.').

omega_variable(
    broker_capture_vs_service,
    'Do the facilitation bodies price and design their services as a competitive market responding to participant and researcher needs, or as a captured intermediary collecting rents from mandatory complexity only they can navigate?',
    'Funding-flow benchmarking: compare facilitation fees and overhead against equivalent non-participatory coordination services, and test whether simplification proposals originating outside the broker class survive.',
    'Evidence of rent capture confirms the extraction side of the tangled-rope structure and strengthens the case that gain_flow correctly names the broker seat; evidence of competitive service provision shifts weight toward the coordination side and would support eventual rope classification if extraction otherwise declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_capture_vs_service, empirical, 'Whether the broker seat serves coordination or captures the surplus the dual gate generates.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression that keeps alternative boundary arrangements weak primarily structural (funder mandates, journal requirements, review criteria) or partly internalized (communities persuaded that submitting to facilitated participation is a duty of good citizenship, researchers persuaded that refusing partnership is epistemically disreputable)?',
    'Withdrawal-trajectory observation: track actors who exit or openly defy the dual gate — do sanctions pursue them structurally, or does the pressure persist as reputational and self-imposed cost after formal barriers lapse?',
    'If a substantial share of measured suppression is internalized, effective suppression exceeds the structural measure and would persist even if mandates were repealed, meaning deregulation alone would not restore the sibling readings'' viability; if suppression is almost wholly structural, mandate reform would rapidly lower the barrier profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized components of the boundary''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(legi_tr_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(legi_tr_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(legi_tr_t35, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 35, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(legi_be_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(legi_be_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(legi_be_t35, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(legi_su_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(legi_su_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(legi_su_t35, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the legitimate_knowledge_boundary kernel per the epsilon-invariance principle: the colloquial label 'who counts as a knower' conflates three structurally distinct boundary regimes. Credentialed_expertise_reading authors low epsilon (single validation gate, established infrastructure); experiential_pluralism_reading authors a different victim set (methodological gatekeepers displaced rather than communities); THIS story, hybrid_coproduction_reading, authors the dual-gate regime with its own epsilon (~0.58), its own broker-capture beneficiary structure, and its own participation-burden victims. Each member links to the others via network.affects_constraints; the upstream sibling (credentialed expertise, highest empirical establishment) is cited as the baseline the hybrid reading modifies, which is why the edge runs bidirectionally through the shared kernel rather than hierarchically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
