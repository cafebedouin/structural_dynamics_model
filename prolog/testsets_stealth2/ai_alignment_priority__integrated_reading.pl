% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated Dual-Priority Mandate for AI Alignment (Complementary-Harms Reading)
 *   domain: technology_governance/ai_ethics
 *
 * SUMMARY:
 *   This story authors the integrated reading of the alignment-priority
 *   kernel as a clean, epsilon-invariant constraint: the operative norm,
 *   embedded in grant criteria, lab policy frameworks, and field governance,
 *   that legitimate alignment work must address catastrophic capability risks
 *   and present deployment harms jointly, with neither subordinated to the
 *   other. Its coordination function is real: it keeps both harm classes
 *   funded and legible, supports methodologies that serve both (adversarial
 *   red-teaming surfaces misuse and bias alike), and prevents the field from
 *   splitting into mutually delegitimizing camps. Its costs are also real:
 *   dual compliance burdens fall on frontier labs, single-domain researchers
 *   experience the mandate as dilution of their core contribution, and the
 *   definitional center (who decides what counts as 'balanced') accrues to
 *   funders and mediating bodies. Constraint-family note (meta-documentation,
 *   not part of the constraint itself): the colloquial label 'AI alignment
 *   priorities' decomposes into three structurally distinct readings. The
 *   existential_risk_reading concentrates the victim set in future
 *   populations and locates neglect in capability-risk work; the
 *   nearterm_harms_reading concentrates it in present marginalized
 *   communities and locates neglect in deployment harms; this reading splits
 *   the victim set across both and carries moderate epsilon on each half.
 *   Each is a separate file linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - deployment_harm_communities: Primary beneficiary (powerless/trapped) — present-day communities bearing discriminatory and extractive system outputs; the mandate keeps their harms on the funded agenda
 *   - - future_populations: Primary beneficiary (powerless/trapped) — represented only by proxy; the mandate keeps capability-risk work funded and legitimated
 *   - - safety_funders: Agenda-setter (institutional/arbitrage) — writes dual-coverage criteria into grant calls and defines what counts as balance
 *   - - frontier_development_labs: Primary payer, secondary beneficiary (institutional/constrained) — bears dual compliance costs, receives legitimacy in exchange
 *   - - single_domain_alignment_researchers: Payer (moderate/identity_locked) — specialists whose fused professional identities make the broadening mandate read as identity threat
 *   - - field_mediating_bodies: Beneficiary and receipt seat (organized/mobile) — institutes and standards initiatives whose charter, convening power, and funding share scale with the dual mandate
 *   - - ai_displaced_workers: Excluded voice (powerless/trapped) — harmed by automation displacement, a concern fitting neither recognized bucket
 *   - - alignment_field_analysts: Analytical observer — tracks whether dual commitments are resourced and whether methods transfer across harm classes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.48).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.42).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated Dual-Priority Mandate for AI Alignment (Complementary-Harms Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "technology_governance/ai_ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, 'b81c2b79-e918-4bee-84ec-baebbbe31101').
narrative_ontology:cs_kernel_codification('b81c2b79-e918-4bee-84ec-baebbbe31101', distributed).
narrative_ontology:cs_authority_grounding('b81c2b79-e918-4bee-84ec-baebbbe31101', distributed).
narrative_ontology:cs_reading_relation('b81c2b79-e918-4bee-84ec-baebbbe31101', ai_alignment_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('b81c2b79-e918-4bee-84ec-baebbbe31101', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_axiom('b81c2b79-e918-4bee-84ec-baebbbe31101', foundational, harm_classes_methodologically_complementary).
narrative_ontology:cs_axiom_status(harm_classes_methodologically_complementary, holdable).
narrative_ontology:cs_axiom_grounding('b81c2b79-e918-4bee-84ec-baebbbe31101', harm_classes_methodologically_complementary, empirically_contingent).
narrative_ontology:cs_axiom('b81c2b79-e918-4bee-84ec-baebbbe31101', foundational, exclusive_prioritization_creates_blind_spots).
narrative_ontology:cs_axiom_status(exclusive_prioritization_creates_blind_spots, holdable).
narrative_ontology:cs_axiom_grounding('b81c2b79-e918-4bee-84ec-baebbbe31101', exclusive_prioritization_creates_blind_spots, empirically_contingent).
narrative_ontology:cs_reference_frame('b81c2b79-e918-4bee-84ec-baebbbe31101', complementary_dual_harm_agenda).
narrative_ontology:cs_drift_state('b81c2b79-e918-4bee-84ec-baebbbe31101', contemporary_funding_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b81c2b79-e918-4bee-84ec-baebbbe31101', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, deployment_harm_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, field_mediating_bodies).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, frontier_development_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, single_domain_alignment_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, frontier_development_labs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities bearing discriminatory and extractive outputs of deployed systems in credit, hiring, policing, housing, and content moderation. Under the dual mandate their harms hold standing in funding criteria and lab commitments alongside capability risks, and advocacy channels into alignment venues stay open. They cannot opt out of algorithmic decision-making in the systems that govern their access to basic goods.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, deployment_harm_communities, beneficiary,
    powerless, immediate, trapped, global).

% People who will exist and whose stakes in catastrophic loss-of-control outcomes are carried entirely by proxy advocates. The dual mandate guarantees capability-risk work remains funded and legitimated rather than traded away for present-harm priorities. They have no voice, no vote, and no exit by construction; everything they receive arrives through representatives with their own incentives.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations, beneficiary,
    powerless, civilizational, trapped, universal).

% Foundations, public agencies, and internal allocation committees that write dual-coverage requirements into grant calls, strategy documents, and lab partnerships. They define what counts as balanced coverage, can shift portfolios between the two harm classes, and bear reputational rather than material exposure. If the dual framework lost favor they could redirect funds to a different framing within a funding cycle.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, safety_funders, agenda_setter,
    institutional, generational, arbitrage, global).

% Labs building frontier systems. They run capability evaluations and adversarial testing for catastrophic-risk coverage while simultaneously conducting bias audits, impact assessments, and affected-community engagement for deployment-harm coverage, and must publish commitments spanning both to retain talent, customers, and regulatory goodwill. The dual burden is a real per-release cost; in exchange they receive legitimacy and a defensible public posture. Exiting means ceding the legitimacy field to rivals or abandoning frontier development.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, frontier_development_labs, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, frontier_development_labs, beneficiary).

% Researchers whose methods, careers, and professional self-concept are fused to one harm class — interpretability aimed at loss-of-control, or auditing aimed at discriminatory deployment. Dual-coverage grant criteria press them to broaden agendas they experience as diluting their core contribution; declining means shrinking funding prospects, and reframing their identity or leaving the field is experienced as unthinkable rather than merely expensive.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, single_domain_alignment_researchers, payer,
    moderate, biographical, identity_locked, global).

% Institutes, standards initiatives, benchmark consortia, and convening organizations occupying the definitional center: they host evaluations spanning both harm classes, draft the frameworks funders cite, and intermediate between funders, labs, and advocacy groups. The dual mandate is effectively their charter — their convening power, staff growth, and funding share scale with it. They could reposition around adjacent governance niches if the framework dissolved, at moderate cost.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, field_mediating_bodies, beneficiary,
    organized, generational, mobile, global).

% Workers whose livelihoods are degraded by automation-driven displacement — a harm that fits neither the catastrophic-loss-of-control bucket nor the discriminatory-deployment bucket. The mandate's two-class taxonomy leaves their concern outside both funding streams and both evaluation frameworks. Were they seated at the table they would object that balance between the two recognized classes still excludes them entirely.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_displaced_workers, excluded,
    powerless, immediate, trapped, global).

% Independent researchers, science-policy scholars, and evaluators tracking whether dual commitments are matched by budget lines, whether methodologies actually transfer across the two harm classes, and how portfolios shift over time. They take no side in the priority contest, bear none of its compliance costs, and publish findings available to every seat.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, alignment_field_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, field_mediating_bodies).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the alignment field's research agenda and resource allocation so that capability-risk mitigation and deployment-harm mitigation are pursued jointly: shared methodologies (adversarial red-teaming, evaluation infrastructure) serve both classes, pooled funding criteria cover both, and a single legitimacy standard governs lab conduct across both.
% TRANSFER_FUNCTION: Moves research attention, funding, and public legitimacy toward actors demonstrating dual coverage; moves compliance costs (evaluation overhead, audit obligations, agenda broadening) onto frontier labs and single-domain specialists; moves agenda-defining power to the funders and mediating bodies that operationalize 'balance'.
% ABSENT_VOICES: Workers harmed by automation displacement would object that the two-class taxonomy excludes their concern altogether; they are outside both funding streams and both evaluation frameworks. Future populations are present only through proxy advocates whose incentives are unverified. Researchers who reject the integrated framing entirely have no seat in funder deliberations that set dual-coverage criteria.
% DISAPPEARANCE_RATIONALE: If the dual mandate vanished overnight, funding would bifurcate along the existing camp lines, labs would optimize whichever criterion their loudest critics tracked least, shared evaluation infrastructure would fragment into separate capability and fairness stacks, and the mediating bodies built on the integrated charter would lose their function — the field would reorganize around the zero-sum competition the arrangement currently suppresses.
% FOUNDING_PROBLEM: The alignment field had fragmented into camps treating catastrophic-risk work and present-harm work as rivals: zero-sum funding fights, mutual accusations of dangerous negligence, duplicated infrastructure, and blind spots in both directions — capability-risk work indifferent to biased deployments, fairness work inattentive to capability escalation.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: each exclusive camp's continuing public critiques of the other document the fragmentation as ongoing; independent science-policy funding-flow analyses and bibliometric work from the late 2010s recorded the bifurcated safety portfolio; multi-country researcher surveys show sharply divergent priority rankings persisting into the present. None of these attestations originates with the integrated coalition's beneficiaries.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 (interval end): dual compliance costs and agenda dilution are material and have grown as the mandate hardened from voluntary statements into grant gates and audit expectations, but the mandate also funds work each camp independently values, so extraction stays moderate rather than severe. Suppression is 0.42: enforcement operates through funding eligibility and legitimacy sanctions rather than coercion, and specialized alternatives remain fully legal and publishable — suppression here is gatekeeping, not prohibition. Theater_ratio is 0.35 and rising slowly: a growing share of dual commitments are statements without budget lines, but the shared-methodology core (red-teaming, evaluation infrastructure) is functionally real. Accessibility_collapse is low (0.30): both exclusive camps continue to operate visibly, so alternatives do not collapse on contact with the constraint. Resistance is 0.50: each camp actively contests the balance point, accusing the other of dilution or tokenization. The claimed type (tangled_rope) and the metrics were authored independently: the claim states my structural belief that the arrangement couples genuine coordination to asymmetric cost-bearing; the metrics state what I believe descriptively true of its operation. Where the engine's per-seat computations diverge from the claim, that divergence is the datum. The temporal series run on one shared grid (2018/2020/2022/2024/2026) with every tracked metric authored at every point; suppression_requirement is tracked because enforcement capacity specifically intensified over the interval (voluntary pledges to codified criteria), not merely because extraction shifted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda/beneficiary seats should compute differently. From the funder and mediating-body seats the mandate is a portfolio discipline they designed and administer — coordination they built. From the specialist-researcher seat it is an imposed broadening that taxes a fused professional identity; from the lab seat it is a compliance regime priced into every release. The identity-lock on single_domain_researchers is the sharpest divergence driver: because their exit is identity-fused rather than merely costly, the same mandate that reads as reasonable balance from the center reads as existential professional threat from the flank, and the engine should compute a substantially higher effective extraction for that seat than for mobile beneficiaries. Deployment-affected communities and future populations, though beneficiaries, experience the mandate only through proxies — their computed benefit is only as good as the proxy-alignment omega allows.
 *
 * DIRECTIONALITY LOGIC:
 *   Deployment_harm_communities and future_populations sit near the beneficiary end: the mandate subsidizes attention to their stakes, and neither has any exit (their d derives low from the beneficiary declarations and trapped exits). Safety_funders derive low-to-symmetric d as agenda-setters whose exposure is reputational; their arbitrage-grade ability to redirect portfolios keeps them near the subsidy side. Field_mediating_bodies derive low d from their beneficiary role, but they are also the seat the arrangement's gains demonstrably accrue to (charter, convening power, intermediated funding) — the receipt surface records this capture even though their role-derived directionality reads as subsidized. Frontier_development_labs derive high d: named payers with constrained exit, partially offset by the legitimacy they receive (secondary beneficiary role). Single_domain_alignment_researchers derive the highest d in the story: payers whose identity_locked exit places them nearest the full-target end — trapped-or-locked targets amplify effective extraction. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms differentiate all eight seats without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a field fragmented into camps treating the two harm classes as rivals, with zero-sum funding fights and mutual delegitimization — is still live, attested from outside the benefiting parties by each camp's ongoing public critiques of the other and by independent funding-flow analyses. Because the problem is live, the mandate has not outlived its function and is not a piton despite its nonzero and rising theater ratio; the theatrical share is a symptom of box-ticking compliance, not evidence of an atrophied core. The tangled_rope classification prevents both symmetric mislabelings: reading the mandate as pure rope would erase the asymmetric costs borne by locked-in specialists and compliance-burdened labs and would ignore the capture seat at the definitional center; reading it as a snare would erase the genuine dual-harm coordination, the open viability of both exclusive alternatives, and the absence of any identifiable population whose exclusion the enforcement machinery exists to maintain. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag — consistent with the computed path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the integrated_reading of the ai_alignment_priority kernel. How would the sibling readings restructure the victim set and the extraction profile if adopted in place of this reading?',
    'Cross-story comparison once the sibling constraints (existential_risk_reading, nearterm_harms_reading) are compiled; tracked against funder and standards-body adoption decisions that shift which reading is operationally dominant.',
    'Under the existential_risk_reading the victim set collapses to future populations alone and measured neglect concentrates on capability-risk work; under the nearterm_harms_reading it collapses to present marginalized communities and concentrates on deployment harms. This reading''s moderate, split epsilon depends on holding both harm classes in scope simultaneously; a shift in operational dominance re-bases the entire measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one of three rival readings of the alignment-priority kernel; sibling adoption changes victim set and epsilon location.').

omega_variable(
    safety_capacity_fungibility,
    'Is safety-relevant capacity (researcher talent, evaluation compute, institutional attention) actually fungible across catastrophic-risk work and deployment-harm work, or is it rivalrous such that dual mandates dilute both?',
    'Longitudinal funding-outcome studies tracking whether labs and programs with dual mandates produce comparable output per dollar in each harm class versus single-mandate peers.',
    'If capacity is strongly rivalrous, the dual mandate functions as mutual dilution and effective extraction rises well above the authored base; if genuinely complementary (shared methods, shared infrastructure), the mandate approaches pure coordination and excess extraction falls toward the resource_allocation floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_capacity_fungibility, empirical, 'Whether the two harm classes compete for the same finite safety capacity or share it productively.').

omega_variable(
    resourced_vs_performative_integration,
    'Are labs'' and funders'' dual-coverage commitments backed by budget lines and staffing, or are they performative statements maintained for legitimacy?',
    'Comparative analysis of published dual commitments against disclosed spending and headcount on deployment-harm work (audits, bias evaluations, affected-community engagement) relative to capability-evaluation spend.',
    'High performative share means the mandate''s coordination function is partly theatrical maintenance, raising piton-drift risk and indicating the theater_ratio understates steady-state decay; a resourced majority supports the tangled_rope reading with a functioning coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resourced_vs_performative_integration, empirical, 'Whether integration is substantively resourced or increasingly box-ticking.').

omega_variable(
    proxy_advocate_alignment,
    'Future populations and deployment-affected communities enter this arrangement only through proxy advocates. Do the proxies'' positions and resource demands track the interests of the people they represent?',
    'Comparative studies of advocacy organizations'' funding sources and stated priorities against surveyed preferences of directly affected community members.',
    'Where proxies diverge from principals, the beneficiary declarations overstate realized benefit, the effective victim set widens beyond the two named groups, and directionality for nominally subsidized seats shifts toward the target end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_advocate_alignment, empirical, 'Validity of proxy representation for the two principal beneficiary classes.').

omega_variable(
    framing_dissent_penalty,
    'Do single-domain researchers face structural penalties under dual-coverage criteria (grant rejection, venue exclusion, hiring disadvantage), or do they self-select away from integrated programs?',
    'Survey and administrative data on rejected grant rationales, reviewer comments, and career trajectories of researchers who declined to broaden their agendas.',
    'Structural penalties confirm externally enforced suppression supporting the active-enforcement characterization; self-selection indicates suppression is largely internalized identity defense, lowering the structural suppression attributable to the constraint itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_dissent_penalty, empirical, 'Whether suppression of single-frame research is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2018, ai_alignment_priority__integrated_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement_basis(ai_a_tr_t2018, observed).
narrative_ontology:measurement(ai_a_tr_t2020, ai_alignment_priority__integrated_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement_basis(ai_a_tr_t2020, observed).
narrative_ontology:measurement(ai_a_tr_t2022, ai_alignment_priority__integrated_reading, theater_ratio, 2022, 0.31).
narrative_ontology:measurement_basis(ai_a_tr_t2022, observed).
narrative_ontology:measurement(ai_a_tr_t2024, ai_alignment_priority__integrated_reading, theater_ratio, 2024, 0.34).
narrative_ontology:measurement_basis(ai_a_tr_t2024, observed).
narrative_ontology:measurement(ai_a_tr_t2026, ai_alignment_priority__integrated_reading, theater_ratio, 2026, 0.35).
narrative_ontology:measurement_basis(ai_a_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2018, ai_alignment_priority__integrated_reading, base_extractiveness, 2018, 0.3).
narrative_ontology:measurement_basis(ai_a_be_t2018, observed).
narrative_ontology:measurement(ai_a_be_t2020, ai_alignment_priority__integrated_reading, base_extractiveness, 2020, 0.36).
narrative_ontology:measurement_basis(ai_a_be_t2020, observed).
narrative_ontology:measurement(ai_a_be_t2022, ai_alignment_priority__integrated_reading, base_extractiveness, 2022, 0.42).
narrative_ontology:measurement_basis(ai_a_be_t2022, observed).
narrative_ontology:measurement(ai_a_be_t2024, ai_alignment_priority__integrated_reading, base_extractiveness, 2024, 0.46).
narrative_ontology:measurement_basis(ai_a_be_t2024, observed).
narrative_ontology:measurement(ai_a_be_t2026, ai_alignment_priority__integrated_reading, base_extractiveness, 2026, 0.48).
narrative_ontology:measurement_basis(ai_a_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2018, ai_alignment_priority__integrated_reading, suppression_requirement, 2018, 0.22).
narrative_ontology:measurement_basis(ai_a_su_t2018, observed).
narrative_ontology:measurement(ai_a_su_t2020, ai_alignment_priority__integrated_reading, suppression_requirement, 2020, 0.28).
narrative_ontology:measurement_basis(ai_a_su_t2020, observed).
narrative_ontology:measurement(ai_a_su_t2022, ai_alignment_priority__integrated_reading, suppression_requirement, 2022, 0.35).
narrative_ontology:measurement_basis(ai_a_su_t2022, observed).
narrative_ontology:measurement(ai_a_su_t2024, ai_alignment_priority__integrated_reading, suppression_requirement, 2024, 0.4).
narrative_ontology:measurement_basis(ai_a_su_t2024, observed).
narrative_ontology:measurement(ai_a_su_t2026, ai_alignment_priority__integrated_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(ai_a_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'AI alignment priorities' covers three structurally distinct claims with different victim sets and different epsilon locations. existential_risk_reading (victims: future populations; epsilon concentrated in capability-risk neglect), nearterm_harms_reading (victims: present marginalized communities; epsilon concentrated in deployment-harm neglect), and this integrated_reading (victim set split across both; moderate epsilon on each half, dual methodology, balanced allocation). Per the epsilon-invariance principle these are authored as separate files, not one story with a priority-ordering parameter; this file links both siblings via affects_constraints, and the upstream/downstream pressure edges are recorded in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
