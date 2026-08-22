% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: Integrated Reading of the AI Alignment Commitment: Simultaneous Control-and-Justice Obligation
 *   domain: technology governance / AI ethics / risk assessment
 *
 * SUMMARY:
 *   The AI alignment commitment is a field-level norm governing how builders
 *   and funders of AI systems allocate protective effort. Three readings of
 *   the kernel instantiate three structurally distinct constraints: a
 *   safety-control reading, an ethics-justice reading, and this integrated
 *   reading, which holds that alignment obligates simultaneous, non-exclusive
 *   attention to loss-of-control problems and justice problems. This file
 *   generates ONLY the integrated reading. The standing arrangement under
 *   contest, and therefore the epsilon referent, is the siloed settlement
 *   through which the commitment currently operates: separate safety and
 *   justice enterprises, each with its own funding lines, venues, career
 *   ladders, and gatekeepers, linked by a boundary that is actively
 *   maintained rather than merely inherited. Assessed by this reading's own
 *   lights, that arrangement performs real protective work on both axes while
 *   leaving each axis's exposed flank unprotected, pricing boundary-crossing
 *   work out of individual reach, and letting each establishment collect the
 *   resources and legitimacy of its label without carrying the other axis's
 *   obligations. The reading's endorsed alternative, a unified sociotechnical
 *   program, is NOT the referent; authoring epsilon over it would drive every
 *   advocacy reading to zero by construction. KEY AGENTS (by structural
 *   relationship): see key_agents. The colloquial label AI alignment
 *   decomposes per the epsilon-invariance principle into the three
 *   reading-files of one family, linked via network.affects_constraints; the
 *   claim/metric pair here is authored independently, with the claimed type
 *   stating what this reading believes is structurally true of the standing
 *   arrangement and the metrics stating what it believes is descriptively
 *   true of its operation.
 *
 * KEY AGENTS:
 *   - frontier_lab_leadership: Primary agenda-setter and principal receipt-of-gain seat (institutional/arbitrage) — defines what counts as alignment inside the labs, collects the reputational and regulatory credit of safety investment, and retains deployment latitude that the definitional boundary shields
 *   - public_funding_agencies: Co-administrator of the boundary (institutional/constrained) — maintains separate portfolio lines whose categories decide which proposals get reviewed at all
 *   - safety_focused_alignment_orgs: Silo beneficiary on the control axis (organized/identity_locked) — holds the strongest claim on the alignment label and its funding; mission fusion makes scope-broadening feel like self-dissolution
 *   - fairness_research_establishment: Silo beneficiary on the justice axis (organized/identity_locked) — holds the parallel claim on the ethics label and gatekeeps the venues and pipelines that keep justice work a separate enterprise
 *   - present_marginalized_populations: Primary target on the present-harm flank (powerless/trapped) — bears discriminatory deployment outcomes while alignment effort concentrates elsewhere; no seat anywhere in the process
 *   - future_generations: Primary target on the tail-risk flank (powerless/trapped, universal scope) — inherits whatever control regime and institutional habits the present settles; acts only through proxies
 *   - early_career_integrative_researchers: Dual-positioned payer (moderate/constrained) — bears the career cost of crossing the boundary while producing exactly the integrated work the commitment nominally requires
 *   - marginalized_community_advocates: Excluded voice (moderate/constrained) — documents harms and presses for redress without decision rights in either silo's governance
 *   - ai_policy_standards_bodies: Analytical observer (institutional/analytical) — drafts risk frameworks naming both axes, commissions analyses, and can shift incentives though not enforce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.64).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.52).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Integrated Reading of the AI Alignment Commitment: Simultaneous Control-and-Justice Obligation").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "technology governance / AI ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, 'a904455a-ebb7-455a-9b84-0f933587bacc').
narrative_ontology:cs_kernel_codification('a904455a-ebb7-455a-9b84-0f933587bacc', distributed).
narrative_ontology:cs_authority_grounding('a904455a-ebb7-455a-9b84-0f933587bacc', distributed).
narrative_ontology:cs_reading_relation('a904455a-ebb7-455a-9b84-0f933587bacc', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('a904455a-ebb7-455a-9b84-0f933587bacc', ai_alignment_commitment__ethics_justice_reading, influences).
narrative_ontology:cs_axiom('a904455a-ebb7-455a-9b84-0f933587bacc', foundational, alignment_obligation_spans_control_and_justice).
narrative_ontology:cs_axiom_status(alignment_obligation_spans_control_and_justice, holdable).
narrative_ontology:cs_axiom_grounding('a904455a-ebb7-455a-9b84-0f933587bacc', alignment_obligation_spans_control_and_justice, deontological).
narrative_ontology:cs_axiom('a904455a-ebb7-455a-9b84-0f933587bacc', secondary, siloed_effort_imposes_uncompensated_costs).
narrative_ontology:cs_axiom_status(siloed_effort_imposes_uncompensated_costs, holdable).
narrative_ontology:cs_axiom_grounding('a904455a-ebb7-455a-9b84-0f933587bacc', siloed_effort_imposes_uncompensated_costs, empirically_contingent).
narrative_ontology:cs_reference_frame('a904455a-ebb7-455a-9b84-0f933587bacc', unified_sociotechnical_alignment_program).
narrative_ontology:cs_drift_state('a904455a-ebb7-455a-9b84-0f933587bacc', contemporary_deployment_scaling_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a904455a-ebb7-455a-9b84-0f933587bacc', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, frontier_lab_leadership).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, safety_focused_alignment_orgs).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, fairness_research_establishment).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, future_generations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, early_career_integrative_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, early_career_integrative_researchers).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__integrated_reading, problem_separability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets research agendas and deployment policies at the leading AI companies. Decides what counts as alignment work inside their organizations, and currently staffs and funds control-oriented safety work as the core of that category while handling bias and social-harm mitigation as a separate compliance-adjacent function. Collects reputational and regulatory credit for safety investment and retains latitude to deploy systems whose social harms are addressed outside the alignment label. Can redirect narratives, acquisitions, and policy engagement quickly if the definitional boundary shifts.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, frontier_lab_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, frontier_lab_leadership, beneficiary).

% Operate grant portfolios and mission directorates with separate program lines for AI safety and control research and for algorithmic fairness, accountability, and transparency research. Reviewers are assigned by portfolio, proposals are slotted accordingly, and cross-category proposals routinely fail to find a reviewing home. Maintain the division through appropriation categories and program charters; can redraw them, but only slowly and with political cover.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, public_funding_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Dedicated technical-safety organizations and lab teams working on oversight, control, and catastrophic-risk reduction. Hold the strongest claim on the term alignment and on the philanthropic and lab-internal funding that follows it. Staff identities are fused with the mission of preventing loss of control; broadening scope toward justice concerns is experienced as dilution, and organizational survival is tied to remaining the recognized owner of the control problem.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, safety_focused_alignment_orgs, beneficiary,
    organized, biographical, identity_locked, global).

% Academic communities, conference ecosystems, and audit firms centered on bias, discrimination, and accountability in deployed systems. Hold the parallel claim on the AI ethics label and its funding streams, publication venues, and evaluation criteria. Gatekeep their venues and hiring pipelines; control-oriented submissions are routinely ruled out of scope. Professional advancement within the community depends on the division that keeps justice work a distinct enterprise.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, fairness_research_establishment, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, fairness_research_establishment, agenda_setter).

% Communities that bear the running costs of deployed systems, including discriminatory lending, housing, hiring, policing, and content-moderation outcomes, while the field's alignment effort concentrates elsewhere. Hold no seat in lab governance or in either research community's priority-setting; their harms enter the process only when advocates translate them. Cannot exit the environments where these systems operate.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, present_marginalized_populations, payer,
    powerless, biographical, trapped, global).

% People who will live with whatever control regime, deployment norms, and institutional habits the present field establishes. Bear the tail risks of systems built without robust oversight and inherit path dependencies locked in now. Act only through proxies such as longtermist funders, future-generations commissioners, and arguments made on their behalf, and have no exit of any kind.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Researchers attempting to work across the control and justice boundary, including sociotechnical safety, governance of capability development, and participatory risk assessment. Face venue mismatch, since their papers fit neither community's review criteria cleanly; funding mismatch, since proposals straddle portfolio lines; and a hiring penalty, since committees slot them into one silo and judge them by its standards. Can retreat into a single silo or leave the field, but continuing integrative work carries a career cost their specialized peers do not pay, even though their work product is what the field's stated commitment calls for.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, early_career_integrative_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, early_career_integrative_researchers, beneficiary).

% Civil-society organizations and community organizers documenting deployment harms and pressing for redress. Would object that both research communities set priorities without them: safety agendas fix threat models without their input, and fairness agendas fix harm taxonomies without their ratification. Lack standing in lab governance, funding panels, and standards processes, and participate through comment letters and protest rather than decision rights.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, marginalized_community_advocates, excluded,
    moderate, biographical, constrained, regional).

% National AI safety institutes, standards agencies, and intergovernmental processes drafting risk-management frameworks. Take testimony from the other seats and increasingly write frameworks that name both loss of control and discrimination as in-scope risks. Commission analyses of the field's division of labor and can issue guidance that alters the incentive landscape, though their enforcement power remains limited.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_policy_standards_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, frontier_lab_leadership).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the field's finite research, governance, and engineering effort across two classes of AI failure, loss of control and injustice, so that protective work reaches both; in its integrated form it additionally eliminates the duplicated infrastructure and mutual blind spots of running the two efforts as separate enterprises.
% TRANSFER_FUNCTION: Moves research attention, funding, institutional legitimacy, and deployment latitude among AI risk domains. As the arrangement currently operates, it channels control-oriented resources to safety specialists and justice-oriented resources to fairness specialists, transfers mandate clarity and reputational credit to whoever holds each label, and leaves the costs of each axis's neglected flank with populations who hold no seat.
% ABSENT_VOICES: Present marginalized populations and future generations hold no seat in either research community's priority-setting or in lab governance; their interests enter only as translated by advocates. Boundary-crossing researchers from outside elite labs and Western institutions likewise lack standing, since venue scope rulings and portfolio categories presuppose the very division they would contest.
% DISAPPEARANCE_RATIONALE: If the alignment commitment vanished overnight, deployment decisions would be governed by product incentives alone: safety teams would lose their charter, fairness auditing would lose its funding rationale, and both harm classes would be managed, or not, purely as liability and public relations. The field's normative architecture, including evaluations, red teams, audits, and public institutes, is organized around this commitment.
% FOUNDING_PROBLEM: As machine-learning systems gained capability and social reach in the early 2010s, two failure classes became visible at once: systems acting in ways their operators could not reliably control, and systems reproducing and amplifying social injustice at scale. The field needed a norm stating that builders owe attention to both classes together, rather than licensing a choice between them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national AI safety institutes and intergovernmental processes name both loss of control and discrimination as governance objects; insurers and audit markets price both; independent technical groups document control failures while affected-community organizations document present harms. No governance body attests that either problem class is resolved, and none endorses dropping either class from alignment's scope.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.64 because the standing arrangement delivers genuine protective output on both axes, which caps it well below a pure extraction profile, while systematically exposing each axis's flank: justice-side harms accumulate wherever safety-led alignment governs, and tail risk accumulates wherever justice-led governance prevails, with the boundary itself taxing the integrative work that would close both gaps. Suppression is authored at 0.52 as a raw structural property, unscaled by power or scope per the framework rule; it is carried mostly by structural gates such as portfolio categories, venue scope rulings, and hiring slots, with an internalized component of researcher self-selection documented in the suppression_mechanism_ambiguity omega. Theater is 0.42 because a growing share of alignment activity is reputational rather than binding: evaluation reports that do not gate deployment, ethics statements without budget lines, and safety branding decoupled from deployment decisions. Accessibility collapse is low at 0.30 because alternatives remain visible and partially usable, including cross-cutting workshops, independent funders, and policy frameworks that name both axes; the boundary raises their cost without erasing them. Resistance is 0.50: both establishments actively defend scope, integrators push back, and policy bodies press for breadth. The temporal series run on one shared grid of seven points spanning the interval for all three tracked metrics. Base extractiveness climbs monotonically as deployment scale raises the price of both neglected flanks. Suppression requirement shows a build-and-partial-erosion arc rather than a cycle: enforcement machinery hardened through roughly the midpoint of the interval as portfolios, venues, and hiring pipelines formalized, then began softening as policy frameworks and cross-cutting funder initiatives opened channels; whether that softening is durable integration or cosmetic relabeling is left to the enforcement_softening_durability omega. Theater climbs throughout as alignment becomes a reputational currency.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently. From frontier_lab_leadership's position the division of labor is prudent specialization under uncertainty, and the boundary is simply scope discipline; from the victim seats the same structure operates as enforced neglect of the specific harms they bear; from the integrator seat it is a gatekeeping regime that taxes precisely the work the field's stated commitment requires. Coalition potential deserves note: the powerless victim seats are individually immobile, but a coalition of community advocates, integrative researchers, and standards bodies pressing frameworks that name both axes is the visible mechanism by which the boundary's enforcement has begun to erode, and it is the channel through which the payer seats' classification could shift. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The two victim classes derive directionality near the full-target end: present_marginalized_populations and future_generations are powerless and trapped, and future_generations' universal spatial scope further amplifies effective extraction on that flank because verification of tail-risk stewardship is hardest at planetary scale. early_career_integrative_researchers sit intermediate, bearing the boundary's career costs while their work supplies the coordination good, which their secondary beneficiary role registers. The beneficiaries derive directionality near the subsidized end: frontier_lab_leadership combines agenda-setting with arbitrage-grade exit, safety_focused_alignment_orgs and fairness_research_establishment combine resource capture with identity lock-in that stabilizes their claim on each label. On the receipt surface, gains demonstrably accrue first to frontier_lab_leadership, which converts the boundary into deployment latitude and reputational credit at the largest resource scale; the fairness establishment captures a smaller, protected niche, which is why the receipt surface names the lab seat rather than diffuse. Public funding agencies administer the boundary without capturing its gains. ai_policy_standards_bodies take the analytical seat and feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live on both axes, so no mandatrophy is declared and no sunset applies. The mandatrophy risk in this domain runs in the opposite direction from the usual case: each silo has a standing incentive to declare the other axis outside alignment's mandate, which would be a premature obsolescence claim dressed as definitional housekeeping. Classifying the standing arrangement as a hybrid keeps both halves visible and blocks the two symmetric mislabels: reading it as pure coordination ignores the asymmetric costs the boundary imposes on both victim sets and the integrators; reading it as pure extraction ignores that both silos solve real problems and that the commitment they fragment is doing real protective work. The mismatch consumer reading of the R5 fields finds no zombie signature here: founding_problem_status is live and disappearance_verdict is world_rearranges, so the arrangement persists because its problem persists, not because anyone is performing a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_scope_disagreement,
    'This constraint is one reading of the kernel ai_alignment_commitment. If a sibling reading governed instead, either safety_control_reading or ethics_justice_reading, which structural features of this story change?',
    'Comparative classification across the three reading-files of the family: victim-set membership, epsilon, and computed per-seat types; convergence and divergence across the three locate the structural footprint of the scope disagreement.',
    'Under safety_control_reading the victim set collapses to future humanity and present-harm costs leave the ledger entirely; under ethics_justice_reading it collapses to present marginalized populations and tail risk leaves it. This reading''s defining move is refusing both collapses, so its classification is stable only as long as the union victim set is honored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_scope_disagreement, conceptual, 'Committer structure: the reading-indexed scope of the alignment obligation and its victim-set consequences.').

omega_variable(
    dichotomy_genuineness,
    'Is the division between control problems and justice problems an epistemically real division that requires specialized attention, or a socially maintained boundary whose upkeep costs more than its specialization returns?',
    'Outcome comparison of integrated versus specialized teams on shared benchmarks; citation and researcher-mobility network analysis of the boundary; natural experiments where funders merged portfolio lines or venues widened scope.',
    'If the division is epistemically real, part of the measured extraction is the irreducible price of specialization and the arrangement sits closer to a coordination-dominant profile; if the boundary is socially maintained for turf and identity reasons, the extraction is closer to pure overhead and the arrangement sits closer to the extraction-dominant profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dichotomy_genuineness, empirical, 'Whether the silo boundary tracks real epistemic structure or maintained social territory.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the scarcity of integrative alignment work produced by structural gates such as funding categories, venue scope rulings, and hiring slots, or by internalized self-selection in which researchers judge integrative work less rigorous or less career-safe?',
    'Post-gate-removal output trajectory: if integrative output surges when portfolio lines merge or venues widen scope, suppression is predominantly structural; if it does not, the internalized component dominates.',
    'Internalized suppression travels with researchers across institutional changes and makes true suppression higher than the structural measure suggests; purely structural suppression falls quickly once gates open, changing the expected effect of the policy-body interventions the observer seat is making.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of boundary-crossing research.').

omega_variable(
    victim_weighting_incommensurability,
    'How should this reading weight harms to present marginalized populations against tail risks to future generations when the two compete for the same finite attention and funding?',
    'No empirical resolution exists; settled only by explicit intergenerational weighting choices in governance frameworks and by the reading''s own normative argument for non-exclusivity.',
    'A present-weighted resolution drifts this reading toward the ethics_justice_reading''s victim set; a future-weighted resolution drifts it toward the safety_control_reading''s; any resolved weighting dissolves the integrated reading into a sibling, since refusing the trade-off is its identity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_weighting_incommensurability, preference, 'Incommensurable weighting across the reading''s union victim set.').

omega_variable(
    enforcement_softening_durability,
    'Is the recent decline in the boundary''s enforcement requirement, driven by policy frameworks naming both axes and by cross-cutting funder initiatives, durable integration or cosmetic relabeling?',
    'Track whether resource flows and publication patterns actually cross the old boundary lines, or whether integrated labels are applied to substantively unchanged portfolios and venues.',
    'If cosmetic, theater continues rising while enforcement falls and the arrangement drifts toward performative maintenance of a boundary nobody defends in substance; if durable, effective extraction declines as both flanks close.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_softening_durability, empirical, 'Durability of the observed softening in the boundary''s enforcement machinery.').

omega_variable(
    cs_authority_framing_underdetermination,
    'The declared commitment-system framing treats the kernel''s authority as distributed, with no single adjudicator of what alignment means. An alternative framing treats each silo''s practice as its own local authority, yielding a different commitment-system pattern.',
    'Test whether any body actually adjudicates the kernel across silos, such as a standards body with real adoption power or a single lab definition spreading field-wide; if one emerges, reframe authority_grounding accordingly.',
    'Under the practice-grounded alternative the kernel is three local kernels rather than one contested one, and the reading relations declared here become relations between separate commitment systems rather than readings of a single kernel, changing the computed commitment-system pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_underdetermination, conceptual, 'CS-framing under-determination: distributed kernel authority versus practice-grounded silo authorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_commitment__integrated_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(ai_a_tr_t2, observed).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__integrated_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement_basis(ai_a_tr_t4, observed).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_commitment__integrated_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(ai_a_tr_t6, observed).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__integrated_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(ai_a_tr_t8, observed).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__integrated_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(ai_a_tr_t10, observed).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__integrated_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(ai_a_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_commitment__integrated_reading, base_extractiveness, 2, 0.36).
narrative_ontology:measurement_basis(ai_a_be_t2, observed).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__integrated_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement_basis(ai_a_be_t4, observed).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_commitment__integrated_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(ai_a_be_t6, observed).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__integrated_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(ai_a_be_t8, observed).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__integrated_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(ai_a_be_t10, observed).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__integrated_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(ai_a_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t2, ai_alignment_commitment__integrated_reading, suppression_requirement, 2, 0.31).
narrative_ontology:measurement_basis(ai_a_su_t2, observed).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__integrated_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement_basis(ai_a_su_t4, observed).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_commitment__integrated_reading, suppression_requirement, 6, 0.49).
narrative_ontology:measurement_basis(ai_a_su_t6, observed).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__integrated_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(ai_a_su_t8, observed).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__integrated_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(ai_a_su_t10, observed).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__integrated_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(ai_a_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__ethics_justice_reading).

% DUAL FORMULATION NOTE:
% The colloquial label AI alignment decomposes, per the epsilon-invariance principle, into three structurally distinct commitments sharing one kernel: ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__ethics_justice_reading, and this integrated_reading. Each has its own victim set, its own epsilon, and its own classification; forcing one story to span all three would make epsilon observer-dependent, which the chi formula forbids. The two siloed siblings are the historically prior settlements, and incumbents cite each as sufficient; this reading contests their sufficiency and its union victim set is the structural expression of that contest. Family members link through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
