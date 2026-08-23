% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
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
 *   human_readable: Integrated Reading of the AI Alignment Commitment: Control and Justice as Non-Exclusive Obligations
 *   domain: technology governance/ethics/risk assessment
 *
 * SUMMARY:
 *   The kernel 'AI alignment commitment' (the field's governing promise that
 *   advanced AI systems be made safe and beneficial) is read three ways. This
 *   file instantiates the integrated_reading: alignment requires simultaneous
 *   attention to control problems (loss of human oversight over capable
 *   systems) and justice problems (reproduction of social bias and
 *   present-day harm), as non-exclusive obligations. The standing arrangement
 *   under contest is the field's de facto siloed allocation of attention,
 *   funding, and legitimacy between a control-centered community and a
 *   justice-centered community. Assessed by this reading's own lights, that
 *   arrangement extracts from both protected constituencies at once: present
 *   marginalized populations absorb deployed-system harms while the field's
 *   center of gravity treats their redress as secondary to catastrophic risk,
 *   and future humanity absorbs unmitigated loss-of-control risk while
 *   present-harm urgency crowds out long-horizon work. The sibling readings
 *   are separate constraint files linked through network.affects_constraints;
 *   per the one-reading rule this file does not hedge or average across them.
 *   Claim/metric independence: claimed_type records my structural judgment
 *   (tangled_rope: genuine coordination performed inside both silos,
 *   asymmetric extraction maintained at the boundary); the metrics record the
 *   arrangement's observed operation; the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed type is the measurement the corpus exists to take. KEY
 *   AGENTS (by structural relationship): - present_marginalized_populations:
 *   primary target (powerless/trapped) — absorbs deferred present-day harms -
 *   future_humanity: primary target (powerless/trapped, civilizational
 *   horizon) — absorbs deferred catastrophic risk; no seat of its own -
 *   frontier_ai_labs: agenda-setter and beneficiary (institutional/arbitrage)
 *   — defines core alignment, harvests the dichotomy -
 *   specialized_safety_institutions: beneficiary
 *   (institutional/identity_locked) - specialized_fairness_institutions:
 *   beneficiary (institutional/identity_locked) -
 *   integrated_alignment_researchers: paying bridge actors
 *   (moderate/constrained) - data_annotation_workers: exposed payers outside
 *   the conversation (powerless/trapped) - alignment_funders: agenda-setters
 *   who drew the category boundaries (institutional/constrained) -
 *   ai_governance_scholars: analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.58).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Integrated Reading of the AI Alignment Commitment: Control and Justice as Non-Exclusive Obligations").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "technology governance/ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, '5cff7e95-5ca5-434f-9773-af7655958b5e').
narrative_ontology:cs_kernel_codification('5cff7e95-5ca5-434f-9773-af7655958b5e', distributed).
narrative_ontology:cs_authority_grounding('5cff7e95-5ca5-434f-9773-af7655958b5e', distributed).
narrative_ontology:cs_reading_relation('5cff7e95-5ca5-434f-9773-af7655958b5e', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('5cff7e95-5ca5-434f-9773-af7655958b5e', ai_alignment_commitment__ethics_justice_reading, influences).
narrative_ontology:cs_axiom('5cff7e95-5ca5-434f-9773-af7655958b5e', foundational, alignment_requires_simultaneous_control_and_justice).
narrative_ontology:cs_axiom_status(alignment_requires_simultaneous_control_and_justice, holdable).
narrative_ontology:cs_axiom_grounding('5cff7e95-5ca5-434f-9773-af7655958b5e', alignment_requires_simultaneous_control_and_justice, deontological).
narrative_ontology:cs_axiom('5cff7e95-5ca5-434f-9773-af7655958b5e', secondary, silo_fragmentation_degrades_both_fronts).
narrative_ontology:cs_axiom_status(silo_fragmentation_degrades_both_fronts, holdable).
narrative_ontology:cs_axiom_grounding('5cff7e95-5ca5-434f-9773-af7655958b5e', silo_fragmentation_degrades_both_fronts, empirically_contingent).
narrative_ontology:cs_reference_frame('5cff7e95-5ca5-434f-9773-af7655958b5e', unified_dual_front_alignment).
narrative_ontology:cs_drift_state('5cff7e95-5ca5-434f-9773-af7655958b5e', contemporary_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5cff7e95-5ca5-434f-9773-af7655958b5e', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, specialized_safety_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, specialized_fairness_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, data_annotation_workers).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__integrated_reading, protection_obligations_are_non_exclusive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research institutes, safety teams inside frontier labs, and long-horizon risk organizations whose missions, funding lines, and staff identities are built around preventing loss of control over advanced AI systems. They receive dedicated funding streams, attract talent through a distinct professional identity, and publish in venues that reward control-focused results. Proposals to fold justice obligations into their mandate are experienced internally as dilution of mission; leaving the silo would mean dismantling the identity the institution has become.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, specialized_safety_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Academic communities, audit firms, and civil-society labs organized around documenting and preventing biased and harmful outputs of deployed systems. They hold conferences, journals, consulting markets, and funding lines keyed to present-day harm. Their standing depends on the urgency of the present-harm frame; demands to prioritize long-horizon risk read internally as displacing concrete victims with hypothetical ones. Exit would mean surrendering the community's accumulated methods and the identity built on them.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, specialized_fairness_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Companies training and deploying frontier models. They set research agendas, define hiring priorities, and exercise outsized influence over what counts as core alignment work through the roles they fund and the problems they publish. The division of the field into separate control and justice camps serves them twice: it lets them present narrow technical safety work as fulfilling the alignment commitment while deferring justice remediation, and it lets them treat justice compliance as the relevant obligation while deferring precautionary restraint. They can shift posture between the camps as regulatory and public pressure moves, which no other seat can do.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, frontier_ai_labs, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, frontier_ai_labs, beneficiary).

% Communities that absorb the present-day harms of deployed systems: discriminatory lending, housing, hiring, policing, and benefits algorithms; degraded service quality; expanded surveillance. Their redress is chronically scheduled behind control-focused work on the argument that catastrophic risk dwarfs present injury. Their access to alignment prioritization runs through a small set of intermediary advocate organizations. They cannot exit the systems that harm them and hold no direct seat in the forums where alignment scope is decided.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, present_marginalized_populations, payer,
    powerless, biographical, trapped, global).

% People who will exist and inherit the consequences of present choices about capable AI systems, including the risk that such systems escape effective human control. They bear the cost of every deferral of long-horizon safety work justified by the urgency of present harms. They have no seat, no vote, and no intermediary that speaks with their own voice; their interests enter the conversation only as arguments made by others.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, future_humanity, excluded).

% Researchers and practitioners who work across both fronts: auditing deployed systems while building oversight methods, or constructing safety cases that incorporate distributional harms. They pay a career tax: review panels at siloed venues discount cross-front papers as insufficiently deep, hiring committees struggle to place them, and funding categories rarely match their portfolios. Their cross-trained expertise is portable only within the field, so exit means abandoning the integration project they exist to pursue.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers, payer,
    moderate, biographical, constrained, global).

% The workforce that labels training data and moderates outputs, often geographically dispersed and precariously employed. They experience the sharpest edge of present-day system harms and hold the least voice in any alignment prioritization forum; their conditions surface in the field mainly as research subjects rather than as participants.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, data_annotation_workers, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, data_annotation_workers, excluded).

% Philanthropic foundations, government programs, and corporate giving arms that allocate resources across the alignment landscape. The category architecture they chose, with separate programs for safety and for ethics, hardened the boundary between the camps, and their review panels reproduce it with every funding cycle. Redrawing the categories is within their power but carries portfolio upheaval, grantee-relationship costs, and an implicit admission of prior misallocation.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, alignment_funders, agenda_setter,
    institutional, generational, constrained, global).

% Academic and think-tank analysts who study the alignment field's structure, map the two-cultures division, and evaluate integration proposals. They collect no rents from the arrangement and bear none of its deferral costs; their seat is evaluative.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement organizes scarce expert attention, funding, and institutional capacity across two distinct protection problems, loss of control over capable systems and present-day distributive harm, allowing each community to develop depth, methods, benchmarks, and standards within its own domain.
% TRANSFER_FUNCTION: Moves attention, funding, talent, and agenda-setting authority toward whichever camp holds definitional power over alignment at a given moment, and moves deferral costs outward onto both protected constituencies: present marginalized populations absorb deployed-system harms while catastrophic-risk work is postponed, and future humanity absorbs unmitigated loss-of-control risk while present-harm urgency crowds out long-horizon work.
% ABSENT_VOICES: Future humanity has no seat and cannot acquire one; its interests appear only as arguments made by others. Present marginalized populations appear mainly through intermediary advocates rather than directly. Cross-silo integrated researchers are scattered with no convening institution. Deployment-facing workers such as data annotators and content moderators sit almost entirely outside alignment prioritization conversations.
% DISAPPEARANCE_RATIONALE: If the siloed allocation regime vanished overnight, funding categories, venue maps, hiring lines, and research agendas would reorganize around integrated programs; methods would pool across the two fronts, and both protection projects would gain from shared infrastructure. The named seats exist because the arrangement allocates something they contend for; its removal rearranges all of them.
% FOUNDING_PROBLEM: Early AI governance confronted two visibly different failure modes, systems causing present discriminatory harm and prospective systems escaping human control, and the field organized separately around each because the methods, timescales, and literatures differed.
% FOUNDING_PROBLEM_CORROBORATION: Both failure modes are documented from outside the benefiting parties: national AI advisory bodies and government risk assessments report on both present-harm incidents and loss-of-control concerns; peer-reviewed literature spanning both communities documents the costs of non-integration; testimony from deployment-affected communities documents the present harms; independent capability evaluations document the control risks. None of these corroborating sources sits inside either silo's beneficiary set.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.68: the siloed arrangement compounds deferral costs on both victim sets simultaneously, duplicates infrastructure and methods across communities working on overlapping systems, and leaves exploitable ambiguity that deployers harvest. It is not higher because both silos perform real, functioning coordination (depth, methods, benchmarks) that a purely extractive structure would not sustain. Suppression is authored at 0.58 and is a raw structural property, unscaled by power or scope: silo boundaries are maintained through funding-category design, venue prestige hierarchies, hiring specialization, and review-panel gatekeeping rather than legal coercion. Theater_ratio 0.45: a large share of nominally integrative activity (responsible-AI boards, multi-stakeholder statements, ethics charters) reallocates no attention; safety-washing and ethics-washing are the characteristic performances. Accessibility_collapse 0.40: alternatives remain partly available, as bridging organizations, cross-listed venues, and integrated governance frameworks demonstrate that exit from the dichotomy is possible, which keeps collapse moderate rather than high. Resistance 0.60: integration mandates meet organized pushback from both silo incumbencies, while integration coalitions supply counter-pressure. The measurement series run on one shared grid (t=0 approximates 2010, t=15 approximates 2025): extractiveness and theater rise monotonically as deployment scale and deferral costs compound; suppression_requirement rises through the boundary-hardening period (dedicated venue formation, institute founding, discourse conflicts around t=6-9) then softens slightly as integrated governance frameworks erode enforcement capacity. A suppression series is authored because this story specifically traces enforcement-capacity change, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From inside either silo, the division presents as epistemically necessary specialization: depth requires focus, and the other front's urgencies are someone else's mandate. From the victim seats, the same division presents as a deferral machine in which each front's priority is perpetually scheduled after the other's. From the bridge actors, it presents as a career tax levied on exactly the integration the field claims to want. The engine computes these divergent classifications from power, exit, and directional data; the divergence, not any single seat's verdict, is the finding. Coalition note: the two powerless victim classes lack independent leverage, but their interests align against the dichotomy itself, and the observed resistance vector is precisely that coalition, affected-community advocacy allied with long-horizon safety researchers pressing integrated mandates on funders.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. present_marginalized_populations, data_annotation_workers, and future_humanity are declared victims with trapped exits and no power above powerless, placing them near the full-target end; future_humanity sits at the extreme since no exit exists even in principle. specialized_safety_institutions and specialized_fairness_institutions are declared beneficiaries with identity_locked exits: their subsidy persists because their institutional selves are constituted by the silo boundary, damping their effective extraction toward or below zero (net subsidized). frontier_ai_labs combine the agenda-setter seat with a secondary beneficiary position and arbitrage-grade exit: they can re-position rhetorically as pressure shifts, so they derive nearest the beneficiary end despite bearing compliance and tail-risk costs. integrated_alignment_researchers are payers with constrained exit; their cross-trained expertise is portable only within the field, so they sit high on the target side. alignment_funders carry no direct beneficiary or victim declaration; their seat derives from administering the category structure rather than collecting from it. Spatial scope is global, so verification of deferral costs is hard and effective extraction scales up modestly for the target seats. Receipt: the arrangement's gains demonstrably accrue to frontier_ai_labs, which collect avoided justice-remediation costs and avoided precautionary restraint simultaneously; no other single seat captures comparable value, which is why gain_flow names that seat rather than asserting diffuseness.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the siloed regime as pure extraction would erase the genuine coordination both communities perform, the methods, benchmarks, and standards that make either front tractable; reading it as pure coordination would erase the deferral extraction that falls on constituencies with no seat. The hybrid preserves both facts: coordination function inside each silo, enforced asymmetry at the boundary. On genealogy: the founding problem, two distinct failure modes requiring organized response, is live, not dead, so this is not resolved mandatrophy; the arrangement persists because the problem persists. What has outrun the function is the form: mutual exclusivity was never required by the problem and now suppresses the unified effort the problem increasingly demands. Fixing is prohibitive for the seats that could fix it: funders and lab leadership would pay identity dissolution, portfolio upheaval, and admitted misallocation far exceeding what each bears of the status quo, which is why the boundary apparatus persists despite broad nominal endorsement of integration. If integration coalitions succeed in redrawing funding categories and venue maps, expect the silo-boundary apparatus to become transitional support with a natural sunset, at which point the operative discipline migrates from boundary-maintenance to transition management.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the integrated_reading of kernel ai_alignment_commitment; how would classification shift if a sibling reading governed the same commitment?',
    'Compare against the sibling files (ai_alignment_commitment__safety_control_reading, ai_alignment_commitment__ethics_justice_reading): observe which victim set each declares, and track which reading''s framing the field''s operative standards (funding criteria, evaluation benchmarks, regulation) adopt over time.',
    'Under safety_control_reading the victim set collapses to future humanity and present-day harms become out-of-scope, concentrating extraction differently; under ethics_justice_reading the victim set collapses to present marginalized populations and catastrophic risk becomes speculative. The integrated reading is the only one whose victim set spans both, and its extraction claim targets the silo boundary itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the alignment-commitment kernel; sibling readings instantiate different constraints with different victim sets and epsilon values.').

omega_variable(
    future_generation_standing,
    'Does deferring catastrophic-risk mitigation constitute extraction from future humanity, or is temporal discounting of not-yet-existing parties a legitimate prioritization among present claimants?',
    'Normative analysis of intergenerational standing combined with revealed-preference evidence: whether institutions treat long-horizon risk as an owed duty or as optional charity, for example whether deferral decisions are audited against duties or merely budgeted as discretionary spend.',
    'If discounting is legitimate, epsilon drops materially and the arrangement reads as ordinary prioritization among present claimants; if future persons hold standing, the deferral is extraction and the victim declaration stands at full weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generation_standing, preference, 'Whether the future-humanity half of the victim set survives scrutiny of intergenerational standing.').

omega_variable(
    silo_boundary_enforcement_mechanism,
    'Is the suppression keeping alignment work siloed structural (funding categories, venue economics, hiring pipelines) or internalized (researcher identity fusion with silo membership, self-censorship of cross-front work)?',
    'Post-exit trajectory of cross-silo movers: if barriers persist after a researcher leaves silo institutions and funding is available, the residual is internalized; if barriers vanish with structural change such as category reform, suppression is structural.',
    'An internalized component raises effective suppression above the structural measure and predicts slow decay of silo boundaries even after funding reform; a purely structural component predicts rapid response to category redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silo_boundary_enforcement_mechanism, empirical, 'Structural versus internalized mechanism sustaining the silo boundary.').

omega_variable(
    integration_feasibility_tradeoff,
    'Is simultaneous attention to control and justice problems feasible at current expert capacity, or does integration impose a genuine depth-for-breadth tradeoff?',
    'Outcome comparison of integrated programs versus siloed ones on both fronts (method-transfer rates, harm-reduction outcomes, risk-mitigation milestones), including natural experiments from institutions that merged teams or funded dual-mandate roles.',
    'If a genuine tradeoff exists, part of the measured extraction is the irreducible price of coverage and the coordination floor rises; if integration is net-feasible, siloing is closer to pure rent and the authored epsilon is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_feasibility_tradeoff, empirical, 'Whether the integrated mandate is capacity-feasible or trades off against depth on both fronts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aic_integrated_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(aic_integrated_tr_t0, observed).
narrative_ontology:measurement(aic_integrated_tr_t3, ai_alignment_commitment__integrated_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement_basis(aic_integrated_tr_t3, observed).
narrative_ontology:measurement(aic_integrated_tr_t6, ai_alignment_commitment__integrated_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement_basis(aic_integrated_tr_t6, observed).
narrative_ontology:measurement(aic_integrated_tr_t9, ai_alignment_commitment__integrated_reading, theater_ratio, 9, 0.4).
narrative_ontology:measurement_basis(aic_integrated_tr_t9, observed).
narrative_ontology:measurement(aic_integrated_tr_t12, ai_alignment_commitment__integrated_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement_basis(aic_integrated_tr_t12, observed).
narrative_ontology:measurement(aic_integrated_tr_t15, ai_alignment_commitment__integrated_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(aic_integrated_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(aic_integrated_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(aic_integrated_be_t0, observed).
narrative_ontology:measurement(aic_integrated_be_t3, ai_alignment_commitment__integrated_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement_basis(aic_integrated_be_t3, observed).
narrative_ontology:measurement(aic_integrated_be_t6, ai_alignment_commitment__integrated_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(aic_integrated_be_t6, observed).
narrative_ontology:measurement(aic_integrated_be_t9, ai_alignment_commitment__integrated_reading, base_extractiveness, 9, 0.64).
narrative_ontology:measurement_basis(aic_integrated_be_t9, observed).
narrative_ontology:measurement(aic_integrated_be_t12, ai_alignment_commitment__integrated_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(aic_integrated_be_t12, observed).
narrative_ontology:measurement(aic_integrated_be_t15, ai_alignment_commitment__integrated_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(aic_integrated_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(aic_integrated_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(aic_integrated_su_t0, observed).
narrative_ontology:measurement(aic_integrated_su_t3, ai_alignment_commitment__integrated_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement_basis(aic_integrated_su_t3, observed).
narrative_ontology:measurement(aic_integrated_su_t6, ai_alignment_commitment__integrated_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(aic_integrated_su_t6, observed).
narrative_ontology:measurement(aic_integrated_su_t9, ai_alignment_commitment__integrated_reading, suppression_requirement, 9, 0.61).
narrative_ontology:measurement_basis(aic_integrated_su_t9, observed).
narrative_ontology:measurement(aic_integrated_su_t12, ai_alignment_commitment__integrated_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(aic_integrated_su_t12, observed).
narrative_ontology:measurement(aic_integrated_su_t15, ai_alignment_commitment__integrated_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(aic_integrated_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__ethics_justice_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the ai_alignment_commitment kernel per the epsilon-invariance principle: the colloquial label 'AI alignment' conflates three structurally distinct claims (control-only, justice-only, integrated), each with its own victim set, epsilon, and stakeholder surface. This file is the integrated member. The linkage runs through shared subject matter rather than evidential dependency: each exclusive reading cites the reality of its own front against the other, while the integrated reading cites the fragmentation costs visible only when both fronts are held together. Classification-relevant deltas: the safety sibling's victim set is future humanity alone; the justice sibling's is present marginalized populations alone; this reading's spans both, and its extraction claim targets the silo boundary itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
