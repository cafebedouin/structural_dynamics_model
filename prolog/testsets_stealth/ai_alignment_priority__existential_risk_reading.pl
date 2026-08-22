% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: Existential-Risk-First Alignment Priority (Loss-of-Control Reading)
 *   domain: technology governance / research ethics
 *
 * SUMMARY:
 *   Over roughly 2012-2026 the center of gravity of AI safety consolidated
 *   around a single framing: alignment means preventing catastrophic loss of
 *   control over advanced AI systems, and existential safety takes lexical
 *   priority over every other safety concern. The arrangement this story
 *   describes is that priority structure in operation — the funding lines,
 *   career ladders, venue hierarchies, and evaluation practices that route
 *   money, talent, and moral legitimacy toward speculative-risk research and,
 *   through the claim that safe advanced AI requires building advanced AI,
 *   toward frontier capability development itself. Documented present-day
 *   harms of deployed systems are formally acknowledged but subordinated;
 *   researchers in deployment-ethics traditions lost grant share and venue
 *   standing across the interval. This file is one member of a three-story
 *   constraint family decomposing the kernel of what alignment means and what
 *   gets priority; the sibling files instantiate the near-term-harms and
 *   integrated readings and author different epsilon values over their own
 *   arrangements — the decomposition exists because the colloquial label
 *   'alignment' covers structurally distinct claims (epsilon-invariance).
 *
 * KEY AGENTS:
 *   - - xrisk_research_labs: Agenda setter (institutional/identity_locked) — defines what counts as alignment work and administers field-wide evaluation
 *   - - longtermist_funders: Primary beneficiary (powerful/arbitrage) — supplies conditioned funding, collects agenda influence
 *   - - frontier_capability_labs: Dual-positioned beneficiary/payer (institutional/arbitrage) — pays for safety teams, receives the framing's material returns
 *   - - junior_safety_researchers: Beneficiary with identity-locked exit (moderate/identity_locked) — careers fused with the agenda
 *   - - nearterm_harm_affected_populations: Primary target (powerless/trapped) — bears deprioritized present harms
 *   - - deployment_ethics_researchers: Secondary target (organized/constrained) — lost resources and standing
 *   - - general_public: Excluded diffuse bearer (powerless/trapped) — opportunity costs without representation
 *   - - interdisciplinary_governance_scholars: Analytical observer (analytical/analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.7).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.6).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential-Risk-First Alignment Priority (Loss-of-Control Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "technology governance / research ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, 'd2877485-f08c-4979-9155-e6a9c6b42945').
narrative_ontology:cs_kernel_codification('d2877485-f08c-4979-9155-e6a9c6b42945', distributed).
narrative_ontology:cs_authority_grounding('d2877485-f08c-4979-9155-e6a9c6b42945', expertise).
narrative_ontology:cs_interpretation_layer_present('d2877485-f08c-4979-9155-e6a9c6b42945').
narrative_ontology:cs_reading_relation('d2877485-f08c-4979-9155-e6a9c6b42945', ai_alignment_priority__nearterm_harms_reading, forecloses).
narrative_ontology:cs_reading_relation('d2877485-f08c-4979-9155-e6a9c6b42945', ai_alignment_priority__integrated_reading, forecloses).
narrative_ontology:cs_axiom('d2877485-f08c-4979-9155-e6a9c6b42945', foundational, loss_of_control_is_defining_alignment_problem).
narrative_ontology:cs_axiom_status(loss_of_control_is_defining_alignment_problem, holdable).
narrative_ontology:cs_axiom_grounding('d2877485-f08c-4979-9155-e6a9c6b42945', loss_of_control_is_defining_alignment_problem, empirically_contingent).
narrative_ontology:cs_axiom('d2877485-f08c-4979-9155-e6a9c6b42945', foundational, future_humanity_stakes_dominate_present_distribution).
narrative_ontology:cs_axiom_status(future_humanity_stakes_dominate_present_distribution, holdable).
narrative_ontology:cs_axiom_grounding('d2877485-f08c-4979-9155-e6a9c6b42945', future_humanity_stakes_dominate_present_distribution, deontological).
narrative_ontology:cs_reference_frame('d2877485-f08c-4979-9155-e6a9c6b42945', existential_control_lexical_priority).
narrative_ontology:cs_drift_state('d2877485-f08c-4979-9155-e6a9c6b42945', contemporary_deployment_harm_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d2877485-f08c-4979-9155-e6a9c6b42945', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, xrisk_research_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, frontier_capability_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, junior_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_harm_affected_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, deployment_ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, frontier_capability_labs).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, instrumental_convergence_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, longtermist_moral_weight_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define what counts as alignment work: publish the field's research agendas, run the benchmark evaluations, and staff the review panels that decide which safety problems receive attention and funding. Senior careers, reputations, and institutional identities are built on the claim that this is the most important problem facing humanity; leaving the agenda would mean abandoning that identity mid-career.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, xrisk_research_labs, agenda_setter,
    institutional, generational, identity_locked, global).

% Grant-making foundations and donors supplying the majority of dedicated alignment funding, conditioning grants on existential-risk framing. Capital is fully mobile across causes within a quarter, and overlapping board seats across labs and think tanks give agenda influence disproportionate to headcount.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, arbitrage, global).

% Build and scale the most capable AI systems. They fund internal safety teams and endorse the existential-risk framing, which secures talent, regulatory goodwill, and investor patience during rapid scaling. They pay for the safety apparatus they endorse, while the framing's headline claim — that safe advanced AI requires building advanced AI — directs the field's resources toward their core activity.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, frontier_capability_labs, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, frontier_capability_labs, payer).

% Early-career scientists whose fellowships, residencies, and first publications sit almost entirely inside the existential-risk pipeline. Training, citation networks, and job prospects are formed by the agenda; moving to adjacent fields means restarting credential accumulation mid-career regardless of private doubts.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, junior_safety_researchers, beneficiary,
    moderate, biographical, identity_locked, global).

% People subject to deployed AI systems today — loan screening, hiring filters, welfare eligibility automation, predictive policing — whose documented harms are ranked below speculative future risks in funding and attention. They did not consent to the trade-off, are rarely present where priorities are set, and cannot exit the systems that affect them.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_harm_affected_populations, payer,
    powerless, immediate, trapped, global).

% Researchers in fairness, accountability, and deployment-harm traditions who produced much of the field's earlier safety knowledge. Grant lines and top-venue attention shifted toward existential-risk topics over the decade; many continue working with shrinking resources, and some moved into industry compliance roles.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, deployment_ethics_researchers, payer,
    organized, biographical, constrained, global).

% Bear the arrangement's diffuse costs — public research subsidies, policy attention, and the opportunity cost of a decade of safety capacity concentrated on scenarios distant from deployed systems. They are not seated in lab governance, funder strategy, or agenda-setting workshops, and learn of priority decisions after they are made.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, general_public, excluded,
    powerless, biographical, trapped, global).

% Academic observers of AI governance who study how safety priorities are set, publish comparative analyses of the framing's effects, and testify to policy bodies. They hold no stake in the agenda's continuation and can adopt whichever framing the evidence supports.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, interdisciplinary_governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, frontier_capability_labs).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: preventing loss of control over increasingly capable systems is a global public good no single lab captures; a shared technical agenda (evaluations, interpretability, control methods) lets scattered safety effort compound instead of duplicating.
% TRANSFER_FUNCTION: Moves money, talent, publication slots, and moral legitimacy from present-harm remediation and alternative safety programs toward frontier capability development and speculative-risk research; moves deference from deployment-affected publics to a small group claiming unique insight into future system behavior.
% ABSENT_VOICES: Deployment-affected communities, data workers, and global-South populations bearing labeling and moderation labor are absent from agenda-setting venues; future generations are present only through self-appointed proxies; dissenting safety traditions attend but with reduced standing.
% DISAPPEARANCE_RATIONALE: If the existential-risk-first priority vanished overnight, funding would redistribute toward deployment harms and plural safety agendas, capability labs would lose the safety license that currently accompanies scaling, career pipelines would re-route, and the field's attention would re-anchor on documented present harms — the AI safety landscape would reorganize within a few grant cycles.
% FOUNDING_PROBLEM: The prospect that advanced AI systems could escape human control and cause irreversible catastrophe — a coordination failure among labs racing ahead without safety guarantees.
% FOUNDING_PROBLEM_CORROBORATION: International AI-safety report processes and national risk assessments — bodies outside the funding ecosystem — corroborate that loss-of-control risk is a real object of concern, and published surveys show ML researchers critical of the framing nonetheless acknowledging tail risk. No external body attests the priority ranking itself: the ordering of existential safety above present harms is attested almost exclusively by the agenda's beneficiaries, and that gap is itself signal.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.70 is authored from this reading's own lights applied to the standing arrangement it governs: even granting the reading's values — that catastrophe-prevention is paramount — the arrangement's operation routes a growing share of safety resources toward capability scaling under a safety license, defers documented present harms indefinitely, and concentrates interpretive authority in a small group whose claims about speculative future system behavior are not externally auditable. It is not higher because the coordination function is genuine: evaluations, interpretability, and control research are real public goods the arrangement demonstrably funds. Suppression 0.60 reflects enforcement without prohibition — grant conditions keyed to existential framing, venue and citation hierarchies, hiring pipelines, and social sanction against 'distraction' arguments; no alternative is banned, all are disadvantaged. Theater_ratio 0.42: substantive interpretability and evaluation work coexists with unfalsifiable scenario literature and lab safety-washing whose output is positioning rather than risk reduction. Accessibility_collapse 0.50: alternatives remain workable at the field's margins but collapse inside major-lab and funder decision loops where the priority is treated as settled. Resistance 0.60: sustained contestation from near-term and integrated coalitions, public criticism of doom framing, and organizing inside labs. Claim and metrics are independent authored facts: claimed_type tangled_rope follows from structure — a real collective-action problem plus asymmetric, actively enforced extraction — while the metrics were authored from observed operation; the engine computes per-seat types from the structural data. All three tracked series share one time grid (t=0..14, step 2); the rising base_extractiveness series is accumulation evidence for investigation, not something reconciled into the claim.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat should compute a coordination-dominant experience: from inside, the priority is a rational response to the largest stake in history, and enforcement feels like rigor. The payer seats compute extraction: nearterm_harm_affected_populations experience the same structure as their concerns being permanently outranked by scenarios they cannot verify and did not consent to; deployment_ethics_researchers experience it as dispossession from a program they built. frontier_capability_labs occupy the pivotal dual seat — they pay for safety teams yet receive the framing's largest material return, so their computed position sits nearer the beneficiary pole than their safety spending suggests. junior_safety_researchers show identity-lock dynamics: professional identity fused with 'the most important problem' makes exit costly regardless of private belief, so their nominal beneficiary position overstates their comfort. Victim heterogeneity — dispersed, unorganized, differently situated — limits coalition formation among payers, which is why powerless seats stay powerless here.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. xrisk_research_labs: agenda control plus mission funding places them near the beneficiary pole (d approximately 0.05-0.10). longtermist_funders: collect agenda influence at near-zero cost with fully mobile capital (d approximately 0.10). frontier_capability_labs: declared beneficiary with a real payer secondary role — they fund safety teams but receive the framing's material returns; net position moderately beneficiary-side (d approximately 0.25). junior_safety_researchers: nominal beneficiaries whose identity-locked exit amplifies effective extraction beyond their raw d (d approximately 0.15 before lock adjustment). nearterm_harm_affected_populations: full targets, trapped (d approximately 0.90). deployment_ethics_researchers: targets with constrained exit (d approximately 0.75). general_public: diffuse target bearing opportunity costs without representation (d approximately 0.60). Receipt diverges from agenda control: gain_flow lands on frontier_capability_labs, not on the labs that set the agenda — the priority's material returns accrue to those it licenses to build. No directionality overrides were needed: beneficiary/victim declarations plus exit options reproduce these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — loss of control over advanced systems — remains live, so mandatrophy is not resolved and no sunset applies. The tangled_rope classification does anti-mislabeling work in both directions: a pure-snare verdict would erase the genuine public-good coordination (evaluations, interpretability, control research) and hand the agenda a persecution defense it would exploit; a pure-rope verdict would erase the documented asymmetric costs borne by unconsenting present populations and the resource channel toward capability scaling. The rising base_extractiveness series (0.40 to 0.70) is the lifecycle signal to watch: if the coordination share of spend keeps falling while the license function grows, the structure drifts toward snare; if enforcement decays as the frame loses dominance, drift runs toward piton. Neither transition is asserted here — the series is accumulation evidence for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel ai_alignment_priority (reading: existential_risk_reading). What structurally changes under the sibling readings?',
    'Compile the sibling stories and compare computed per-seat types and epsilon over the shared referent; divergence localizes the disagreement to priority ordering and victim-set granularity.',
    'Under nearterm_harms_reading the victim set becomes identified present populations and measured suppression rises; under integrated_reading the beneficiary/target asymmetry compresses and epsilon falls toward the coordination floor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of the alignment-priority kernel.').

omega_variable(
    speculative_harm_verifiability,
    'Are catastrophic loss-of-control scenarios probable and well-characterized enough that the arrangement''s present costs are justified, or does unverifiability insulate the agenda from accountability?',
    'Adversarial evaluation results, incident databases, and forecasting track records scored against the agenda''s own published predictions over time.',
    'Low verifiability raises the extraction component (an accountability vacuum) and pushes computed type toward snare; strong verifiability strengthens the coordination component and stabilizes rope-like computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_harm_verifiability, empirical, 'Whether the evidential basis for the priority can bear the resource weight placed on it.').

omega_variable(
    safety_license_vs_coordination,
    'Does the existential-risk framing function primarily as coordination on safety research, or as a moral license that accelerates capability development?',
    'Compare capability-versus-safety milestone ratios and funding flows in labs adopting the framing versus comparable labs that do not; audit whether safety-team findings bind deployment decisions.',
    'License-dominant operation pushes the computed type toward snare and confirms gain_flow on capability labs; coordination-dominant operation supports the tangled-rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_license_vs_coordination, empirical, 'The arrangement''s dominant causal channel: safety coordination or capability license.').

omega_variable(
    undifferentiated_victim_abstraction,
    'The reading''s declared protected class — all of humanity, weighted toward the future — is undifferentiated and cannot answer back. Does that abstraction itself serve enforcement by making harm claims unanswerable?',
    'Trace which concrete populations bear measurable costs now against the abstract class the arrangement claims to protect; test whether identified-victim counterarguments change allocation anywhere.',
    'If abstraction is load-bearing, effective suppression is higher than the scalar suggests and victim-seat extraction should be computed against identified payers rather than the abstract class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(undifferentiated_victim_abstraction, conceptual, 'Whether victim-set abstraction functions as accountability insulation.').

omega_variable(
    redteam_methodology_sufficiency,
    'Is adversarial red-teaming adequate verification for claims that discipline field-wide resource allocation?',
    'Audit the uptake of red-team findings against agenda continuity: do negative results ever reroute funding, or only refine the same agenda?',
    'If findings never bind allocation, theater_ratio is understated and the methodology functions as legitimation; if they bind, the methodology vindicates a real feedback loop.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redteam_methodology_sufficiency, empirical, 'Adequacy of the reading''s characteristic verification method.').

omega_variable(
    identity_fusion_persistence,
    'Is the agenda sustained by its evidential position or by identity fusion of researchers and funders with the ''most important problem'' frame?',
    'Post-exit interviews with departing researchers; funding continuity across founder turnover and after publicized prediction failures.',
    'Fusion-dominant persistence predicts piton-direction drift if the evidential position weakens; evidence-dominant persistence predicts stable tangled-rope operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_persistence, conceptual, 'Persistence mechanism: evidence or identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aip_xrisk_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(aip_xrisk_tr_t0, observed).
narrative_ontology:measurement(aip_xrisk_tr_t2, ai_alignment_priority__existential_risk_reading, theater_ratio, 2, 0.19).
narrative_ontology:measurement_basis(aip_xrisk_tr_t2, observed).
narrative_ontology:measurement(aip_xrisk_tr_t4, ai_alignment_priority__existential_risk_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement_basis(aip_xrisk_tr_t4, observed).
narrative_ontology:measurement(aip_xrisk_tr_t6, ai_alignment_priority__existential_risk_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(aip_xrisk_tr_t6, observed).
narrative_ontology:measurement(aip_xrisk_tr_t8, ai_alignment_priority__existential_risk_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(aip_xrisk_tr_t8, observed).
narrative_ontology:measurement(aip_xrisk_tr_t10, ai_alignment_priority__existential_risk_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(aip_xrisk_tr_t10, observed).
narrative_ontology:measurement(aip_xrisk_tr_t12, ai_alignment_priority__existential_risk_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(aip_xrisk_tr_t12, observed).
narrative_ontology:measurement(aip_xrisk_tr_t14, ai_alignment_priority__existential_risk_reading, theater_ratio, 14, 0.42).
narrative_ontology:measurement_basis(aip_xrisk_tr_t14, projected).

% Extraction over time
narrative_ontology:measurement(aip_xrisk_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(aip_xrisk_be_t0, observed).
narrative_ontology:measurement(aip_xrisk_be_t2, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2, 0.46).
narrative_ontology:measurement_basis(aip_xrisk_be_t2, observed).
narrative_ontology:measurement(aip_xrisk_be_t4, ai_alignment_priority__existential_risk_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(aip_xrisk_be_t4, observed).
narrative_ontology:measurement(aip_xrisk_be_t6, ai_alignment_priority__existential_risk_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(aip_xrisk_be_t6, observed).
narrative_ontology:measurement(aip_xrisk_be_t8, ai_alignment_priority__existential_risk_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(aip_xrisk_be_t8, observed).
narrative_ontology:measurement(aip_xrisk_be_t10, ai_alignment_priority__existential_risk_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(aip_xrisk_be_t10, observed).
narrative_ontology:measurement(aip_xrisk_be_t12, ai_alignment_priority__existential_risk_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(aip_xrisk_be_t12, observed).
narrative_ontology:measurement(aip_xrisk_be_t14, ai_alignment_priority__existential_risk_reading, base_extractiveness, 14, 0.7).
narrative_ontology:measurement_basis(aip_xrisk_be_t14, projected).

% Suppression requirement over time
narrative_ontology:measurement(aip_xrisk_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(aip_xrisk_su_t0, observed).
narrative_ontology:measurement(aip_xrisk_su_t2, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2, 0.38).
narrative_ontology:measurement_basis(aip_xrisk_su_t2, observed).
narrative_ontology:measurement(aip_xrisk_su_t4, ai_alignment_priority__existential_risk_reading, suppression_requirement, 4, 0.43).
narrative_ontology:measurement_basis(aip_xrisk_su_t4, observed).
narrative_ontology:measurement(aip_xrisk_su_t6, ai_alignment_priority__existential_risk_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement_basis(aip_xrisk_su_t6, observed).
narrative_ontology:measurement(aip_xrisk_su_t8, ai_alignment_priority__existential_risk_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement_basis(aip_xrisk_su_t8, observed).
narrative_ontology:measurement(aip_xrisk_su_t10, ai_alignment_priority__existential_risk_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(aip_xrisk_su_t10, observed).
narrative_ontology:measurement(aip_xrisk_su_t12, ai_alignment_priority__existential_risk_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement_basis(aip_xrisk_su_t12, observed).
narrative_ontology:measurement(aip_xrisk_su_t14, ai_alignment_priority__existential_risk_reading, suppression_requirement, 14, 0.62).
narrative_ontology:measurement_basis(aip_xrisk_su_t14, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI alignment priority' decomposes into three structurally distinct claims — existential-risk-first (this file), near-term-harms-first, and integrated — because assigning a single epsilon across them would violate epsilon-invariance (the observable 'whose harms count and in what order' changes the measured extraction). This upstream reading influences the siblings' operating environment: its funding and venue dominance is the background condition against which the near-term and integrated readings define themselves. Sibling files must link back via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
