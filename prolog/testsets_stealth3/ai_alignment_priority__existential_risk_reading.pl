% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Existential-Risk Reading of AI Alignment Prioritization
 *   domain: technology_governance/ai_ethics/risk_assessment
 *
 * SUMMARY:
 *   The standing arrangement under contest is the institutionalized regime
 *   through which AI alignment is defined and funded under existential-risk
 *   primacy: frontier labs writing the operative definitions of alignment in
 *   their own safety frameworks, a concentrated philanthropic and public
 *   funding apparatus steering research money toward loss-of-control work, an
 *   emerging regulatory layer building systemic-risk tiers on the same
 *   taxonomy, and the career structures that recruit talent into it. The
 *   epsilon referent is this arrangement as it actually operates — not the
 *   ideal the frame invokes, and not the alternative arrangements the sibling
 *   readings would install. Values are reading-indexed per OQ-26/OQ-258: an
 *   adherent of this reading authors epsilon from their own lights, and an
 *   honest adherent condemns the arrangement's capture dynamics (rents
 *   flowing to labs under safety cover, justification resting on harms from
 *   capabilities that do not yet exist and therefore cannot verify anything,
 *   remediation of present harms crowded out) even while endorsing the
 *   arrangement's telos. Epsilon measures the arrangement's operation, not
 *   its stated aim. KEY AGENTS (by structural relationship): frontier labs —
 *   beneficiary-administrator (institutional/arbitrage), frames the problem
 *   and nets legitimacy and capital; x-risk research community — concentrated
 *   beneficiary (organized/identity_locked), careers fused to the mission;
 *   governance funders — agenda-setter (institutional/mobile); marginalized
 *   affected communities — present-harm payer (powerless/trapped); fairness
 *   audit researchers — displaced payer and excluded voice
 *   (moderate/constrained); general public — diffuse payer and nominal
 *   protectee (powerless/constrained); future humanity — declared
 *   constituency, a non-agent that collects nothing; policy regulators —
 *   analytical observer (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.6).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential-Risk Reading of AI Alignment Prioritization").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "technology_governance/ai_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '2795db71-bd15-4588-aa78-79e0d2262d17').
narrative_ontology:cs_kernel_codification('2795db71-bd15-4588-aa78-79e0d2262d17', distributed).
narrative_ontology:cs_authority_grounding('2795db71-bd15-4588-aa78-79e0d2262d17', lineage).
narrative_ontology:cs_interpretation_layer_present('2795db71-bd15-4588-aa78-79e0d2262d17').
narrative_ontology:cs_reading_relation('2795db71-bd15-4588-aa78-79e0d2262d17', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_reading_relation('2795db71-bd15-4588-aa78-79e0d2262d17', ai_alignment_priority__nearterm_harms_reading, forecloses).
narrative_ontology:cs_axiom('2795db71-bd15-4588-aa78-79e0d2262d17', foundational, catastrophic_risk_dominates_priority_ordering).
narrative_ontology:cs_axiom_status(catastrophic_risk_dominates_priority_ordering, holdable).
narrative_ontology:cs_axiom_grounding('2795db71-bd15-4588-aa78-79e0d2262d17', catastrophic_risk_dominates_priority_ordering, empirically_contingent).
narrative_ontology:cs_axiom('2795db71-bd15-4588-aa78-79e0d2262d17', foundational, capability_progress_necessary_for_alignment).
narrative_ontology:cs_axiom_status(capability_progress_necessary_for_alignment, holdable).
narrative_ontology:cs_axiom_grounding('2795db71-bd15-4588-aa78-79e0d2262d17', capability_progress_necessary_for_alignment, instrumental).
narrative_ontology:cs_reference_frame('2795db71-bd15-4588-aa78-79e0d2262d17', existential_safety_primacy_framework).
narrative_ontology:cs_drift_state('2795db71-bd15-4588-aa78-79e0d2262d17', contemporary_post_deployment_scaling_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2795db71-bd15-4588-aa78-79e0d2262d17', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, xrisk_research_community).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, ai_governance_funders).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, marginalized_affected_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, fairness_audit_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, general_public).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, catastrophic_loss_of_control_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, intelligence_explosion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train and deploy frontier models, and write the safety frameworks that define what alignment means operationally inside their own walls: capability thresholds, evaluation batteries, red-team protocols, staged-release commitments. The catastrophic-risk frame funnels capital, talent, and regulatory goodwill toward them, because they are positioned as both the principal danger and the only plausible preventer. They fund safety teams and occasionally delay a release; they receive legitimacy, valuation premiums, and a seat at every table where the problem gets defined.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, beneficiary).

% Alignment researchers, evaluation organizations, forecasting groups, and safety nonprofits whose work, funding, and professional identities are constituted around preventing loss of control. Many left academic or industry tracks to join it; grant portfolios, conference circuits, and citation networks reinforce membership. Leaving would mean abandoning the mission identity that organizes their careers and self-concept.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, xrisk_research_community, beneficiary,
    organized, generational, identity_locked, global).

% Philanthropic foundations, donor collaboratives, and public research programs that decide which safety problems receive resources. Their grantmaking operationalizes the priority ordering: loss-of-control research draws funding at scales present-harm work rarely reaches. Stewarding the agenda confers influence and standing; sunk portfolios make redirection costly for them personally and institutionally.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_governance_funders, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, ai_governance_funders, beneficiary).

% People subject to deployed-system harms today — discriminatory scoring in credit, hiring, housing, and policing, degraded service access, moderation failures, pervasive surveillance — whose remediation competes with speculative-risk preparation for the same finite pool of governance attention and money. Few had any seat in the conversations that defined alignment; exiting affected systems is often impossible because credit, housing, and employment run through them.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, marginalized_affected_communities, payer,
    powerless, biographical, trapped, global).

% Researchers measuring disparate impact, conducting algorithmic audits, and building participatory evaluation methods. Funding lines and prestige have shifted toward frontier-model evaluations and control research; their agenda is routinely characterized as secondary to the real alignment problem. They can change subfields at real career cost, and their findings increasingly lack an institutional buyer.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, fairness_audit_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, fairness_audit_researchers, excluded).

% The diffuse population bearing the arrangement's ambient costs: public attention, democratic deliberation, and research agendas steered toward scenario planning for systems that do not yet exist, while deployed systems reshape information, labor, and civic life under thin oversight. They are simultaneously the constituency the frame claims to protect, and they cannot opt out of deployment externalities.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, general_public, payer,
    powerless, biographical, constrained, global).

% The declared constituency: people who will exist if civilization navigates the coming decades safely. The frame speaks in their name. They cannot participate, collect, object, or confirm that anything done on their behalf helps them; they exist in the arrangement purely as its normative hook.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, future_humanity).

% Agencies and intergovernmental bodies building systemic-risk tiers, evaluation mandates, and summit processes. They take testimony principally from labs and safety organizations, commission technical analyses, and retain the authority to restructure the arrangement through binding requirements; presently they observe and calibrate.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, policy_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real under-coordination problem: civilization-scale tail risks are systematically underweighted by markets (externalities beyond any actor's horizon) and by electoral politics (payoffs deferred past any term). The arrangement concentrates scarce expertise on worst-case capability scenarios, builds shared evaluations and disclosure norms, and gives states a template for acting on risks no single actor internalizes.
% TRANSFER_FUNCTION: Moves research funding, talent, compute allocation, and regulatory attention away from present-harm mitigation and broad public-interest oversight toward frontier-adjacent control research; and moves legitimacy and narrative authority to the actors credibly claiming to avert catastrophe — chiefly the labs whose continued scaling the frame licenses, and the research community it employs.
% ABSENT_VOICES: Marginalized communities living inside deployed systems' harms were absent from the rooms where alignment was defined; likewise global-majority voices, gig and clerical workers facing displacement, and audit practitioners. Their absence is what allows the all-of-humanity constituency to read as unanimous, when the actual cost-bearing seats are specific and differentiated.
% DISAPPEARANCE_RATIONALE: Funding portfolios, careers, lab legitimacy strategies, and emerging regulatory tier systems are organized around this priority. Overnight removal would redistribute hundreds of millions in research money, strand a professionally constituted community, strip frontier labs of their principal safety narrative, and force regulators to rebuild their risk taxonomies from scratch — the surrounding arrangements visibly depend on it.
% FOUNDING_PROBLEM: In the early 2010s, machine-learning capability curves steepened faster than any governance response. A small group of researchers argued that sufficiently capable systems could pursue objectives misaligned with human intentions at civilizational scale, and no institution existed to study, measure, or prevent that outcome.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: the 2023 one-line statement on extinction risk from AI drew thousands of signatures including many scientists with no stake in alignment funding, and several state risk assessments reached catastrophic-risk conclusions through processes independent of the funded community. What is NOT corroborated outside that community is the priority claim itself — that this problem outranks present harms in resource allocation. No disinterested body attests that ordering; the problem is corroborated, the priority is not.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.68) because the arrangement moves large resource flows under a justification — prevention of harms from capabilities that do not yet exist — that no outcome within the interval can verify or refute; unfalsifiable justification is the classic condition under which extraction grows without a feedback brake. Suppression is moderate-high (0.60) and UNSCALED by design: it records the raw structural fact that alternative framings face soft exclusion (grant-gatekeeping, prestige hierarchies, the characterization of present-harm work as distraction or category error), not prohibition; only extractiveness is scaled by directionality and scope in the engine's computation. Theater ratio 0.42: safety cases, red-team reports, and responsibility framings are partly genuine engineering artifacts and partly performance aimed at boards, regulators, and donors — the share that is performance grows as the frame's legitimacy value grows. Accessibility collapse 0.48: alternatives remain workable (fairness research, participatory audit, integrated agendas all continue), but understanding the frame's prestige economy makes exit costly. Resistance 0.58: the contest is live and visible — audit communities, affected-community advocates, and integrated-agenda researchers contest the priority openly rather than being silenced. Claim/metric independence: claimed_type tangled_rope is authored from structure — a genuine collective-action problem (civilizational tail risk is real and systematically underweighted) combined with asymmetric extraction through the same structure — while the metrics are authored as descriptive facts; the engine computes per-seat classifications and owns any divergence. Temporal note: all three tracked series share one grid (t=0..12, roughly annual units mapping 2013-2025); trajectories are monotonic-rising with step changes at the contest spikes (the representation-harm critique around t=6, the post-chatbot mainstreaming and mass endorsement around t=10). Rising suppression_requirement is authored deliberately: the story traces the maturation of the frame's enforcement infrastructure — funding funnel consolidation, hiring-norm hardening, rhetorical boundary maintenance — which is exactly the enforcement-capacity dynamic the scalar base_properties.suppression cannot carry alone. The arrangement is not cyclical; it ratchets.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats should compute very differently. From frontier-lab and funder positions, the arrangement is a coordination triumph they built and steward: they see the tail risk, the institution-building, the policy traction. From the marginalized-affected-communities seat, the same structure operates as displacement — their harms are real, documented, and deprioritized in favor of scenarios they had no hand in weighting, and they cannot exit the systems harming them. Fairness audit researchers experience it as professional dispossession with an identity dimension: told their agenda is not real alignment. The general public sits ambivalently — bearer of diffuse costs, nominal protectee — which the undifferentiated constituency framing papers over. Coalition potential among victims is real but currently blocked: affected communities and audit researchers share an interest in rebalancing the portfolio, yet the frame's universal-humanity rhetoric obscures their common seat. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low d: frontier labs (net collectors of legitimacy and capital despite real safety spend), the x-risk research community (identity-locked into collecting), funders (agenda stewards deriving standing). Victims derive high d: marginalized affected communities (trapped, full-cost bearing, near the target pole), fairness audit researchers (constrained, high d), the general public (high d from the victims declaration, though genuinely ambivalent — they are also the declared protection target, and the derivation cannot see that second face; the undifferentiated_constituency_masking omega carries this residual). Future humanity is declared a beneficiary but authored agent:false: a non-agent excluded from beneficiary/victim derivation and directionality, because a constituency that cannot collect must not feed d-to-effective-extraction arithmetic as if it received anything. Policy regulators sit at the analytical pole. No directionality_overrides are authored: the override surface binds at power-atom granularity, and this story's agents sharing any power atom (three powerless seats with materially different positions; three institutional seats with opposed roles) would be smeared together by any single correction — the structural declarations plus exits carry the derivation better than a blunt override would.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no institution addressed civilizational loss-of-control risk — remains live, so this is not a resolved-mandatrophy case and no mandatrophy_resolved flag is authored; the R5 interview carries the signal instead (status live, verdict world_rearranges: no mismatch, no zombie flag under the consumption rule). The classification discipline cuts both ways here. Reading the arrangement as a snare would erase the genuine coordination achievement — tail-risk attention that markets and elections structurally underprovide, evaluation infrastructure that did not previously exist, and the real possibility that some preparedness is worth its cost. Reading it as a rope would erase the asymmetry — that the same structure channels legitimacy and capital to the actors it licenses, displaces differently-positioned harm-bearers, and insulates itself from accountability by invoking harms no one can yet observe. The tangled-rope classification holds both truths and obliges the analysis to ask what share of activity is coordination cost versus capture. Forward risk: if capability progress plateaus without loss-of-control precursors while the founding problem's salience dies, the arrangement's status flips dead while the world still rearranges around it — that mismatch is the precise signature the corpus exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_priority_underdetermination,
    'Is there a principled arbiter for which reading of the ai_alignment_priority kernel should govern resource allocation, or is every classification seat-relative?',
    'Cross-reading corpus comparison of the three sibling stories under matched scenarios: observe whether any convergent verdict survives across seats or whether verdicts track seat position identically. Institutional lock-in of one reading would shift every story''s epsilon referent to the new standing arrangement.',
    'If seat-relative, the engine''s per-seat outputs are the primary result and story-level type claims are provisional; if a reading gains field-wide dominance, this story''s referent shifts and its metrics require re-authoring against the new arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_priority_underdetermination, conceptual, 'Whether the alignment-priority kernel admits a neutral arbiter across readings or resolves only per-seat.').

omega_variable(
    speculative_capability_accountability_gap,
    'What share of the measured extraction is a justified insurance premium against genuine tail risk, versus rent protected by the impossibility of verifying prevented catastrophes?',
    'Retrospective audits tied to capability milestones: when predicted loss-of-control precursors arrive or fail to arrive, compare realized prevention value against cumulative resource flows; interim, require falsifiable intermediate benchmarks (evaluation suites, incident rates) with pre-committed update rules.',
    'If no verifiable prevention product emerges across the interval, the coordination component thins and the classification slides toward pure extraction; if intermediate benchmarks validate, part of the measured extraction reclassifies as legitimate coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(speculative_capability_accountability_gap, empirical, 'Extraction justified by harms from capabilities that do not yet exist and therefore cannot verify anything.').

omega_variable(
    undifferentiated_constituency_masking,
    'Does invoking all of humanity as an undifferentiated victim-and-beneficiary set conceal specific, differentiated cost-bearers — present-harmed communities chief among them — behind a universal constituency that cannot object?',
    'Disaggregated incidence analysis tracing which concrete populations absorb costs (budget share, remediation delay, governance attention) versus which populations receive any measurable protective output; also resolves the general-public ambivalence the directionality derivation flattens.',
    'If differentiation is confirmed, the victim set decomposes into named seats with distinct directionalities, raising computed extraction for specific payers and weakening the universal-protection justification that stabilizes the arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(undifferentiated_constituency_masking, conceptual, 'Universal-humanity framing versus the differentiated actual cost-bearers beneath it.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the x-risk research community''s persistence under the frame structural (funding monoculture, prestige economy) or internalized (mission identity that would survive defunding)?',
    'Post-exit trajectory study of researchers who lose funding or leave: relocation to adjacent fields without mission persistence indicates structural lock; maintenance of the mission identity outside institutions (independent writing, volunteer organization) indicates internalized lock.',
    'Internalized lock raises effective suppression above the structural measure and predicts the arrangement persisting after funding withdrawal; structural lock predicts rapid rearrangement if funders redirect. Determines what share of suppression travels with the agent after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Composition of the identity-lock binding the existential-risk research community: professional-ideological fusion versus economic dependency.').

omega_variable(
    intergenerational_weighting_value_conflict,
    'How much present, certain, unequally distributed welfare may justly be traded against uncertain, speculatively large future benefits — and who is entitled to make that trade?',
    'Not resolvable by evidence. Legitimate resolutions are procedural (deliberative bodies with present-affected parties actually seated) or explicit axiological commitment; the engine should treat divergent weightings as different preference orderings, not as measurement error.',
    'Under a heavy-discounting weighting the measured extraction collapses toward coordination cost; under near-zero discounting the displacement of present harms registers as severe extraction. The same arrangement classifies oppositely under different weightings — this omega marks that the flip is a values difference, not an empirical discovery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_weighting_value_conflict, preference, 'The irreducible value question underneath the priority ordering; a preference conflict, not an empirical unknown.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_priority__existential_risk_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(ai_a_tr_t2, observed).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__existential_risk_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(ai_a_tr_t4, observed).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_priority__existential_risk_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(ai_a_tr_t6, observed).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__existential_risk_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(ai_a_tr_t8, observed).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__existential_risk_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(ai_a_tr_t10, observed).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__existential_risk_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(ai_a_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement_basis(ai_a_be_t2, observed).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__existential_risk_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement_basis(ai_a_be_t4, observed).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_priority__existential_risk_reading, base_extractiveness, 6, 0.53).
narrative_ontology:measurement_basis(ai_a_be_t6, observed).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__existential_risk_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(ai_a_be_t8, observed).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__existential_risk_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(ai_a_be_t10, observed).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__existential_risk_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(ai_a_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t2, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2, 0.36).
narrative_ontology:measurement_basis(ai_a_su_t2, observed).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__existential_risk_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement_basis(ai_a_su_t4, observed).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_priority__existential_risk_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement_basis(ai_a_su_t6, observed).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__existential_risk_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(ai_a_su_t8, observed).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__existential_risk_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(ai_a_su_t10, observed).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__existential_risk_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(ai_a_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'AI alignment' decomposes into three structurally distinct constraints, one per reading of the ai_alignment_priority kernel. All three share a single epsilon referent — the standing arrangement of alignment definition and funding — while authoring different reading-indexed epsilon values over it (OQ-26): this existential_risk_reading authors 0.68, condemning the arrangement's capture dynamics while endorsing its telos; the nearterm_harms_reading authors a higher epsilon, counting displaced present harms as the arrangement's central extraction; the integrated_reading authors a lower epsilon, reading the same arrangement as an incomplete synthesis of complementary functions. The readings are linked rather than merged because their victim sets, beneficiary sets, and failure modes differ structurally. Upstream/downstream: this reading is upstream — its resource dominance and narrative authority shape the operating environment of both siblings (relation: influences toward the integrated reading, which it pressures without foreclosing; the nearterm_harms_reading competes for the same exclusive priority slot with an opposite assignment and is foreclosed within any single framework that fixes one top priority). Each family member links to the others via affects_constraints; orphaning any member would hide the contamination path by which a shift in this reading's standing propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
