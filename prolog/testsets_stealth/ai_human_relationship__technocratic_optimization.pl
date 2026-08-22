% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__technocratic_optimization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__technocratic_optimization, []).

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
 *   constraint_id: ai_human_relationship__technocratic_optimization
 *   human_readable: Technocratic Optimization Reading: AI as Efficiency Instrument Indexing Human Worth to Productivity
 *   domain: political_theology/technology_ethics/economic
 *
 * SUMMARY:
 *   The standing arrangement under contest is the deployment of AI systems as
 *   instruments of efficiency maximization, under which human worth is
 *   operationally indexed to productivity and optimization potential: hiring,
 *   credit, insurance, benefits, logistics, content distribution, and
 *   workplace management increasingly run through scoring and optimization
 *   machinery, and persons enter these systems as data profiles whose life
 *   chances are set by outputs they rarely see. Expected structural delta
 *   realized: reduction of persons to profiles, exclusion of low-scoring
 *   ('inefficient') populations, concentration of decision authority in
 *   algorithmic gatekeepers, and work paced by machine schedules. This file
 *   instantiates ONLY the technocratic_optimization reading of the
 *   ai_human_relationship kernel; the sibling readings are separate
 *   constraints with their own epsilon and victim structures. Claim/metric
 *   independence is preserved: claimed_type is authored as tangled_rope
 *   because I judge the arrangement to possess a genuine
 *   allocation-coordination core WITH asymmetric capture and active
 *   enforcement, while the metrics are authored as my independent descriptive
 *   assessment of its actual operation. Note that the reading's own
 *   self-presentation trends toward a natural-progress framing
 *   ('objectivity', 'inevitability of automation') — that self-claim is
 *   precisely what the corpus should test as a false-summit candidate, and it
 *   is recorded here as presentation, not as the authored claim.
 *
 * KEY AGENTS:
 *   - algorithmic_gatekeepers: agenda-setting beneficiary (institutional/arbitrage) — designs the metrics, operates the systems, collects the margin
 *   - capital_owners: passive beneficiary (powerful/arbitrage) — receives productivity gains without administering anything
 *   - algorithmically_managed_workers: primary target (powerless/trapped) — bears machine-set pace and rating discipline
 *   - populations_deemed_inefficient: primary target (powerless/trapped) — filtered out by scores they rarely see
 *   - data_profile_subjects: dual-positioned target (moderate/constrained) — supplies behavioral raw material, receives services back
 *   - cst_social_ethicists: excluded voice (organized/analytical) — objects from outside the room on dignity grounds
 *   - ai_ethics_regulators: analytical observer (institutional/analytical) — audits the surface after deployment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__technocratic_optimization, 0.72).
domain_priors:suppression_score(ai_human_relationship__technocratic_optimization, 0.68).
domain_priors:theater_ratio(ai_human_relationship__technocratic_optimization, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__technocratic_optimization, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__technocratic_optimization, "Technocratic Optimization Reading: AI as Efficiency Instrument Indexing Human Worth to Productivity").
narrative_ontology:topic_domain(ai_human_relationship__technocratic_optimization, "political_theology/technology_ethics/economic").

domain_priors:requires_active_enforcement(ai_human_relationship__technocratic_optimization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__technocratic_optimization, 'e344a153-4e98-4bf5-aa74-4e0dc42499cb').
narrative_ontology:cs_kernel_codification('e344a153-4e98-4bf5-aa74-4e0dc42499cb', formalized).
narrative_ontology:cs_authority_grounding('e344a153-4e98-4bf5-aa74-4e0dc42499cb', expertise).
narrative_ontology:cs_interpretation_layer_present('e344a153-4e98-4bf5-aa74-4e0dc42499cb').
narrative_ontology:cs_reading_relation('e344a153-4e98-4bf5-aa74-4e0dc42499cb', ai_human_relationship__instrumental_subsidiarity, coexists_with).
narrative_ontology:cs_reading_relation('e344a153-4e98-4bf5-aa74-4e0dc42499cb', ai_human_relationship__incarnational_humanism, forecloses).
narrative_ontology:cs_axiom('e344a153-4e98-4bf5-aa74-4e0dc42499cb', foundational, productivity_measures_human_worth).
narrative_ontology:cs_axiom_status(productivity_measures_human_worth, holdable).
narrative_ontology:cs_axiom_grounding('e344a153-4e98-4bf5-aa74-4e0dc42499cb', productivity_measures_human_worth, empirically_contingent).
narrative_ontology:cs_axiom('e344a153-4e98-4bf5-aa74-4e0dc42499cb', secondary, quantified_value_admissibility_requirement).
narrative_ontology:cs_axiom_status(quantified_value_admissibility_requirement, holdable).
narrative_ontology:cs_axiom_grounding('e344a153-4e98-4bf5-aa74-4e0dc42499cb', quantified_value_admissibility_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('e344a153-4e98-4bf5-aa74-4e0dc42499cb', efficiency_progress_baseline).
narrative_ontology:cs_drift_state('e344a153-4e98-4bf5-aa74-4e0dc42499cb', contemporary_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e344a153-4e98-4bf5-aa74-4e0dc42499cb', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__technocratic_optimization, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, algorithmic_gatekeepers).
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, capital_owners).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, algorithmically_managed_workers).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, populations_deemed_inefficient).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, data_profile_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, data_profile_subjects).
narrative_ontology:constraint_vindicates(ai_human_relationship__technocratic_optimization, productivity_as_measure_of_worth).
narrative_ontology:constraint_vindicates(ai_human_relationship__technocratic_optimization, efficiency_neutrality_doctrine).
narrative_ontology:constraint_vindicates(ai_human_relationship__technocratic_optimization, quantification_objectivity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, train, and operate the scoring and optimization systems through which hiring, lending, logistics, content distribution, and workplace management now run. Set the objective functions, decide what gets measured, and collect fees, data advantages, and market position from the systems' operation. Their exit is easy: the same infrastructure sells across industries and jurisdictions.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, algorithmic_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold equity in the firms that deploy these systems. Productivity gains from automation and algorithmic management flow to margins and valuations; they bear none of the shop-floor adjustment. Capital moves between sectors freely.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, capital_owners, beneficiary,
    powerful, generational, arbitrage, global).

% Warehouse pickers, drivers, content moderators, and increasingly office staff whose tasks, pace, breaks, and pay are set by management software. Ratings determine continued access to work. Refusing the app means losing the income stream; switching platforms means starting the rating history over.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, algorithmically_managed_workers, payer,
    powerless, biographical, trapped, global).

% Elderly, disabled, low-scoring, or otherwise low-metric people filtered out by hiring screens, credit models, insurance pricing, and benefit-eligibility algorithms. Most never learn which score excluded them, or that a process ran at all.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, populations_deemed_inefficient, payer,
    powerless, generational, trapped, national).

% Everyone whose browsing, movement, purchases, and speech are captured as the raw material of prediction systems. They receive usable services back — search, navigation, connection — in exchange; the profile built from them outlives any single service relationship and follows them across contexts.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, data_profile_subjects, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__technocratic_optimization, data_profile_subjects, beneficiary).

% Theologians and social-teaching bodies arguing from imago Dei, solidarity, and the preferential option for the poor that persons outrank their output. They publish critiques, advise bishops and agencies, and shape public language, but hold no seat in product reviews, procurement decisions, or benchmark governance.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, cst_social_ethicists, excluded,
    organized, generational, analytical, global).

% Agencies drafting audit, transparency, and risk rules for automated decision systems. They encounter deployments after the fact, rely on operator disclosures, and can slow or condition specific systems, but not the underlying measurement culture that produced them.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, ai_ethics_regulators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__technocratic_optimization, algorithmic_gatekeepers).
narrative_ontology:fixing_cost_class(ai_human_relationship__technocratic_optimization, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates labor, credit, attention, and goods across millions of simultaneous decisions faster and more consistently than deliberative human management can — matching supply to demand, routing fleets, ranking candidates and content, and standardizing evaluation where case-by-case judgment cannot scale.
% TRANSFER_FUNCTION: Moves decision authority and surplus from workers and scored populations to the operators of the measurement infrastructure: work hours and pace from workers to machine-set schedules; life chances (jobs, credit, housing, benefits) from individuals to scoring models; behavioral data from everyone to system owners; fees and margin to the gatekeeping firms.
% ABSENT_VOICES: Those the scores exclude are absent by construction — the same mechanism that filters them out removes their standing to object, and most never learn a decision was made. Unpaid caregivers and future generations whose option space the trained models pre-shape have no seat. The Catholic social tradition speaks adjacent to the room (magisterial documents, synods, university chairs) but is not inside procurement, product review, or benchmark governance.
% DISAPPEARANCE_RATIONALE: Overnight removal would halt algorithmic scheduling, freeze automated hiring and credit pipelines, and force logistics and content systems back onto slower human judgment within days — supply chains, labor markets, and attention markets would visibly reorganize around whatever replaced the scoring layer.
% FOUNDING_PROBLEM: Mid-twentieth-century operations research confronted coordination problems exceeding human deliberative capacity: routing, scheduling, and allocation across millions of interacting decisions, where human-managed processes were slow, inconsistent, and wasteful. Optimization machinery was built to make those decisions at speed and scale.
% FOUNDING_PROBLEM_CORROBORATION: Operations-research history and documented logistics outcomes attest from outside the benefiting parties that the scale problem was and remains real. Whether the current arrangement still serves that problem — rather than rent collection layered atop largely solved coordination — is attested only by the beneficiaries; CST social documents, labor sociology of algorithmic management, and independent audit studies attest the arrangement now exceeds the founding problem. Corroboration is therefore split: the problem is live, its use as justification is contested.
narrative_ontology:disappearance_verdict(ai_human_relationship__technocratic_optimization, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__technocratic_optimization, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__technocratic_optimization, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_human_relationship__technocratic_optimization, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__technocratic_optimization, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__technocratic_optimization_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__technocratic_optimization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__technocratic_optimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Interval mapping: time 0 approximates 2010 (early large-scale algorithmic management and gig platforms), time 15 approximates 2025 (scoring expansion into white-collar work, welfare eligibility, and generative-model-mediated evaluation). Extractiveness 0.72: value transfer runs through metric-based management decoupled from consent — workers accept machine-set pace to keep income, applicants are denied by models they cannot inspect, and behavioral data is appropriated as the raw material of further optimization. Suppression 0.68 is authored as a raw structural property (unscaled by power or scope in the engine's arithmetic): platform dependence, rating-gated access to work, and silent denial pipelines, with a growing internalized component (see suppression_mechanism_internalization omega). Theater 0.40: dashboards, ethics boards, and responsible-AI reporting perform oversight while the objective functions and training data remain proprietary. Accessibility_collapse 0.52: inside a managed system the alternatives collapse completely (one cannot negotiate the metric), but exit to unmanaged sectors, informal work, or offline life remains partially available, so collapse is partial rather than total. Resistance 0.55: union drives over algorithmic management, audit and litigation campaigns, regulatory action, and slowdowns are real and growing but have not yet altered the core arrangement. The three temporal series share one grid ({0,3,6,9,12,15}) so every metric is authored at every examined point; trajectories are monotonic rather than cyclical because the enforcement ratchet (surveillance tooling, API gating, compliance hardening) intensified steadily across the interval with no observed relaxation phase. Coalition note: the victim classes overlap heavily (the same profiles are managed at work and scored everywhere else), so latent coalition capacity — platform councils, data trusts, cross-sector organizing — is the main upside risk to the suppression picture.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the gatekeeper seat the arrangement is merit made scalable: waste eliminated, judgment standardized, contribution finally legible — near-zero experienced extraction, reinforced by professional identity fusion (engineering and management cultures whose self-concept is constituted by shipping measurable improvements; if that frame broke, 'objective efficiency' would become a contestable policy choice rather than a fact). From the managed-worker seat the same structure is pace discipline and rating precarity. From the excluded-population seat it is worse: the mechanism that removed them also removed their standing to perceive it. Same-level lateral divergence: two firms of equal capitalization sit at opposite directionalities purely by stack position — one collects optimization rents upstream, one is subjected to customer-side scoring downstream — global standing identical, constraint-specific position everything. The regulator seat sees only the auditable surface, after deployment, through operator disclosures.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: algorithmic_gatekeepers (beneficiary, institutional, arbitrage exit) and capital_owners (beneficiary, powerful, arbitrage) derive d near the subsidy end — the arrangement pays them. Victim declarations drive the target end: algorithmically_managed_workers (powerless, trapped) and populations_deemed_inefficient (powerless, trapped) derive d near full target — trapped or identity-locked targets sit at the extreme regardless of nominal powerlessness, and their powerlessness amplifies effective extraction through the engine's scaling. One override is declared: data_profile_subjects derive near-full-target d from victim status plus constrained exit, but the derivation misses the service-for-data exchange (search, navigation, connection returned for behavioral capture), which partially subsidizes them; the override sets d to 0.78 for the moderate power atom to reflect net rather than gross targeting. Vindicated propositions (productivity_as_measure_of_worth, efficiency_neutrality_doctrine, quantification_objectivity_claim) are listed separately: they collect no rents and are not beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordination of allocation decisions beyond human deliberative capacity — remains live, so no mandatrophy resolution is declared and the arrangement is not a vestige. The classification guards both characteristic misreadings. Reading the genuine coordination core as pure extraction (snare) would license demolishing real allocation capacity that logistics, credit access, and matching genuinely depend on. Accepting the reading's natural-progress self-presentation (mountain/false summit) would immunize the extraction layer from revision as if it were physics. Tangled_rope keeps both visible: coordination function real, extraction asymmetric, enforcement load-bearing. The rising theater_ratio series is the Goodhart-drift signal to watch: proxy metrics and performative oversight displacing the allocation function would move this toward piton territory (administrator could change it, cost asymmetry prevents it, no seat meaningfully profits from the residue) if enforcement decayed while dashboards multiplied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'How would classification shift if the ai_human_relationship kernel were instantiated by a sibling reading instead of technocratic_optimization?',
    'Generate the sibling stories (instrumental_subsidiarity, incarnational_humanism) against the same deployment record and compare per-seat classifications; divergence localizes the disagreement to the worth-indexing premise.',
    'Under incarnational_humanism the victim set widens to everyone reduced to a data profile and epsilon rises on dignity grounds; under instrumental_subsidiarity epsilon falls toward governance-failure levels and the type drifts toward a defective rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: this story is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    efficiency_naturalness_ambiguity,
    'Is efficiency maximization an inescapable law of technological development, or a constructed arrangement that identifiable actors chose and profit from?',
    'Comparative institutional history of sectors that adopted versus declined optimization management at similar cost, plus disclosure records of gatekeeper lobbying against measurement limits.',
    'If naturalized, resistance reads as friction against progress and the arrangement approaches false-mountain immunity; if constructed, beneficiaries become visible and the arrangement is revisable by ordinary political and legal levers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_naturalness_ambiguity, conceptual, 'Natural-law versus constructed-status ambiguity of the efficiency imperative.').

omega_variable(
    productivity_metric_validity,
    'Do productivity and optimization metrics track genuine human contribution, or do they systematically misprice care, maintenance, and unpaid work?',
    'Audit studies comparing scored output against independent valuation of unscored contributions (care work, mentoring, repair) within the same organizations.',
    'Systematic mispricing raises true extraction above the authored 0.72 and extends the victim set to entire unrecognized occupational classes; accurate metrics would support the gatekeepers'' merit framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_metric_validity, empirical, 'Whether the measuring rod itself is extractively biased.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression holding workers to machine pace structural (platform dependence, rating gates) or internalized (self-worth fused with output)?',
    'Post-exit trajectory studies of workers who leave algorithmically managed employment: if pace-anxiety and compulsive self-scoring persist off-platform, part of the suppression is internalized.',
    'Internalized components raise effective suppression above the structural 0.68 and persist after any regulatory fix; purely structural suppression falls with platform reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized split of measured suppression.').

omega_variable(
    inefficiency_exclusion_scale,
    'Is exclusion of low-scoring populations a marginal sorting effect or a mass exclusion mechanism comparable to earlier redlining regimes?',
    'Population-level audit of automated denials across hiring, credit, insurance, and benefits, benchmarked against demographic baselines.',
    'Mass exclusion would push extractiveness toward snare territory and elevate coalition risk among the excluded; marginal sorting keeps the tangled-rope reading stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inefficiency_exclusion_scale, empirical, 'Scale of population-level exclusion by scoring systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__technocratic_optimization, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__technocratic_optimization, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ai_h_tr_t0, observed).
narrative_ontology:measurement(ai_h_tr_t3, ai_human_relationship__technocratic_optimization, theater_ratio, 3, 0.24).
narrative_ontology:measurement_basis(ai_h_tr_t3, observed).
narrative_ontology:measurement(ai_h_tr_t6, ai_human_relationship__technocratic_optimization, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(ai_h_tr_t6, observed).
narrative_ontology:measurement(ai_h_tr_t9, ai_human_relationship__technocratic_optimization, theater_ratio, 9, 0.32).
narrative_ontology:measurement_basis(ai_h_tr_t9, observed).
narrative_ontology:measurement(ai_h_tr_t12, ai_human_relationship__technocratic_optimization, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(ai_h_tr_t12, observed).
narrative_ontology:measurement(ai_h_tr_t15, ai_human_relationship__technocratic_optimization, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(ai_h_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__technocratic_optimization, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(ai_h_be_t0, observed).
narrative_ontology:measurement(ai_h_be_t3, ai_human_relationship__technocratic_optimization, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(ai_h_be_t3, observed).
narrative_ontology:measurement(ai_h_be_t6, ai_human_relationship__technocratic_optimization, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(ai_h_be_t6, observed).
narrative_ontology:measurement(ai_h_be_t9, ai_human_relationship__technocratic_optimization, base_extractiveness, 9, 0.64).
narrative_ontology:measurement_basis(ai_h_be_t9, observed).
narrative_ontology:measurement(ai_h_be_t12, ai_human_relationship__technocratic_optimization, base_extractiveness, 12, 0.69).
narrative_ontology:measurement_basis(ai_h_be_t12, observed).
narrative_ontology:measurement(ai_h_be_t15, ai_human_relationship__technocratic_optimization, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(ai_h_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__technocratic_optimization, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(ai_h_su_t0, observed).
narrative_ontology:measurement(ai_h_su_t3, ai_human_relationship__technocratic_optimization, suppression_requirement, 3, 0.48).
narrative_ontology:measurement_basis(ai_h_su_t3, observed).
narrative_ontology:measurement(ai_h_su_t6, ai_human_relationship__technocratic_optimization, suppression_requirement, 6, 0.54).
narrative_ontology:measurement_basis(ai_h_su_t6, observed).
narrative_ontology:measurement(ai_h_su_t9, ai_human_relationship__technocratic_optimization, suppression_requirement, 9, 0.6).
narrative_ontology:measurement_basis(ai_h_su_t9, observed).
narrative_ontology:measurement(ai_h_su_t12, ai_human_relationship__technocratic_optimization, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(ai_h_su_t12, observed).
narrative_ontology:measurement(ai_h_su_t15, ai_human_relationship__technocratic_optimization, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(ai_h_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__technocratic_optimization, resource_allocation).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, ai_human_relationship__instrumental_subsidiarity).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, ai_human_relationship__incarnational_humanism).

% DUAL FORMULATION NOTE:
% The colloquial label 'the AI-human relationship question' decomposes into three structurally distinct constraints sharing one kernel, per the epsilon-invariance principle. This technocratic reading authors epsilon 0.72 for the standing optimization arrangement with victims defined by scoring and pacing (managed workers, scored-out populations, profiled subjects). instrumental_subsidiarity authors lower epsilon against the same deployment record, treating pathologies as correctable governance gaps rather than features of the arrangement. incarnational_humanism authors the highest dignity-weighted epsilon with a victim set defined by incommensurability violations (persons reduced to profiles as such). The technocratic reading is upstream: its deployed systems create the fact-pattern the other two readings evaluate, so influence edges run from this story to both siblings. Family membership requires each sibling to link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_human_relationship__technocratic_optimization, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
