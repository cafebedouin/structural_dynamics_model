% ============================================================================
% CONSTRAINT STORY: persona_as_valid_proxy_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_persona_as_valid_proxy_flat_control, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: persona_as_valid_proxy_flat_control
 *   human_readable: Persona-Record-as-Valid-Human-Proxy Substitution Commitment
 *   domain: ai_evaluation_infrastructure/research_methodology
 *
 * SUMMARY:
 *   MatrAIx-style infrastructure formalizes a substitution claim already
 *   implicit across much of contemporary simulated-user research: a
 *   schema-instantiated persona record, enacted by an LLM, is treated as a
 *   legitimate proxy for a human user whose costly participation it replaces.
 *   The paper's organizers, its validation methodology, its responsible-use
 *   disclaimers, and its deferred future-work agenda all take the
 *   substitution as given and argue only about scope and licensing conditions
 *   — what conclusions the substitution can support, under what caveats. This
 *   flat story authors that shared, undisputed commitment as a single
 *   constraint rather than decomposing it into separate readings, since no
 *   reading set was supplied; the contestation appears instead as sharp
 *   perspectival divergence across stakeholder seats and as named omegas
 *   about where the substitution's validity actually holds.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(persona_as_valid_proxy_flat_control, 0.58).
domain_priors:suppression_score(persona_as_valid_proxy_flat_control, 0.42).
domain_priors:theater_ratio(persona_as_valid_proxy_flat_control, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(persona_as_valid_proxy_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(persona_as_valid_proxy_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(persona_as_valid_proxy_flat_control, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(persona_as_valid_proxy_flat_control, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(persona_as_valid_proxy_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(persona_as_valid_proxy_flat_control, tangled_rope).
narrative_ontology:human_readable(persona_as_valid_proxy_flat_control, "Persona-Record-as-Valid-Human-Proxy Substitution Commitment").
narrative_ontology:topic_domain(persona_as_valid_proxy_flat_control, "ai_evaluation_infrastructure/research_methodology").

domain_priors:requires_active_enforcement(persona_as_valid_proxy_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(persona_as_valid_proxy_flat_control, persona_as_valid_proxy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(persona_as_valid_proxy_flat_control, matraix_infrastructure_developers).
narrative_ontology:constraint_beneficiary(persona_as_valid_proxy_flat_control, platform_researchers_publishing_under_deadline).
narrative_ontology:constraint_beneficiary(persona_as_valid_proxy_flat_control, llm_vendors_selling_evaluation_capacity).
narrative_ontology:constraint_beneficiary(persona_as_valid_proxy_flat_control, conference_reviewers_needing_tractable_submissions).
narrative_ontology:constraint_victim(persona_as_valid_proxy_flat_control, underrepresented_user_populations_poorly_modeled_by_personas).
narrative_ontology:constraint_victim(persona_as_valid_proxy_flat_control, human_crowdworkers_displaced_from_evaluation_pipelines).
narrative_ontology:constraint_victim(persona_as_valid_proxy_flat_control, downstream_product_teams_relying_on_persona_validated_findings).
narrative_ontology:constraint_victim(persona_as_valid_proxy_flat_control, end_users_of_systems_tuned_against_synthetic_proxies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(persona_as_valid_proxy_flat_control, downstream_product_teams_relying_on_persona_validated_findings).
narrative_ontology:constraint_victim(persona_as_valid_proxy_flat_control, platform_researchers_publishing_under_deadline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built and maintain the persona-generation schema and the LLM-driven enactment pipeline. They decide what counts as a valid persona record, what the responsible-use section permits, and what future-work items get deferred rather than resolved now. They benefit from every downstream adoption citing the infrastructure, and bear none of the cost if a persona-based finding later fails to generalize to real users.
narrative_ontology:constraint_stakeholder(persona_as_valid_proxy_flat_control, matraix_infrastructure_developers, agenda_setter,
    institutional, generational, arbitrage, global).

% Use persona-based evaluation because recruiting, compensating, and screening human participants is slow and expensive relative to publication and product cycles. They get faster iteration and larger nominal sample sizes. They also inherit the risk: if the substitution claim is later shown unsound for their domain, their published results and shipped features are implicated, but by then attribution is diffuse.
narrative_ontology:constraint_stakeholder(persona_as_valid_proxy_flat_control, platform_researchers_publishing_under_deadline, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(persona_as_valid_proxy_flat_control, platform_researchers_publishing_under_deadline, payer).

% Sell the compute and API access that personas run on. Every persona-based evaluation is billable inference volume. They have no stake in whether the substitution is methodologically sound, only in whether it is adopted; their commercial interest and the infrastructure's adoption interest point the same direction.
narrative_ontology:constraint_stakeholder(persona_as_valid_proxy_flat_control, llm_vendors_selling_evaluation_capacity, beneficiary,
    institutional, generational, arbitrage, global).

% Evaluate submissions under time pressure and reward papers that produce clean, large-N, reproducible-looking results. Persona-based studies are easier to review favorably than small human studies with messy variance. Reviewers who insist on human validation slow the pipeline for everyone and face pushback from authors citing the infrastructure's own responsible-use framing as sufficient.
narrative_ontology:constraint_stakeholder(persona_as_valid_proxy_flat_control, conference_reviewers_needing_tractable_submissions, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(persona_as_valid_proxy_flat_control, conference_reviewers_needing_tractable_submissions, agenda_setter).

% Are statistically thin or absent in the categorical attribute schemas the personas are instantiated from — schemas built from convenience demographics and existing survey taxonomies. Findings validated against persona populations that underweight them get built into products they then have to use, with no seat in the validation loop and no mechanism to flag mismatch after deployment.
narrative_ontology:constraint_stakeholder(persona_as_valid_proxy_flat_control, underrepresented_user_populations_poorly_modeled_by_personas, payer,
    powerless, biographical, trapped, global).

% Previously paid (poorly, but paid) to provide the human-evaluation data the substitution claim is designed to replace. As persona-based evaluation is adopted, this income stream and the associated data-labor relationship shrinks. They have no formal role in the substitution debate and no compensation for the training data their prior labor contributed to the models now standing in for them.
narrative_ontology:constraint_stakeholder(persona_as_valid_proxy_flat_control, human_crowdworkers_displaced_from_evaluation_pipelines, payer,
    powerless, biographical, trapped, global).

% Consume research findings produced via persona-based evaluation to make product decisions, trusting the paper's validation methodology because re-deriving it with human subjects is not in their budget or timeline. They benefit from cheap, fast evidence but bear the cost of decisions built on a substitution claim they did not independently verify.
narrative_ontology:constraint_stakeholder(persona_as_valid_proxy_flat_control, downstream_product_teams_relying_on_persona_validated_findings, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(persona_as_valid_proxy_flat_control, downstream_product_teams_relying_on_persona_validated_findings, beneficiary).

% Interact with products whose design and tuning decisions were validated against LLM-driven personas rather than against people like them. Where the persona's behavior diverges from theirs, the mismatch surfaces only as a degraded product experience with no visible causal chain back to the substitution claim that produced it.
narrative_ontology:constraint_stakeholder(persona_as_valid_proxy_flat_control, end_users_of_systems_tuned_against_synthetic_proxies, payer,
    powerless, biographical, trapped, global).

% Independent researchers who could, in principle, run parallel human studies against persona-based findings to test whether the substitution holds. Currently a small, underfunded community relative to the volume of persona-based publication; their capacity to corroborate or refute the substitution claim lags far behind the rate at which the claim is being operationalized into products and papers.
narrative_ontology:constraint_stakeholder(persona_as_valid_proxy_flat_control, methodology_auditors_and_replication_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(persona_as_valid_proxy_flat_control, diffuse).
narrative_ontology:fixing_cost_class(persona_as_valid_proxy_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common schema and enactment pipeline so that researchers across labs and institutions can run comparable, reproducible, low-cost user-behavior evaluations without each lab independently recruiting, screening, and compensating human panels — a genuine coordination problem when human-subject evaluation at the needed scale and speed would otherwise be prohibitively expensive or simply not attempted.
% TRANSFER_FUNCTION: Moves evaluation cost and labor away from paid human participants and crowdworkers toward compute providers and infrastructure maintainers, while moving epistemic risk (the chance that persona behavior does not track real user behavior) onto whoever is thinly represented in the persona schema and onto downstream consumers of the resulting findings, who bear the cost of any mismatch without having had a voice in the substitution.
% ABSENT_VOICES: The populations the personas are meant to stand in for — especially those underrepresented in the categorical schema — have no seat in deciding what counts as a valid persona, what the responsible-use section permits, or what future-work items get deferred. Displaced crowdworkers, whose prior labor and data underwrote the LLMs now enacting personas, are similarly absent from the substitution debate despite being its most direct economic casualty.
% DISAPPEARANCE_RATIONALE: If the substitution commitment were withdrawn — if personas were no longer accepted as valid stand-ins for human evaluation — the entire fast-iteration research and product pipeline built on MatrAIx-style infrastructure would need to revert to slower, costlier human-subject studies. Publication timelines would lengthen, some lines of research would become infeasible at current budgets, and LLM vendors would lose a category of billable usage. The rearrangement would be substantial, which is exactly why the commitment is defended rather than merely assumed.
% FOUNDING_PROBLEM: Human-subject user evaluation is slow, expensive, hard to scale, and hard to reproduce across labs — a real bottleneck on iterating and validating systems that need to be tested against diverse simulated user behavior before or in place of costly live deployment testing.
% FOUNDING_PROBLEM_CORROBORATION: The infrastructure developers and adopting researchers attest the bottleneck is real and still binding — human evaluation remains expensive and slow. Independent methodology auditors and a minority of the reviewer community attest that the bottleneck, while real, does not establish that persona-based substitution actually solves it faithfully rather than merely appearing to at publication speed; no population external to the beneficiary set (the represented-and-modeled users, the crowdworkers, or the poorly-modeled populations) has been asked to corroborate that the substitution tracks their behavior, and the paper's own future-work section defers this validation rather than resolving it now.
narrative_ontology:disappearance_verdict(persona_as_valid_proxy_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(persona_as_valid_proxy_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(persona_as_valid_proxy_flat_control, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-10',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(persona_as_valid_proxy_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(persona_as_valid_proxy_flat_control, 0.58, 'claude-sonnet-5', 'matraix_persona_simulation_2026_20260810_114056', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(persona_as_valid_proxy_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(persona_as_valid_proxy_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(persona_as_valid_proxy_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as moderate-to-substantial and rising (0.35 to 0.58 over the interval) because the primary cost — the risk that findings validated on personas do not generalize to real users, and the labor displacement of human evaluators — is structurally externalized onto powerless, poorly-represented, or entirely absent parties, while the infrastructure developers, LLM vendors, and time-pressured researchers capture the efficiency gains. Theater ratio is authored as rising in parallel (0.20 to 0.47) because the responsible-use section and the deferred future-work agenda increasingly function as a compliance gesture — a place to name the substitution's limits in prose without actually resolving them empirically before further adoption proceeds. Suppression (0.42) is moderate rather than severe: no one is coercively barred from running parallel human studies, but the incentive gradient (speed, cost, publication tractability) makes the alternative practically unavailable to most actors in the pipeline, which is a softer but real form of alternative-foreclosure.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure developer and LLM vendor seats, the constraint reads as pure coordination — a shared standard solving a real cost-and-scale bottleneck, exactly the rope framing the paper's own language invites. From the seat of underrepresented populations and displaced crowdworkers, the same structure reads as extraction: their exclusion or displacement is the mechanism by which the coordination gain is realized, and the active maintenance of the substitution claim (through responsible-use framing that permits rather than restricts continued use) is what tangled-rope enforcement looks like from underneath. The engine's per-seat computation should surface this divergence rather than requiring the story to average it into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries — infrastructure developers, LLM vendors, deadline-pressured researchers, reviewers seeking tractable submissions — sit near the low end of the directionality scale: the substitution subsidizes their throughput and revenue. Victims sit near the high end: underrepresented populations bear silent mismatch risk with no exit (trapped, powerless, global scope compounding the difficulty of ever tracing a harm back to its source); displaced crowdworkers lose income with no seat in the debate; downstream product teams and end users inherit epistemic risk they did not choose and cannot easily audit. The auditor/replication seat is analytical and structurally underpowered relative to the volume of persona-based output it would need to check.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — human evaluation is slow and expensive — remains genuinely live; this is not a pure zombie mandate. But the paper's future-work section defers the actual test of whether personas validly substitute for humans, rather than resolving it, which means the infrastructure's continued expansion outruns its own evidentiary licensing. Classifying this as tangled_rope rather than rope or snare prevents two mislabelings: treating it as pure coordination would erase the externalized risk borne by unrepresented and displaced parties; treating it as pure extraction would erase the genuine and increasingly binding cost problem that motivated the infrastructure in the first place. The tangled_rope label holds both facts open simultaneously, which is what the founding-problem-status of 'contested' is meant to reflect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persona_behavioral_fidelity_ambiguity,
    'Does an LLM-enacted persona record, however carefully schema-instantiated, actually reproduce the behavioral distribution of the human population it claims to represent, or does it reproduce the LLM''s own priors about that population filtered through the schema''s categorical attributes?',
    'Systematic parallel studies comparing persona-driven evaluation outcomes against matched human-subject studies across multiple domains and demographic strata, with particular attention to populations thinly represented in the persona schema''s training and design data.',
    'If fidelity is high and broadly uniform across populations, the coordination function dominates and the constraint drifts toward rope; if fidelity is low or systematically worse for underrepresented populations, the extraction function dominates and the constraint drifts toward snare, with the coordination language serving primarily as cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persona_behavioral_fidelity_ambiguity, empirical, 'Whether persona enactment tracks real human behavioral distributions or only the model''s priors about them.').

omega_variable(
    responsible_use_section_as_licensing_vs_limiting,
    'Does the paper''s responsible-use section function to genuinely limit the scope of claims the substitution can support, or does it function to license broader adoption by pre-empting the objection that validation is incomplete?',
    'Track citation patterns: do downstream papers and products cite the responsible-use caveats to restrict their own claims, or cite the existence of a responsible-use section as evidence the methodology has already addressed validity concerns?',
    'If citations show restriction, the section functions as intended coordination infrastructure; if citations show pre-emptive licensing without restriction, the theater ratio is understated and the section is functioning as compliance theater masking accelerating adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsible_use_section_as_licensing_vs_limiting, empirical, 'Whether the responsible-use framing genuinely constrains claims or licenses their expansion.').

omega_variable(
    displaced_labor_compensation_question,
    'Is the displacement of human crowdworkers from evaluation pipelines a genuine efficiency gain (the task is now better done by cheaper automated means) or a transfer that should trigger some compensation obligation given that crowdworker-generated data trained the LLMs now performing the substitution?',
    'This is fundamentally a normative and policy question about data-labor provenance and compensation, not resolvable by further technical measurement alone, though technical work could establish how directly current persona behavior traces to specific historical human-labeled datasets.',
    'If a compensation obligation is recognized, the extraction framing strengthens considerably and victim status for crowdworkers becomes harder to dispute even by the infrastructure''s defenders; if not, the displacement is framed as ordinary technological substitution outside the constraint''s extraction accounting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_labor_compensation_question, preference, 'Whether crowdworker displacement constitutes uncompensated extraction or ordinary technological substitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(persona_as_valid_proxy_flat_control, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, persona_as_valid_proxy_flat_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pers_tr_t4, persona_as_valid_proxy_flat_control, theater_ratio, 4, 0.26).
narrative_ontology:measurement(pers_tr_t8, persona_as_valid_proxy_flat_control, theater_ratio, 8, 0.32).
narrative_ontology:measurement(pers_tr_t12, persona_as_valid_proxy_flat_control, theater_ratio, 12, 0.37).
narrative_ontology:measurement(pers_tr_t16, persona_as_valid_proxy_flat_control, theater_ratio, 16, 0.41).
narrative_ontology:measurement(pers_tr_t20, persona_as_valid_proxy_flat_control, theater_ratio, 20, 0.44).
narrative_ontology:measurement(pers_tr_t24, persona_as_valid_proxy_flat_control, theater_ratio, 24, 0.47).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, persona_as_valid_proxy_flat_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pers_be_t4, persona_as_valid_proxy_flat_control, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(pers_be_t8, persona_as_valid_proxy_flat_control, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(pers_be_t12, persona_as_valid_proxy_flat_control, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(pers_be_t16, persona_as_valid_proxy_flat_control, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(pers_be_t20, persona_as_valid_proxy_flat_control, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(pers_be_t24, persona_as_valid_proxy_flat_control, base_extractiveness, 24, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(persona_as_valid_proxy_flat_control, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(persona_as_valid_proxy_flat_control, resource_allocation).
narrative_ontology:boltzmann_floor_override(persona_as_valid_proxy_flat_control, 0.12).

% DUAL FORMULATION NOTE:
% This is a flat (undecomposed) construction of the persona-as-valid-proxy substrate, authored per instruction without a reading set. Should a decomposition later be authored (e.g., separating a 'persona validity for low-stakes UI testing' reading from a 'persona validity for high-stakes policy or safety evaluation' reading), those stories should link back to this one via affects_constraints and document the ε divergence explicitly, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
