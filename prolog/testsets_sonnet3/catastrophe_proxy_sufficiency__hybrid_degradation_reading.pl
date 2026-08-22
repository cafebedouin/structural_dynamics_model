% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__hybrid_degradation_reading, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Simulation-as-Sufficient-Proxy: Hybrid Degradation Reading
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the hybrid_degradation_reading of the
 *   catastrophe_proxy_sufficiency kernel: the contested question of whether
 *   simulation-based training is sufficient to maintain long-run operational
 *   competence in high-reliability organizations absent real catastrophic
 *   events. This reading holds that simulation genuinely maintains PROCEDURAL
 *   competence (the checklist-executable layer) while a distinct and
 *   harder-to-measure layer — tacit knowledge and improvisational
 *   stress-response capacity — degrades across generational timescales
 *   specifically because real catastrophes are (fortunately) rare, and the
 *   cohort that once carried lived catastrophe exposure eventually retires
 *   without having fully transmitted what it knew. The constraint is the
 *   recertification-and-simulation regime itself: it is not pure Rope
 *   (procedural competence genuinely is maintained — there is real
 *   coordination value) and not pure Snare (no single party is
 *   straightforwardly looting the system), but a Tangled Rope, because the
 *   same structure that solves the genuine training-access problem also
 *   generates a certification industry with a durable financial interest in
 *   the proxy remaining the accepted standard of sufficiency, and because the
 *   costs of the reading's hypothesized decay are borne by parties (future
 *   cohorts, the public) who have no seat in setting the standard.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.62).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-as-Sufficient-Proxy: Hybrid Degradation Reading").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '2b6d3408-75af-4047-9aa9-5c6557f9e47a').
narrative_ontology:cs_kernel_codification('2b6d3408-75af-4047-9aa9-5c6557f9e47a', distributed).
narrative_ontology:cs_authority_grounding('2b6d3408-75af-4047-9aa9-5c6557f9e47a', practice).
narrative_ontology:cs_interpretation_layer_present('2b6d3408-75af-4047-9aa9-5c6557f9e47a').
narrative_ontology:cs_reading_relation('2b6d3408-75af-4047-9aa9-5c6557f9e47a', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('2b6d3408-75af-4047-9aa9-5c6557f9e47a', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('2b6d3408-75af-4047-9aa9-5c6557f9e47a', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('2b6d3408-75af-4047-9aa9-5c6557f9e47a', foundational, competence_is_layered_and_differentially_decayable).
narrative_ontology:cs_axiom_status(competence_is_layered_and_differentially_decayable, holdable).
narrative_ontology:cs_axiom_grounding('2b6d3408-75af-4047-9aa9-5c6557f9e47a', competence_is_layered_and_differentially_decayable, empirically_contingent).
narrative_ontology:cs_axiom('2b6d3408-75af-4047-9aa9-5c6557f9e47a', foundational, generational_turnover_is_the_operative_decay_mechanism).
narrative_ontology:cs_axiom_status(generational_turnover_is_the_operative_decay_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('2b6d3408-75af-4047-9aa9-5c6557f9e47a', generational_turnover_is_the_operative_decay_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('2b6d3408-75af-4047-9aa9-5c6557f9e47a', procedural_certification_as_competence_proxy).
narrative_ontology:cs_drift_state('2b6d3408-75af-4047-9aa9-5c6557f9e47a', post_multigenerational_operator_turnover, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2b6d3408-75af-4047-9aa9-5c6557f9e47a', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulator_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_compliance_apparatus).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_operator_cohorts).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, downstream_public_safety_bearers).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__hybrid_degradation_reading, procedural_competence_is_measurable_and_sufficient).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, sells, and administers the simulator-based recertification cycles that regulators require. Revenue is recurring and scales with cycle frequency, not with any measured improvement in actual crisis outcomes. Has no structural incentive to fund the harder, more expensive, and reputationally riskier work of measuring tacit-knowledge or stress-response decay, since the current proxy (procedural pass rates) already satisfies the compliance requirement it profits from administering.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry, agenda_setter).

% Sell increasingly sophisticated simulation hardware and software contracted on the premise that fidelity improvements close the gap with real catastrophic stress. Each generation of simulator is marketed as solving the fidelity problem the previous generation admitted to. Benefits from the credentialing cycle continuing indefinitely; a declared 'fidelity ceiling' below true catastrophe stress would undercut future upgrade sales.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulator_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Mandates simulation-based recertification as the operative safety standard because it is auditable, standardized, and legally defensible in a way that 'genuine tacit competence' is not. Administering a checkable proxy discharges the regulator's own liability even if the proxy's relationship to real emergency performance is uncertain. Has structural reasons to prefer a measurable standard over an unmeasurable truth.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_compliance_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_compliance_apparatus, beneficiary).

% Carry tacit knowledge and calibrated fear from having lived through or been trained by people who lived through real catastrophic events. As this cohort retires, no simulation curriculum has captured what they knew that wasn't in the procedure manual — the loss shows up only as a gap in institutional memory, and there is no forum where their qualitative warnings about simulation's limits are formally weighted against certification pass-rate data.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, veteran_operators, excluded,
    moderate, biographical, constrained, national).

% Enter the profession fully trained under the simulation regime, procedurally certified, and structurally unaware of what stress-response capacity or improvisational judgment they lack, because the gap is invisible until a real event occurs. They cannot exit the system that trained them, cannot benchmark themselves against a standard they were never exposed to, and bear the eventual consequences of a decay process the current metrics cannot see.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, future_operator_cohorts, payer,
    powerless, generational, trapped, national).

% The public that depends on high-reliability organizations (nuclear operators, air traffic control, disaster response) performing correctly during the rare real event. They have no visibility into the degradation of tacit competence beneath certified procedural competence and no mechanism to demand a different verification standard; they discover the gap, if it exists, only at the moment of failure.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, downstream_public_safety_bearers, payer,
    powerless, civilizational, trapped, national).

% Study near-miss data, incident reconstructions, and organizational memory loss across high-reliability industries. Positioned to detect the divergence between procedural pass rates and actual crisis performance but structurally dependent on the same institutions (regulators, operators, training industry) for access and funding, which constrains how forcefully the divergence can be documented.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation-based recertification solves a genuine and otherwise intractable problem: real catastrophes are too rare, too costly, and too dangerous to use as the primary training mechanism, so a repeatable, auditable, safe substitute is needed to maintain baseline procedural competence across a large workforce.
% TRANSFER_FUNCTION: Recurring certification fees and simulator contracts move from operating organizations (and ultimately from public/ratepayer budgets) to the training and simulator-vendor industry; in exchange, the industry supplies a credential whose relationship to actual catastrophe-grade competence attenuates generationally as the cohort with lived catastrophe exposure retires.
% ABSENT_VOICES: Retired and retiring veteran operators who hold the tacit knowledge the simulation curriculum was never able to fully encode are not systematically consulted in setting recertification content; the public that ultimately bears the consequence of a stress-response failure has no representation in the certification design process at all.
% DISAPPEARANCE_RATIONALE: If simulation-based certification vanished overnight, the certification industry's assertion is that competence would collapse immediately (world_rearranges); the hybrid-degradation reading's assertion is different and more unsettling — visible procedural competence would look identical in the short run, because the decay this reading tracks is generational and latent, not immediate. The disagreement about what 'disappearance' would reveal is itself part of what makes this reading distinct from its siblings.
% FOUNDING_PROBLEM: High-reliability industries needed a way to train and re-train operators for rare, high-consequence events without waiting for or manufacturing real catastrophes, and needed a defensible, auditable standard regulators could certify against.
% FOUNDING_PROBLEM_CORROBORATION: The certification industry and regulators attest the founding problem remains fully live and solved by current practice. Independent safety researchers studying incident reconstructions (e.g., post-event analyses in aviation and nuclear operations) attest that procedural certification correlates poorly with in-crisis improvisational performance, and that this gap widens as the last catastrophe-experienced cohort retires — corroboration from outside the certifying and vending institutions, though constrained by those researchers' partial funding dependence on the same institutions.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.58 rather than extreme, because the coordination function is real and substantial — simulation training is not a scam, it demonstrably prevents a great deal of harm relative to no training at all. What is extractive is the certification industry's structural indifference to closing the specific gap this reading identifies (tacit/stress-response decay), because closing it would require admitting the current proxy is insufficient, which threatens the recurring-revenue model built on the proxy's presumed sufficiency. Theater ratio rises over the interval (0.28 → 0.61) because, absent this reading's hypothesized decay being visibly measured, the recertification cycle increasingly performs assurance (more sessions, more hours logged, more credentials issued) without a corresponding measurement of the thing that actually matters for rare-event performance. Suppression is mid-range and rising (0.45 → 0.62): there is no coercive suppression of alternatives in a simple sense, but the audit/liability logic of regulators structurally suppresses serious investment in unmeasurable-but-important competencies in favor of measurable-but-partial ones.
 *
 * DIRECTIONALITY LOGIC:
 *   The certification and vendor industries sit near the beneficiary end: they collect recurring, scalable revenue from a standard whose sufficiency is exactly the contested question, and they have institutional reasons not to fund the research that would test the hybrid-degradation hypothesis rigorously. Future operator cohorts and the downstream public sit near the target end: they are structurally trapped (a new operator cannot self-certify past the required simulation regime; the public cannot select a different high-reliability operator on the basis of tacit-competence retention) and they bear the tail-risk cost of a decay process that, by this reading's own logic, is invisible until a real catastrophe occurs. Veteran operators are excluded rather than positioned as clean beneficiaries or victims — they are a diminishing resource whose knowledge is the very thing at stake, but they hold no formal power over the standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite mislabelings. Calling this a pure Rope (as the simulation_as_proxy_catastrophe_reading effectively does) would erase the certification industry's structural stake in the proxy remaining unquestioned, and would treat the founding problem as permanently solved rather than as contested and potentially decaying. Calling it a pure Snare (extraction with no genuine function) would be equally wrong and would ignore that simulation training measurably reduces baseline procedural failure — the coordination function is real. Tangled Rope holds both facts without collapsing them: genuine coordination value plus a hidden, generationally-scaled extraction mechanism riding on the same structure, sustained by active enforcement (mandatory recertification) rather than by voluntary participation alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_decay_measurability,
    'Is generational tacit-knowledge and stress-response decay actually occurring and measurable, or is this reading a plausible-sounding hypothesis with no clean empirical instrument to confirm or falsify it before a real catastrophe provides the test?',
    'Longitudinal comparison of near-miss and actual-incident response quality across operator cohorts stratified by whether their training included direct exposure to real catastrophic events versus simulation-only training, controlling for procedural certification level.',
    'If decay is confirmed and measurable, this reading strengthens toward snare (the certification industry is knowingly selling insufficient assurance); if decay cannot be detected even with real incident data, this reading collapses toward the simulation_as_proxy_catastrophe_reading (rope), and the tangled_rope classification here would be overclaiming extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tacit_decay_measurability, empirical, 'Whether the hybrid reading''s central causal claim is empirically detectable.').

omega_variable(
    certification_industry_incentive_structure,
    'Does the certification and simulator-vendor industry have a structural incentive to under-invest in detecting or addressing the specific decay this reading identifies, or is the industry a neutral technical actor whose standards evolve with the state of the art regardless of revenue implications?',
    'Compare R&D and standard-revision investment by certification bodies against independent academic safety-research funding and findings; look for cases where industry standards lagged or resisted evidence of a gap the reading would predict.',
    'If the industry demonstrably resists or slow-walks standard revisions that would reveal insufficiency, that corroborates the beneficiary/victim structure and the tangled_rope classification; if the industry actively funds and adopts the research that would undercut its own proxy, the beneficiary framing weakens toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_industry_incentive_structure, empirical, 'Whether beneficiary incentives structurally oppose closing the identified gap.').

omega_variable(
    kernel_reading_disagreement_location,
    'Is the disagreement among the four kernel readings actually about a shared empirical fact (does simulation preserve stress-response capacity), or about an unshareable definitional question (what counts as ''competence'' and whether procedural and tacit competence are even the same kind of thing)?',
    'This is a conceptual, not empirical, ambiguity — it would require the kernel''s parties to agree on a shared operational definition of ''competence'' before the sibling readings could even be tested against the same evidence.',
    'If the readings are talking past each other on definitions, no amount of incident data resolves the kernel contest, and each reading remains a live, non-foreclosed position (consistent with coexists_with relations to the siblings rather than forecloses).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel dispute is empirical or definitional at its root.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 24, 0.51).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 32, 0.57).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cata_su_t8, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(cata_su_t16, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(cata_su_t24, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(cata_su_t32, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language concept 'is simulation sufficient to maintain catastrophe-readiness competence' (the catastrophe_proxy_sufficiency kernel). Each sibling reading authors a different epsilon and a different structural type because each makes a different causal and definitional claim about what 'competence' is and whether simulation reaches it: simulation_as_proxy_catastrophe_reading claims full sufficiency (expected lower epsilon, rope-leaning); catastrophe_necessity_reading claims categorical insufficiency (expected higher epsilon, snare-leaning); simulation_fidelity_threshold makes sufficiency a technology-contingent engineering variable (expected variable epsilon depending on fidelity generation); this hybrid_degradation_reading claims partial, time-decaying sufficiency (moderate epsilon, tangled_rope, because a real coordination function coexists with a hidden extraction mechanism riding on unmeasured decay). Per the ε-invariance principle, these are four separate constraints, not one constraint measured four ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
