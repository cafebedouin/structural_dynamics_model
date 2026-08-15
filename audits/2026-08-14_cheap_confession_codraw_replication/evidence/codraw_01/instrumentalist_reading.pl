% ============================================================================
% CONSTRAINT STORY: instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_instrumentalist_reading, []).

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
 *   constraint_id: instrumentalist_reading
 *   human_readable: Instrumentalist Reading: Positional Disagreement Resolved via Generative Falsifier Tooling
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the instrumentalist reading of the kernel
 *   commitment 'positional disagreement becomes tractable evidence.' Under
 *   this reading, what makes disagreement tractable is not an epistemic
 *   principle about whose standpoint should count, nor a pragmatist appeal to
 *   what works, nor a proceduralist commitment to fair adjudication process —
 *   it is simply that a cheap generative tool now exists that can produce
 *   candidate falsifiers and alternative-position samples at scale, and that
 *   the model's classification and bookkeeping capacity does the sorting. The
 *   reading treats the kernel as realized through infrastructure capability
 *   rather than through any normative commitment about knowledge or
 *   standpoint. As the expected structural delta specifies, this exposes a
 *   beneficiary/victim split organized around SLACK (time, tooling,
 *   survivable public error) rather than around standpoint-theoretic
 *   marginalization — a genuinely new extraction path (curated menus, model
 *   agreeableness, whoever can afford to iterate the pipeline) that the
 *   standpoint, pragmatist, and proceduralist readings do not surface at all,
 *   because those readings locate the tractability claim in a different place
 *   entirely.
 *
 * KEY AGENTS:
 *   - tooling_operators: agenda_setter (institutional/arbitrage) — administers the classification/bookkeeping pipeline
 *   - well_resourced_research_labs: beneficiary (powerful/mobile) — converts tractability into publication and funding advantage
 *   - model_vendors: beneficiary/agenda_setter (institutional/arbitrage) — routes epistemic authority through proprietary tooling
 *   - under_resourced_disputants: payer (moderate/constrained) — cannot generate or curate a competing register
 *   - domain_experts_without_compute_access: payer (moderate/constrained) — substantive knowledge discounted absent tooling access
 *   - communities_whose_positions_are_absent_from_training_data: excluded (powerless/trapped) — never enter the candidate sample at all
 *   - epistemologists_of_technology: observer (analytical/analytical) — traces the substitution of tooling capacity for epistemic principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(instrumentalist_reading, 0.61).
domain_priors:suppression_score(instrumentalist_reading, 0.47).
domain_priors:theater_ratio(instrumentalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(instrumentalist_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(instrumentalist_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(instrumentalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(instrumentalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(instrumentalist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(instrumentalist_reading, "Instrumentalist Reading: Positional Disagreement Resolved via Generative Falsifier Tooling").
narrative_ontology:topic_domain(instrumentalist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(instrumentalist_reading, '945ef199-5d48-47e7-a30c-e3f8d8afbe68').
narrative_ontology:cs_kernel_codification('945ef199-5d48-47e7-a30c-e3f8d8afbe68', distributed).
narrative_ontology:cs_authority_grounding('945ef199-5d48-47e7-a30c-e3f8d8afbe68', practice).
narrative_ontology:cs_interpretation_layer_present('945ef199-5d48-47e7-a30c-e3f8d8afbe68').
narrative_ontology:cs_reading_relation('945ef199-5d48-47e7-a30c-e3f8d8afbe68', instrumentalist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('945ef199-5d48-47e7-a30c-e3f8d8afbe68', instrumentalist_reading__pragmatist_reading, influences).
narrative_ontology:cs_reading_relation('945ef199-5d48-47e7-a30c-e3f8d8afbe68', instrumentalist_reading__proceduralist_reading, influences).
narrative_ontology:cs_axiom('945ef199-5d48-47e7-a30c-e3f8d8afbe68', foundational, tractability_is_a_capacity_fact_not_a_standpoint_fact).
narrative_ontology:cs_axiom_status(tractability_is_a_capacity_fact_not_a_standpoint_fact, holdable).
narrative_ontology:cs_axiom_grounding('945ef199-5d48-47e7-a30c-e3f8d8afbe68', tractability_is_a_capacity_fact_not_a_standpoint_fact, instrumental).
narrative_ontology:cs_axiom('945ef199-5d48-47e7-a30c-e3f8d8afbe68', secondary, classification_bookkeeping_capacity_substitutes_for_epistemic_principle).
narrative_ontology:cs_axiom_status(classification_bookkeeping_capacity_substitutes_for_epistemic_principle, holdable).
narrative_ontology:cs_axiom_grounding('945ef199-5d48-47e7-a30c-e3f8d8afbe68', classification_bookkeeping_capacity_substitutes_for_epistemic_principle, instrumental).
narrative_ontology:cs_reference_frame('945ef199-5d48-47e7-a30c-e3f8d8afbe68', manual_curation_baseline).
narrative_ontology:cs_drift_state('945ef199-5d48-47e7-a30c-e3f8d8afbe68', post_generative_tooling_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('945ef199-5d48-47e7-a30c-e3f8d8afbe68', '').
narrative_ontology:cs_kernel_id(instrumentalist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(instrumentalist_reading, tooling_operators).
narrative_ontology:constraint_beneficiary(instrumentalist_reading, well_resourced_research_labs).
narrative_ontology:constraint_beneficiary(instrumentalist_reading, model_vendors).
narrative_ontology:constraint_victim(instrumentalist_reading, under_resourced_disputants).
narrative_ontology:constraint_victim(instrumentalist_reading, domain_experts_without_compute_access).
narrative_ontology:constraint_victim(instrumentalist_reading, communities_whose_positions_are_absent_from_training_data).
narrative_ontology:constraint_vindicates(instrumentalist_reading, generative_falsification_tractability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the generative pipeline that produces candidate falsifiers and alternative-position samples at scale. Sets what counts as a 'candidate' worth bookkeeping, curates prompt menus, and administers the classification scheme that turns raw disagreement into tractable evidence. Bears none of the cost of being wrong about what got excluded from the sample.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, tooling_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Has compute budget, engineering staff, and institutional patience to run the falsifier-generation loop repeatedly, iterate on prompts, and treat model output as a legitimate evidentiary register. Gains publishable, tractable 'resolutions' to previously intractable positional disputes and converts that tractability into career and funding advantage.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, well_resourced_research_labs, beneficiary,
    powerful, biographical, mobile, global).

% Supplies the underlying generative model whose classification and bookkeeping capacity is doing the actual epistemic work under this reading. Profits from the reframing of positional disagreement as a tooling problem, since that reframing routes intellectual authority and paid API access through their product rather than through any procedural or standpoint-based adjudication that would not need them.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, model_vendors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(instrumentalist_reading, model_vendors, agenda_setter).

% Holds a genuine positional stake in the disagreement but lacks the compute, tooling literacy, or institutional slack to generate and curate a competing register of falsifiers. Their position is either absent from the sample or represented only as filtered through someone else's prompt design, and they cannot survive the reputational cost of a wrong or hallucinated 'candidate falsifier' being attributed to their view.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, under_resourced_disputants, payer,
    moderate, biographical, constrained, national).

% Has deep substantive knowledge of the disputed domain but no access to the generative infrastructure now treated as the legitimate arbiter of tractability. Watches disagreements they understand from decades of fieldwork get 'resolved' by a classification system they cannot audit, contest, or reproduce without the same tooling access.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, domain_experts_without_compute_access, payer,
    moderate, biographical, constrained, national).

% Their positions never enter the generative sample at all because the underlying model was not trained on their record, language, or framing. Under this reading their exclusion is invisible — the tooling reports high coverage and tractability while their disagreement was never in the candidate menu to begin with. They have no seat at the table that decides what counts as an alternative-position sample.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, communities_whose_positions_are_absent_from_training_data, excluded,
    powerless, biographical, trapped, regional).

% Studies how the classification and bookkeeping capacity of a generative tool came to substitute for an epistemic principle about standpoint or procedure. Can trace which disagreements got 'resolved' by tractability and which got quietly excluded from the candidate set, without holding a stake in either outcome.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, epistemologists_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(instrumentalist_reading, diffuse).
narrative_ontology:fixing_cost_class(instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a genuinely useful mechanism for converting previously intractable positional disagreement into a bounded, checkable set of candidate falsifiers and alternative-position samples — something no manual review process could produce at comparable speed or scale.
% TRANSFER_FUNCTION: Moves epistemic authority and the practical capacity to 'resolve' disagreement away from parties without generative tooling access and toward whoever operates or can afford to run the pipeline; also moves attention and citation credit toward the model vendor whose classification scheme is doing the sorting.
% ABSENT_VOICES: Communities whose positions never entered training data cannot object because the tooling's tractability claim never surfaces their absence as a gap; under-resourced disputants and domain experts without compute access can see the exclusion but lack a venue that adjudicates tooling-access disparities as an epistemic harm rather than a mere resource complaint.
% DISAPPEARANCE_RATIONALE: If the generative tooling vanished, well-resourced labs would lose their primary route to publishable 'resolutions' of positional disputes, model vendors would lose the reframing that routes epistemic authority through their product, and disagreements currently treated as tractable would revert to being genuinely unresolved or would require slower, non-tooling-mediated adjudication — a real institutional and publication-pipeline rearrangement.
% FOUNDING_PROBLEM: Positional disagreements (whose framing is correct, whose standpoint should weigh more) were historically intractable to adjudicate at scale because generating and cataloguing the full space of alternative positions and their falsifiers was prohibitively labor-intensive.
% FOUNDING_PROBLEM_CORROBORATION: Tooling operators and model vendors attest the founding problem is live and substantially solved by generative capacity. Domain experts without compute access and independent epistemologists of technology attest, from outside the beneficiary set, that the underlying disagreement was never actually about generation cost — it was about whose standpoint counts — and that the tooling reframing has substituted a solvable proxy problem (candidate-generation cost) for the original, still-unresolved one (whose position gets weight).
narrative_ontology:disappearance_verdict(instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(instrumentalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(instrumentalist_reading, 0.61, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(instrumentalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(instrumentalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.32 to 0.61) as the generative pipeline moves from novelty to institutionalized evidentiary register — early adoption looked like pure coordination gain (cheap falsifier generation genuinely helps), but as labs and vendors converge on treating model output as the legitimate register, the asymmetry between who can run and curate the loop versus who cannot hardens into a durable extraction structure. Theater ratio also rises (0.18 to 0.40) because an increasing share of the pipeline's activity becomes about presenting curated menus as comprehensive coverage rather than doing the harder work of surfacing what the model's training data leaves out. Suppression rises more gently (0.22 to 0.47): it is not coercive in the classic sense, but the accessibility_collapse (0.50) reflects that once tooling-mediated tractability becomes the accepted evidentiary standard, arguing from a non-tooling-mediated standpoint increasingly reads as simply failing to engage with 'the evidence,' which functions as a soft suppression of alternative adjudication modes.
 *
 * PERSPECTIVAL GAP:
 *   From the tooling-operator and model-vendor seats, this looks like rope: a genuine coordination breakthrough that makes previously intractable disagreement checkable. From the under-resourced disputant and domain-expert seats, the same structure computes as extraction — their substantive positional stake is now adjudicated by a register they cannot afford to generate, curate, or contest, and losing that adjudication carries real professional and reputational cost they cannot survive as cheaply as the well-resourced labs can survive a wrong candidate falsifier.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (tooling_operators, well_resourced_research_labs, model_vendors) are agents with the compute, engineering staff, and institutional slack to run the generative loop repeatedly and treat its output as authoritative — low d, benefiting from the arrangement. Victims (under_resourced_disputants, domain_experts_without_compute_access, communities_whose_positions_are_absent_from_training_data) lack that slack; their exit options range from constrained to trapped, and the constraint's costs land on them regardless of whether their original standpoint was marginalized in the traditional sense — this is the structural delta the kernel context specifies: the split tracks slack, not standpoint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cataloguing alternative positions and their falsifiers was prohibitively labor-intensive) is now genuinely solved at the level of raw generation cost — that part is not mandatrophy. But the reading's institutionalization has drifted the arrangement toward treating solved generation-cost as equivalent to solved epistemic-adjudication, which it is not; the tangled_rope classification captures that a real coordination function (cheap falsifier generation) now coexists with an asymmetric extraction structure (who can afford to run and curate the loop) riding on the same mechanism, requiring active institutional enforcement (citation norms, publication standards treating tooling-mediated evidence as default) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentalism_masks_standpoint_erasure,
    'Does treating tractability as a tooling-capacity fact (rather than a standpoint or procedural fact) quietly re-encode standpoint marginalization under a new, harder-to-contest label of ''access to compute''?',
    'Compare the set of positions systematically absent from generative candidate samples against the set of positions historically marginalized under standpoint-theoretic analysis; a high overlap would indicate the instrumentalist reading is standpoint erasure wearing an infrastructure-access costume.',
    'If the overlap is high, the instrumentalist reading''s claimed independence from standpoint considerations is false, and the tangled_rope classification understates the extraction — the constraint would function closer to a snare wearing a coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalism_masks_standpoint_erasure, conceptual, 'Whether slack-based exclusion under this reading reproduces standpoint-based exclusion under a different name.').

omega_variable(
    model_agreeableness_as_extraction_vector,
    'To what extent does model agreeableness (the tendency of generative tools to produce candidate falsifiers that flatter the prompt-writer''s framing) distort the alternative-position sample in ways that specifically benefit whoever curates the prompts?',
    'Adversarial audit: have parties on opposite sides of a genuine positional dispute generate candidate falsifiers independently using the same model and compare divergence; systematic tilt toward the prompt-writer''s priors would confirm the mechanism.',
    'Confirms the story''s claim of a NEW extraction path specific to this reading — one absent from standpoint, pragmatist, and proceduralist readings, which do not depend on model output at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_agreeableness_as_extraction_vector, empirical, 'Whether generative agreeableness systematically advantages prompt-curators over disputants without curation control.').

omega_variable(
    tractability_illusion_vs_genuine_gain,
    'Is the tractability this reading claims genuine (previously irresolvable disagreements now actually get resolved) or illusory (disagreements that look resolved because the sample space was narrowed to what the tool could generate, not because the underlying dispute was settled)?',
    'Longitudinal tracking: do disputes ''resolved'' via this pipeline stay resolved under later scrutiny, or do they resurface once the excluded positions are eventually represented?',
    'If illusory, the coordination function underlying the tangled_rope classification is weaker than authored, and the constraint drifts toward snare (extraction dressed as coordination with no real coordination gain).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tractability_illusion_vs_genuine_gain, empirical, 'Whether tooling-mediated tractability represents genuine dispute resolution or narrowed sample space misread as resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(instrumentalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, instrumentalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(inst_tr_t4, instrumentalist_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(inst_tr_t8, instrumentalist_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(inst_tr_t12, instrumentalist_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(inst_tr_t16, instrumentalist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(inst_tr_t20, instrumentalist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(inst_tr_t24, instrumentalist_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, instrumentalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(inst_be_t4, instrumentalist_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(inst_be_t8, instrumentalist_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(inst_be_t12, instrumentalist_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(inst_be_t16, instrumentalist_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(inst_be_t20, instrumentalist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(inst_be_t24, instrumentalist_reading, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(inst_su_t0, instrumentalist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(inst_su_t4, instrumentalist_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(inst_su_t8, instrumentalist_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(inst_su_t12, instrumentalist_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(inst_su_t16, instrumentalist_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(inst_su_t20, instrumentalist_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(inst_su_t24, instrumentalist_reading, suppression_requirement, 24, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(instrumentalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(instrumentalist_reading, 0.12).
narrative_ontology:affects_constraint(instrumentalist_reading, standpoint_reading).
narrative_ontology:affects_constraint(instrumentalist_reading, pragmatist_reading).
narrative_ontology:affects_constraint(instrumentalist_reading, proceduralist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the kernel positional_disagreement_as_evidence. The instrumentalist_reading locates the kernel's realization in generative-tool classification/bookkeeping capacity rather than in a standpoint, pragmatist, or proceduralist principle. Its ε (0.61) and beneficiary/victim structure (slack-based, not standpoint-based) are specific to this reading and must not be averaged with or substituted for the siblings' values; each reading is its own constraint with its own stable ε per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
