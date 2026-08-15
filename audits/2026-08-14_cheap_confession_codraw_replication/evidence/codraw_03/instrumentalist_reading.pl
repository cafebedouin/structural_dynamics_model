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
 *   human_readable: Instrumentalist Reading: Positional Disagreement as Machine-Curated Evidence
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the instrumentalist reading of the kernel
 *   'positional disagreement as evidence': the claim that positional
 *   disagreement becomes tractable evidence specifically because a cheap
 *   generative tool now exists to mass-produce candidate falsifiers and
 *   alternative-position samples. On this reading, the kernel commitment is
 *   realized through the model's classification and bookkeeping capacity —
 *   its ability to generate, tag, and curate a register of alternative
 *   positions at scale — not through any prior epistemic principle about
 *   standpoint, lived experience, or procedural fairness. The coordination
 *   function is real: disputes that were previously unfalsifiable in practice
 *   (too expensive to generate a representative sample of counter-positions)
 *   now have a cheap mechanism for doing so. But the same mechanism installs
 *   a new extraction path absent from the sibling readings: whoever can run,
 *   iterate, and curate the generative loop controls what counts as the
 *   tractable register, and bears none of the epistemic cost of positions the
 *   tool didn't generate or a curator didn't have time to check. Extraction
 *   is measured at the standing arrangement (curated-menu-as-evidence), not
 *   at any endorsed alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(instrumentalist_reading, 0.61).
domain_priors:suppression_score(instrumentalist_reading, 0.42).
domain_priors:theater_ratio(instrumentalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(instrumentalist_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(instrumentalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(instrumentalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(instrumentalist_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(instrumentalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(instrumentalist_reading, "Instrumentalist Reading: Positional Disagreement as Machine-Curated Evidence").
narrative_ontology:topic_domain(instrumentalist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(instrumentalist_reading, 'bb005f14-64fa-403e-b541-4c83d50ba64c').
narrative_ontology:cs_kernel_codification('bb005f14-64fa-403e-b541-4c83d50ba64c', distributed).
narrative_ontology:cs_authority_grounding('bb005f14-64fa-403e-b541-4c83d50ba64c', practice).
narrative_ontology:cs_interpretation_layer_present('bb005f14-64fa-403e-b541-4c83d50ba64c').
narrative_ontology:cs_reading_relation('bb005f14-64fa-403e-b541-4c83d50ba64c', instrumentalist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb005f14-64fa-403e-b541-4c83d50ba64c', instrumentalist_reading__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb005f14-64fa-403e-b541-4c83d50ba64c', instrumentalist_reading__proceduralist_reading, influences).
narrative_ontology:cs_axiom('bb005f14-64fa-403e-b541-4c83d50ba64c', foundational, tractability_realized_through_generative_capacity).
narrative_ontology:cs_axiom_status(tractability_realized_through_generative_capacity, holdable).
narrative_ontology:cs_axiom_grounding('bb005f14-64fa-403e-b541-4c83d50ba64c', tractability_realized_through_generative_capacity, instrumental).
narrative_ontology:cs_axiom('bb005f14-64fa-403e-b541-4c83d50ba64c', secondary, classification_bookkeeping_supersedes_standpoint_criteria).
narrative_ontology:cs_axiom_status(classification_bookkeeping_supersedes_standpoint_criteria, holdable).
narrative_ontology:cs_axiom_grounding('bb005f14-64fa-403e-b541-4c83d50ba64c', classification_bookkeeping_supersedes_standpoint_criteria, conventional).
narrative_ontology:cs_reference_frame('bb005f14-64fa-403e-b541-4c83d50ba64c', generative_tractability_via_classification).
narrative_ontology:cs_drift_state('bb005f14-64fa-403e-b541-4c83d50ba64c', contemporary_toolchain_saturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bb005f14-64fa-403e-b541-4c83d50ba64c', '').
narrative_ontology:cs_kernel_id(instrumentalist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(instrumentalist_reading, tooling_operators).
narrative_ontology:constraint_beneficiary(instrumentalist_reading, well_resourced_research_labs).
narrative_ontology:constraint_beneficiary(instrumentalist_reading, credentialed_analysts_with_compute_access).
narrative_ontology:constraint_victim(instrumentalist_reading, under_resourced_disputants).
narrative_ontology:constraint_victim(instrumentalist_reading, communities_without_curation_capacity).
narrative_ontology:constraint_victim(instrumentalist_reading, domain_experts_outside_the_toolchain).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(instrumentalist_reading, credentialed_analysts_with_compute_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds and administers the generative model used to produce candidate falsifiers and alternative-position samples at scale. Sets the classification scheme, the bookkeeping conventions, and the default menu of generated alternatives that count as 'the register' of live positions. Can iterate the model, adjust its agreeableness, and change what counts as a tractable falsifier at will.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, tooling_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Has the compute budget, staff time, and institutional standing to run the generate-and-curate loop repeatedly, survive public errors when a generated falsifier turns out embarrassing, and fold the resulting register into publications and grant applications. Treats the tool's output as evidence because it can afford to interrogate, discard, and regenerate it.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, well_resourced_research_labs, beneficiary,
    organized, biographical, mobile, national).

% Individual scholars or practitioners who can run the tool but bear real reputational risk from a single bad generated sample entering the record; they gain tractability but must also spend real time curating output against model agreeableness (the tool's tendency to generate whatever plausible-sounding alternative fits the prompt).
narrative_ontology:constraint_stakeholder(instrumentalist_reading, credentialed_analysts_with_compute_access, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(instrumentalist_reading, credentialed_analysts_with_compute_access, payer).

% Parties to the original positional disagreement who lack the time, tooling, or organizational slack to generate their own falsifiers or curate a competing register. Their position gets represented, if at all, through someone else's generated menu of 'plausible alternative positions' rather than their own articulation, and they cannot survive a public error the way a lab can.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, under_resourced_disputants, payer,
    powerless, immediate, trapped, local).

% Groups whose disagreements get rendered tractable only by adopting the toolchain's output wholesale, since they cannot afford the labor of checking generated candidates against lived specifics. Whatever the model classifies as the live alternative-position set becomes their evidentiary record by default, not by choice.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, communities_without_curation_capacity, payer,
    powerless, generational, trapped, regional).

% Possess deep positional knowledge but do not use, trust, or have access to the generative tool. Their disagreements remain classified as intractable or 'merely positional' precisely because they never enter the tool's bookkeeping, regardless of the substantive merit of their objections.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, domain_experts_outside_the_toolchain, excluded,
    moderate, biographical, constrained, national).

% Traces how tractability claims move from 'this disagreement can now be evidenced' to 'this disagreement can now be evidenced by whoever can run the loop,' and documents which register becomes canonical and why.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, standing_arrangement_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(instrumentalist_reading, tooling_operators).
narrative_ontology:fixing_cost_class(instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely converts previously intractable positional disagreements into something with tractable evidentiary structure: a cheap generative tool can mass-produce candidate falsifiers and alternative-position samples, giving disputants a shared register of testable claims where none existed before.
% TRANSFER_FUNCTION: Moves epistemic authority from unaided positional articulation toward whoever can operate, iterate, and curate the generative tool's output — shifting evidentiary weight from lived standpoint or procedural agreement toward classification-and-bookkeeping capacity, and moving reputational risk of curation errors onto those who can least absorb it.
% ABSENT_VOICES: Under-resourced disputants and domain experts who never touch the toolchain would object that the resulting register reflects what the model can cheaply generate and what curators had time to check, not what their disagreement actually turns on; they are not consulted because the loop runs without them by construction.
% DISAPPEARANCE_RATIONALE: Tooling operators and well-resourced labs would say the world rearranges badly — a genuine tractability gain is lost and disagreements revert to unresolvable positional standoff. Under-resourced disputants and excluded domain experts would say comparatively little rearranges for them, since the register was never built from their standpoint and its disappearance mainly removes a filter they never controlled.
% FOUNDING_PROBLEM: Positional disagreements (disputes rooted in standpoint, values, or procedure rather than shared facts) historically resisted evidentiary treatment because generating a representative sample of falsifiers or alternative positions was too labor-intensive to do at scale for any given dispute.
% FOUNDING_PROBLEM_CORROBORATION: Tooling operators and well-resourced labs attest the problem is now solved and the register is genuinely evidentiary. Independent methodologists studying model-generated alternative-position sets, and domain experts excluded from the toolchain, attest from outside the beneficiary set that the register mainly reflects what is cheap to generate and easy to curate under time pressure, not what is representative of the actual space of positions — corroboration for the contested reading exists on both sides, with no single outside arbiter settling it.
narrative_ontology:disappearance_verdict(instrumentalist_reading, contested).
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
 *   Extractiveness (0.61) reflects that the transfer is real but partial: genuine tractability gains exist alongside asymmetric capture of evidentiary authority by whoever has compute, time, and reputational slack. Suppression (0.42) is moderate rather than high because no one is barred by rule from using the tool — the barrier is resource asymmetry (time, compute, survivable error) rather than formal exclusion, which is a softer suppression mechanism than legal or physical barriers. Theater ratio (0.38) captures a real and growing share of activity that is curatorial performance — running the loop repeatedly to produce a defensible-looking register rather than to actually surface the strongest counter-positions. Accessibility collapse (0.47) is moderate: alternatives to using the tool (manual positional articulation, older adjudication procedures) still exist and are used by excluded domain experts, so collapse is partial, not total. Resistance (0.55) is substantial because domain experts and under-resourced disputants actively contest the register's authority rather than acquiescing.
 *
 * PERSPECTIVAL GAP:
 *   From the tooling-operator and well-resourced-lab seats, this is coordination working as intended: a previously intractable class of disagreement is now evidenced. From the under-resourced-disputant and excluded-domain-expert seats, the same mechanism looks like an extraction structure wearing the coordination story as cover — their disagreement is no more resolved than before, it has simply been re-described using someone else's generated menu of alternatives, which they had no part in producing and cannot afford to contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are defined by slack, not standpoint: tooling operators set the classification scheme and profit from its adoption as evidentiary; well-resourced labs and moderately-resourced credentialed analysts can run the generate-curate-discard loop repeatedly and survive public error, converting cheap generation into low-risk evidentiary capital. Victims are defined by the same axis inverted: under-resourced disputants and communities without curation capacity cannot generate their own falsifiers or check the tool's output against lived specifics, so the model's default register becomes their evidentiary record whether or not it represents their actual position. Crucially, this cuts across standpoint-theoretic marginalization — a domain expert with deep positional knowledge but no tool access is excluded regardless of how legitimate their standpoint is, which is the structural delta this reading introduces relative to the standpoint reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that generating a representative sample of counter-positions was too labor-intensive to do at scale — is genuinely partially solved; that is what prevents this from being classified as a pure snare. But the arrangement's persistence increasingly serves the interests of whoever controls the toolchain rather than the tractability function alone, which is what the tangled_rope classification is built to hold apart: a real coordination gain (cheap falsifier generation) riding alongside asymmetric extraction (curation capacity as the new site of epistemic power) via the same mechanism, requiring active maintenance (continued tool operation, curatorial labor, adjudication of what counts as 'the' register) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice_instrumentalist,
    'Is the instrumentalist reading — that tractability is realized through the generative model''s classification/bookkeeping capacity rather than through any epistemic principle about standpoint, practice, or procedure — the correct account of why positional disagreement has become newly tractable, or is it a convenient redescription that lets toolchain operators claim epistemic authority without adjudicating the underlying value/standpoint questions the other readings foreground?',
    'Compare disputes resolved via the instrumentalist toolchain against matched disputes resolved via standpoint-based, pragmatist, or proceduralist mechanisms; check whether the instrumentalist register''s classifications survive scrutiny by domain experts excluded from the toolchain, or whether they merely reflect what the model found cheap to generate.',
    'If the instrumentalist account is correct, the coordination gain is real and the classification should remain tangled_rope with an ongoing extraction-monitoring obligation on curation practices. If it is a redescription, the coordination story is closer to cover and the constraint drifts toward snare as toolchain control concentrates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_instrumentalist, conceptual, 'Whether the instrumentalist reading names a real distinct mechanism or is a convenient relabeling of standpoint/procedural authority as technical capacity.').

omega_variable(
    sibling_reading_structural_delta,
    'Where exactly does the disagreement between this reading and the standpoint, pragmatist, and proceduralist readings locate the tractability-conferring element — in the generative model''s output volume, in whose standpoint gets sampled, in practical outcomes achieved, or in procedural fairness of adjudication — and can a single dispute be classified under more than one reading simultaneously without contradiction?',
    'Trace a small number of concrete disputes through all four readings'' lenses and document whether the beneficiary/victim sets and extraction mechanisms remain genuinely distinct or collapse into one another under scrutiny.',
    'If the readings genuinely stay distinct (as designed), each requires independent monitoring and remedy; if they collapse, the four-way decomposition should be revisited and possibly merged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Whether the four kernel readings are structurally independent or partially redundant redescriptions of the same underlying mechanism.').

omega_variable(
    model_agreeableness_as_hidden_extraction,
    'Does the generative model''s tendency toward agreeableness (producing plausible-sounding alternative positions that satisfy the prompt rather than genuinely adversarial falsifiers) constitute a systematic, quantifiable bias in the register, and if so, who benefits from that bias persisting undetected?',
    'Adversarial audits comparing model-generated falsifier sets against independently, manually curated falsifier sets for the same disputes across multiple domains and tool versions.',
    'A confirmed agreeableness bias would sharpen the extraction reading — tooling operators and well-resourced curators would be shown to benefit from an unacknowledged systematic distortion, strengthening the case for treating this as extraction requiring remedy rather than a coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(model_agreeableness_as_hidden_extraction, empirical, 'Whether model agreeableness constitutes a hidden, differentially-beneficial distortion of the evidentiary register.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(instrumentalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, instrumentalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(inst_tr_t4, instrumentalist_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(inst_tr_t8, instrumentalist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(inst_tr_t12, instrumentalist_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(inst_tr_t16, instrumentalist_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(inst_tr_t20, instrumentalist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(inst_tr_t24, instrumentalist_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, instrumentalist_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(inst_be_t4, instrumentalist_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(inst_be_t8, instrumentalist_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(inst_be_t12, instrumentalist_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(inst_be_t16, instrumentalist_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(inst_be_t20, instrumentalist_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(inst_be_t24, instrumentalist_reading, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(inst_su_t0, instrumentalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(inst_su_t4, instrumentalist_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(inst_su_t8, instrumentalist_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(inst_su_t12, instrumentalist_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(inst_su_t16, instrumentalist_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(inst_su_t20, instrumentalist_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(inst_su_t24, instrumentalist_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(instrumentalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(instrumentalist_reading, 0.12).
narrative_ontology:affects_constraint(instrumentalist_reading, standpoint_reading).
narrative_ontology:affects_constraint(instrumentalist_reading, pragmatist_reading).
narrative_ontology:affects_constraint(instrumentalist_reading, proceduralist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraint stories decomposing the natural-language kernel 'positional disagreement as evidence' per the ε-invariance principle: instrumentalist_reading (this file — tractability via generative classification/bookkeeping capacity), standpoint_reading, pragmatist_reading, and proceduralist_reading. Each reading has its own ε, its own beneficiary/victim structure, and its own claimed_type; they are linked here rather than merged because measuring the kernel by a different lights (whose standpoint counts vs. what practically resolves vs. what procedure is fair vs. what the tool can cheaply generate) yields materially different extraction profiles and different victim sets. The instrumentalist reading uniquely exposes a curation/tooling-access extraction path invisible to the other three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
