% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Welfare Reading of Sentient Status
 *   domain: applied ethics / legal philosophy / political economy
 *
 * SUMMARY:
 *   This constraint instantiates the welfare_reading of the animal_status
 *   kernel: animals are sentient beings whose interests constrain but do not
 *   prohibit human use. It occupies the middle position between the
 *   abolitionist_reading (inherent value precludes all instrumental use) and
 *   the property_reading (animals are legal objects without independent moral
 *   standing). The welfare reading provides genuine coordination by
 *   prohibiting gratuitous cruelty, yet its exemption structures for
 *   agriculture, research, and entertainment permit substantial instrumental
 *   extraction. The authored metrics describe a tangled rope with
 *   moderate-high extractiveness (Îµ = 0.45), rising theater, and active
 *   enforcement â the engine will measure the gap between the coordination
 *   claim and the extraction profile.
 *
 * KEY AGENTS:
 *   - animals: structural payer (powerless/trapped) â bears the costs of exemption-structured use
 *   - animal_use_industries: structural beneficiary (powerful/mobile) â captures surplus under welfare legitimation
 *   - state_welfare_regulators: agenda setter (institutional/constrained) â designs and enforces the distinction between gratuitous harm and permitted use
 *   - abolitionist_advocates: excluded voice (organized/constrained) â contests the reading from outside the policy frame
 *   - animal_product_consumers: diffuse beneficiary (moderate/mobile) â receives inexpensive products externalized onto animals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.55).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Welfare Reading of Sentient Status").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied ethics / legal philosophy / political economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, 'e77f6ab3-0a67-41f9-8ce0-92fc21e3888a').
narrative_ontology:cs_kernel_codification('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', formalized).
narrative_ontology:cs_authority_grounding('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', lineage).
narrative_ontology:cs_interpretation_layer_present('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a').
narrative_ontology:cs_reading_relation('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', animal_status__abolitionist_reading, influences).
narrative_ontology:cs_reading_relation('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', foundational, sentience_generates_interest_constraint).
narrative_ontology:cs_axiom_status(sentience_generates_interest_constraint, holdable).
narrative_ontology:cs_axiom_grounding('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', sentience_generates_interest_constraint, deontological).
narrative_ontology:cs_axiom('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', foundational, instrumental_use_permissible_under_welfare).
narrative_ontology:cs_axiom_status(instrumental_use_permissible_under_welfare, holdable).
narrative_ontology:cs_axiom_grounding('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', instrumental_use_permissible_under_welfare, conventional).
narrative_ontology:cs_reference_frame('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', sentience_constrained_use_framework).
narrative_ontology:cs_drift_state('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', industrial_agriculture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e77f6ab3-0a67-41f9-8ce0-92fc21e3888a', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_product_consumers).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animals).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, sentience_as_moral_relevance).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, welfare_exemption_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of confinement, instrumental labor, and death permitted under welfare exemptions; recognized as sentient by the framework but their interests are systematically subordinated to human use categories; no exit from the structures of use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animals, payer,
    powerless, immediate, trapped, local).

% Capture economic surplus from animal-based food, research, and entertainment sectors under welfare frameworks that legitimate continued use; benefit from legal exemptions that externalize the costs of sentience onto animals; could shift capital to plant-based models but face sunk-cost infrastructure and market inertia.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_use_industries, beneficiary,
    powerful, biographical, mobile, global).

% Draft, administer, and enforce animal welfare standards that distinguish prohibited gratuitous harm from permitted instrumental use; maintain the legal architecture of exemptions for agricultural and research practices; dependent on political support from industry and public tolerance.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, state_welfare_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Argue that sentience precludes all instrumental use regardless of welfare safeguards; structurally marginalized from mainstream policy discourse where the welfare paradigm dominates funding, regulatory capture, and public moral imagination; would object to the exemption structures if given equal standing.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Receive inexpensive animal products and services whose costs are partly externalized onto animals through welfare-exemption structures; face low friction for continued consumption and high friction for systemic alternatives; culturally habituated to the welfare framing as sufficient moral due diligence.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_product_consumers, beneficiary,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a moral and legal framework that coordinates human conduct toward animals by prohibiting gratuitous cruelty while permitting instrumental use with welfare safeguards, thereby preventing social conflict over the status of animals and enabling predictable, regulated coexistence.
% TRANSFER_FUNCTION: Moves the direct costs of confinement, labor, and death to animals, while transferring economic surplus and moral legitimation to human users; moves the political burden of reform away from beneficiaries by embedding exemptions in welfare law.
% ABSENT_VOICES: Abolitionist advocates who reject all instrumental use are structurally excluded from mainstream policy; their exclusion is maintained by the welfare paradigm's capture of reform discourse and its framing of abolition as ethically excessive or economically infeasible.
% DISAPPEARANCE_RATIONALE: If the welfare reading vanished, the legal and moral distinction between gratuitous harm and legitimate use would collapse; animal use industries would face either a regulatory vacuum favoring unrestricted property-like use or mounting pressure toward abolitionist rights frameworks; the current compromise equilibrium and its associated exemption structures would dissolve.
% FOUNDING_PROBLEM: Unregulated animal use produced visible gratuitous cruelty and social disorder; competing moral views on animal status created legal uncertainty and conflict over the boundaries of acceptable human conduct.
% FOUNDING_PROBLEM_CORROBORATION: Animal use industries and state regulators attest the problem remains live, citing ongoing cruelty risks. Abolitionist advocates and critical animal studies scholars attest the founding problem was misdiagnosed â the issue is instrumental use itself, not merely its gratuitous forms. Veterinary history and comparative agricultural policy provide mixed corroboration from outside the pure beneficiary set.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45 because welfare exemption structures systematically permit the use of sentient beings for human ends despite their recognized interests. Suppression is 0.55 because the constraint's persistence depends on actively enforcing welfare boundaries and on marginalizing abolitionist alternatives (through ag-gag laws, regulatory capture, and discursive framing). Theater ratio is 0.45 and rising: welfare signaling (humane labels, enriched cages) increasingly exceeds functional protection as industrial scale outpaces inspection capacity. Accessibility collapse is 0.40 because abolitionist alternatives remain intellectually accessible but are socially collapsed within mainstream policy discourse. Resistance is 0.45 due to sustained abolitionist activism and undercover documentation. The measurement series run on a single shared time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The industry seat experiences the constraint as coordination â legal certainty, legitimated market access, and a predictable regulatory environment. The animal seat experiences the same structure as extraction â interests recognized in principle but overridden in practice by exemption categories. The abolitionist observer seat experiences it as a snare: the coordination story (preventing cruelty) serves as cover for systematic extraction. The engine computes each seat's type from this structural asymmetry; the authored claim (tangled_rope) does not adjudicate between these perceptions.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are declared in the victim set and sit as structural payers with trapped exit and local scope, placing their derived directionality near the full-target end (d â 1.0). Animal use industries are declared beneficiaries with powerful/global positioning, placing their directionality near the full-beneficiary end (d â 0.0). State regulators are agenda setters with constrained exit; their directionality is intermediate but asymmetry is preserved because they do not bear the direct costs. The beneficiary/victim declarations are the primary structural input; no directionality overrides are needed because the derivation chain produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The welfare reading risks mandatrophy if its founding problem (gratuitous cruelty) is substantially solved while the exemption structures persist purely for inertia. The temporal measurements show extraction accumulation and theater growth over the interval, suggesting drift toward a snare-like profile rather than resolution. A true scaffold would carry a sunset clause for the exemption structures; their absence indicates the constraint is justified as a steady state, not a transition. If the welfare framework were transitional toward reduced use or abolition, it would require explicit sunset mechanisms that are absent in the current architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exemption_structure_nature,
    'Are the welfare exemption structures (for agriculture, research, and entertainment) a necessary coordination cost of managing human-animal coexistence, or are they extractive overhead masquerading as regulatory compromise?',
    'Comparative jurisdictional analysis: measure whether stricter welfare regimes or abolitionist transitions reduce total animal use and associated externalities, or whether welfare reforms merely legitimate continued extraction without reducing net harm.',
    'If the exemptions are primarily extractive, the constraint''s effective extractiveness exceeds the base metric and its coordination function is weaker than claimed; if they are necessary cost, the tangled-rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_structure_nature, conceptual, 'Whether welfare exemptions are coordination cost or extractive overhead.').

omega_variable(
    practice_drift_gap,
    'Has the actual practice of industrial animal use drifted so far from the welfare reading''s reference frame that the constraint now operates as a legitimating facade for extraction regimes its own ideals would reject?',
    'Empirical audit of compliance rates, enforcement budgets, and welfare outcomes against statutory standards; comparison with the reference frame of humane use.',
    'If practice drift is severe and unacknowledged, the theater_ratio understates the performative gap, and the constraint may functionally operate as a snare despite its tangled-rope architecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practice_drift_gap, empirical, 'Gap between welfare ideals and industrial practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of abolitionist alternatives structural (legal exclusion, regulatory capture, economic barriers to entry for plant-based sectors) or internalized (public moral complacency produced by welfare framing)?',
    'Track suppression and resistance trajectories in jurisdictions before and after welfare reform; if suppression rises post-reform while structural barriers remain constant, internalization is dominant.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest â the public carries the suppression of alternatives even where legal barriers are low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of abolitionist alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(anim_tr_t10, animal_status__welfare_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(anim_tr_t20, animal_status__welfare_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(anim_tr_t30, animal_status__welfare_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(anim_tr_t50, animal_status__welfare_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anim_be_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(anim_be_t20, animal_status__welfare_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(anim_be_t30, animal_status__welfare_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(anim_be_t50, animal_status__welfare_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(anim_su_t10, animal_status__welfare_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(anim_su_t20, animal_status__welfare_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(anim_su_t30, animal_status__welfare_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(anim_su_t50, animal_status__welfare_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three structurally distinct readings: welfare_reading (this constraint), abolitionist_reading, and property_reading. The welfare reading coexists with both siblings in public discourse but influences the operating environment of abolitionism by absorbing reform energy. This story carries the welfare reading's Îµ (~0.45), beneficiary/victim structure, and drift profile; siblings carry different Îµ values and structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
