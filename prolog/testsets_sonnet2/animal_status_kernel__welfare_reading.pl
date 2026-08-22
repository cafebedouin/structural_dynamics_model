% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Animal Welfare Regulation Under Retained Property Status
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the welfare reading of the animal status
 *   kernel: animals are recognized as sentient beings whose suffering matters
 *   morally, but this recognition is operationalized through regulation of
 *   use rather than prohibition of use. Property status is retained — animals
 *   remain assets that can be bred, bought, sold, and killed — but that
 *   status is now conditioned on meeting suffering-minimization standards.
 *   The reading generates a genuine coordination function (a shared,
 *   verifiable definition of acceptable treatment) fused with genuine
 *   extraction (animals still bear confinement, transport, and slaughter, now
 *   legitimated by compliance). This is a tangled rope: the welfare apparatus
 *   coordinates industry, consumers, and regulators around a workable
 *   standard, while the underlying transfer of animal bodies and suffering to
 *   human use continues, now with reduced political friction because the
 *   suffering has been visibly 'addressed.' Extraction is authored as
 *   moderate (0.52) rather than high, because welfare regulation genuinely
 *   does reduce specific measurable harms (stunning before slaughter, minimum
 *   space allocations) — it is not pure theater. But theater ratio rises over
 *   the interval (0.25 to 0.44) as certification schemes proliferate and
 *   'humane' branding increasingly serves marketing rather than verified
 *   suffering reduction.
 *
 * KEY AGENTS:
 *   - livestock_industry: beneficiary/agenda_setter — retains use rights, shapes compliance bar, monetizes certification
 *   - farmed_animals: primary payer — bears residual suffering, no voice or exit
 *   - laboratory_animals: primary payer — bears procedural suffering under ethics review that permits continued use
 *   - welfare_certification_bodies: beneficiary/agenda_setter — institutional survival depends on continued use-with-regulation framework
 *   - abolitionist_advocates: excluded — structurally outside the framework's own legitimacy conversation
 *   - regulatory_agencies: agenda_setter — mediates between industry feasibility and animal interests with no direct animal representation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.52).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.58).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Animal Welfare Regulation Under Retained Property Status").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '9ab8a3d0-bced-4aed-b7e0-d2a58960f796').
narrative_ontology:cs_kernel_codification('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', distributed).
narrative_ontology:cs_authority_grounding('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', distributed).
narrative_ontology:cs_reading_relation('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', foundational, suffering_capacity_grounds_partial_moral_status).
narrative_ontology:cs_axiom_status(suffering_capacity_grounds_partial_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', suffering_capacity_grounds_partial_moral_status, empirically_contingent).
narrative_ontology:cs_axiom('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', foundational, regulated_use_is_morally_sufficient_response_to_sentience).
narrative_ontology:cs_axiom_status(regulated_use_is_morally_sufficient_response_to_sentience, holdable).
narrative_ontology:cs_axiom_grounding('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', regulated_use_is_morally_sufficient_response_to_sentience, instrumental).
narrative_ontology:cs_reference_frame('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', sentience_conditioned_property_status).
narrative_ontology:cs_drift_state('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', contemporary_industrial_agriculture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ab8a3d0-bced-4aed-b7e0-d2a58960f796', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, livestock_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, welfare_certification_bodies).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, small_scale_farmers_facing_compliance_costs).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, animal_sentience_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, regulated_use_permissibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Absorbs welfare compliance costs (cage-free mandates, transport limits, slaughter protocols) but retains the core right to breed, confine, and kill animals for profit. Lobbies for the specific welfare standards it must meet, shaping the compliance bar to something it can pass at manageable cost. Markets 'humane' certification as a premium product line, converting the constraint into a revenue stream.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, livestock_industry, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, livestock_industry, agenda_setter).

% Continue using animals in experimentation subject to institutional animal care committee review and 'three Rs' (replace, reduce, refine) protocols. The welfare framework legitimizes continued use by demonstrating due diligence around suffering, while imposing procedural cost rather than any hard cap on the practice itself.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Purchase animal products with reduced moral discomfort because welfare labeling signals suffering has been 'addressed.' Can choose higher-welfare options at a price premium or continue purchasing conventional products; the welfare frame does not require any change in consumption, only optional upgrading.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, mobile, national).

% Design and audit welfare standards, charging industry for certification and labeling rights. Their institutional survival depends on the continued existence of a use-with-regulation framework — full abolition would eliminate their function, so their incentive is toward incremental standard-raising rather than ending use.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_certification_bodies, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, welfare_certification_bodies, agenda_setter).

% Bred, confined, and killed under regulated conditions that reduce (but do not eliminate) pain during specific procedures — stunning before slaughter, minimum cage dimensions, transport time limits. Have no capacity to consent, exit, or be represented directly; their interests are proxied entirely by human advocates and regulators who also answer to industry.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, national).

% Used in procedures ranging from mild to severely painful, subject to ethics review that weighs scientific value against suffering but does not prohibit suffering outright. Refinement protocols reduce but do not eliminate pain; the animal's life and bodily autonomy remain fully at the disposal of the research program.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, laboratory_animals, payer,
    powerless, immediate, trapped, national).

% Bear welfare compliance costs (facility upgrades, veterinary documentation, inspection fees) that are proportionally heavier for smaller operations than for large integrated agribusiness. Some exit the industry rather than absorb costs; others operate at reduced margin. Did not design the standards they must meet.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, small_scale_farmers_facing_compliance_costs, payer,
    moderate, biographical, constrained, regional).

% Argue that welfare regulation is a legitimating mechanism that entrenches property status rather than challenging it, producing public complacency ('happy meat,' 'ethical eggs') that forecloses the political possibility of ending use altogether. Their position is structurally excluded from the welfare framework's own legitimacy claims — welfare regulation proceeds without needing to answer their critique.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, national).

% Draft and enforce welfare standards, balancing industry input, scientific evidence on animal suffering capacity, and public opinion. Structurally positioned between industry (which shapes the feasible standard) and animal interests (which have no direct representation), producing standards that industry can meet without existential threat to the use-based business model.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__welfare_reading, livestock_industry).
narrative_ontology:fixing_cost_class(animal_status_kernel__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, verifiable standard for reducing gratuitous animal suffering within continued-use systems, allowing industry, consumers, and regulators to coordinate around a common definition of 'acceptable' treatment rather than each actor privately negotiating suffering thresholds.
% TRANSFER_FUNCTION: Moves some costs of suffering-reduction (facility changes, procedural limits, certification fees) from animals (who bear the residual pain) onto industry and consumers (who bear compliance costs and price premiums), while the core value of animal bodies and labor continues to flow from animals to industry and consumers largely undiminished.
% ABSENT_VOICES: The animals themselves have no capacity for voice or consent and are represented only through proxies (regulators, advocates) who must also negotiate with the industry that profits from their use. Abolitionist advocates who reject the property-plus-welfare framing entirely are treated as outside the legitimate policy conversation, which proceeds without needing to answer whether property status itself is defensible.
% DISAPPEARANCE_RATIONALE: If welfare regulation vanished overnight, industry could revert to unregulated confinement and slaughter practices, consumer-facing certification markets would collapse, and public tolerance of animal-product consumption would likely destabilize without the reassurance that suffering is 'being addressed' — the entire legitimating apparatus around continued use depends on the regulation's existence.
% FOUNDING_PROBLEM: Industrial-scale animal agriculture and biomedical research produced suffering visible enough (battery cages, factory slaughter conditions, unanesthetized experimentation) to generate public and legislative pressure that threatened the industries' social license to operate.
% FOUNDING_PROBLEM_CORROBORATION: Industry and certification bodies attest the founding problem (gratuitous, unnecessary suffering) has been substantially addressed by modern standards. Independent animal behavior scientists and abolitionist scholars — outside the beneficiary set — attest that suffering remains structurally built into confinement and slaughter systems regardless of welfare compliance, and that the 'problem' as originally framed (public discomfort with visible cruelty) has been solved more thoroughly than the underlying suffering itself.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that welfare regulation constrains but does not eliminate use — animals still lose their lives and bodily autonomy to industry and research, just under conditions with somewhat reduced pain intensity. Suppression (0.58) is moderate-high because the framework actively forecloses the abolitionist alternative from mainstream legitimacy: welfare compliance itself is offered as proof that no further moral reckoning is needed, which suppresses political pressure toward ending use rather than regulating it. Theater ratio rises over time (0.25 to 0.44) as certification and labeling markets grow faster than verified suffering reduction — a Goodhart-style drift where the proxy (certified label) outpaces the target (actual suffering reduction). Accessibility collapse (0.50) and resistance (0.55) sit at moderate levels appropriate to a contested, actively debated arrangement rather than a settled natural fact or a fully coercive extraction with no defenders.
 *
 * DIRECTIONALITY LOGIC:
 *   Livestock industry, biomedical institutions, consumers, and certification bodies are beneficiaries: the constraint permits continued profitable and low-friction use while providing moral cover. Farmed and laboratory animals are victims with maximal directionality toward extraction — trapped, powerless, immediate time horizon, no capacity for consent or exit. Small-scale farmers occupy an intermediate position: they do not design the standards and bear compliance costs disproportionate to their scale, but they are still beneficiaries of the underlying use-permission the welfare framework grants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (gratuitous, publicly visible cruelty threatening industries' social license) is contested as live or dead: industry claims it is substantially solved; independent scientists and abolitionist scholars argue the arrangement has drifted into a legitimating function that outlives the suffering-reduction mandate it was built for. This is precisely the mismatch the R5 genealogy interview is designed to surface — founding_problem_status is contested while disappearance_verdict is world_rearranges, which is the signature of an arrangement whose stated purpose may have been substantially achieved (visible cruelty reduced) while its actual operative function (permitting continued use with reduced friction) persists and even strengthens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_as_legitimation_or_progress,
    'Does welfare regulation function primarily as incremental moral progress toward eventual abolition, or as a legitimating mechanism that forecloses abolition by manufacturing public comfort with continued use (''new welfarism'')?',
    'Longitudinal tracking of per-capita animal product consumption and political support for abolition-adjacent policy (e.g., bans on specific practices) in jurisdictions with strong versus weak welfare regimes; if strong welfare regimes correlate with declining use over multi-decade horizons, the progress reading gains support; if strong welfare regimes correlate with stable or increasing use alongside reduced abolitionist political traction, the legitimation reading gains support.',
    'If legitimation, the coordination function claimed by this reading is substantially cover for extraction, pushing the classification toward snare; if genuine progress, the tangled_rope classification with a real coordination component is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_as_legitimation_or_progress, conceptual, 'Whether welfare reform is a stepping-stone to abolition or a stabilizing mechanism for continued use — the central ''new welfarism'' critique.').

omega_variable(
    sentience_threshold_for_moral_relevance,
    'What is the appropriate threshold and scope of sentience (pain capacity, self-awareness, future-directed interest) that grounds moral relevance under this reading, and does that threshold justify property status at all once crossed?',
    'Convergence or persistent disagreement in comparative cognition and neuroscience research on which taxa possess morally relevant suffering capacity, cross-referenced against whether welfare regulation tracks that research or lags/ignores it for economically important species.',
    'A wide gap between demonstrated sentience and regulatory protection would suggest the welfare reading''s suffering-relevance premise is applied selectively to preserve economic use, undermining the reading''s internal coherence and strengthening the abolitionist critique that suffering-relevance logically entails ending property status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_threshold_for_moral_relevance, empirical, 'Whether the sentience premise is applied consistently or selectively calibrated to preserve permitted uses.').

omega_variable(
    certification_market_capture,
    'Are welfare certification bodies structurally independent adjudicators of animal welfare, or are they revenue-dependent on industry participation in ways that bias standard-setting toward feasibility for industry rather than suffering-reduction for animals?',
    'Financial disclosure analysis of certification body funding sources and standard-revision history; compare standards revisions correlated with industry lobbying versus independent animal science recommendations.',
    'If certification bodies are substantially industry-funded and standards track industry feasibility more than animal science, the theater_ratio trajectory is understated and the coordination claim for this stakeholder group weakens further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_market_capture, empirical, 'Whether certification bodies function as independent welfare adjudicators or industry-aligned standard-setters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__welfare_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__welfare_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__welfare_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(anim_tr_t32, animal_status_kernel__welfare_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__welfare_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__welfare_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__welfare_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__welfare_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(anim_be_t32, animal_status_kernel__welfare_reading, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__welfare_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__welfare_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__welfare_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__welfare_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(anim_su_t32, animal_status_kernel__welfare_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__welfare_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__welfare_reading, 0.1).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'animal welfare law' concept per the epsilon-invariance principle. property_reading authors near-zero extraction (no suffering-relevant victim class from that reading's own lights — animals are assets, not victims). abolitionist_reading authors much higher extraction and a wider victim set (all use is extraction, property status itself is the injustice). This welfare_reading sits between: moderate extraction because suffering-minimization imposes real costs and produces real (if partial) harm reduction while permitting continued use. All three share the underlying kernel (animal_status_kernel) but instantiate structurally distinct constraints with different ε, different beneficiary/victim sets, and different classifications — they are linked via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
