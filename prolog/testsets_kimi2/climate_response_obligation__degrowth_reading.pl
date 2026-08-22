% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Planetary Boundaries Throughput Obligation â Degrowth Reading
 *   domain: climate_policy_political_economy_intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the degrowth reading of the
 *   climate_response_obligation kernel, which holds that the obligation is to
 *   reduce aggregate material throughput and prioritize sufficiency over
 *   efficiency. Sibling readings (mitigation_priority, adaptation_priority)
 *   are treated as separate constraints per the Îµ-invariance principle. The
 *   constraint treats planetary boundaries as a hard coordination ceiling and
 *   imposes asymmetric contraction costs on Global North consumption and
 *   capital accumulation while creating ecological space for Global South
 *   frontline communities. Capital accumulation itself is identified as an
 *   extractive mechanism incompatible with the kernel.
 *
 * KEY AGENTS:
 *   - global_north_governments (agenda_setter / institutional / analytical): Sets and enforces binding material throughput reductions domestically; negotiates international burden-sharing that targets Northern contraction first
 *   - global_north_consumer_class (payer / moderate / constrained): Bears lifestyle reduction through enforced sufficiency limits on energy and material consumption
 *   - fossil_capital_complex (payer / institutional / constrained): Faces accumulation limits and stranded assets under throughput ceilings that treat growth as incompatible with planetary boundaries
 *   - global_south_development_advocates (payer / organized / constrained): Pay the opportunity cost of constrained industrialization unless Global North contraction creates adequate ecological space
 *   - global_south_frontline_communities (beneficiary / powerless / trapped): Receive reduced extraction pressure on their lands and waters from Northern material contraction
 *   - climate_science_community (observer / analytical / analytical): Monitors planetary boundary indicators and compliance; neither pays nor benefits materially from enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.82).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.85).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Planetary Boundaries Throughput Obligation â Degrowth Reading").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy_political_economy_intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '12ab5d3c-af5e-455d-ba19-36a6ccfad6f4').
narrative_ontology:cs_kernel_codification('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', distributed).
narrative_ontology:cs_authority_grounding('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', distributed).
narrative_ontology:cs_reading_relation('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', foundational, material_throughput_ceiling).
narrative_ontology:cs_axiom_status(material_throughput_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', material_throughput_ceiling, empirically_contingent).
narrative_ontology:cs_axiom('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', foundational, sufficiency_priority_over_efficiency).
narrative_ontology:cs_axiom_status(sufficiency_priority_over_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', sufficiency_priority_over_efficiency, deontological).
narrative_ontology:cs_reference_frame('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', planetary_sufficiency_state).
narrative_ontology:cs_drift_state('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', contemporary_growth_hegemony_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('12ab5d3c-af5e-455d-ba19-36a6ccfad6f4', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_frontline_communities).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumer_class).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, fossil_capital_complex).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_development_advocates).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundary_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, sufficiency_economics_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer binding material throughput reductions and sufficiency standards domestically; negotiate international burden-sharing agreements that assign contraction obligations to Northern economies first. Can abandon the degrowth frame for alternative climate responses, but doing so fractures the kernel's coherence.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_governments, agenda_setter,
    institutional, generational, analytical, global).

% Face enforced lifestyle reductions as material and energy consumption are capped by regulation, taxation, and supply-side limits. High-carbon consumption becomes structurally inaccessible rather than merely expensive; individual exit is limited by the aggregate cap regime.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumer_class, payer,
    moderate, biographical, constrained, global).

% Confronts stranded assets and a regulatory ceiling on total extraction and accumulation. The constraint treats growth-dependent investment as incompatible with planetary boundaries; capital mobility does not escape a global throughput limit.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, fossil_capital_complex, payer,
    institutional, biographical, constrained, global).

% Advocate for rapid industrialization and material development in the Global South. Under a uniform throughput cap they face foreclosed development pathways unless Global North contraction creates sufficient ecological space; they bear the opportunity cost of delayed industrialization.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_development_advocates, payer,
    organized, generational, constrained, global).

% Experience reduced land-grabbing, deforestation pressure, and climate destabilization if Northern material extraction declines. Their ecological livelihoods depend on the constraint actually reducing extraction rather than merely displacing it.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_frontline_communities, beneficiary,
    powerless, generational, trapped, regional).

% Provide the biophysical evidence base for planetary boundaries and throughput indicators. Monitor compliance and overshoot trajectories; do not themselves pay or benefit materially from the constraint's enforcement.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, climate_science_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__degrowth_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global economic activity to remain within hard biophysical limits, preventing collective overshoot of planetary boundaries by capping aggregate material and energy throughput and prioritizing sufficiency over efficiency.
% TRANSFER_FUNCTION: Transfers material and energy use privileges from high-throughput Northern consumers and capital-intensive producers to ecological space for Global South frontline communities and non-human systems; shifts consumption capacity from present excess toward intergenerational sufficiency.
% ABSENT_VOICES: Global North carbon-intensive workers whose livelihoods depend on extraction industries; technological optimists arguing for decoupling and innovation-driven growth; and market-liberal institutions who would argue for price signals over hard throughput limits are structurally excluded from the degrowth policy frame.
% DISAPPEARANCE_RATIONALE: If the throughput obligation vanished, Northern economies would revert to growth-centric extraction, Global South frontline communities would face renewed land and climate pressure, and the biophysical stabilization pathway would collapse â the world would reorganize around unbounded material accumulation.
% FOUNDING_PROBLEM: Unlimited material growth in a finite biosphere, causing overshoot of planetary boundaries and asymmetric ecological burden on Global South territories and future generations.
% FOUNDING_PROBLEM_CORROBORATION: Earth system scientists attest to ongoing planetary boundary overshoot from outside the degrowth advocacy community. Global South social movements corroborate asymmetric ecological burden. Mainstream economic institutions and growth coalitions contest the framing, which itself corroborates the problem's contested but live status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint forcibly reduces material and energy consumption and accumulation across the global economy. Suppression is very high (0.85) because market signals, growth imperatives, and consumerist institutions must be actively overridden by regulatory caps and sufficiency standards. Theater ratio is moderate-high (0.40): while the biophysical grounding is substantive, implementation carries significant performative risk where states adopt sufficiency rhetoric without actual throughput reduction. Accessibility collapse is high (0.85) because acceptance of hard planetary boundaries conceptually forecloses infinite-growth alternatives. Resistance is very high (0.88) from growth-dependent coalitions, consumer constituencies, and capital holders.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats experience the constraint as imposed extraction â a forced reduction of consumption, accumulation, or development opportunity. The beneficiary seat experiences it as liberation from extractive pressure. The agenda-setter seat experiences it as necessary coordination under biophysical duress. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North governments sit near the symmetric middle as agenda-setters who both administer and are bound by the constraint. Global North consumers, fossil capital, and Global South development advocates are structural targets: they bear the costs of forced contraction and constrained pathways, with limited exit from a global throughput regime, yielding high d. Global South frontline communities are structural beneficiaries: reduced extraction pressure flows to them, yielding low d. The climate science community is an analytical observer with no material stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â biophysical overshoot from unlimited growth â remains live, as corroborated by Earth system science and frontline communities. This prevents misclassification as a piton (the mandate has not atrophied) or scaffold (no sunset clause is possible for permanent planetary boundaries). The constraint is not a snare because a genuine coordination function exists: preventing collective ecological collapse requires managing aggregate throughput. The classification as tangled_rope preserves both the coordination function and the asymmetric extraction it encodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_kernel_reading_position,
    'How does the degrowth reading''s core axiom (sufficiency over efficiency) relate structurally to the mitigation_priority and adaptation_priority readings of the same kernel?',
    'Cross-reading analysis of axiom contradiction, policy incompatibility, and coalition overlap across the three constraint stories in this kernel family.',
    'Determines whether the readings are mutually foreclosing, merely coexisting, or exert structural influence on one another within the broader climate policy discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_kernel_reading_position, conceptual, 'Structural relationship of degrowth reading to sibling kernel readings').

omega_variable(
    global_south_net_position_ambiguity,
    'Do Global South development advocates benefit from or pay for this constraint, given the ''North first'' conditionality and the risk of uniform caps without adequate burden-sharing?',
    'Empirical analysis of historical and proposed burden-sharing agreements; measurement of whether Northern contraction actually creates ecological space or merely shifts extraction South.',
    'If Global South development advocates are net payers under realistic implementation, effective extraction is higher than the idealized reading suggests and directionality shifts toward more targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_net_position_ambiguity, empirical, 'Whether Global South is net beneficiary or victim under realistic implementation').

omega_variable(
    planetary_boundary_rigidity,
    'Is the material throughput limit a genuine biophysical necessity or a politically constructed scarcity that could be relaxed by innovation or substitution?',
    'Earth system science consensus assessment of planetary boundary rigidity; tracking of decoupling claims against material footprint data.',
    'If boundaries are soft, the constraint is more extractive and less coordinative; if hard, the coordination function is stronger and the extraction is the necessary price of biophysical survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(planetary_boundary_rigidity, empirical, 'Biophysical rigidity versus political construct of throughput limits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__degrowth_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__degrowth_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__degrowth_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__degrowth_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(clim_tr_t50, climate_response_obligation__degrowth_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__degrowth_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__degrowth_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__degrowth_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__degrowth_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(clim_be_t50, climate_response_obligation__degrowth_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__degrowth_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__degrowth_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__degrowth_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__degrowth_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(clim_su_t50, climate_response_obligation__degrowth_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is the degrowth reading of the climate_response_obligation kernel, decomposed from the mitigation_priority and adaptation_priority readings per the Îµ-invariance principle. Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
