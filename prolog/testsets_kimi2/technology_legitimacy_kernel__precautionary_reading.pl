% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Precautionary Reading of Climate Technology Legitimacy
 *   domain: energy_policy/climate_governance
 *
 * SUMMARY:
 *   This constraint instantiates the precautionary reading of the technology
 *   legitimacy kernel: a technology is legitimate for climate mitigation only
 *   if its worst-case failures and legacy costs are bounded and reversible
 *   within a generation. This reading structurally benefits renewable energy
 *   sectors (whose decommissioning and failure modes are comparatively
 *   bounded) and structurally excludes nuclear technologies (due to waste
 *   legacy and accident tail risks). Future generations are positioned as the
 *   moral constituency whose irreversible costs the constraint prevents,
 *   though the constraint's operation may also impose climate costs on them
 *   if it slows decarbonization by excluding viable technologies. The reading
 *   is authored as one of three sibling readings; the others prioritize
 *   reliability or deployment velocity.
 *
 * KEY AGENTS:
 *   - renewable_energy_sector: Primary beneficiary (organized/constrained) â gains legitimacy and policy space from nuclear exclusion
 *   - nuclear_industry: Primary payer (powerful/constrained) â bears delegitimization and loss of climate investment
 *   - future_generations: Silent payer (powerless/trapped) â bear climate risk if transition slows, and waste risk if bad tech slips through
 *   - precautionary_policy_coalition: Agenda setter (institutional/mobile) â administers the reversibility criterion
 *   - dissenting_energy_analysts: Analytical observer (analytical/analytical) â documents decarbonization trade-offs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.62).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.55).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Reading of Climate Technology Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, '043a3661-4e53-4d69-8e69-7ea7ca87077f').
narrative_ontology:cs_kernel_codification('043a3661-4e53-4d69-8e69-7ea7ca87077f', formalized).
narrative_ontology:cs_authority_grounding('043a3661-4e53-4d69-8e69-7ea7ca87077f', lineage).
narrative_ontology:cs_interpretation_layer_present('043a3661-4e53-4d69-8e69-7ea7ca87077f').
narrative_ontology:cs_reading_relation('043a3661-4e53-4d69-8e69-7ea7ca87077f', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('043a3661-4e53-4d69-8e69-7ea7ca87077f', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('043a3661-4e53-4d69-8e69-7ea7ca87077f', foundational, irreversibility_disqualifies_climate_legitimacy).
narrative_ontology:cs_axiom_status(irreversibility_disqualifies_climate_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('043a3661-4e53-4d69-8e69-7ea7ca87077f', irreversibility_disqualifies_climate_legitimacy, deontological).
narrative_ontology:cs_axiom('043a3661-4e53-4d69-8e69-7ea7ca87077f', secondary, generational_horizon_sufficient_for_reversibility).
narrative_ontology:cs_axiom_status(generational_horizon_sufficient_for_reversibility, holdable).
narrative_ontology:cs_axiom_grounding('043a3661-4e53-4d69-8e69-7ea7ca87077f', generational_horizon_sufficient_for_reversibility, conventional).
narrative_ontology:cs_reference_frame('043a3661-4e53-4d69-8e69-7ea7ca87077f', precautionary_ecological_integrity).
narrative_ontology:cs_drift_state('043a3661-4e53-4d69-8e69-7ea7ca87077f', contemporary_climate_emergency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('043a3661-4e53-4d69-8e69-7ea7ca87077f', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_energy_sector).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from technology legitimacy frameworks that classify their technologies as compliant while excluding nuclear competitors. Receives preferential policy support, subsidies, and grid access decisions aligned with the precautionary criterion. Exit is constrained by path-dependent infrastructure commitments but the sector is not identity-locked to the constraint itself.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_energy_sector, beneficiary,
    organized, biographical, constrained, global).

% Bears the cost of delegitimization under the precautionary frame. Despite offering low-carbon generation, its waste legacy and accident tail risks place it outside the legitimacy boundary. Attempts to demonstrate improved safety and waste management are systematically discounted by the precautionary criterion. Exit options are constrained by long-lived assets and the lack of alternative large-scale baseload markets.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_industry, payer,
    powerful, generational, constrained, global).

% Are the nominal beneficiaries of the precautionary constraint but also bear costs if the exclusion of viable technologies slows decarbonization and intensifies climate damages. They have no voice in current legitimacy determinations and cannot opt out of the energy and climate trajectory chosen by present generations.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Comprises environmental ministries, international bodies, and advocacy networks that articulate and enforce the generational-reversibility criterion. They set the terms of technology assessment and determine which technologies qualify for climate finance and regulatory approval. They can shift emphasis among risk criteria but currently administer the reversibility threshold.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, precautionary_policy_coalition, agenda_setter,
    institutional, generational, mobile, global).

% Analysts and scientists who argue that climate urgency requires including all low-carbon technologies regardless of reversibility profiles. They observe the structural effects of the precautionary constraint and document the decarbonization trade-offs but do not set the agenda or bear direct costs.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, dissenting_energy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__precautionary_reading, renewable_energy_sector).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__precautionary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared decision criterion for which technologies society will support and deploy for climate mitigation, reducing paralyzing disagreement over risk thresholds and directing investment toward technologies whose failures can be unwound.
% TRANSFER_FUNCTION: Moves legitimacy, investment, and policy support from technologies with irreversible or long-tail risks toward technologies with bounded, reversible failure modes, and transfers the moral hazard of irreversible decisions onto excluded technology sectors and future populations.
% ABSENT_VOICES: Future generations cannot speak in current policy forums; nuclear industry representatives are formally present but structurally marginalized in precautionary-framed deliberations; dissenting scientists who argue that climate urgency overrides precaution on specific technologies are discounted as risking irreversibility.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, nuclear would regain legitimacy in climate portfolios, investment would shift toward mixed low-carbon portfolios including nuclear and possibly geoengineering research, and the renewable sector would face competitive pressure from previously excluded alternatives. The policy discourse would reorganize around different criteria such as reliability or velocity.
% FOUNDING_PROBLEM: How to prevent the deployment of technologies whose catastrophic failure modes or waste legacies impose irreversible burdens on future generations and ecosystems, given past experiences with industrial toxins and nuclear accidents.
% FOUNDING_PROBLEM_CORROBORATION: Environmental historians and indigenous rights advocates attest to the reality of irreversible harm from unbounded industrial deployment. However, energy systems analysts and some climate economists outside the precautionary coalition attest that the founding problem has been partially solved by improved reactor designs and that the constraint now functions to exclude viable climate solutions, corroborating a shifted-function reading.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the constraint excludes an entire low-carbon technology class from climate legitimacy, concentrating benefits in compliant sectors. Suppression is moderate (0.55): the constraint operates through legitimacy denial, regulatory exclusion, and funding gatekeeping rather than direct coercion. Theater ratio rises to 0.40 as the precautionary frame becomes a policy shibboleth partially decoupled from empirical risk differentiation between modern nuclear and other technologies. Accessibility collapse is significant (0.65): once the generational-reversibility frame is adopted in governance institutions, nuclear alternatives become nearly unthinkable in mainstream green policy spaces. Resistance is moderate (0.50): nuclear industry, some climate scientists, and energy security advocates actively contest the frame. The measurement series share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The renewable sector and precautionary policy coalition experience this as necessary protective coordination that bounds catastrophic risk; the nuclear industry experiences it as exclusionary extraction of their climate legitimacy; future generations are structurally silent but bear the countervailing risk of slower decarbonization. The engine computes this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The renewable energy sector sits near the beneficiary end (low d) because the constraint subsidizes its market position and policy access. The nuclear industry sits near the target end (high d) because the constraint extracts legitimacy and investment from it. Future generations sit at the extreme target end (very high d) because they are trapped and powerless against the framework's long-term consequences. The precautionary policy coalition sits low-to-moderate d: they benefit from agenda-setting authority but are not the primary economic recipients of the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both a genuine coordination function (providing a shared risk threshold for technology governance) and asymmetric extraction (systematic nuclear exclusion). Without the coordination component it would be a pure snare against nuclear; without the extraction component it would be a rope. The temporal measurements show extraction accumulating and theater rising as the framework matures, suggesting potential drift toward snare-like operation if the coordination function atrophies into mere performative exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precautionary_boundary_validity,
    'Is the generational-reversibility boundary a genuine physical and ethical necessity, or a constructed political threshold that arbitrarily serves specific technology interests?',
    'Comparative technology assessment using uniform risk metrics across wind, solar, advanced nuclear, and geothermal to determine whether the reversibility differential is as large as the legitimacy gap implies.',
    'If the boundary is constructed, the constraint''s extraction component dominates and the coordination function is cover for industrial policy; if genuine, the extraction is the necessary price of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precautionary_boundary_validity, empirical, 'Whether the generational reversibility threshold is structurally justified or politically constructed').

omega_variable(
    nuclear_contemporary_compliance,
    'Would advanced modular nuclear reactors and closed fuel cycles pass the precautionary test if assessed by contemporary rather than historical risk and waste profiles?',
    'Independent geological and engineering review of modern waste confinement and reactor safety against the ''bounded and reversible within a generation'' criterion.',
    'If modern nuclear passes the test, the constraint''s nuclear exclusion is anachronistic extraction rather than risk-responsive coordination; if it fails, the exclusion is empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_contemporary_compliance, empirical, 'Whether modern nuclear technology actually violates the precautionary criterion').

omega_variable(
    inter_reading_foreclosure,
    'Does the precautionary reading''s irreversibility axiom structurally foreclose the velocity reading''s deployment imperative, or can the two criteria be held simultaneously by different parties?',
    'Analysis of whether any single governance framework has successfully combined precautionary reversibility with emergency-deployment velocity, or whether the two criteria select mutually exclusive technology portfolios in practice.',
    'If they foreclose each other, the kernel is structurally fractured and the readings are competing for institutional dominance; if they coexist, the conflict is political rather than logical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_reading_foreclosure, conceptual, 'Structural relationship between precautionary and velocity readings of the same kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_leg_prec_tr_t0, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tech_leg_prec_tr_t10, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(tech_leg_prec_tr_t20, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(tech_leg_prec_tr_t30, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(tech_leg_prec_tr_t40, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(tech_leg_prec_tr_t50, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(tech_leg_prec_be_t0, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tech_leg_prec_be_t10, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(tech_leg_prec_be_t20, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(tech_leg_prec_be_t30, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(tech_leg_prec_be_t40, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(tech_leg_prec_be_t50, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tech_leg_prec_su_t0, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(tech_leg_prec_su_t10, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(tech_leg_prec_su_t20, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(tech_leg_prec_su_t30, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(tech_leg_prec_su_t40, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(tech_leg_prec_su_t50, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the technology_legitimacy_kernel. The kernel decomposes into three structurally distinct claims (precautionary, reliability, velocity) because each reading assigns legitimacy based on a different primary criterion, producing different beneficiary/victim structures and epsilon profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
