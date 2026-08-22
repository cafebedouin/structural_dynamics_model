% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe-as-Necessary Reading of Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-reliability domains (aviation, nuclear, offshore drilling,
 *   healthcare), a persistent institutional narrative holds that real
 *   disasters — not drills, not simulators — are what actually reset
 *   organizational vigilance and produce durable competence. This narrative
 *   is partly true (vigilance decay during quiet periods is well documented)
 *   and partly self-serving for the parties whose authority derives from
 *   having lived through a real event. The reading channels safety investment
 *   toward post-catastrophe response infrastructure and disaster-derived
 *   credentialing, and away from simulation fidelity, even where simulation
 *   could plausibly substitute.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.58).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.47).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe-as-Necessary Reading of Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '2aa68320-a8b8-422b-8b62-71bf8470c7b0').
narrative_ontology:cs_kernel_codification('2aa68320-a8b8-422b-8b62-71bf8470c7b0', distributed).
narrative_ontology:cs_authority_grounding('2aa68320-a8b8-422b-8b62-71bf8470c7b0', practice).
narrative_ontology:cs_interpretation_layer_present('2aa68320-a8b8-422b-8b62-71bf8470c7b0').
narrative_ontology:cs_reading_relation('2aa68320-a8b8-422b-8b62-71bf8470c7b0', competence_retention_exercise__simulation_as_sufficient, coexists_with).
narrative_ontology:cs_reading_relation('2aa68320-a8b8-422b-8b62-71bf8470c7b0', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('2aa68320-a8b8-422b-8b62-71bf8470c7b0', foundational, lived_catastrophe_is_epistemically_irreplaceable).
narrative_ontology:cs_axiom_status(lived_catastrophe_is_epistemically_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('2aa68320-a8b8-422b-8b62-71bf8470c7b0', lived_catastrophe_is_epistemically_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('2aa68320-a8b8-422b-8b62-71bf8470c7b0', secondary, visceral_stakes_cannot_be_manufactured_by_design).
narrative_ontology:cs_axiom_status(visceral_stakes_cannot_be_manufactured_by_design, holdable).
narrative_ontology:cs_axiom_grounding('2aa68320-a8b8-422b-8b62-71bf8470c7b0', visceral_stakes_cannot_be_manufactured_by_design, empirically_contingent).
narrative_ontology:cs_reference_frame('2aa68320-a8b8-422b-8b62-71bf8470c7b0', disaster_derived_competence_tradition).
narrative_ontology:cs_drift_state('2aa68320-a8b8-422b-8b62-71bf8470c7b0', post_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2aa68320-a8b8-422b-8b62-71bf8470c7b0', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, incident_investigation_specialists).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, veteran_operators_with_disaster_experience).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, regulatory_bodies_citing_past_disasters).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, frontline_operators_denied_credible_simulation_investment).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, organizations_starved_of_simulator_funding).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, future_disaster_victims_of_unaddressed_latent_failure).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, high_reliability_organization_theory_via_disaster_case_study).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build careers, methodology, and institutional standing on post-catastrophe forensic analysis. Their expertise and authority are strongest immediately after real disasters; a world where simulation is treated as sufficient shrinks the demand for their specific post-mortem craft.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, incident_investigation_specialists, beneficiary,
    organized, biographical, mobile, national).

% Hold elevated status and deference within their organizations because they 'were there' for a real event. Their tacit authority depends on the belief that lived-through catastrophe confers a kind of competence simulation cannot replicate; this belief is not fully falsifiable and is convenient for them to hold.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, veteran_operators_with_disaster_experience, beneficiary,
    moderate, biographical, constrained, national).

% Write and enforce safety mandates that invoke prior disasters as the justifying case study. They administer certification and audit regimes that implicitly treat disaster-derived lessons as a superior evidentiary tier to simulator data, shaping what counts as adequate preparedness.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, regulatory_bodies_citing_past_disasters, agenda_setter,
    institutional, generational, arbitrage, national).

% Work incident-free systems where competence quietly decays. Because the organizational culture treats simulation as inherently second-tier, investment in high-fidelity training is chronically underfunded relative to what the risk profile demands, and their actual readiness is untested until a real event exposes the gap — at personal cost to them.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, frontline_operators_denied_credible_simulation_investment, payer,
    powerless, immediate, trapped, local).

% Compete for finite safety budgets. Under this reading, dollars spent on simulation are discounted as inherently insufficient, which channels investment toward post-hoc disaster response capacity instead of prevention infrastructure, leaving them structurally less prepared going into the next incident-free stretch.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, organizations_starved_of_simulator_funding, payer,
    moderate, biographical, constrained, national).

% Bear the ultimate cost when a system whose competence was never genuinely tested (because simulation was dismissed as inadequate and no catastrophe had yet occurred to force the reset) fails. They have no voice in the current framing and no way to intervene before the fact.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, future_disaster_victims_of_unaddressed_latent_failure, payer,
    powerless, generational, trapped, local).

% Would argue their high-fidelity training systems already produce structurally equivalent cognitive and procedural demands to real events. Their evidence is systematically discounted by a framing that treats only lived catastrophe as authoritative, so they are not meaningfully part of the standard-setting conversation even where their data is strong.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_technology_vendors, excluded,
    moderate, biographical, mobile, national).

% Study organizational learning across sectors and can compare outcomes at organizations that relied on simulation-heavy versus disaster-experience-heavy cultures. They see the full pattern and the incentive structures that keep the catastrophe-necessity belief attractive to those who hold disaster-derived authority.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, diffuse).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates organizational attention and resource allocation around the genuinely real problem that competence decays during incident-free periods and that people become overconfident precisely when nothing has gone wrong recently — this vigilance-maintenance function is real regardless of whether the catastrophe-necessity claim is correct.
% TRANSFER_FUNCTION: Moves authority, funding priority, and narrative credibility away from simulation investment and toward disaster-derived expertise and post-hoc investigation infrastructure; moves risk exposure onto frontline workers and future stakeholders whose organizations under-invest in simulation because it is culturally coded as insufficient.
% ABSENT_VOICES: Simulation technology vendors and simulation-trained operators who could offer counter-evidence that high-fidelity exercise produces comparable competence are structurally discounted in venues where disaster-experience carries the interpretive authority — they are present in the room but their evidence is weighted as inherently second-tier before it is heard.
% DISAPPEARANCE_RATIONALE: If this reading vanished — if organizations stopped treating actual catastrophe as the necessary competence reset and instead credited simulation as sufficient — safety budgets would reallocate toward simulator fidelity and frequency, disaster-experience status hierarchies within organizations would flatten, and regulatory certification regimes anchored to past-disaster case studies would need new evidentiary bases. The shift would be visible in budget lines and promotion patterns within a few fiscal cycles.
% FOUNDING_PROBLEM: Organizations genuinely do lose visceral appreciation of tail risk during long incident-free stretches, and normalization of deviance genuinely does creep in when nothing bad has happened for a while — the founding problem is real: how does an organization keep taking low-probability catastrophic risk seriously when it never materializes.
% FOUNDING_PROBLEM_CORROBORATION: High-reliability organization researchers and safety psychologists outside the disaster-investigation profession corroborate that vigilance decay during quiet periods is real and well-documented (e.g., normalization-of-deviance literature). However, those same outside researchers do NOT corroborate the specific further claim that only real catastrophe (as opposed to well-designed simulation) can address it — that stronger claim is corroborated mainly by parties whose institutional standing depends on disaster-derived expertise, which is exactly the beneficiary group the claim favors.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the reading systematically redirects resources and legitimacy toward disaster-experience-holders and investigation specialists at the expense of frontline workers and future risk-bearers, without those payers having consented to or benefiting from the redirection. Suppression (0.47) is moderate rather than high: the belief persists mainly through cultural and institutional inertia and selective evidentiary weighting, not through overt coercion — dissenting voices (simulation vendors, simulation-trained staff) are discounted rather than silenced. Theater ratio (0.42) captures that a meaningful share of 'lessons learned' activity following disasters becomes ritualized commemoration and credentialing rather than actionable system change. Accessibility collapse (0.4) is moderate-low: organizations that want to invest heavily in simulation instead of waiting for the 'necessary' disaster are not blocked from doing so, they are merely disadvantaged in status and funding competitions. Resistance (0.68) is comparatively high because safety researchers, simulation vendors, and increasingly regulators actively contest the catastrophe-necessity claim with counter-evidence from HRO theory and simulator validation studies.
 *
 * DIRECTIONALITY LOGIC:
 *   Incident investigation specialists, veteran disaster-experienced operators, and disaster-anchored regulatory bodies are declared beneficiaries: their authority, career capital, and citation base are strongest under this reading. Frontline operators, budget-starved organizations, and future disaster victims are declared victims: they bear the downstream cost of underinvestment in simulation-based readiness that the reading discourages. Simulation vendors are excluded rather than victimized outright — their evidence is discounted rather than actively suppressed, which is why they are marked excluded/moderate-power/mobile rather than payer/powerless/trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — vigilance decay during long incident-free periods — is real and unresolved (status: contested, trending live), which is precisely why this reading has not calcified into pure inertial theater; it retains genuine coordination content. The tangled_rope classification (rather than snare) reflects that a real coordination function (keeping organizations from becoming complacent) coexists with an asymmetric extraction pattern (channeling resources and authority to disaster-experience holders at frontline and future-victim expense) enforced by institutional certification and cultural status hierarchies. If the founding problem were shown to be fully solvable by simulation alone, this reading would slide toward snare; the contested status keeps it tangled rather than settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_necessity_vs_simulation_sufficiency,
    'Is lived catastrophic experience structurally necessary for genuine competence retention, or does this reading persist because it privileges the authority and status of those who possess disaster experience over those who do not?',
    'Controlled comparison of incident response performance between organizations with high-fidelity simulation investment but no recent disaster versus organizations with recent disaster experience but comparatively weak simulation programs, matched for domain and system complexity.',
    'If simulation-only organizations perform comparably, the catastrophe-necessity claim is substantially a status-preserving narrative for disaster-experience holders rather than a genuine competence requirement, strengthening the tangled_rope/extractive reading; if disaster-experienced organizations perform reliably better even controlling for simulation investment, the coordination function is stronger than currently credited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_necessity_vs_simulation_sufficiency, empirical, 'Whether catastrophe-derived competence is empirically distinct from simulation-derived competence.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three kernel readings (catastrophe_as_necessary, near_miss_as_bridge, simulation_as_sufficient) locate their disagreement — is it about the sufficiency of the STIMULUS (real vs. simulated vs. near-miss) or about the sufficiency of the ORGANIZATIONAL RESPONSE to any stimulus (investigation rigor, blame-free reporting, resource follow-through)?',
    'Structured comparison of case studies where organizations received a real catastrophe, a near-miss, and a high-fidelity simulation respectively, holding investigation rigor and follow-through constant, to isolate whether the stimulus type or the response quality drives the competence outcome.',
    'If response quality dominates, all three readings converge to a common underlying claim about investigation and follow-through rigor, and the catastrophe-necessity claim''s distinctiveness collapses; if stimulus type dominates independent of response quality, the readings remain genuinely structurally distinct constraints as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel''s three readings disagree about stimulus type or response quality.').

omega_variable(
    disaster_experience_authority_naturalness,
    'Is the elevated deference given to disaster-experienced veterans a natural and appropriate epistemic weighting of first-hand knowledge, or a constructed status hierarchy that this reading benefits from and helps sustain?',
    'Trace whether veteran-operator authority persists in domains where simulation has been independently validated as equivalent (e.g., commercial aviation, where simulator-only certification is already the norm for many competencies) — if authority does not concentrate around disaster-experience there, the hierarchy is domain-contingent rather than naturally warranted.',
    'If the authority pattern is domain-contingent rather than a natural epistemic fact, the beneficiary structure named in this story is substantially constructed rather than earned, reinforcing the extractive reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disaster_experience_authority_naturalness, conceptual, 'Whether disaster-experience authority is epistemically warranted or constructed status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 8, 0.28).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 16, 0.33).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 24, 0.37).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 32, 0.4).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 32, 0.45).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__catastrophe_as_necessary, 0.1).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the competence_retention_exercise kernel. simulation_as_sufficient claims high-fidelity simulation is structurally equivalent to real catastrophic exercise (lower ε expected, closer to rope). near_miss_as_bridge claims near-misses provide adequate real feedback without full catastrophe (intermediate ε expected). This story (catastrophe_as_necessary) authors the highest-ε, most extraction-laden reading because it is the reading most tied to a concentrated beneficiary class (disaster-experience holders, investigation specialists) whose authority depends on devaluing the alternatives. Each reading is ε-invariant on its own terms; they are linked via affects_constraints, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
