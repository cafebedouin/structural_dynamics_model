% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_instrument, []).

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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Capital Punishment as Deterrent Instrument
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence instrument' reading of state
 *   killing authority, where capital punishment is justified solely by its
 *   ability to prevent future murders at an acceptable cost. It functions as
 *   a coercive mechanism intended to coordinate public safety, but its
 *   efficacy is highly contested, leading to significant extraction from
 *   condemned individuals. The claimed type is 'tangled_rope' because it
 *   purports a coordination function (deterrence) while demonstrably
 *   involving asymmetric extraction (from the condemned) and requiring active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.85).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.95).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment as Deterrent Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, 'f2906923-ec18-4870-a074-052be0024d5a').
narrative_ontology:cs_kernel_codification('f2906923-ec18-4870-a074-052be0024d5a', formalized).
narrative_ontology:cs_authority_grounding('f2906923-ec18-4870-a074-052be0024d5a', lineage).
narrative_ontology:cs_interpretation_layer_present('f2906923-ec18-4870-a074-052be0024d5a').
narrative_ontology:cs_reading_relation('f2906923-ec18-4870-a074-052be0024d5a', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('f2906923-ec18-4870-a074-052be0024d5a', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_axiom('f2906923-ec18-4870-a074-052be0024d5a', foundational, punishment_as_deterrent).
narrative_ontology:cs_axiom_status(punishment_as_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('f2906923-ec18-4870-a074-052be0024d5a', punishment_as_deterrent, empirically_contingent).
narrative_ontology:cs_axiom('f2906923-ec18-4870-a074-052be0024d5a', secondary, state_right_to_protect_citizens).
narrative_ontology:cs_axiom_status(state_right_to_protect_citizens, holdable).
narrative_ontology:cs_axiom_grounding('f2906923-ec18-4870-a074-052be0024d5a', state_right_to_protect_citizens, deontological).
narrative_ontology:cs_reference_frame('f2906923-ec18-4870-a074-052be0024d5a', utilitarian_crime_prevention).
narrative_ontology:cs_drift_state('f2906923-ec18-4870-a074-052be0024d5a', contemporary_criminological_evidence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f2906923-ec18-4870-a074-052be0024d5a', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, state_authority).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_individuals).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, utilitarian_justice_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The governmental body responsible for maintaining public order and enforcing laws. It claims the authority to impose capital punishment as a necessary tool for crime prevention and public safety, benefiting from the perceived deterrent effect and social stability.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals sentenced to death. They bear the ultimate cost of the constraint, serving as the instrumental means by which future murders are purportedly prevented. Their options for exit are exhausted, and their fate is determined by the state's legal process.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_individuals, payer,
    powerless, immediate, trapped, local).

% The hypothetical individuals whose lives are believed to be saved by the deterrent effect of capital punishment. They are diffuse beneficiaries, gaining from a reduction in violent crime, though this benefit is often debated and difficult to quantify directly.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, potential_future_victims, beneficiary,
    powerless, generational, analytical, national).

% Organizations and individuals who categorically oppose capital punishment on moral or ethical grounds, or due to concerns about its efficacy and fairness. They are excluded from the direct decision-making process regarding individual sentences but actively campaign for legislative change and public opinion shifts.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Legal professionals who implement the capital punishment system. Prosecutors seek death sentences, and judges preside over trials and appeals. Their careers and professional identities are often tied to the effective functioning of the criminal justice system, including its most severe penalties.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, prosecutors_judges, agenda_setter,
    institutional, biographical, constrained, national).

% Academics and researchers who study the effects of capital punishment, particularly its deterrent efficacy and societal costs. They provide empirical data and analysis that often challenges the foundational claims of this reading, but their findings may or may not influence policy.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, criminologists_researchers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate public safety by deterring potential murderers through the threat and application of capital punishment, thereby reducing violent crime rates.
% TRANSFER_FUNCTION: Transfers the ultimate cost (life) from potential future victims to condemned individuals, with the state acting as the agent of this transfer to maintain social order.
% ABSENT_VOICES: Categorical abolitionists, human rights organizations, and many criminologists who dispute the deterrent effect are often marginalized in policy debates, particularly in jurisdictions where capital punishment is strongly supported. Their arguments for inherent human dignity or empirical inefficacy are not given equal weight by proponents of this reading.
% DISAPPEARANCE_RATIONALE: If capital punishment vanished overnight, the criminal justice system would need to fundamentally reorganize its approach to severe crime, finding alternative ultimate sanctions. Public discourse on justice, retribution, and state power would shift dramatically, and the perceived balance of public safety would be re-evaluated.
% FOUNDING_PROBLEM: The problem of severe violent crime, particularly murder, and the perceived need for an ultimate sanction to deter potential offenders and ensure public safety.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (state authorities, some segments of the public) attest that the problem of violent crime and the need for deterrence remain live. Opponents (abolitionist groups, many criminologists) argue that the founding problem is either not effectively addressed by capital punishment or that its efficacy as a deterrent is empirically disproven, citing extensive research and international comparisons.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_instrument_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) because the condemned individual pays the ultimate price, and the 'acceptable cost' clause often minimizes this cost in favor of perceived societal benefit. Suppression is very high (0.95) due to the state's monopoly on legitimate force and the finality of execution, which eliminates all alternatives for the condemned. Theater ratio is moderate-high (0.6) because while the performance of justice and deterrence is maintained, empirical evidence for its actual deterrent effect is weak or contested, suggesting a significant performative component. Resistance is high (0.7) from abolitionist movements and human rights organizations. Accessibility collapse is near total (0.98) for the condemned. The temporal measurements reflect a period where the perceived need for deterrence and the actual application of capital punishment fluctuated, with a general trend towards higher theatricality as empirical support for deterrence waned.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state authority and those who believe in its deterrent effect, this constraint is a necessary, albeit severe, tool for public safety (a form of 'rope' or 'tangled_rope' with justified extraction). From the perspective of the condemned and abolitionists, it is a pure extraction (a 'snare') based on a flawed premise, where the coordination story is a cover for state violence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state authority and potential future victims are the primary beneficiaries, as the constraint is framed as protecting them. Condemned individuals are the clear targets, bearing the full cost. Prosecutors and judges are agenda-setters who implement the system. Abolitionist advocates are excluded voices, while criminologists act as analytical observers, often challenging the foundational premise of deterrence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_efficacy_ambiguity,
    'Does capital punishment actually prevent future murders more effectively than life imprisonment, and at what statistical significance?',
    'Comprehensive, longitudinal, and methodologically rigorous empirical studies comparing murder rates in jurisdictions with and without capital punishment, controlling for confounding variables.',
    'If deterrence is empirically disproven, the foundational premise of this reading collapses, reclassifying it closer to a ''snare'' or ''piton'' (if maintained theatrically). If proven, it strengthens the ''tangled_rope'' classification by validating its coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrent_efficacy_ambiguity, empirical, 'Uncertainty regarding the actual deterrent effect of capital punishment.').

omega_variable(
    acceptable_cost_definition,
    'What constitutes an ''acceptable cost'' in terms of human life, financial expenditure, and risk of executing the innocent, for the perceived benefit of deterrence?',
    'Public deliberation, legislative consensus, and judicial review that explicitly weighs the various costs against the claimed benefits, with clear ethical frameworks.',
    'A higher threshold for ''acceptable cost'' would make the constraint harder to justify, potentially leading to its abandonment or reclassification as a ''snare'' due to disproportionate extraction. A lower threshold would make it easier to maintain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_definition, preference, 'Ambiguity in defining the ''acceptable cost'' for capital punishment''s deterrent function.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''deterrence_instrument'' reading of the ''state_killing_authority'' kernel, distinct from other justifications?',
    'Conceptual analysis of legal texts, philosophical arguments, and judicial opinions to confirm the primary grounding in deterrence, rather than retribution or categorical impermissibility.',
    'Misidentification would lead to an inaccurate classification of the constraint''s structural properties and its relationship to sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being analyzed within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_authority__deterrence_instrument, theater_ratio, 1976, 0.4).
narrative_ontology:measurement(stat_tr_t1986, state_killing_authority__deterrence_instrument, theater_ratio, 1986, 0.45).
narrative_ontology:measurement(stat_tr_t1996, state_killing_authority__deterrence_instrument, theater_ratio, 1996, 0.55).
narrative_ontology:measurement(stat_tr_t2006, state_killing_authority__deterrence_instrument, theater_ratio, 2006, 0.6).
narrative_ontology:measurement(stat_tr_t2016, state_killing_authority__deterrence_instrument, theater_ratio, 2016, 0.62).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__deterrence_instrument, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_authority__deterrence_instrument, base_extractiveness, 1976, 0.75).
narrative_ontology:measurement(stat_be_t1986, state_killing_authority__deterrence_instrument, base_extractiveness, 1986, 0.8).
narrative_ontology:measurement(stat_be_t1996, state_killing_authority__deterrence_instrument, base_extractiveness, 1996, 0.88).
narrative_ontology:measurement(stat_be_t2006, state_killing_authority__deterrence_instrument, base_extractiveness, 2006, 0.87).
narrative_ontology:measurement(stat_be_t2016, state_killing_authority__deterrence_instrument, base_extractiveness, 2016, 0.86).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__deterrence_instrument, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_authority__deterrence_instrument, suppression_requirement, 1976, 0.85).
narrative_ontology:measurement(stat_su_t1986, state_killing_authority__deterrence_instrument, suppression_requirement, 1986, 0.9).
narrative_ontology:measurement(stat_su_t1996, state_killing_authority__deterrence_instrument, suppression_requirement, 1996, 0.95).
narrative_ontology:measurement(stat_su_t2006, state_killing_authority__deterrence_instrument, suppression_requirement, 2006, 0.96).
narrative_ontology:measurement(stat_su_t2016, state_killing_authority__deterrence_instrument, suppression_requirement, 2016, 0.95).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__deterrence_instrument, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__deterrence_instrument, 0.1).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, criminal_justice_system_legitimacy).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, sentencing_guidelines).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_authority' kernel, focusing on deterrence. It is linked to sibling readings that offer alternative justifications or categorical rejections of capital punishment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
