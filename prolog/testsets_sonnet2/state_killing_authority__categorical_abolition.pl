% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: State Capital Punishment Authority (Categorical Abolition Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the categorical-abolition reading of the
 *   state-killing-authority kernel: the claim that state execution is
 *   impermissible regardless of the crime committed or the consequences
 *   achieved, grounded in an inalienable right to life that the condemned
 *   person cannot forfeit through any act. Under this reading, ε is authored
 *   for the standing arrangement — actual ongoing capital prosecution and
 *   execution as practiced — assessed by the abolitionist reading's own
 *   lights, not for the abolitionist's endorsed alternative (life
 *   imprisonment), which would trivially read as ε≈0. The condemned person
 *   remains inside the rights-holder set throughout; the state itself is
 *   repositioned as the entity that becomes a rights-violator at the moment
 *   of execution. Sibling readings (retributive_desert,
 *   deterrence_instrument) are separate constraint stories with their own ε
 *   and stakeholder structures — this file does not average over them or
 *   hedge between them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.62).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.58).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, snare).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "State Capital Punishment Authority (Categorical Abolition Reading)").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '550cea81-6fb1-4cf0-b39a-5374bdabcb8d').
narrative_ontology:cs_kernel_codification('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', distributed).
narrative_ontology:cs_authority_grounding('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', distributed).
narrative_ontology:cs_reading_relation('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', foundational, life_is_inalienable_and_nonforfeitable).
narrative_ontology:cs_axiom_status(life_is_inalienable_and_nonforfeitable, holdable).
narrative_ontology:cs_axiom_grounding('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', life_is_inalienable_and_nonforfeitable, deontological).
narrative_ontology:cs_axiom('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', foundational, state_execution_is_categorically_impermissible_irrespective_of_outcome).
narrative_ontology:cs_axiom_status(state_execution_is_categorically_impermissible_irrespective_of_outcome, holdable).
narrative_ontology:cs_axiom_grounding('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', state_execution_is_categorically_impermissible_irrespective_of_outcome, deontological).
narrative_ontology:cs_reference_frame('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', inalienable_life_natural_rights_tradition).
narrative_ontology:cs_drift_state('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', contemporary_abolition_movement_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('550cea81-6fb1-4cf0-b39a-5374bdabcb8d', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, prosecutorial_offices_seeking_death_verdicts).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, elected_officials_running_on_tough_on_crime_platforms).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, abolitionist_victims_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, retributive_victims_families).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, inalienable_right_to_life_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death by the state; under this reading they remain full members of the rights-holder set regardless of the crime committed, because life is treated as inalienable and non-forfeitable. They have no exit from the apparatus once sentenced except clemency, appeal, or exoneration — all administered by the same state that condemns them.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, payer,
    powerless, biographical, trapped, national).

% A documented subset of condemned persons who did not commit the crime; under a categorical prohibition their execution is impermissible independent of guilt, but wrongful convictions are the starkest evidence used by this reading that the killing authority cannot be exercised without irreversible error. They bear the maximal possible cost of the constraint's continued operation.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, wrongfully_convicted_death_row_inmates, payer,
    powerless, biographical, trapped, national).

% Prosecutes capital cases, carries out sentences, and defends the practice's constitutionality. Under this reading, the apparatus itself enters the potential-violator set the moment it executes anyone — the constraint reclassifies the state's own conduct as the rights violation, inverting the standard framing where the condemned is the wrongdoer and the state is the enforcer of justice.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain political capital from visible pursuit and execution of capital sentences, particularly in high-profile cases. They face no direct cost from the killing authority's exercise and can shift positions or move to different offices without personal exposure to the apparatus they champion.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, elected_officials_running_on_tough_on_crime_platforms, beneficiary,
    powerful, biographical, mobile, national).

% Family members of murder victims who want the execution carried out, believing it delivers proportional justice or closure. Under this reading their preference is treated as morally irrelevant to the constraint's permissibility — the categorical prohibition does not weigh consequences or desert, which many experience as their loss being instrumentalized then dismissed.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_victims_families, excluded,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, retributive_victims_families, beneficiary).

% Family members of murder victims who oppose execution on principle, sometimes because they hold the same inalienability commitment the constraint asserts. Prosecutors routinely do not call them as sentencing-phase witnesses because their testimony undercuts the state's case for death, structurally marginalizing them even though their loss is identical in kind to that of pro-execution families.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_victims_families, excluded,
    moderate, biographical, constrained, local).

% Adjudicate constitutional challenges to capital punishment, including cruel-and-unusual-punishment claims and procedural due process. They can narrow, expand, or eliminate the killing authority through doctrine but do not themselves carry out executions.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, appellate_and_constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine coordination problem this reading recognizes the death penalty as solving — the categorical-abolition premise holds that whatever social order or retributive function capital punishment purports to serve, none of it can license the state crossing the inalienable-life threshold. Any coordination the practice appears to provide (closure, deterrence signaling, cost management of long incarceration) is, on this reading, either illusory or achievable without killing.
% TRANSFER_FUNCTION: The arrangement moves the condemned person's life from the category of protected right to the category of state-disposable asset, and moves political and psychological capital (closure narratives, tough-on-crime credibility) to prosecutors and elected officials — irrespective of whether the underlying conviction is accurate.
% ABSENT_VOICES: Abolitionist victims' families are structurally excluded from sentencing proceedings when their testimony would undercut a death verdict; wrongfully convicted individuals are by definition unable to contest the classification of their case as legitimate before execution; international human-rights bodies condemning the practice have no standing in domestic sentencing.
% DISAPPEARANCE_RATIONALE: If the state's authority to kill were eliminated overnight under this reading, capital prosecutions would halt, death rows would be commuted to life sentences, prosecutorial charging strategy would shift entirely toward incarceration-based outcomes, and the political theater built around seeking death verdicts would lose its highest-stakes stage — a substantial rearrangement of criminal justice practice and electoral messaging.
% FOUNDING_PROBLEM: The practice of state execution was historically built to solve retribution, deterrence, and finality — signaling that the gravest crimes receive the gravest response and permanently incapacitating the offender.
% FOUNDING_PROBLEM_CORROBORATION: Death penalty prosecutors and elected officials attest the founding problem (proportional response, public safety, victim closure) remains live. Exoneration organizations, several state supreme courts issuing moratoria, and international human-rights monitoring bodies — all outside the class of officials who benefit from continued capital prosecution — attest that the empirical premises (deterrence effect, error-free administration) have not held up, which is the evidentiary basis this reading relies on to treat the founding problem as either dead or never legitimately curable by killing.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects the ongoing exercise of an authority this reading holds to be categorically illegitimate against the condemned population, rising modestly over the interval as documented exoneration counts have grown, sharpening the visible cost of continued practice. Suppression (0.58, falling slightly) reflects the apparatus's continued but increasingly contested enforcement of death sentences against mounting appellate and moratorium pressure — the enforcement machinery persists but is under real duress across the interval, hence the falling trajectory. Theater ratio (0.40, rising) captures a growing share of capital case processing consumed by procedural, symbolic, and appellate ritual rather than actual execution — many sentences are handed down and litigated for decades without being carried out, which this reading treats as a maintained performance of a supposedly deterrent/retributive function whose deterrent premise is contested. Accessibility collapse is comparatively low (0.35) because legal and political alternatives to capital punishment (life without parole, moratoria, legislative abolition) remain visibly available and increasingly adopted, unlike a genuine natural-law constraint. Resistance is high (0.72) because organized abolitionist advocacy, exoneration litigation, and international pressure actively contest the practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons and wrongfully convicted inmates sit at the target end: trapped exit, powerless, bearing the full and irreversible cost of the constraint's operation. The state execution apparatus is the agenda-setter with institutional power and arbitrage-grade exit (it can adjust charging policy, seek moratoria, or continue prosecuting without personal exposure). Elected officials benefit politically without bearing the executed sentence's cost, placing them near the beneficiary end. Both families-of-victims groups are excluded from full participation in the process that determines the outcome, but asymmetrically: retributive families are heard and sometimes elevated as beneficiaries of the death-seeking narrative, while abolitionist families are structurally silenced by prosecutorial strategy, which is why abolitionist_victims_families carries role excluded without a beneficiary secondary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem interview surfaces the mandatrophy question directly: officials who benefit from capital prosecution attest the founding problem (proportional justice, deterrence, closure) remains live, while exoneration bodies, moratorium-issuing courts, and international monitors — outside the beneficiary class — attest the empirical premises underlying that problem have substantially collapsed. This is the live status=contested + verdict=world_rearranges pattern the R5 interview is built to expose: rather than asserting resolution, the story routes the corroboration gap to the six_questions object and leaves the mismatch for the engine's flag rather than declaring mandatrophy_resolved unilaterally in prose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inalienability_versus_forfeiture_premise,
    'Is the inalienable-life premise this reading rests on itself a defensible foundational claim, or is it one contestable position within a live moral dispute that the retributive_desert reading rejects on its own terms (forfeiture through wrongdoing)?',
    'No empirical resolution mechanism exists — this is a genuinely conceptual/normative disagreement between readings of the same kernel; each reading''s axioms are irreducible starting points rather than derivable conclusions.',
    'If inalienability is treated as the only defensible premise, the retributive and deterrence readings are structurally unstable rather than merely disfavored; if treated as one live option among several, all three readings coexist as genuine alternatives within the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inalienability_versus_forfeiture_premise, conceptual, 'Whether the inalienable-life axiom forecloses or merely competes with forfeiture-based readings.').

omega_variable(
    state_as_violator_reclassification,
    'Does reclassifying the executing state itself as a potential rights-violator (rather than a legitimate enforcer of justice) depend on empirical facts about wrongful convictions, or does it hold even in a hypothetical zero-error system?',
    'Examine whether abolitionist argumentation and advocacy persist in hypothetical zero-error framings (e.g. philosophical thought experiments with certain guilt) versus real-world wrongful-conviction statistics; if the argument holds identically in both cases it is a pure conceptual claim, if it weakens in the zero-error case it is partly empirically contingent.',
    'If the reading is partly empirically contingent on error rates, improved forensic accuracy could someday partially undercut this reading''s force; if fully conceptual, no empirical improvement in accuracy would change the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_as_violator_reclassification, conceptual, 'Whether the state-as-violator premise is empirically contingent on error rates or purely deontological.').

omega_variable(
    victims_families_split_representation,
    'Is the marginalization of abolitionist victims'' families in sentencing proceedings a deliberate prosecutorial strategy or an emergent artifact of adversarial trial structure that happens to favor death-seeking narratives?',
    'Comparative analysis of sentencing-phase witness selection across jurisdictions and prosecutorial offices; interviews with capital defense attorneys about how often abolitionist family testimony is offered versus excluded.',
    'If deliberate, this strengthens the snare classification (active suppression of a specific voice class to maintain extraction); if emergent, it suggests a structural feature of adversarial process rather than intentional suppression, which would soften but not eliminate the suppression metric''s structural reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victims_families_split_representation, empirical, 'Whether abolitionist-family marginalization is strategic suppression or a structural byproduct of trial procedure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__categorical_abolition, theater_ratio, 8, 0.27).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__categorical_abolition, theater_ratio, 16, 0.31).
narrative_ontology:measurement(stat_tr_t24, state_killing_authority__categorical_abolition, theater_ratio, 24, 0.35).
narrative_ontology:measurement(stat_tr_t32, state_killing_authority__categorical_abolition, theater_ratio, 32, 0.38).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__categorical_abolition, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__categorical_abolition, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(stat_be_t24, state_killing_authority__categorical_abolition, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(stat_be_t32, state_killing_authority__categorical_abolition, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__categorical_abolition, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__categorical_abolition, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(stat_su_t24, state_killing_authority__categorical_abolition, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(stat_su_t32, state_killing_authority__categorical_abolition, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the state_killing_authority kernel. categorical_abolition (this file) authors ε=0.62 for the standing execution apparatus by abolitionist lights; retributive_desert and deterrence_instrument are separate files with independently authored ε, stakeholder sets, and classifications, sharing only the kernel identity and not the metric values. The families-of-victims split (retributive vs. abolitionist) and the state's reclassification as potential-violator are structural deltas unique to this reading and do not carry over to the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
