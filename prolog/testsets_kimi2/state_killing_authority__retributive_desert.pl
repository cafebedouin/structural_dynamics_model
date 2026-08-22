% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Retributive Desert: Capital Punishment as Proportional Forfeiture
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint story models the retributive-desert reading of state
 *   killing authority: the claim that murderers forfeit their right to life
 *   and that proportional justice under lex talionis requires the state to
 *   execute those convicted of capital murder. The murdered victim enters the
 *   beneficiary set posthumously through symbolic vindication; the condemned
 *   person exits the rights-holder set via forfeiture. State authority is
 *   grounded in a proportionality norm rather than outcome. This is one
 *   reading of the state_killing_authority kernel; sibling readings
 *   (categorical_abolition, deterrence_instrument) are separate constraints.
 *
 * KEY AGENTS:
 *   - State execution authority (agenda_setter, institutional): administers death sentences and defends retributive statutes.
 *   - Condemned persons (payer, powerless): bear the ultimate extraction of life; physically trapped post-sentence.
 *   - Murder victims (beneficiary, non-agent): posthumously vindicated by proportional state response.
 *   - Victims' survivors (beneficiary, moderate): receive symbolic closure and retributive equilibrium.
 *   - Retributive justice community (beneficiary, organized): normative order upheld by the constraint.
 *   - Abolitionist advocates (excluded, organized): structurally overruled by the retributive logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.88).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.78).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.88).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Retributive Desert: Capital Punishment as Proportional Forfeiture").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '96546e6c-11dc-42d8-b442-78759edf7491').
narrative_ontology:cs_kernel_codification('96546e6c-11dc-42d8-b442-78759edf7491', formalized).
narrative_ontology:cs_authority_grounding('96546e6c-11dc-42d8-b442-78759edf7491', lineage).
narrative_ontology:cs_interpretation_layer_present('96546e6c-11dc-42d8-b442-78759edf7491').
narrative_ontology:cs_reading_relation('96546e6c-11dc-42d8-b442-78759edf7491', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('96546e6c-11dc-42d8-b442-78759edf7491', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom('96546e6c-11dc-42d8-b442-78759edf7491', foundational, forfeiture_of_life_right).
narrative_ontology:cs_axiom_status(forfeiture_of_life_right, holdable).
narrative_ontology:cs_axiom_grounding('96546e6c-11dc-42d8-b442-78759edf7491', forfeiture_of_life_right, deontological).
narrative_ontology:cs_axiom('96546e6c-11dc-42d8-b442-78759edf7491', foundational, lex_talionis_proportionality).
narrative_ontology:cs_axiom_status(lex_talionis_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('96546e6c-11dc-42d8-b442-78759edf7491', lex_talionis_proportionality, deontological).
narrative_ontology:cs_reference_frame('96546e6c-11dc-42d8-b442-78759edf7491', classical_retributive_order).
narrative_ontology:cs_drift_state('96546e6c-11dc-42d8-b442-78759edf7491', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('96546e6c-11dc-42d8-b442-78759edf7491', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, victims_survivors).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, retributive_justice_community).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State agencies and officials authorized to impose and carry out death sentences under retributive statutes. They set execution protocols, schedule executions, and defend the constraint's legitimacy as proportional justice, drawing authority from legal lineage rather than outcome metrics.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_execution_authority, agenda_setter,
    institutional, generational, constrained, national).

% Individuals convicted of capital murder and sentenced to death. Under this reading they have forfeited their right to life and bear the ultimate extraction: state-administered death. Exit is physically impossible once sentence is final and all appeals are exhausted.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_persons, payer,
    powerless, immediate, trapped, local).

% Persons killed in capital murders. Under retributive desert, their death is symbolically vindicated by the execution of their murderer. They are posthumous beneficiaries of proportional justice but collect no material benefit and exercise no agency.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murder_victims).

% Family members and communities of murder victims. The retributive framework grants them symbolic closure and the assurance that proportional harm has been visited upon the perpetrator, vindicating their loss through state-administered equivalence.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victims_survivors, beneficiary,
    moderate, biographical, constrained, national).

% Judges, prosecutors, legal scholars, and citizens who affirm lex talionis. The constraint vindicates their normative commitment to proportional punishment and maintains a social order in which grave harm receives a grave, measured response.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, retributive_justice_community, beneficiary,
    organized, generational, constrained, national).

% Organizations and individuals who reject all state killing on moral and legal grounds. They are formally heard in appeals and political debate but are structurally excluded from the retributive decision logic that mandates death for death.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable, proportionate response to the most severe intentional harm: the state administers death to those who unlawfully cause death, creating a moral equilibrium under lex talionis and preventing cycles of private vengeance.
% TRANSFER_FUNCTION: Transfers the condemned person's life to the state's retributive ledger, and transfers symbolic vindication to the victim and their community. No material transfer occurs; the currency is life and moral status.
% ABSENT_VOICES: The condemned person after final sentence is procedurally muted; abolitionist advocates are heard but overruled by the retributive framework; future potential victims of erroneous execution are unrepresented in the sentencing phase.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, capital sentences would halt, death rows would empty into life-without-parole or other sentences, retributive legal frameworks would require rewriting, and the symbolic equilibrium between murderer and victim would collapse into other justifications such as deterrence, rehabilitation, or abolition.
% FOUNDING_PROBLEM: Unpunished murder creates a moral and social imbalance; without proportionate state response, victims' survivors pursue private vengeance, social order erodes, and the severity of murder is not symbolically matched by the community's response.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by comparative legal historians and anthropologists outside the retributive community who document blood-feud cycles in state-weak environments; contested by criminologists who argue state execution does not restore equilibrium and by abolitionists who reject the equilibrium framing entirely.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__retributive_desert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is near-maximum (0.88) because the constraint extracts life itself. Suppression is high (0.78) because physical incarceration, execution apparatus, and active exclusion of abolitionist alternatives require continuous state violence. Theater ratio is moderate and rising (0.30 to 0.50) because a growing gap between death sentences and actual executions turns increasing shares of the machinery into performative maintenance of the retributive symbol. Accessibility collapse is very high (0.92) because once sentenced, the condemned person's alternatives collapse to procedural delay only. Resistance is substantial (0.62) due to ongoing abolitionist legal and political challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the retributive seat, the constraint is moral equilibrium and necessary social order; from the condemned seat, it is state-administered annihilation with no exit. The survivor/victim seat experiences symbolic benefit at no material cost. The engine computes these divergences from structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are full targets: they are declared victims, powerless, trapped, and local, yielding d near 1.0. Murder victims and survivors are beneficiaries with low power and trapped exit but are structurally subsidized by the constraint's symbolic vindication, yielding d near the beneficiary end. The retributive justice community benefits from normative validation (low d). The state execution authority administers the extraction and sits near symmetric, though its institutional power and constrained political exit place it slightly toward the beneficiary side.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by capturing the hybrid nature of the constraint: it coordinates a genuine moral/legal order (preventing private vengeance, providing symbolic closure) while simultaneously extracting the ultimate asymmetric cost (life). If the retributive justification had fully atrophied and the constraint persisted only for political signaling, it would drift toward piton or snare. The live founding problem and active enforcement maintain the tangled_rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_vs_inalienability,
    'Does the forfeiture of the right to life logically foreclose the abolitionist claim that life is inalienable, or do these premises merely coexist in incommensurable frameworks?',
    'Philosophical analysis of whether inalienability and forfeiture are logical contraries within a unified theory of rights, or whether they operate in separate normative frameworks that cannot adjudicate one another.',
    'If forfeiture forecloses inalienability, the retributive reading structurally displaces abolition within a single framework; if not, the kernel remains irreducibly contested with no logical resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_vs_inalienability, conceptual, 'Logical relationship between forfeiture and inalienability premises').

omega_variable(
    error_rate_under_retributive_justice,
    'What is the rate of wrongful conviction in capital cases, and does it structurally undermine the retributive claim that only the factually guilty forfeit their rights?',
    'Innocence-project exoneration data, post-conviction DNA review, and criminological studies of capital error rates.',
    'A material error rate would mean the constraint extracts life from parties who have not, under retributive logic, forfeited their rights, shifting the effective classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_rate_under_retributive_justice, empirical, 'Wrongful conviction rate and its impact on retributive legitimacy').

omega_variable(
    vindication_mechanism,
    'Is posthumous vindication a real benefit to murdered victims or a symbolic construct that primarily benefits the living community?',
    'Sociological study of survivor well-being and sense of closure under retributive versus restorative justice frameworks.',
    'If vindication is purely symbolic for the living, the murdered victim''s beneficiary status is nominal and the extraction is asymmetrically borne by the condemned for the psychic benefit of survivors and the retributive community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vindication_mechanism, empirical, 'Whether posthumous vindication benefits the dead or the living').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__retributive_desert, theater_ratio, 10, 0.33).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__retributive_desert, theater_ratio, 20, 0.38).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__retributive_desert, theater_ratio, 30, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.46).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__retributive_desert, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__retributive_desert, base_extractiveness, 10, 0.84).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__retributive_desert, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__retributive_desert, base_extractiveness, 30, 0.86).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.87).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__retributive_desert, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__retributive_desert, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__retributive_desert, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__retributive_desert, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__retributive_desert, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_killing_authority kernel, which decomposes into at least three structurally distinct constraints: retributive_desert (this file), deterrence_instrument, and categorical_abolition. Each reading has a different beneficiary/victim structure, epsilon profile, and normative grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
