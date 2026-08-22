% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Categorical Abolitionist Reading of State Killing Authority
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the categorical-abolition reading of the
 *   contested state-killing-authority kernel: state killing is inherently
 *   impermissible regardless of crime or consequence because life is
 *   inalienable and cannot be forfeited by any act, including murder. On this
 *   reading, the condemned person never leaves the rights-holder set, and the
 *   state itself enters the potential-violator set the moment it executes.
 *   The constraint as authored here describes the STANDING ARRANGEMENT under
 *   contest — the continued legal availability and practice of capital
 *   punishment — assessed by the abolitionist reading's own lights, not the
 *   rights-respecting abolition this reading would install. That is why
 *   extractiveness is authored high: from this reading's seat, every
 *   execution and every capital sentence is an extraction of the ultimate
 *   irreversible kind, layered onto a coordination function (finality,
 *   retribution-adjacent public order) that this reading holds is illusory
 *   cover.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.68).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.6).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, snare).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolitionist Reading of State Killing Authority").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, 'bcf873e5-052e-467d-a584-306f8b684ac7').
narrative_ontology:cs_kernel_codification('bcf873e5-052e-467d-a584-306f8b684ac7', distributed).
narrative_ontology:cs_authority_grounding('bcf873e5-052e-467d-a584-306f8b684ac7', distributed).
narrative_ontology:cs_reading_relation('bcf873e5-052e-467d-a584-306f8b684ac7', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('bcf873e5-052e-467d-a584-306f8b684ac7', state_killing_authority__deterrence_instrument, influences).
narrative_ontology:cs_axiom('bcf873e5-052e-467d-a584-306f8b684ac7', foundational, life_is_inalienable_regardless_of_desert).
narrative_ontology:cs_axiom_status(life_is_inalienable_regardless_of_desert, holdable).
narrative_ontology:cs_axiom_grounding('bcf873e5-052e-467d-a584-306f8b684ac7', life_is_inalienable_regardless_of_desert, deontological).
narrative_ontology:cs_axiom('bcf873e5-052e-467d-a584-306f8b684ac7', foundational, state_execution_constitutes_potential_rights_violation).
narrative_ontology:cs_axiom_status(state_execution_constitutes_potential_rights_violation, holdable).
narrative_ontology:cs_axiom_grounding('bcf873e5-052e-467d-a584-306f8b684ac7', state_execution_constitutes_potential_rights_violation, deontological).
narrative_ontology:cs_axiom('bcf873e5-052e-467d-a584-306f8b684ac7', secondary, irreversible_error_risk_bars_capital_punishment).
narrative_ontology:cs_axiom_status(irreversible_error_risk_bars_capital_punishment, holdable).
narrative_ontology:cs_axiom_grounding('bcf873e5-052e-467d-a584-306f8b684ac7', irreversible_error_risk_bars_capital_punishment, empirically_contingent).
narrative_ontology:cs_created_at('bcf873e5-052e-467d-a584-306f8b684ac7', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, prosecutorial_offices).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, death_penalty_political_constituencies).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, abolitionist_victim_families).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, wrongfully_convicted_death_row_inmates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, executing_state_apparatus).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, inalienability_of_life_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, state_monopoly_on_violence_illegitimacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held under sentence of death by a state apparatus that this reading holds has no legitimate authority to kill, regardless of the crime committed. They remain, on this reading, full members of the rights-holder set — their life claim is not forfeited by conviction. Their only exits are appeal, clemency, or exoneration, all mediated by the same institutions that condemned them.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, payer,
    powerless, biographical, trapped, national).

% Carries out executions through corrections departments, courts, and legislatures. On this reading, when it executes it enters the potential-violator set itself — the act of killing is what the constraint condemns, not merely regulates. The state faces no comparable exit cost: it can suspend, resume, or abolish the practice through ordinary legislative and judicial channels.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, executing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, executing_state_apparatus, beneficiary).

% Use the threat and occasional exercise of capital charges as leverage in plea bargaining and as a career and electoral asset. They benefit from the death penalty's continued legal availability independent of whether it is ever carried out, and often frame victims' families as unanimous in wanting execution to justify pursuing it.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, prosecutorial_offices, beneficiary,
    institutional, biographical, arbitrage, regional).

% Family members of murder victims who oppose execution on moral or religious grounds, sometimes explicitly asking prosecutors not to seek death. Prosecutors frequently proceed regardless, marginalizing or omitting their views from sentencing narratives that present victims' families as monolithically pro-execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_victim_families, excluded,
    powerless, biographical, constrained, local).

% A documented subset of the condemned population later exonerated by DNA evidence, recanted testimony, or reinvestigation — sometimes after execution has already occurred. On the abolitionist reading, their existence demonstrates that the state's error rate alone makes the practice categorically impermissible, independent of any individual case's factual guilt.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, wrongfully_convicted_death_row_inmates, payer,
    powerless, biographical, trapped, national).

% Voters and advocacy groups whose political identity and electoral mobilization are organized around retaining capital punishment. They benefit symbolically and politically from the practice's continuation regardless of its deterrent effect, and can shift allegiance or exit the issue coalition without personal cost.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, death_penalty_political_constituencies, beneficiary,
    organized, biographical, mobile, regional).

% Litigators, scholars, and organizations who articulate and press the categorical-abolition reading through appeals, clemency petitions, and legislative campaigns. They document wrongful convictions and racial disparities as evidence for the reading's empirical predicates, without themselves bearing execution risk.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_legal_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, prosecutorial_offices).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Insofar as any coordination function exists, it is negative: the categorical-abolition reading coordinates opposition to state killing by unifying disparate constituencies (religious objectors, exoneration advocates, families opposed to execution, international human-rights bodies) around a single inalienability claim that forecloses case-by-case justification.
% TRANSFER_FUNCTION: Where the state executes despite this reading's claim, the arrangement transfers the condemned person's life itself — the ultimate transfer — to the state's exercise of authority, with no possibility of restitution given irreversibility. Politically, it transfers electoral and prosecutorial capital to institutions that retain the practice.
% ABSENT_VOICES: Abolitionist victim-family members are structurally excluded from sentencing narratives that prosecutors construct as unanimous demands for death; international human rights bodies and comparative-law scholars documenting near-universal developed-world abolition are rarely admitted as authoritative in domestic capital sentencing proceedings.
% DISAPPEARANCE_RATIONALE: If the state's authority to kill were categorically withdrawn overnight, condemned persons would be resentenced to life imprisonment, prosecutorial charging leverage in capital cases would collapse, political constituencies organized around retention would lose their central issue, and the wrongful-execution risk that abolitionists point to as irreversible harm would cease to be possible going forward.
% FOUNDING_PROBLEM: The reading was built to address the specific harm of irreversible, systematically error-prone state killing — responding to documented wrongful executions, racial and class disparities in capital sentencing, and the claim that no institution's fact-finding is reliable enough to warrant an irreversible penalty.
% FOUNDING_PROBLEM_CORROBORATION: Independent exoneration registries (e.g., National Registry of Exonerations), international human rights bodies, and comparative sentencing data from abolitionist jurisdictions outside the U.S. corroborate persistent wrongful-conviction rates in capital cases from sources outside the abolitionist advocacy movement itself; retentionist prosecutors and political constituencies dispute that this makes the underlying founding problem live in the sense the reading claims.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.55 to 0.68) reflecting the accumulation of documented wrongful convictions and executions that, on this reading, retroactively indict the whole practice's legitimacy. Suppression is substantial but not maximal (0.6) because appellate and clemency channels exist, however imperfectly, and abolitionist advocacy operates openly. Theater ratio rises moderately (0.25 to 0.4) reflecting this reading's view that procedural safeguards (extended appeals, competency reviews, clemency boards) increasingly function as legitimating performance around a practice whose core premise this reading holds is irredeemable, rather than as genuine error-correction sufficient to justify irreversible punishment. Accessibility collapse is authored moderate-low (0.35): on the abolitionist reading, alternatives (life imprisonment, restorative approaches) are readily available and increasingly adopted, so alternatives have NOT collapsed — which is part of the reading's case that retention is a choice, not a necessity. Resistance is high (0.75): this reading is actively contested by retentionist prosecutors, victims'-rights organizations, and political constituencies, and requires continuous advocacy, litigation, and international pressure to sustain against reversal.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons and wrongfully convicted death-row inmates sit at the extraction pole: powerless, trapped, bearing the irreversible cost the reading identifies as impermissible in principle. The executing state apparatus and prosecutorial offices sit at the beneficiary pole: institutional actors with arbitrage-level exit (they can suspend, resume, or reframe the practice at will) who derive leverage, legitimacy, or political capital from the death penalty's continued availability. Abolitionist victim families are structurally between these poles but functionally excluded — they bear moral injury from executions carried out in their relative's name against their wishes, yet lack the institutional standing prosecutors hold. Death penalty political constituencies benefit symbolically without bearing the irreversibility risk, and can exit the coalition without personal cost, distinguishing their directionality sharply from the condemned.
 *
 * MANDATROPHY ANALYSIS:
 *   The categorical-abolition reading resists mandatrophy misclassification in both directions: it does not collapse into treating capital punishment as pure coordination (the retentionist framing) because it names identifiable victims (the condemned, the wrongfully executed) and an identifiable extraction mechanism (irreversible killing exceeding any correctable error). It also does not collapse into treating the state's killing authority as if it were value-neutral procedure — the reading insists the coordination story (public safety, closure, retribution) is cover for what remains, at bottom, categorically impermissible regardless of any coordination benefit claimed for it. The founding-problem status is authored 'live' precisely because the empirical predicate (wrongful convictions, disparate application) that motivates the reading continues to be documented by sources outside the abolitionist movement itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the categorical-abolition reading the correct lens for evaluating state killing authority, or do the retributive_desert and deterrence_instrument readings capture legitimate normative considerations this reading forecloses too quickly?',
    'This is not empirically resolvable within a single reading; it depends on which normative framework (deontological inalienability vs. desert-based forfeiture vs. consequentialist deterrence calculus) is adopted. Cross-reading comparison via the linked sibling constraint files (retributive_desert, deterrence_instrument) is the mechanism for surfacing where the readings diverge structurally.',
    'If the retributive_desert reading is adopted instead, condemned persons exit the rights-holder set upon conviction and the extraction/victim structure authored here collapses entirely — the same execution event would be classified as forfeiture-consistent rather than extractive. If deterrence_instrument is adopted, the classification becomes contingent on empirical deterrence data rather than categorical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading of state killing authority is normatively correct; this story deliberately does not adjudicate and instead authors one reading cleanly.').

omega_variable(
    victim_family_representation_ambiguity,
    'How should the reading account for victims'' families who are split between wanting execution and opposing it, when prosecutorial narratives present them as unanimous?',
    'Direct testimony and amicus filings from abolitionist victim-family organizations, compared against prosecutorial sentencing memoranda, would surface the frequency and handling of family dissent from the pro-execution narrative.',
    'If dissenting family voices are systematically underrepresented in capital sentencing, this strengthens the ''absent_voices'' finding and the reading''s claim that the coordination story (unified victim demand for justice) is partly manufactured rather than found.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_family_representation_ambiguity, empirical, 'Whether victims'' families'' actual heterogeneity of view is suppressed in capital case narratives.').

omega_variable(
    irreversibility_vs_correctable_error_boundary,
    'Does documented wrongful-execution risk alone establish categorical impermissibility, or only impermissibility conditional on current error rates, which could in principle fall to near zero with improved forensic and procedural safeguards?',
    'Longitudinal tracking of exoneration rates and forensic reliability improvements would show whether error rates are structurally irreducible or merely currently high; the categorical reading holds the former, a reformist retentionist position would hold the latter.',
    'If error rates are shown to be reducible to near-zero, the categorical reading''s empirical predicate weakens even though its deontological core (inalienability regardless of guilt) would remain untouched — separating the reading''s empirical and deontological supports.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_vs_correctable_error_boundary, empirical, 'Whether the categorical claim rests on irreducible error risk or on a pure inalienability premise independent of error.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__categorical_abolition, theater_ratio, 8, 0.29).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__categorical_abolition, theater_ratio, 16, 0.33).
narrative_ontology:measurement(stat_tr_t24, state_killing_authority__categorical_abolition, theater_ratio, 24, 0.36).
narrative_ontology:measurement(stat_tr_t32, state_killing_authority__categorical_abolition, theater_ratio, 32, 0.38).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__categorical_abolition, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__categorical_abolition, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(stat_be_t24, state_killing_authority__categorical_abolition, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(stat_be_t32, state_killing_authority__categorical_abolition, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__categorical_abolition, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__categorical_abolition, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(stat_su_t24, state_killing_authority__categorical_abolition, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(stat_su_t32, state_killing_authority__categorical_abolition, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the state_killing_authority kernel. categorical_abolition (this file) authors the condemned person as remaining within the rights-holder set and the executing state as a potential rights-violator; retributive_desert authors the condemned as having forfeited the right to life through the crime itself (removing them from the victim set entirely); deterrence_instrument authors legitimacy as conditional on empirical deterrence efficacy rather than categorical. Each reading carries its own stable ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not measurement variants of one constraint but three structurally distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
