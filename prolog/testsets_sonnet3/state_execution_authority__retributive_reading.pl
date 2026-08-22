% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: Capital Punishment as Retributive Moral Restoration
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story authors the retributive reading of the contested
 *   state-execution-authority kernel: execution is justified because it
 *   restores a proportionate moral balance disrupted by a heinous crime, an
 *   outcome the reading holds cannot be substituted by imprisonment (which it
 *   treats as categorically less than what proportionality requires). This is
 *   one of three sibling readings of the same kernel — deterrence_reading
 *   (execution as future-crime prevention) and abolition_reading (execution
 *   as categorically impermissible) are separate constraints, not alternative
 *   measurements of this one. Under the ε-invariance principle each reading
 *   gets its own file: this reading's ε is high because the moral-restoration
 *   function has no non-lethal substitute by its own lights, which is a
 *   structurally different claim from the deterrence reading's
 *   empirically-contingent ε or the abolition reading's ε for the same
 *   standing arrangement viewed as impermissible from the outset.
 *
 * KEY AGENTS:
 *   - victims_families: primary beneficiary (moderate/constrained) — collects moral restitution the reading promises
 *   - retributive_justice_polity: institutional beneficiary and agenda-setter (institutional/arbitrage) — maintains capital statutes as the polity's proportionality mechanism
 *   - condemned_offenders: primary target (powerless/trapped) — bears the cost the reading treats as legitimate and required
 *   - wrongfully_convicted_death_row_inmates: primary target under error (powerless/trapped) — bears an irreversible cost the reading treats as tragic error, not framework failure
 *   - capital_defense_attorneys: excluded structural challenger (moderate/constrained) — raises the irreversibility argument the reading does not admit as a challenge to its premise
 *   - state_execution_apparatus: administering agenda-setter (institutional/arbitrage) — carries out sentences without bearing the moral or physical cost
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates proportionality and procedure without being a party under the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.62).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.55).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "Capital Punishment as Retributive Moral Restoration").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, '37caafaf-8220-46c0-acb8-96a78cff305b').
narrative_ontology:cs_kernel_codification('37caafaf-8220-46c0-acb8-96a78cff305b', formalized).
narrative_ontology:cs_authority_grounding('37caafaf-8220-46c0-acb8-96a78cff305b', lineage).
narrative_ontology:cs_interpretation_layer_present('37caafaf-8220-46c0-acb8-96a78cff305b').
narrative_ontology:cs_reading_relation('37caafaf-8220-46c0-acb8-96a78cff305b', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('37caafaf-8220-46c0-acb8-96a78cff305b', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('37caafaf-8220-46c0-acb8-96a78cff305b', foundational, proportionate_death_is_non_substitutable_desert).
narrative_ontology:cs_axiom_status(proportionate_death_is_non_substitutable_desert, holdable).
narrative_ontology:cs_axiom_grounding('37caafaf-8220-46c0-acb8-96a78cff305b', proportionate_death_is_non_substitutable_desert, deontological).
narrative_ontology:cs_axiom('37caafaf-8220-46c0-acb8-96a78cff305b', secondary, wrongful_execution_is_administrative_error_not_framework_defeater).
narrative_ontology:cs_axiom_status(wrongful_execution_is_administrative_error_not_framework_defeater, holdable).
narrative_ontology:cs_axiom_grounding('37caafaf-8220-46c0-acb8-96a78cff305b', wrongful_execution_is_administrative_error_not_framework_defeater, conventional).
narrative_ontology:cs_reference_frame('37caafaf-8220-46c0-acb8-96a78cff305b', classical_proportional_retribution).
narrative_ontology:cs_drift_state('37caafaf-8220-46c0-acb8-96a78cff305b', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('37caafaf-8220-46c0-acb8-96a78cff305b', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_justice_polity).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, condemned_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, proportionate_desert_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_balance_restoration_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have suffered the loss of a family member to a heinous crime. Under this reading, the state's execution of the offender is owed to them as moral restitution — the sentence is framed as vindicating the proportionate weight of their loss. They cannot opt out of the state's proportionality calculus; their standing to demand execution is itself constituted by the reading.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, national).

% The political and legal community that maintains capital statutes, juries that impose death sentences, and legislatures that decline to abolish them. Collects the intangible good of a moral order in which the gravest crimes meet a commensurately grave response; sets prosecutorial and sentencing policy that channels cases toward execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributive_justice_polity, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__retributive_reading, retributive_justice_polity, agenda_setter).

% Convicted of a capital crime and sentenced to death. Under this reading their execution is not an unfortunate byproduct but the constraint's entire point — the cost that restores balance. They have no exit: appeals narrow procedural error but cannot contest the legitimacy of execution as the proportionate response itself.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, condemned_offenders, payer,
    powerless, immediate, trapped, local).

% Individuals sentenced to death who did not commit the crime. Within this reading a wrongful execution is treated as a tragic operational error in an otherwise sound moral framework, not as evidence against the framework's legitimacy. Their exhausted appeals leave no exit; exoneration, when it comes, typically arrives too late or not at all.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, local).

% Represent the condemned and argue against the moral-restoration premise itself, not merely procedure. Their structural argument — that no execution can be undone if wrong, so the framework's error tolerance is incompatible with genuine moral balance — is heard in courtrooms on narrow procedural grounds but is not permitted to unsettle the retributive premise as a matter of law.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, capital_defense_attorneys, excluded,
    moderate, biographical, constrained, national).

% Departments of corrections, execution teams, and appellate courts that administer capital sentences. Enforces the sentence, manages the procedural machinery (clemency review, execution protocols), and bears institutional exposure when errors surface, but does not itself bear the moral or physical cost of a wrongful execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicate Eighth Amendment and due-process challenges to capital statutes and specific executions. Assess proportionality, evolving standards of decency, and procedural adequacy without themselves being a party that benefits or pays under the retributive reading; their rulings can narrow or widen the reading's scope.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, state-administered mechanism through which a political community can express that the gravest crimes receive a response proportionate to their gravity, rather than leaving proportionality to private vengeance or informal reprisal.
% TRANSFER_FUNCTION: Moves the offender's life, as the ultimate cost the state can impose, from the condemned to the satisfaction of a moral ledger the state maintains on behalf of victims' families and the broader polity's sense of proportionate order.
% ABSENT_VOICES: Capital defense attorneys and wrongful-conviction advocates raise the argument that irreversibility itself is incompatible with the moral-restoration premise, since an error can never be corrected once carried out; this argument is heard only as narrow procedural claims, never as a challenge to the retributive premise itself, which stays outside the room where sentencing policy is set.
% DISAPPEARANCE_RATIONALE: If capital punishment were abolished overnight under this reading, the polity's institutional claim to have vindicated victims' losses through proportionate punishment would disappear; sentencing would default to life imprisonment, victims' families would lose the specific form of restitution this reading promises them, and legislatures/courts that currently administer capital statutes would need to reconstitute their sentencing frameworks entirely.
% FOUNDING_PROBLEM: Communities historically lacked a legitimate, non-vigilante mechanism to respond to the gravest crimes with a punishment proportionate to the harm — without state-administered capital punishment, retribution risked devolving into private blood feuds or under-punishment that failed to vindicate victims.
% FOUNDING_PROBLEM_CORROBORATION: Victims'-rights organizations and legislatures that retain capital statutes attest the problem remains live — that only execution vindicates the gravest losses. Independent bodies outside the beneficiary set, including exoneration projects, the American Law Institute (which withdrew its own model penal code provision supporting capital sentencing in 2009 citing irremediable administration problems), and comparative data showing life-without-parole regimes report comparable victim-family closure outcomes, corroborate that the founding problem is at minimum substantially addressable by non-lethal proportionate punishment — undercutting the claim that execution is the only vindicating response.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.62) because the retributive reading's core claim is that no substitute punishment satisfies the moral-restoration function — imprisonment is categorically insufficient by the reading's own terms, which makes the cost extracted from the condemned (their life) structurally non-negotiable rather than a policy choice among comparable options. Suppression is moderate (0.55): the reading does not suppress procedural appeal, but it does suppress the specific argument that irreversibility invalidates the premise — that argument is structurally excluded from altering sentencing policy, only from contesting individual procedural error. Theater ratio is modest (0.28) and rising slowly, reflecting increasing procedural ritual (extended appeals, clemency review, execution protocol litigation) layered onto a function whose core administration has not changed. Accessibility collapse is moderate (0.4): non-lethal proportionate alternatives (life without parole) are visible and administratively available, so alternatives have not collapsed as completely as in a genuine mountain case — the reading's insistence that no substitute suffices is a normative claim, not an accessibility fact. Resistance is high (0.7), reflecting sustained international and domestic abolitionist and exoneration-project pressure against the practice.
 *
 * PERSPECTIVAL GAP:
 *   Victims' families and the retributive polity experience this as legitimate coordination — a shared moral ledger genuinely balanced. Condemned offenders and, most sharply, wrongfully convicted inmates experience the identical structure as a tangled or extractive one: coordinated benefit for one party purchased through an irreversible cost imposed on another, with active enforcement (execution apparatus, exhausted appeals machinery) holding the arrangement in place. The engine should compute divergent seat types precisely because the beneficiary and payer positions are structurally opposed, not because either party is wrong about the facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims' families and the retributive polity are declared beneficiaries: the reading's entire justificatory structure exists to deliver something to them (moral restitution, proportionate order), so their derived directionality sits near the beneficiary end. Condemned offenders and wrongfully convicted inmates are declared victims: they bear the terminal cost the reading requires, so their derived directionality sits near the full-target end, amplified further by trapped exit options. Capital defense attorneys are excluded rather than beneficiary or victim — they are structurally positioned to lose the argument regardless of the reading's outcome for any given client, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is authored as contested rather than dead: the retributive reading's own tradition maintains the problem (absence of legitimate proportionate response to heinous crime) is still live, while corroborating evidence from outside the beneficiary set — the ALI's 2009 withdrawal of its own model provision, exoneration data, and comparative victim-outcome research under life-without-parole regimes — suggests the problem is substantially addressable without execution. This keeps the story from either (a) certifying the reading's self-account as settled fact or (b) prematurely declaring the arrangement a dead-function zombie; the mismatch between the reading's internal 'still live' claim and external corroborating evidence toward 'substantially addressable otherwise' is exactly the signal the R5 consumer is built to surface, not something this story resolves on the reading's behalf.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wrongful_execution_framework_validity,
    'Does a documented wrongful execution constitute evidence against the retributive framework''s legitimacy, or is it properly treated as an isolated administrative error within an otherwise sound moral structure?',
    'Track whether courts, legislatures, or the retributive tradition itself revise the framework''s core premise (versus only procedural safeguards) following high-confidence wrongful-execution findings (e.g., posthumous DNA exoneration).',
    'If wrongful executions are treated as framework-invalidating rather than as tragic error, the retributive reading collapses toward the abolition reading''s premise that irreversibility is incompatible with any execution regime — this would be a reading change, not a metric adjustment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_framework_validity, conceptual, 'Whether wrongful execution is error-within-framework or evidence against the framework itself.').

omega_variable(
    moral_restoration_substitutability,
    'Is moral balance restoration for the gravest crimes achievable through severe non-lethal punishment (e.g., life without parole), or does the retributive reading''s insistence on non-substitutability reflect a genuine structural claim rather than a preference?',
    'Comparative victim-family outcome studies across capital and non-capital jurisdictions measuring reported senses of justice, closure, and moral vindication.',
    'If non-lethal punishment produces comparable moral-restoration outcomes, the reading''s high ε (grounded in non-substitutability) loses its structural warrant and the constraint''s extraction becomes harder to distinguish from preference-driven severity rather than a required function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_restoration_substitutability, empirical, 'Whether execution is functionally required for moral restoration or merely preferred.').

omega_variable(
    kernel_framing_under_determination,
    'Is the retributive reading''s core distinguishing claim best framed as ''execution restores moral balance'' (a metaphysical/moral claim about proportionality) or as ''the polity requires an authoritative closure ritual for its gravest crimes'' (a sociological/functional claim about institutional legitimacy)?',
    'Examine whether retributive-reading advocates would accept a non-lethal closure ritual as satisfying their claim, or whether they insist specifically on the offender''s death — this distinguishes the moral-metaphysical framing from the functional-ritual framing.',
    'The moral-metaphysical framing (adopted here) grounds the foundational axiom as deontological and produces the high non-substitutable ε; the functional-ritual framing would ground it as conventional or instrumental, likely lowering ε and shifting the axiom''s grounding_type and potentially its foreclosure relationship to the deterrence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Alternative framings of the retributive claim''s core content and their effect on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__retributive_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__retributive_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__retributive_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__retributive_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__retributive_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__retributive_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__retributive_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__retributive_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__retributive_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__retributive_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__retributive_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__retributive_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_execution_authority kernel. The retributive reading treats the executed offender's death as intrinsically required moral restitution (deontological, non-substitutable), giving it a distinct high-ε profile from the deterrence reading (empirically contingent on crime-prevention efficacy, substitutable if imprisonment is shown equally deterrent) and from the abolition reading (which authors ε for the same standing arrangement as categorically impermissible extraction from the outset). All three share the same underlying practice (state-administered capital punishment) but instantiate structurally distinct constraints with different beneficiary/victim structures and different failure conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
