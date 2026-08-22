% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Capital Punishment as Deterrence Instrument (Conditional Justification Rule)
 *   domain: criminal justice/political philosophy/constitutional law
 *
 * SUMMARY:
 *   This story instantiates the deterrence_instrument reading of the
 *   contested kernel state_killing_authority: capital punishment is justified
 *   if and only if it prevents future murders at acceptable cost. The
 *   standing arrangement under contest — and the ε referent — is the death
 *   penalty as actually maintained under this rationale, assessed by the
 *   reading's own lights: the reading itself prices the condemned person as
 *   an instrumental cost and seats future potential victims as the
 *   beneficiaries of the preventive effect, and it conditions the whole
 *   arrangement on an empirical claim the National Research Council found
 *   inconclusive in 2012. Sibling readings (retributive_desert,
 *   categorical_abolition) are separate constraints with different
 *   beneficiary/victim structures and different ε values; nothing about them
 *   is averaged into this file. The claim/metrics split is deliberate: the
 *   constraint is claimed as tangled_rope (a real public-safety coordination
 *   function gated by evidence, carrying asymmetric extraction), while the
 *   authored metrics describe substantially extractive, actively enforced
 *   operation with a contested coordination core — the engine measures the
 *   divergence per seat; the claim does not reconcile it.
 *
 * KEY AGENTS:
 *   - condemned_persons: primary target (powerless/trapped) — bears the ultimate cost; their death is the mechanism the arrangement's promise runs on
 *   - wrongly_convicted_defendants: prospective target (powerless/trapped) — the priced-in error cost, occupied without the occupants' knowledge
 *   - future_potential_victims: primary beneficiary (powerless/constrained) — the claimed lives saved; a statistical class with no organized seat of its own
 *   - prosecutorial_offices: agenda_setter and secondary beneficiary (institutional/mobile) — applies the instrument and collects plea leverage from its mere availability
 *   - state_legislatures: agenda_setter (institutional/constrained) — authorizes, defines, and funds the instrument; repeal costs them electoral capital
 *   - appellate_courts: agenda_setter (institutional/constrained) — adjudicates the arrangement's limits and nominally tests its evidentiary condition
 *   - elected_officials: beneficiary (institutional/mobile) — collects electoral capital from the penalty's defense; executions carried out by others
 *   - abolition_advocates: excluded (organized/identity_locked) — categorical objection with no slot in the arrangement's cost-benefit frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.78).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.8).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.78).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment as Deterrence Instrument (Conditional Justification Rule)").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal justice/political philosophy/constitutional law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '695f10cb-2e82-421d-b20a-62aa6673327f').
narrative_ontology:cs_kernel_codification('695f10cb-2e82-421d-b20a-62aa6673327f', formalized).
narrative_ontology:cs_authority_grounding('695f10cb-2e82-421d-b20a-62aa6673327f', expertise).
narrative_ontology:cs_interpretation_layer_present('695f10cb-2e82-421d-b20a-62aa6673327f').
narrative_ontology:cs_reading_relation('695f10cb-2e82-421d-b20a-62aa6673327f', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('695f10cb-2e82-421d-b20a-62aa6673327f', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('695f10cb-2e82-421d-b20a-62aa6673327f', foundational, deterrence_is_necessary_and_sufficient_ground).
narrative_ontology:cs_axiom_status(deterrence_is_necessary_and_sufficient_ground, holdable).
narrative_ontology:cs_axiom_grounding('695f10cb-2e82-421d-b20a-62aa6673327f', deterrence_is_necessary_and_sufficient_ground, instrumental).
narrative_ontology:cs_axiom('695f10cb-2e82-421d-b20a-62aa6673327f', secondary, retributive_desert_insufficient_for_death).
narrative_ontology:cs_axiom_status(retributive_desert_insufficient_for_death, holdable).
narrative_ontology:cs_axiom_grounding('695f10cb-2e82-421d-b20a-62aa6673327f', retributive_desert_insufficient_for_death, instrumental).
narrative_ontology:cs_reference_frame('695f10cb-2e82-421d-b20a-62aa6673327f', evidence_gated_lethal_authority).
narrative_ontology:cs_drift_state('695f10cb-2e82-421d-b20a-62aa6673327f', post_nrc_2012_inconclusiveness, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('695f10cb-2e82-421d-b20a-62aa6673327f', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, prosecutorial_offices).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, elected_officials).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, wrongly_convicted_defendants).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, deterrence_hypothesis).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, consequentialist_punishment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define by statute which crimes may carry a death sentence, authorize or restrict the penalty, and fund the courts and corrections apparatus that carry it out. They act under electoral demand for the ultimate sanction; repealing or narrowing the penalty costs them measurable voter support, while expanding it costs them little.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Review every death sentence through direct appeal and collateral attack, set the procedural rules that speed or slow the process, and test the legislative findings behind the penalty against the Constitution. They have stated on the record that the research record on the penalty's claimed preventive effect is inconclusive, while continuing to defer to legislative judgments; their review is where the arrangement's evidentiary condition is nominally checked.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, appellate_courts, agenda_setter,
    institutional, generational, constrained, national).

% Decide which homicides to charge capitally, seek death sentences at trial, and negotiate pleas against the shadow of a possible execution. The availability of the penalty gives the office bargaining power that a life-sentence-only regime does not; the office collects that leverage in ordinary charging practice, whether or not an execution ever follows.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, prosecutorial_offices, agenda_setter,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__deterrence_instrument, prosecutorial_offices, beneficiary).

% Live for years under a death sentence through mandatory review, and are executed if review fails. Their death is the mechanism by which the arrangement's preventive promise is supposed to operate. They have no exit short of successful appeal, the avenues of appeal have been narrowed by federal statute, and they are drawn from a pool skewed by poverty and race relative to the population who commit death-eligible crimes.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, biographical, trapped, national).

% Are sentenced to death for crimes they did not commit and learn their position in this arrangement only when evidence surfaces years later. The arrangement's cost-benefit framing counts their risk as one term in 'acceptable cost'; they bear the whole of the error while others do the counting. Post-conviction DNA testing and investigative work have exonerated enough of them to sustain standing innocence projects.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, wrongly_convicted_defendants, payer,
    powerless, biographical, trapped, national).

% Are the people the arrangement claims to protect: potential murder victims whose deaths, on the preventive hypothesis, executions avert. They exist as a statistical class with no organization of their own; no one can identify in advance whose life a given execution saves. Organizations of past victims' families speak in their name but occupy a different seat.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, future_potential_victims, beneficiary,
    powerless, generational, constrained, national).

% Governors, legislators, and candidates who collect electoral support from defending or expanding the penalty and spend political capital when they commute, impose moratoria, or repeal. Their gain is positional — appearing resolute against the worst crimes — and the executions themselves are carried out by the correctional apparatus, not by them.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, elected_officials, beneficiary,
    institutional, biographical, mobile, national).

% Organize to end the penalty on categorical grounds: that the state may not kill a captive person regardless of what that person did or what the killing might purchase. Inside the arrangement's own cost-benefit frame their claim has no slot — the frame prices lives in efficacy terms and cannot register an objection priced at infinity. They work legislatures, courts, and public opinion from outside the frame, and cannot accept the frame's terms without dissolving their position.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolition_advocates, excluded,
    organized, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, prosecutorial_offices).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gates the state's ultimate coercive act on a publicly adjudicable evidentiary test: the rule converts an act of sovereign violence into a conditional policy instrument whose validity must be continuously re-earned by demonstrated preventive benefit. If the preventive hypothesis holds, it also coordinates public-safety policy — executions become the mechanism by which the state purchases future lives at a stated price.
% TRANSFER_FUNCTION: Moves life from condemned persons to the state's punitive apparatus, and — on the preventive hypothesis — moves safety from that apparatus to the future-potential-victim class. It also moves plea-bargaining leverage to prosecutorial offices and electoral capital to elected officials, both collected whether or not any preventive effect exists.
% ABSENT_VOICES: The wrongly convicted cannot speak — the seat is occupied by people who do not yet know they occupy it, so the 'acceptable cost' term is priced without the voice of those who pay it. Future murder victims of preventive failures (if the hypothesis is false) are equally absent and uncounted. The condemned speak only through counsel, filtered into procedural arguments the framework can process; their objection to being used as instruments has no slot in a cost-benefit frame.
% DISAPPEARANCE_RATIONALE: Abolition-side seats hold that the world would rearrange only trivially: life imprisonment without parole already incapacitates, the fiscal and wrongful-execution costs would end, and no reliable evidence ties executions to murder rates. Retention-side seats hold that the world would rearrange lethally: the preventive effect, on their reading of the record, is real and its loss would surface as additional murders. The empirical question on which the two verdicts diverge is exactly the one this reading's condition turns on, and it is unresolved — so the parties dispute which world we live in.
% FOUNDING_PROBLEM: Reconcile the state's exercise of lethal force with a political order that grounds legitimacy in citizen welfare rather than sovereign right or divine sanction: the deterrence reading was built to make each killing admissible only as a demonstrated purchase of public safety, replacing 'the murderer deserves death' with 'the killing prevents deaths.'
% FOUNDING_PROBLEM_CORROBORATION: Beccaria's 1764 Essay on Crimes and Punishments and the utilitarian tradition attest the framing from outside the benefiting parties — punishment admissible only by social utility — while drawing the opposite conclusion (Beccaria argued permanent servitude deters more than death). The National Research Council's 2012 committee report attests from outside that the evidentiary condition remains undemonstrated. Retentionist legislatures attest the problem is live; no source outside the benefiting parties attests that the condition is actually met.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, contested).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.78) because the arrangement's operation takes the condemned person's life as the means to a benefit others receive, and the preventive premise on which the trade is justified has never been demonstrated: four decades of deterrence research culminated in the NRC's 2012 finding of inconclusiveness, so the arrangement continues to collect its cost while its claimed product remains unverified. Suppression is higher still (0.80) because the condemned's position is custodial and terminal — no exit exists short of successful appeal, and the appeal avenues themselves were narrowed by federal statute (AEDPA 1996); the enforcement machinery is the arrangement's operating cost. Theater is moderate (0.45): the machinery is real and people die, but a growing share of the arrangement's activity is symbolic and political — legislative findings recited rather than re-examined, deterrence asserted in floor speeches while the research program stagnated — and the ratio rises across the interval as the evidentiary content thins even as execution counts fall. Accessibility collapse is mid-range (0.50): the alternative (life without parole) is live, constitutionally unproblematic, and cheaper, so alternatives do not fully collapse — but within the reading's own logic, if the preventive premise holds, the alternative is strictly inferior, which is how the arrangement forecloses its substitute. Resistance is high (0.70): sustained litigation, innocence-driven moratoria, repeal waves across a quarter of the states, and international pressure. Coalition potential for the powerless victim seats is deliberately fragmented: capital litigation proceeds case-by-case, custody isolates the condemned, and the wrongly convicted cannot self-identify as a class — the arrangement's procedural individualization is itself what suppresses victim-side coalition formation. The measurement series share one grid (t = 0, 9, 18, 27, 36, 45, 54 over 1972–2026) so no metric is sampled against another metric's scalar. Receipt surface: the gains demonstrably accrue to prosecutorial offices — plea leverage collected from the penalty's availability independent of any execution — with elected officials collecting a secondary positional gain; fixing is prohibitive for the seat that could fix it, because a legislature that repeals spends electoral capital against a measurable pro-penalty bloc for a benefit distributed diffusely, which is why repeal takes decades per state even though the fix is legislatively simple.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently, and the beneficiary seat differently from both. From the condemned person's position the arrangement is terminal custody ending in state killing — the full cost, none of the benefit. From the prosecutorial position it is a charging option that wins pleas. From the future-potential-victim position it is protection whose very existence is the empirical question. From the appellate-court position it is an administrable conditional whose evidentiary premise the court can acknowledge as inconclusive without disturbing the practice — the interpretation layer absorbs the drift. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Condemned persons and the wrongly convicted sit at the full-target end (d near 1.0): declared victims, custodially trapped, no exit. Future potential victims sit near the beneficiary end (d near 0.1): declared beneficiaries of the preventive effect — with the contingency of that benefit carried in the omegas rather than in an override, because overrides key on power_atom and would wrongly move the trapped victim seats that share the powerless atom. Prosecutorial offices derive low d (declared secondary beneficiaries, mobile exit, agenda power): they collect whether or not the preventive effect exists. Elected officials derive low-moderate d (beneficiary, mobile). Legislatures and courts derive near-symmetric d: they administer the arrangement and bear its political and institutional costs from both directions. No directionality overrides are authored: the structural derivation from beneficiary/victim data plus exit options already differentiates the seats, and the powerless atom is shared by seats at opposite ends of d — exactly the case the derivation chain handles and a power-atom override would corrupt.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is keeping the coordination story honest. If the preventive premise is true and material, the arrangement is a tangled rope: a real collective-action function (purchasing public safety) that genuinely extracts — the condemned pays with life for a benefit distributed to others. If the premise is false, the coordination story is cover and the arrangement is maintained by political demand and institutional inertia — the conditional_gate_integrity omega is the tripwire for that boundary. The founding problem is recorded as contested and the disappearance verdict as contested, so the mismatch consumer finds no dead-problem-plus-rearranging-world flag: the arrangement's problem is live precisely because the parties dispute whether the instrument solves it. Mandatrophy is not declared — the arrangement has not outlived its mandate; it never demonstrably had one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the kernel state_killing_authority — what would the sibling readings (retributive_desert, categorical_abolition) change structurally if instantiated instead?',
    'Reading selection is not resolvable inside this story; it is resolved at the kernel level by which axioms a party adopts. The corpus models each reading as a separate constraint file and compares classifications across the family.',
    'Under retributive_desert, future_potential_victims leave the beneficiary set, the condemned''s death is owed rather than an instrumental cost, and the ε referent shifts from an efficacy-conditional arrangement to a desert-owed one. Under categorical_abolition, there are no beneficiaries and the condemned is a rights-holder; the arrangement''s classification would be computed against a rights-violation structure rather than a conditional-benefit one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer structure: which reading of the state-killing kernel this constraint instantiates and what the siblings would change.').

omega_variable(
    deterrence_efficacy_inconclusiveness,
    'Does capital punishment actually prevent future murders relative to the operative alternative (life imprisonment without parole)?',
    'The NRC 2012 committee concluded the existing literature cannot identify a deterrent effect with confidence separating policy effects from other factors; resolution would require research designs the observational record has not supported in four decades of attempt.',
    'If the preventive effect is zero or negative, the coordination function is cover and the arrangement computes toward snare; if it is real and material, the coordination function is genuine and the extraction is, by this reading''s own lights, a justified price. The reading''s validity rides on this omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_efficacy_inconclusiveness, empirical, 'The load-bearing empirical premise: whether executions deter at all.').

omega_variable(
    acceptable_cost_pricing,
    'What prices into ''acceptable cost'' — wrongful-execution risk, fiscal cost, method suffering, brutalization effects — and who has standing to set the threshold?',
    'Comparative analysis of death-penalty versus life-without-parole regimes on wrongful-conviction rates, homicide trajectories, and fiscal cost, combined with democratic-theoretic analysis of pricing authority; the wrongly convicted seat prices the error at infinity but holds no vote in the pricing.',
    'A higher-priced cost term raises the arrangement''s measured extractiveness per claimed unit of prevention and pushes classification toward snare; a lower-priced one supports the conditional-justification structure. The standing question determines whose costs are even counted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_pricing, conceptual, 'The ''acceptable cost'' term is underspecified: its contents and the authority to set it are both open.').

omega_variable(
    conditional_gate_integrity,
    'Is the ''if and only if'' condition actually operative — would jurisdictions abandon the penalty if the preventive evidence turned decisively negative, or has the arrangement become evidence-insensitive?',
    'Track retention and repeal decisions against the evidence record: post-2012 repeal waves in some states versus retention without re-examination in others; compare jurisdictions that cite deterrence findings with those that never re-open the question.',
    'If the condition is operative, the arrangement is a genuine evidence-gated instrument whose classification follows the evidence; if evidence-insensitive, the deterrence rationale is cover for extraction maintained by political demand — the snare boundary. This omega governs whether the tangled_rope claim survives contact with behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_gate_integrity, empirical, 'Whether the conditional justification actually gates the practice or is asserted post hoc.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t9, state_killing_authority__deterrence_instrument, theater_ratio, 9, 0.28).
narrative_ontology:measurement(stat_tr_t18, state_killing_authority__deterrence_instrument, theater_ratio, 18, 0.33).
narrative_ontology:measurement(stat_tr_t27, state_killing_authority__deterrence_instrument, theater_ratio, 27, 0.38).
narrative_ontology:measurement(stat_tr_t36, state_killing_authority__deterrence_instrument, theater_ratio, 36, 0.42).
narrative_ontology:measurement(stat_tr_t45, state_killing_authority__deterrence_instrument, theater_ratio, 45, 0.44).
narrative_ontology:measurement(stat_tr_t54, state_killing_authority__deterrence_instrument, theater_ratio, 54, 0.45).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t9, state_killing_authority__deterrence_instrument, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(stat_be_t18, state_killing_authority__deterrence_instrument, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(stat_be_t27, state_killing_authority__deterrence_instrument, base_extractiveness, 27, 0.71).
narrative_ontology:measurement(stat_be_t36, state_killing_authority__deterrence_instrument, base_extractiveness, 36, 0.75).
narrative_ontology:measurement(stat_be_t45, state_killing_authority__deterrence_instrument, base_extractiveness, 45, 0.77).
narrative_ontology:measurement(stat_be_t54, state_killing_authority__deterrence_instrument, base_extractiveness, 54, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t9, state_killing_authority__deterrence_instrument, suppression_requirement, 9, 0.62).
narrative_ontology:measurement(stat_su_t18, state_killing_authority__deterrence_instrument, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(stat_su_t27, state_killing_authority__deterrence_instrument, suppression_requirement, 27, 0.76).
narrative_ontology:measurement(stat_su_t36, state_killing_authority__deterrence_instrument, suppression_requirement, 36, 0.79).
narrative_ontology:measurement(stat_su_t45, state_killing_authority__deterrence_instrument, suppression_requirement, 45, 0.8).
narrative_ontology:measurement(stat_su_t54, state_killing_authority__deterrence_instrument, suppression_requirement, 54, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% The colloquial label 'the capital punishment debate' conflates three structurally distinct constraints sharing one kernel (state killing authority). This file instantiates the deterrence_instrument reading only. The retributive reading has a different beneficiary structure (the desert doctrine is vindicated; future potential victims are not beneficiaries) and the abolition reading has no beneficiaries at all and treats the condemned as a rights-holder rather than an instrumental cost. The upstream empirical claim — the deterrence hypothesis — is load-bearing only within this reading and is what the siblings do not share; decomposition follows the ε-invariance principle: one reading, one constraint, one ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
