% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Capital Punishment as Deterrence Instrument
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story models the deterrence-instrument reading of the
 *   state killing authority kernel: capital punishment is justified if and
 *   only if it prevents future murders at acceptable cost. The arrangement
 *   under contest is the standing practice of state-sanctioned execution in
 *   retentionist jurisdictions, assessed by the deterrence reading's own
 *   lights — i.e., does this practice actually deter at acceptable cost? The
 *   condemned person is the instrumental cost; future potential murder
 *   victims are the claimed beneficiaries; the state's authority to kill is
 *   grounded in consequentialist crime-prevention efficacy rather than
 *   retributive desert or inherent rights. Over the 40-year interval (roughly
 *   1976–present, post-Gregg v. Georgia), the constraint has accumulated
 *   extraction (rising base_extractiveness) as the deterrence evidence base
 *   has weakened while the machinery of death penalty administration has
 *   expanded, and suppression has hardened as procedural barriers to
 *   execution have been erected to manage constitutional challenges. The
 *   theater ratio has risen as 'deterrence' becomes an increasingly
 *   performative justification for a machinery that persists for
 *   institutional and political reasons.
 *
 * KEY AGENTS:
 *   - condemned_persons: Primary target (powerless/identity_locked) — bears the ultimate extraction (life)
 *   - future_potential_victims: Primary beneficiary (powerless/trapped) — claimed beneficiaries of deterrence, cannot consent or exit
 *   - state_prosecutorial_authority: Agenda setter (institutional/arbitrage) — administers the machinery, collects political capital
 *   - law_enforcement_agencies: Beneficiary (organized/constrained) — gains investigative leverage, resource allocation
 *   - families_of_condemned: Victim (powerless/trapped) — bears collateral extraction, no voice in calibration
 *   - wrongfully_convicted: Victim (powerless/trapped) — bears the cost of systemic error, excluded from cost calculus
 *   - abolitionist_advocates: Excluded (organized/mobile) — would object to the arrangement's existence
 *   - retributive_desert_theorists: Observer (analytical/analytical) — holds a sibling reading of the same kernel
 *   - deterrence_criminologists: Observer (analytical/analytical) — empirical seat on the deterrence question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.82).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.91).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.82).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment as Deterrence Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '53d7223e-4c27-4c80-a1a5-fcb7ef072a7f').
narrative_ontology:cs_kernel_codification('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', formalized).
narrative_ontology:cs_authority_grounding('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', extraction).
narrative_ontology:cs_interpretation_layer_present('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f').
narrative_ontology:cs_reading_relation('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_axiom('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', foundational, deterrence_justifies_state_killing).
narrative_ontology:cs_axiom_status(deterrence_justifies_state_killing, holdable).
narrative_ontology:cs_axiom_grounding('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', deterrence_justifies_state_killing, empirically_contingent).
narrative_ontology:cs_axiom('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', foundational, acceptable_cost_threshold_is_calibratable).
narrative_ontology:cs_axiom_status(acceptable_cost_threshold_is_calibratable, holdable).
narrative_ontology:cs_axiom_grounding('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', acceptable_cost_threshold_is_calibratable, instrumental).
narrative_ontology:cs_reference_frame('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', post_gregg_deterrence_calibration).
narrative_ontology:cs_drift_state('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('53d7223e-4c27-4c80-a1a5-fcb7ef072a7f', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, state_prosecutorial_authority).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, law_enforcement_agencies).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, families_of_condemned).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, wrongfully_convicted).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death under the deterrence-instrument arrangement. Their life is the instrumental cost paid for the claimed deterrence benefit. They have no exit from the constraint once sentenced; appeals are procedural delays within the machinery, not exits. The constraint extracts their entire future.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, national).

% The class of persons whose lives the deterrence claim says are saved by the threat of execution. They cannot consent to this protection, cannot opt out of being 'beneficiaries,' and have no way to verify whether the protection is real. Their beneficiary status is assigned by the arrangement's logic, not chosen.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, future_potential_victims, beneficiary,
    powerless, biographical, trapped, national).

% Controls the machinery: decides when to seek death, negotiates pleas, manages the political capital of 'tough on crime' positioning. Collects the primary institutional benefit (conviction leverage, political resource). Can decline to use the constraint in any given case — has arbitrage-grade exit from the extraction side while controlling its deployment.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_prosecutorial_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains investigative leverage (death-eligible charges as plea bargaining chips), resource allocation (capital case units, specialized prosecutors), and institutional prestige. Constrained exit: the institution is committed to the machinery but individual actors can transfer or advocate reform.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, law_enforcement_agencies, beneficiary,
    organized, biographical, constrained, national).

% Bears collateral extraction: the killing of their kin, the prolonged uncertainty of appeals, the social stigma, the financial burden of legal defense. No voice in the 'acceptable cost' calculus; their costs are not counted in the deterrence accounting.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, families_of_condemned, payer,
    powerless, biographical, trapped, national).

% The subset of condemned persons who are factually innocent. Bears the ultimate unpriced cost: execution for a crime they did not commit. Their existence is the empirical falsifier of the 'acceptable cost' threshold, yet they are structurally excluded from the cost calculus. No exit once convicted.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, wrongfully_convicted, payer,
    powerless, immediate, trapped, national).

% Would object to the arrangement's existence on categorical grounds. Their exclusion from the calibration of 'acceptable cost' is structural: the deterrence reading only counts costs internal to its consequentialist framework (deterrence failure, fiscal cost), not the categorical moral cost abolitionists assert.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% Holds the sibling reading (retributive_desert) of the same kernel. Sees the deterrence reading as a category error: desert, not consequences, grounds the authority to kill. Their seat is analytical — they do not collect or pay under this constraint but their reading competes for the kernel's legitimacy.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, retributive_desert_theorists, observer,
    analytical, civilizational, analytical, universal).

% Empirical seat on the deterrence question. Their research (panel studies, natural experiments, meta-analyses) produces the evidence base that the 'acceptable cost' threshold claims to respect. Their findings (mostly null or marginal deterrence effects) are the primary challenge to the constraint's coordination function.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, deterrence_criminologists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, state_prosecutorial_authority).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a consequentialist justification for state killing: if killing some deters others from killing, the net lives saved validates the arrangement. Solves the legitimacy problem of state killing by grounding it in crime prevention rather than retribution or inherent authority.
% TRANSFER_FUNCTION: Moves the lives of condemned persons (and collateral costs to their families, risk of wrongful execution) to the credit of future potential victims (claimed lives saved) and institutional beneficiaries (prosecutorial leverage, law enforcement resources, political capital). The transfer is mediated by the 'acceptable cost' threshold — a calculus controlled by the benefiting authorities.
% ABSENT_VOICES: The wrongfully convicted (who cannot speak post-execution), future potential victims (who cannot consent to being 'protected' by killing), and abolitionist advocates (whose categorical objection is excluded from the consequentialist framework). Also absent: the deterrence criminologists whose null findings are systematically discounted in the 'acceptable cost' calibration.
% DISAPPEARANCE_RATIONALE: If the deterrence-instrument justification vanished overnight, retentionist jurisdictions would either abandon capital punishment (revealing the deterrence claim was load-bearing) or fall back on retributive_desert (revealing the deterrence reading was a cover for a different justification). The machinery of death rows, execution protocols, and capital appeals would face immediate legitimacy crisis. The world rearranges because the constraint's claimed coordination function is its primary public legitimation.
% FOUNDING_PROBLEM: Post-Furman (1972) constitutional crisis: the Supreme Court struck down existing death penalty statutes as arbitrary and capricious. The deterrence-instrument reading offered a reconstitution: if capital punishment is calibrated to deter murder at acceptable cost, it satisfies the Eighth Amendment's proportionality requirement and the Fourteenth's due process requirement. The founding problem was legitimating state killing under a constitutional framework that rejected arbitrary authority.
% FOUNDING_PROBLEM_CORROBORATION: The deterrence-instrument reading's founding problem is attested by the Joint Opinion in Gregg v. Georgia (1976) and subsequent deterrence-justification jurisprudence. However, the corroboration from OUTSIDE the benefiting parties is mixed: the National Research Council (2012) concluded deterrence research is not informative; the American Law Institute withdrew its death penalty framework (2009) citing irreparable structural defects; multiple state-level abolition commissions found the deterrence justification unsupported. No independent body affirms the founding problem remains live on the deterrence reading's own terms.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.82) is high because the arrangement takes a life (the condemned person's) as an instrumental cost for a claimed deterrence benefit that is empirically contested and likely marginal or zero. The 'acceptable cost' threshold is defined by the benefiting authorities themselves (prosecutors, legislators) with no independent calibration — this self-calibration is an extraction amplifier. Suppression (0.91) is very high because the constraint's persistence depends on actively suppressing alternatives: abolition, moratoria, life-without-parole substitution, and empirical challenge to the deterrence claim. The condemned person has zero exit (identity_locked/trapped); future potential victims are trapped in the beneficiary role (cannot opt out of being 'protected'); prosecutorial authorities have arbitrage-grade exit (can decline to seek death, can plea-bargain). Theater ratio (0.28) is moderate: the deterrence justification is increasingly performative as the evidence base erodes, but the machinery (death rows, execution protocols, appellate review) has real operational costs that are not purely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   From the prosecutor's seat (agenda_setter, institutional, arbitrage exit), the constraint appears as genuine coordination: a calibrated tool that deters at acceptable cost. From the condemned person's seat (victim, powerless, trapped), it is pure extraction: their life is taken for a benefit they do not receive and a deterrence claim that may be false. From the future potential victim's seat (beneficiary, powerless, trapped), it is a coordination benefit they cannot refuse and did not consent to. The engine computes these seat divergences from the structural data — the claimed_type (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally present, not that they are balanced.
 *
 * DIRECTIONALITY LOGIC:
 *   The deterrence reading positions future potential victims as beneficiaries (d → low, constraint subsidizes them via claimed deterrence). Condemned persons are the instrumental cost — full targets (d → 1.0, constraint extracts their lives). Prosecutorial authorities are agenda-setters who benefit politically and institutionally (d → 0.15, near beneficiary end). Law enforcement agencies benefit from leverage and resources (d → 0.25). Families of condemned and wrongfully convicted bear diffuse unpriced costs (d → 0.85). The derivation chain from beneficiary/victim declarations + power + exit produces these directionalities; no overrides are needed because the structural positions are cleanly differentiated by exit options and power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deterring murder at acceptable cost) is contested: deterrence criminologists and abolitionists argue the problem is either unsolved by this means (deterrence fails) or solved by alternatives (life without parole). The arrangement persists despite the contested founding problem because the machinery serves institutional interests (prosecutorial leverage, political capital, correctional employment) that are distinct from the stated deterrence function. This is mandatrophy: the mandate (deterrence) has atrophied relative to the arrangement's persistence, but the constraint is not a piton because active enforcement is required and concentrated beneficiaries (prosecutorial authorities) exist. The 'acceptable cost' condition functions as a moving goalpost — as evidence against deterrence accumulates, the cost threshold is implicitly raised rather than the arrangement abandoned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_effect_magnitude,
    'What is the actual marginal deterrent effect of capital punishment over life imprisonment without parole?',
    'Natural experiments from abolition/reinstatement cycles, cross-jurisdiction panel studies with adequate controls for confounding variables, or randomized policy experiments where ethically feasible.',
    'If the marginal deterrent effect is zero or statistically indistinguishable from zero, the constraint''s claimed coordination function collapses and it reclassifies as snare (pure extraction of condemned lives without deterrence benefit). If positive but below the ''acceptable cost'' threshold, it remains tangled_rope but with contested calibration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_effect_magnitude, empirical, 'Whether the deterrence benefit claimed by the reading actually exists at the magnitude required to justify the extraction.').

omega_variable(
    acceptable_cost_threshold_definition,
    'Who defines and how is calculated the ''acceptable cost'' threshold for state killing — including error rates, racial disparities, and fiscal expenditure?',
    'Legislative record analysis, judicial opinions on proportionality, cost-benefit frameworks in correctional policy, and democratic deliberation records.',
    'If the threshold is undefined or defined solely by the benefiting authorities (prosecutors, legislators), the constraint extracts without accountable calibration — strengthening snare classification. If an independent body sets and enforces the threshold, coordination function is more credible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_threshold_definition, conceptual, 'Whether the ''acceptable cost'' condition has determinate content or functions as a blank check for the benefiting authorities.').

omega_variable(
    reading_of_kernel_state_killing_authority,
    'This constraint is the deterrence_instrument reading of the state_killing_authority kernel. How does the kernel''s structural ambiguity across readings affect this constraint''s classification?',
    'Compare classification outputs across the three declared readings (deterrence_instrument, retributive_desert, categorical_abolition) holding the same referent arrangement. Divergence in computed type signals that the kernel''s reading under-determines the constraint''s structure.',
    'If deterrence_instrument computes as tangled_rope while categorical_abolition computes as mountain (from the abolitionist seat) and retributive_desert computes as rope (from the desert-theorist seat), the kernel''s reading is a decisive structural variable — not merely interpretive. This validates the ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_kernel_state_killing_authority, conceptual, 'Kernel-reading structural delta: this reading instantiates a deterrence-calibrated constraint with future victims as beneficiaries and condemned persons as instrumental costs; sibling readings instantiate different constraints from the same kernel.').

omega_variable(
    wrongful_execution_rate_uncertainty,
    'What is the true rate of wrongful conviction in capital cases, and how does it factor into the ''acceptable cost'' calculation?',
    'Post-conviction DNA exoneration data, innocence project caseloads, judicial error-rate estimates, and systematic review of capital case reversals.',
    'A non-trivial wrongful execution rate that is excluded from the cost calculus converts the constraint from tangled_rope (coordination with asymmetric extraction) to snare (extraction with cover story) because the coordination function''s own terms — ''acceptable cost'' — are violated by uncounted costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_rate_uncertainty, empirical, 'Uncounted costs in the deterrence calculus: wrongful executions as unpriced extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.12).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__deterrence_instrument, theater_ratio, 10, 0.18).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__deterrence_instrument, theater_ratio, 20, 0.22).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__deterrence_instrument, theater_ratio, 30, 0.26).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__deterrence_instrument, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__deterrence_instrument, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__deterrence_instrument, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__deterrence_instrument, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__deterrence_instrument, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__deterrence_instrument, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__deterrence_instrument, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__deterrence_instrument, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__deterrence_instrument, suppression_requirement, 40, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__deterrence_instrument, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, penal_proportionality_doctrine).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, due_process_death_penalty_jurisprudence).

% DUAL FORMULATION NOTE:
% This constraint is one member of the state_killing_authority constraint family (kernel_id: state_killing_authority). The three readings — deterrence_instrument, retributive_desert, categorical_abolition — decompose the kernel's colloquial label into structurally distinct claims with different ε values, beneficiary/victim sets, and classifications. deterrence_instrument: ε≈0.82, tangled_rope (coordination + asymmetric extraction). retributive_desert: ε≈0.45, rope (coordination of desert-proportionality, minimal extraction if desert is accepted). categorical_abolition: ε≈0.95 from the abolitionist seat, mountain (natural law prohibition) or snare (if viewed from the state's seat). The family is linked by network.affects_constraints edges; the upstream empirical claim (deterrence effect) influences the downstream desert and abolition claims through the legitimacy conditions of state killing authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
