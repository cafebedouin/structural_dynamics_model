% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: Capital Punishment under Proportional Desert (Retributive Reading of the State-Killing Legitimacy Kernel)
 *   domain: criminal justice/political philosophy/legal theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the state_killing_legitimacy
 *   kernel: the retributive reading, under which a murderer forfeits the
 *   life-right through proportional desert (lex talionis). The standing
 *   arrangement under contest — and the sole referent of epsilon — is the
 *   state's execution of convicted murderers on desert-proportionality
 *   grounds, assessed by this reading's own lights: the reading holds the
 *   taking justified, and simultaneously the cost borne is real, total, and
 *   terminal. The kernel decomposes into three structurally distinct
 *   constraints (this file, the deterrence reading, the abolition reading),
 *   each with its own stable epsilon and victim set; per the
 *   epsilon-invariance principle they are separate stories linked through
 *   network.affects_constraints, not one constraint with a measurement
 *   parameter. The claim/metrics gap is deliberate: the arrangement is
 *   CLAIMED as tangled_rope (a genuine desert-vindication coordination
 *   function carrying asymmetric, life-priced extraction through the same
 *   structure) while the metrics are authored descriptively — high
 *   extractiveness, actively maintained suppression, rising theatricality as
 *   use declines. The engine computes per-seat classifications from the
 *   structural data; this claim does not adjudicate them.
 *
 * KEY AGENTS:
 *   - - state_execution_authority: Agenda-setter (institutional/arbitrage) — administers sentencing and execution; could commute, moratorium, or repeal at will
 *   - - capital_case_prosecutors: Concentrated beneficiary (organized/mobile) — select capital charges, accrue career and electoral gains
 *   - - surviving_victims_families: Beneficiary (moderate/constrained) — receive expressive vindication and the promised terminal remedy
 *   - - law_abiding_citizens: Diffuse beneficiary/payer (organized/constrained) — receive moral-order assurance, fund the apparatus, carry wrongful-conviction exposure
 *   - - condemned_murder_offenders: Primary target (powerless/trapped) — bear the ultimate cost under the desert verdict
 *   - - wrongfully_convicted_capital_defendants: Collateral target (powerless/trapped) — bear the identical cost without the desert that legitimizes it
 *   - - abolition_advocates: Excluded (organized/mobile) — dignity-based objection kept outside the sentencing framework
 *   - - legal_philosophy_analysts: Analytical observer (analytical/analytical) — sees the full kernel structure and all sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.88).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.75).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Capital Punishment under Proportional Desert (Retributive Reading of the State-Killing Legitimacy Kernel)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal justice/political philosophy/legal theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '25f627fc-cd95-4619-89e1-714ed34f8a1e').
narrative_ontology:cs_kernel_codification('25f627fc-cd95-4619-89e1-714ed34f8a1e', fixed_text).
narrative_ontology:cs_authority_grounding('25f627fc-cd95-4619-89e1-714ed34f8a1e', lineage).
narrative_ontology:cs_interpretation_layer_present('25f627fc-cd95-4619-89e1-714ed34f8a1e').
narrative_ontology:cs_reading_relation('25f627fc-cd95-4619-89e1-714ed34f8a1e', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('25f627fc-cd95-4619-89e1-714ed34f8a1e', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('25f627fc-cd95-4619-89e1-714ed34f8a1e', foundational, aggravated_murder_desert_forfeits_life_right).
narrative_ontology:cs_axiom_status(aggravated_murder_desert_forfeits_life_right, holdable).
narrative_ontology:cs_axiom_grounding('25f627fc-cd95-4619-89e1-714ed34f8a1e', aggravated_murder_desert_forfeits_life_right, deontological).
narrative_ontology:cs_axiom('25f627fc-cd95-4619-89e1-714ed34f8a1e', secondary, punishment_commensurate_with_crime_required).
narrative_ontology:cs_axiom_status(punishment_commensurate_with_crime_required, holdable).
narrative_ontology:cs_axiom_grounding('25f627fc-cd95-4619-89e1-714ed34f8a1e', punishment_commensurate_with_crime_required, deontological).
narrative_ontology:cs_reference_frame('25f627fc-cd95-4619-89e1-714ed34f8a1e', proportional_desert_regime).
narrative_ontology:cs_drift_state('25f627fc-cd95-4619-89e1-714ed34f8a1e', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25f627fc-cd95-4619-89e1-714ed34f8a1e', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, surviving_victims_families).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, capital_case_prosecutors).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, law_abiding_citizens).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_murder_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, wrongfully_convicted_capital_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, law_abiding_citizens).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, lex_talionis_proportionality_doctrine).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, moral_desert_forfeiture_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and administers the capital statutes: charges, tries, sentences, houses the condemned on death row, and carries out executions through court-supervised protocol. Claims a sovereign duty to render a proportionate answer to aggravated murder. Holds commutation, moratorium, and repeal as permanently available options, so leaving the arrangement is always open to it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_execution_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Decide which murders to charge capitally and argue for death at sentencing. Capital cases confer visibility, promotion, and elective office; the arrangement supplies their gravest professional instrument. They move between offices and into politics freely.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, capital_case_prosecutors, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, capital_case_prosecutors, agenda_setter).

% Lost a member to murder. Attend trials and executions under victims'-rights provisions and report vindication, closure, or — in longitudinal accounts — renewed grief. They cannot exit the loss itself; their participation in the process is bounded by prosecutors' charging choices and court schedules.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, surviving_victims_families, beneficiary,
    moderate, biographical, constrained, national).

% Receive the assurance that the worst crimes meet a proportionate public answer, and pay for the apparatus through taxation and jury service. Any random member also carries a small personal exposure to wrongful capital conviction. Their voice reaches the arrangement mainly through referenda and legislators.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, law_abiding_citizens, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, law_abiding_citizens, payer).

% Convicted of capital murder and sentenced to death. Spend years in isolation-heavy death-row confinement awaiting execution or appellate reversal. Once the sentence is final, no choice, purchase, relocation, or reform removes them from the process; clemency rests entirely with the executive.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, condemned_murder_offenders, payer,
    powerless, immediate, trapped, national).

% Convicted of capital crimes they did not commit — surfaced by DNA exonerations and error-rate studies. They pass through the identical confinement and face the identical terminal outcome, without the culpability the arrangement's legitimacy rests on. Many learn their status only after decades, or posthumously.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, wrongfully_convicted_capital_defendants, payer,
    powerless, immediate, trapped, national).

% Organize against capital punishment on dignity grounds: litigators, religious bodies, human-rights organizations, and repeal legislators. They stand outside the sentencing framework that decides individual cases, lobbying, litigating procedure, and contesting the arrangement's premises in public.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolition_advocates, excluded,
    organized, generational, mobile, continental).

% Scholars of criminal law and moral philosophy who map the desert, deterrence, and dignity arguments and the institutions that carry them. They bear no cost and collect no benefit; their output shapes neither individual sentences nor statutes directly.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, legal_philosophy_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, capital_case_prosecutors).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a terminal, rule-bound, proportionate state answer to aggravated murder: it closes the desert ledger for the gravest crime through a single authorized public act instead of private vengeance, and concentrates communal condemnation into one procedurally regulated event.
% TRANSFER_FUNCTION: Moves the condemned offender's remaining life — years of death-row confinement ending in execution — into the state's punitive process; moves expressive vindication, closure claims, and political credit outward to surviving families, the law-abiding public, and prosecuting officials.
% ABSENT_VOICES: The condemned, after final judgment, have no further voice in the arrangement that ends them. Death-qualification voir dire screens categorical objectors off capital juries, so abolition-minded citizens are excluded at the point of decision. The wrongfully convicted speak only posthumously, through exoneration records. Deterrence theorists and dignity philosophers deliberate outside the courtroom where the reading operates case by case.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, capital statutes, death rows, execution protocols, and the surrounding litigation economy would dismantle; hundreds of condemned prisoners would face resentencing to life terms; surviving families promised a terminal remedy would lose it; prosecutors would lose their gravest charging instrument; and the international human-rights architecture would register the change immediately. Nothing about the arrangement is self-maintaining — every part is administered.
% FOUNDING_PROBLEM: In societies without a state monopoly on lethal response, murder was answered by private vengeance and blood feud, escalating without limit. Lex talionis bounded retaliation to strict proportionality, and the later state absorbed the retaliatory function outright — honoring desert while ending the feud cycle.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians of the Code of Hammurabi, Mosaic law, and the medieval peace movements corroborate the anti-feud origin from outside the benefiting parties, and Beccaria's 1764 critique already attested that feud suppression had been superseded by the state's monopoly on force. Retentionist jurisdictions attest the desert problem remains live; abolitionist jurists and the international human-rights system attest it is dead as a warrant for killing. No neutral attestation exists — the corroboration itself splits along the kernel's readings.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.88 because the sanctioned transfer is the offender's life — the maximal good a legal process can take — and the wrongfully convicted subclass bears that same transfer with no desert behind it. Suppression is 0.75 and is authored as a raw structural property, unscaled by power or scope: persistence depends on active machinery (death-qualified juries that screen out categorical objectors, layered capital litigation, method-secrecy statutes, executive clemency discipline), not on voluntary participant preference. Theater is 0.32 and rising: executions have fallen far faster than death sentencing, so in low-use jurisdictions the retained apparatus performs finality more often than it produces it — the 'death row phenomenon' of sentences that are never carried out. Accessibility_collapse is 0.30 because alternatives remain fully live: life without parole operates in every peer jurisdiction, and a dozen democracies abolished the practice outright while maintaining order — understanding this arrangement does not close its alternatives. Resistance is 0.65: abolition movements, international treaty pressure, exoneration-driven moratoria, and declining jury imposition all press against it continuously. Coalition check: the condemned are the classic candidates for coalition power among the powerless, but post-final-judgment isolation, solitary confinement regimes, and staggered timelines destroy coalition capacity; the wrongfully convicted typically discover their position too late to organize. All three metric series run on one shared six-point grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically differently. From the condemned seat the arrangement is a total, terminal taking with zero exit — the fullest target position the structural data can express. From the state seat it is a justice function its administrator could exit tomorrow by signature. From the prosecutor seat it is a career asset. From the survivors' seat it is a real but uncontrollable expressive good. Same statute, same execution chamber — four incompatible lived types. The engine derives this divergence from power, exit, and role data; the authored claim stays silent on which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned offenders and wrongfully convicted defendants derive d near 1.0 (full targets): they bear the entire cost, and trapped exit pushes them to the full-target end. Surviving families derive low d (beneficiary): they receive expressive vindication and pay nothing they control. Law-abiding citizens derive low-to-symmetric d: genuine assurance benefit, but tax funding and diffuse wrongful-conviction exposure pull them off the pure-beneficiary pole. Prosecutors derive low d with concentrated gains — the clearest capture-shaped seat in the structure. The state authority sits nearest the beneficiary pole (d near 0): designer, administrator, and unconditional exit-holder. National spatial scope modestly amplifies effective extraction for the target seats via verification difficulty; the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bounding unlimited private vengeance through proportionality — is genuinely contested: the state's monopoly on force and life-without-parole now deliver feud-free answers, which suggests the original warrant has aged out; yet retentionist publics and survivors attest the desert question as perennially live. Because founding_problem_status is 'contested' rather than 'dead,' the mismatch consumer (status x disappearance_verdict) does not fire the automatic zombie flag — but the pairing routes investigation to exactly the right question: does the retained apparatus serve desert, or inertia dressed as desert? The rising theater series feeds that inquiry directly. The classification guards against both mislabels: calling the arrangement pure extraction erases the desert-vindication function that this reading, many survivors, and retentionist electorates sincerely coordinate around; calling it pure coordination erases the total price borne by the condemned and the documented error class that the desert rationale alone is supposed to legitimize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the retributive reading of the state_killing_legitimacy kernel — how would the victim set and epsilon shift under the sibling readings?',
    'Read alongside the sibling files (state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading): the abolition reading makes every executed person a dignity-violation victim regardless of desert; the deterrence reading recasts the executed as instrumental means to a preventive signal.',
    'Under this reading the condemned enter the victim set as desert-bearers whose forfeiture is held legitimate; under abolition the same deaths are rights violations with maximal epsilon; under deterrence the victim set is instrumentally defined. Valid comparison is file-by-file only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: one kernel, three readings, each its own constraint with its own stable epsilon.').

omega_variable(
    desert_metaphysical_status,
    'Does proportional desert name a mind-independent moral property that life-forfeiture can track, or a socially constructed stance the state enacts?',
    'Metaethical analysis plus cross-cultural convergence testing: if desert attributions converge across independent traditions on life-forfeiture cases, treat the grounding as robust; if attributions track local retributive cultures, treat it as constructed.',
    'If constructed, the arrangement''s legitimacy reduces to enforcement preference and its effective profile trends toward pure extraction with the desert story as cover; if robust, the coordination function is genuine and the hybrid classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_metaphysical_status, conceptual, 'Whether the desert grounding of the forfeiture is real or constructed.').

omega_variable(
    wrongful_execution_base_rate,
    'What fraction of capital convictions are wrongful, and does the error rate stay within any bound the desert rationale can absorb?',
    'Posthumous DNA and non-DNA exoneration studies with error-rate estimation, matched against execution counts over the interval.',
    'A material error rate means the arrangement takes lives lacking the desert that alone legitimizes them under this reading — pushing the effective profile toward pure extraction; a negligible rate stabilizes the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_base_rate, empirical, 'Error rate inside the desert-legitimated class.').

omega_variable(
    survivor_benefit_durability,
    'Do surviving families receive durable benefit from execution, or transient relief that decays?',
    'Longitudinal studies of survivors before and after executions, compared against matched cohorts in non-execution cases.',
    'If the benefit decays quickly, the beneficiary structure thins toward theatrical maintenance and the theater trajectory steepens; if durable, the coordination side of the hybrid is substantiated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivor_benefit_durability, empirical, 'Durability of the expressive benefit flowing to survivors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skl_rr_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(skl_rr_tr_t0, observed).
narrative_ontology:measurement(skl_rr_tr_t10, state_killing_legitimacy__retributive_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(skl_rr_tr_t10, observed).
narrative_ontology:measurement(skl_rr_tr_t20, state_killing_legitimacy__retributive_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(skl_rr_tr_t20, observed).
narrative_ontology:measurement(skl_rr_tr_t30, state_killing_legitimacy__retributive_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(skl_rr_tr_t30, observed).
narrative_ontology:measurement(skl_rr_tr_t40, state_killing_legitimacy__retributive_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(skl_rr_tr_t40, observed).
narrative_ontology:measurement(skl_rr_tr_t50, state_killing_legitimacy__retributive_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(skl_rr_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(skl_rr_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.92).
narrative_ontology:measurement_basis(skl_rr_be_t0, observed).
narrative_ontology:measurement(skl_rr_be_t10, state_killing_legitimacy__retributive_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement_basis(skl_rr_be_t10, observed).
narrative_ontology:measurement(skl_rr_be_t20, state_killing_legitimacy__retributive_reading, base_extractiveness, 20, 0.89).
narrative_ontology:measurement_basis(skl_rr_be_t20, observed).
narrative_ontology:measurement(skl_rr_be_t30, state_killing_legitimacy__retributive_reading, base_extractiveness, 30, 0.88).
narrative_ontology:measurement_basis(skl_rr_be_t30, observed).
narrative_ontology:measurement(skl_rr_be_t40, state_killing_legitimacy__retributive_reading, base_extractiveness, 40, 0.88).
narrative_ontology:measurement_basis(skl_rr_be_t40, observed).
narrative_ontology:measurement(skl_rr_be_t50, state_killing_legitimacy__retributive_reading, base_extractiveness, 50, 0.88).
narrative_ontology:measurement_basis(skl_rr_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(skl_rr_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(skl_rr_su_t0, observed).
narrative_ontology:measurement(skl_rr_su_t10, state_killing_legitimacy__retributive_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(skl_rr_su_t10, observed).
narrative_ontology:measurement(skl_rr_su_t20, state_killing_legitimacy__retributive_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(skl_rr_su_t20, observed).
narrative_ontology:measurement(skl_rr_su_t30, state_killing_legitimacy__retributive_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(skl_rr_su_t30, observed).
narrative_ontology:measurement(skl_rr_su_t40, state_killing_legitimacy__retributive_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(skl_rr_su_t40, observed).
narrative_ontology:measurement(skl_rr_su_t50, state_killing_legitimacy__retributive_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement_basis(skl_rr_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'capital punishment' conflates three structurally distinct constraints instantiating one kernel (state_killing_legitimacy). This retributive reading carries epsilon ~0.88 with victims = condemned offenders as desert-bearers plus the wrongfully convicted; the deterrence reading indexes epsilon to the execution's signal value and defines victims instrumentally; the abolition reading assigns maximal epsilon with every executed person a dignity-violation victim. Each is a separate file with a single stable epsilon per the epsilon-invariance principle; this story links both siblings via affects_constraints, and the upstream desert tradition cited in retentionist statutes is the channel by which this reading influences its siblings' operating environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
