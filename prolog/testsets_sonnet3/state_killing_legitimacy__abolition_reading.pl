% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Capital Punishment as Categorical Dignity Violation (Abolitionist Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This story authors the abolitionist reading of the contested
 *   state-killing-legitimacy kernel: the claim that state execution
 *   categorically violates human dignity irrespective of the desert of the
 *   condemned or the utility (deterrent or otherwise) the killing might
 *   produce. Under this reading the standing arrangement under contest is
 *   capital punishment as currently practiced and defended — measured by the
 *   abolitionist's own lights, ε is authored high because the reading holds
 *   the harm (irreversible termination of a rights-bearer) to be total and
 *   non-fungible with any offsetting benefit, and because the arrangement
 *   persists only through active state enforcement (charging decisions,
 *   appellate defense of sentences, execution machinery) against sustained
 *   resistance. The sibling readings (retributive_reading,
 *   deterrence_reading) are separate constraints with their own ε,
 *   beneficiary/victim structures, and stakeholder surfaces — they are not
 *   described here beyond the kernel_context note and the reading_relations
 *   edges below, per Rule 1's ε-invariance discipline.
 *
 * KEY AGENTS:
 *   - condemned_persons: primary target (powerless/trapped) — bears the categorical dignity violation this reading names
 *   - wrongfully_convicted_death_row_inmates: limiting case (powerless/trapped) — irreversibility with zero remedy
 *   - state_prosecutorial_apparatus: primary beneficiary/agenda_setter (institutional/arbitrage) — administers and defends the killing power
 *   - political_actors_running_on_toughness: secondary beneficiary (powerful/mobile) — collects low-cost severity signaling
 *   - abolitionist_advocates: excluded challenger (organized/constrained) — names the harm but lacks unilateral authority to stop it
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates boundary cases without adopting the categorical claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.86).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Capital Punishment as Categorical Dignity Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '6a535e16-2724-4816-ad53-38b381ccab80').
narrative_ontology:cs_kernel_codification('6a535e16-2724-4816-ad53-38b381ccab80', distributed).
narrative_ontology:cs_authority_grounding('6a535e16-2724-4816-ad53-38b381ccab80', distributed).
narrative_ontology:cs_reading_relation('6a535e16-2724-4816-ad53-38b381ccab80', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a535e16-2724-4816-ad53-38b381ccab80', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('6a535e16-2724-4816-ad53-38b381ccab80', foundational, dignity_categorically_nondefeasible).
narrative_ontology:cs_axiom_status(dignity_categorically_nondefeasible, holdable).
narrative_ontology:cs_axiom_grounding('6a535e16-2724-4816-ad53-38b381ccab80', dignity_categorically_nondefeasible, deontological).
narrative_ontology:cs_axiom('6a535e16-2724-4816-ad53-38b381ccab80', secondary, irreversibility_forecloses_error_correction).
narrative_ontology:cs_axiom_status(irreversibility_forecloses_error_correction, holdable).
narrative_ontology:cs_axiom_grounding('6a535e16-2724-4816-ad53-38b381ccab80', irreversibility_forecloses_error_correction, empirically_contingent).
narrative_ontology:cs_reference_frame('6a535e16-2724-4816-ad53-38b381ccab80', abolitionist_dignity_framework).
narrative_ontology:cs_drift_state('6a535e16-2724-4816-ad53-38b381ccab80', contemporary_international_human_rights_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('6a535e16-2724-4816-ad53-38b381ccab80', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, state_prosecutorial_apparatus).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, political_actors_running_on_toughness).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, families_of_the_condemned).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, murder_victims_families_seeking_execution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held under sentence of death by the state's penal apparatus. Under this reading they retain an inalienable dignity interest that no verdict, however procedurally correct, can extinguish; execution terminates that interest irreversibly and admits no correction if the conviction was wrong. Their only exits are clemency, appellate reversal, or abolition itself — none of which they control.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, payer,
    powerless, biographical, trapped, national).

% A subset of the condemned population later shown (or plausibly believed) to be innocent. Because the harm is irreversible once carried out, this group has zero remedy after execution — they represent the strongest evidentiary case for this reading's categorical claim rather than a case-by-case desert calculation.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, biographical, trapped, national).

% Bear the secondary harm of state killing: grief compounded by the state's active role, exclusion from mainstream sympathy, and often years of stigma during the appeals process. They have no standing to halt the sentence and limited resources to contest it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, families_of_the_condemned, payer,
    powerless, biographical, constrained, regional).

% Seeks, obtains, and defends death sentences; controls charging decisions, plea leverage, and the machinery of execution. Collects institutional legitimacy, closure narratives for victims' families, and prosecutorial career capital from capital convictions. Faces essentially no personal cost from wrongful executions once appeals are exhausted.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_prosecutorial_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__abolition_reading, state_prosecutorial_apparatus, beneficiary).

% Campaign and govern on visible severity toward violent crime; capital punishment is a low-cost signal of toughness that requires no sustained policy investment. They can shift positions as electoral winds change without bearing the downstream cost of an execution.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, political_actors_running_on_toughness, beneficiary,
    powerful, biographical, mobile, national).

% Some family members of murder victims experience the execution as vindication or closure. Under this reading their felt need is acknowledged as real but is held not to license a categorical exception to the dignity claim; their voice is present in retributive and deterrence readings but structurally subordinated here to the categorical prohibition.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, murder_victims_families_seeking_execution, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__abolition_reading, murder_victims_families_seeking_execution, excluded).

% Litigate, lobby, and organize against capital statutes on the categorical dignity claim this constraint states. They are structurally excluded from final charging and clemency decisions and can only act at the margins — moratoria, litigation, legislative repeal campaigns — never with unilateral authority to stop an execution.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate whether specific applications of capital punishment violate constitutional dignity or cruelty prohibitions. They can narrow or expand the practice but under prevailing doctrine have not adopted the categorical claim this reading asserts; they sit outside the direct beneficiary/victim structure while shaping its boundaries.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__abolition_reading, state_prosecutorial_apparatus).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state's monopoly on lawful violence is generally invoked to coordinate collective punishment and prevent private vengeance; capital punishment specifically is framed by its defenders as extending that monopoly to the ultimate sanction for the gravest crimes.
% TRANSFER_FUNCTION: Moves life itself — irreversibly — from the condemned person to the state's exercise of penal authority, and moves political and institutional legitimacy capital to prosecutors and elected officials who secure and defend death sentences.
% ABSENT_VOICES: The condemned, once executed, have no further voice in any proceeding that might reveal error; wrongfully convicted individuals who are exonerated only posthumously never enter the record as beneficiaries of correction. Abolitionist advocates are present in public discourse but excluded from the charging, sentencing, and clemency decisions that actually determine outcomes.
% DISAPPEARANCE_RATIONALE: If state killing were abolished overnight, prosecutorial charging strategy, plea bargaining leverage in capital-eligible cases, appellate court dockets, and the political theater around 'toughness on crime' would all reorganize substantially; resources currently devoted to capital litigation and death-row incarceration would shift toward life-sentence administration and (per this reading) no coordination function specific to the taking of life would be lost, since the state's ordinary punitive and incapacitative functions do not depend on the power to kill.
% FOUNDING_PROBLEM: State killing was historically justified as necessary for public order, deterrence of the gravest crimes, and proportional satisfaction of the harm done to victims and communities.
% FOUNDING_PROBLEM_CORROBORATION: Prosecutorial and political beneficiaries attest the founding problem (deterring the worst crimes, delivering proportional justice) remains live. Exoneration data compiled by innocence-project litigation, international human rights bodies, and comparative studies of abolitionist states with stable or falling homicide rates — sources outside the beneficiary set — attest that the deterrence and necessity claims are empirically unsupported and that the categorical dignity harm is irreversible and undiminished by any demonstrated public-safety benefit.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.86, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.86 at interval end) because this reading treats the harm as categorical and irreversible: no procedural correctness, no desert calculation, and no demonstrated deterrent benefit can offset the taking of a rights-bearer's life. Suppression (0.72) reflects that capital statutes persist through active prosecutorial and political defense against sustained abolitionist resistance — this is not a passive natural fact but a maintained legal architecture. Accessibility_collapse is authored comparatively low (0.35) because alternative sentencing regimes (life imprisonment, restorative frameworks) are demonstrably available and practiced in abolitionist jurisdictions — the categorical claim does not rest on the absence of alternatives, only on their moral necessity. Resistance is high (0.78): this is among the most actively contested constraints in criminal law, with organized litigation, international human rights pressure, and legislative repeal campaigns constantly pushing against it. The theater_ratio's modest rise across the measured interval (0.22 to 0.40) tracks the reading's observation that procedural safeguards (extended appeals, clemency review, execution protocols) increasingly function as legitimating performance around a practice whose underlying justificatory claims (deterrence, proportional desert) have weakened under empirical and philosophical challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the state_prosecutorial_apparatus seat, capital punishment is coordinated public justice defensibly administered through extensive procedural safeguards. From the condemned_persons seat, the identical structure is an irreversible extraction of the one thing no remedy can restore. The engine computes these as structurally different seat-classifications from the same authored data; this story does not adjudicate between them, it authors the abolitionist's structural claim that the harm is categorical and that no seat's felt benefit changes that fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are declared the structural victims/payers: the constraint's operation (state killing) extracts the totality of their remaining life and, under this reading, their dignity as rights-bearers, regardless of any process that preceded it. Their exit_options are trapped — no market or institutional alternative exists once sentence is imposed, only clemency or reversal outside their control. State prosecutorial and political actors are the structural beneficiaries: they collect legitimacy, closure narratives, and electoral capital from the killing power's continued existence and exercise, while bearing essentially none of the irreversible cost if a conviction later proves wrong. Murder victims' families seeking execution occupy a genuinely mixed position — real felt benefit from vindication, but this reading holds that felt benefit does not license overriding the categorical prohibition, which is why they carry both beneficiary and excluded roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deterring the gravest crimes, satisfying proportional justice, maintaining public order) is authored as contested rather than flatly dead, because prosecutorial and political beneficiaries continue to assert it is live while corroborating evidence from outside that beneficiary set (exoneration data, comparative-jurisdiction homicide studies, human rights body findings) undermines the deterrence and necessity claims specifically. This reading does not claim the founding problem never existed — it claims that even a fully live founding problem (the worst crimes demand a response) does not license this particular means, because the categorical dignity harm is not defeasible by any showing of continued utility. This is the structural core of the abolitionist reading and what distinguishes it from a mere claim that capital punishment doesn't work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_versus_thresholded_dignity_claim,
    'Is human dignity a categorical, non-gradable property that flatly forbids state killing in all cases, or is it a thresholded property that could in principle be outweighed by sufficiently extreme desert or sufficiently large utility gains?',
    'No empirical resolution mechanism exists — this is a foundational normative-conceptual question about the structure of dignity claims. Philosophical argument (Kantian dignity theory versus consequentialist or mixed frameworks) is the only route to resolution, and disciplines remain divided.',
    'If dignity is genuinely categorical, this reading''s high ε and snare classification are structurally correct and the sibling readings are structurally mistaken in kind, not just in weighting. If dignity is thresholded, this reading collapses into a very strong retributive/deterrence-skeptical position rather than a categorical prohibition, and ε should be lower and contingent on empirical desert/utility findings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_versus_thresholded_dignity_claim, conceptual, 'Whether the abolitionist dignity claim is categorical or thresholded — the central conceptual fork this reading depends on.').

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading (abolition_reading) of the contested state_killing_legitimacy kernel; the sibling readings (retributive_reading, deterrence_reading) are separate constraints. Where exactly is the disagreement located — is it about facts (does execution deter?), about desert (does murder forfeit the life-right?), or about the nature of dignity itself (is it defeasible)?',
    'Decompose further: deterrence disagreement is empirically resolvable in principle (natural experiments across abolitionist and retentionist jurisdictions, though causal identification is contested); desert disagreement is a question of retributive theory; dignity-defeasibility is the conceptual fork named in the sibling omega above. Each sibling reading should be checked for which of these three loci its core axiom actually occupies.',
    'If the disagreement is purely empirical (deterrence), all three readings could in principle converge once evidence is settled. If it is about desert or dignity-defeasibility, no amount of empirical data resolves the dispute and the readings remain permanently coexisting positions — which is the relation this story declares in cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the structural site of disagreement among the three kernel readings — factual, desert-based, or dignity-conceptual.').

omega_variable(
    wrongful_conviction_rate_as_categorical_evidence,
    'Does the documented existence of wrongful capital convictions and posthumous exonerations constitute decisive evidence for the categorical claim, or merely evidence that procedural safeguards need improvement (a position compatible with retributive and deterrence readings)?',
    'This is partly empirical (what is the actual wrongful conviction rate in capital cases, and is it structurally irreducible given adversarial trial limits) and partly conceptual (does any nonzero, irreversible wrongful-execution rate suffice to establish the categorical claim, or is a nonzero rate compatible with a calibrated retributive system that accepts some error).',
    'If wrongful conviction is shown to be structurally irreducible (not fixable by more process), this strengthens the categorical claim significantly. If it is shown to be a fixable procedural defect, the retributive and deterrence readings retain a defensible position that reform, not abolition, is the correct remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_rate_as_categorical_evidence, empirical, 'Whether wrongful-conviction irreversibility is structural (supports categorical claim) or a fixable procedural defect (supports reform-not-abolition).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__abolition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__abolition_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__abolition_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__abolition_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__abolition_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__abolition_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(stat_tr_t60, state_killing_legitimacy__abolition_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__abolition_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__abolition_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__abolition_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__abolition_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__abolition_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__abolition_reading, base_extractiveness, 50, 0.84).
narrative_ontology:measurement(stat_be_t60, state_killing_legitimacy__abolition_reading, base_extractiveness, 60, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__abolition_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__abolition_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__abolition_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__abolition_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__abolition_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__abolition_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(stat_su_t60, state_killing_legitimacy__abolition_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'is capital punishment legitimate' per the ε-invariance principle: measuring the constraint through a categorical-dignity lens yields high, non-negotiable ε (this story); measuring it through a proportional-desert lens yields a different ε keyed to calibration of desert (retributive_reading); measuring it through a deterrence-efficacy lens yields an ε keyed to contested empirical deterrence findings (deterrence_reading). These are not one constraint viewed three ways — they have different beneficiary/victim structures, different failure modes, and different resolution mechanisms, so they are authored as three linked files rather than one story with a hidden observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
