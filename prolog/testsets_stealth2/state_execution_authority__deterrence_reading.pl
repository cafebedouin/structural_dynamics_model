% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__deterrence_reading, []).

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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority - Deterrence Reading (Execution Prevents Future Murders)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the deterrence reading of the contested kernel
 *   'state execution authority': the claim that executing convicted murderers
 *   prevents future murders by raising the cost of killing. Under this
 *   reading the arrangement is defended instrumentally - its beneficiaries
 *   are prospective victims of homicide, the executed offender is an
 *   instrumental cost, and its legitimacy stands or falls on whether the
 *   deterrent effect is real and exceeds what life-without-parole achieves.
 *   The referent of every metric below is the standing arrangement -
 *   capital-punishment regimes as they actually operate in retentionist
 *   jurisdictions - assessed by this reading's own consequentialist lights,
 *   never by the abolitionist alternative it rejects. The sibling readings
 *   (retributive_reading, abolition_reading) are separate constraint files,
 *   not positions inside this one. Claim/metric independence is preserved:
 *   claimed_type records my structural judgment (a genuine attempted
 *   coordination function entangled with certain asymmetric costs and
 *   dependent on active enforcement), while the metrics record the
 *   arrangement's observed operation, including the fact that its central
 *   premise remains unverified after four decades of research.
 *
 * KEY AGENTS:
 *   - - retentionist_state_executives: agenda-setting beneficiary (institutional/arbitrage) - administers the arrangement and collects political returns from it
 *   - - condemned_prisoners: primary payer (powerless/trapped) - bears the terminal cost with no exit
 *   - - wrongly_convicted_capital_defendants: pure-error payer (powerless/trapped) - bears the full cost without culpability; the reading's designated minimization class
 *   - - future_potential_murder_victims: contingent beneficiary (powerless/trapped) - the intended dividend, receivable only if the deterrence premise holds
 *   - - surviving_victims_families: dual-positioned (organized/constrained) - receive claimed closure, pay in prolonged engagement; internally split
 *   - - taxpayers_in_retentionist_states: fiscal payer (moderate/constrained) - fund the premium over ordinary life imprisonment
 *   - - capital_case_legal_apparatus: process beneficiary (institutional/constrained) - the fiscal recipient of the enlarged apparatus
 *   - - abolition_advocates: excluded voice (organized/constrained) - categorical premise inadmissible inside the utilitarian frame
 *   - - pharmaceutical_manufacturers: excluded supplier (institutional/arbitrage) - exited the supply chain at negligible cost
 *   - - criminologists_deterrence_researchers: analytical observer (institutional/analytical) - produced the unverified-verdict evidence the reading's legitimacy depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.62).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.75).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority - Deterrence Reading (Execution Prevents Future Murders)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '85b37ead-c87a-4aab-9355-918be7f5f5f2').
narrative_ontology:cs_kernel_codification('85b37ead-c87a-4aab-9355-918be7f5f5f2', formalized).
narrative_ontology:cs_authority_grounding('85b37ead-c87a-4aab-9355-918be7f5f5f2', lineage).
narrative_ontology:cs_interpretation_layer_present('85b37ead-c87a-4aab-9355-918be7f5f5f2').
narrative_ontology:cs_reading_relation('85b37ead-c87a-4aab-9355-918be7f5f5f2', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('85b37ead-c87a-4aab-9355-918be7f5f5f2', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('85b37ead-c87a-4aab-9355-918be7f5f5f2', foundational, execution_justified_by_net_deterrent_effect).
narrative_ontology:cs_axiom_status(execution_justified_by_net_deterrent_effect, holdable).
narrative_ontology:cs_axiom_grounding('85b37ead-c87a-4aab-9355-918be7f5f5f2', execution_justified_by_net_deterrent_effect, empirically_contingent).
narrative_ontology:cs_axiom('85b37ead-c87a-4aab-9355-918be7f5f5f2', foundational, deterrence_operates_through_cost_signaling).
narrative_ontology:cs_axiom_status(deterrence_operates_through_cost_signaling, holdable).
narrative_ontology:cs_axiom_grounding('85b37ead-c87a-4aab-9355-918be7f5f5f2', deterrence_operates_through_cost_signaling, empirically_contingent).
narrative_ontology:cs_reference_frame('85b37ead-c87a-4aab-9355-918be7f5f5f2', utilitarian_severity_lever_framework).
narrative_ontology:cs_drift_state('85b37ead-c87a-4aab-9355-918be7f5f5f2', post_nrc_2012_review, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('85b37ead-c87a-4aab-9355-918be7f5f5f2', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_murder_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, surviving_victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, retentionist_state_executives).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, capital_case_legal_apparatus).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, condemned_prisoners).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongly_convicted_capital_defendants).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, taxpayers_in_retentionist_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, surviving_victims_families).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, deterrence_efficacy_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governors, legislators, and attorneys general in the jurisdictions that retain capital punishment. They write the statutes defining death-eligible crimes, sign execution warrants, and defend the practice in court and in campaigns. Each execution and each high-profile capital case generates visible proof of responsiveness to homicide, and the issue reliably mobilizes voters and donors. Individual officeholders can and do change positions or move to other issues when the politics shift; the position is held as long as it pays.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, retentionist_state_executives, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__deterrence_reading, retentionist_state_executives, beneficiary).

% People sentenced to death in retentionist jurisdictions. They spend years to decades in specialized death-row confinement under an announced outcome that may or may not arrive. Once the sentence is final, nothing they do short of a successful appeal or discretionary clemency changes it; the sentence follows them until it is carried out, commuted, or they die of another cause.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, condemned_prisoners, payer,
    powerless, biographical, trapped, national).

% People sentenced to death for killings they did not commit. Post-conviction reviews and exonerations indicate a small but persistent fraction of death sentences involve innocent defendants. Inside the deterrence rationale their deaths are counted as losses to be minimized through procedure rather than prevented by ending the practice; they bear the heaviest possible cost of the arrangement with no culpability of their own.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongly_convicted_capital_defendants, payer,
    powerless, biographical, trapped, national).

% Everyone exposed to homicidal violence, considered prospectively: the class whose lives the arrangement is supposed to save if prospective killers weigh the ultimate price. They exercise no agency of their own; their stake is asserted on their behalf by officials and advocacy groups. Whether they ever receive anything depends entirely on whether the deterrent effect is real - they are the arrangement's intended dividend, payable only on an unverified premise.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_murder_victims, beneficiary,
    powerless, generational, trapped, national).

% Families of murder victims. Some report that an execution delivers the finality and answer they sought; others describe decades of appeals and scheduled dates as repeated reopening of the wound, and a vocal minority organize publicly against executions in their relatives' names. They receive whatever closure the process offers and pay in attention, testimony, and prolonged public engagement with the killing.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, surviving_victims_families, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__deterrence_reading, surviving_victims_families, payer).

% Residents of retentionist jurisdictions who fund the capital-punishment system. Capital trials, mandatory multi-stage appeals, specialized death-row housing, and execution logistics cost substantially more per inmate than ordinary life imprisonment. They pay the difference whether or not the promised safety return materializes, and their main recourse - voting the system down - runs against decades of sustained pro-penalty campaigning.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, taxpayers_in_retentionist_states, payer,
    moderate, biographical, constrained, national).

% Prosecutors, capital-trial and appellate defense specialists, and judges who staff death-penalty cases. The extended multi-stage process sustains specialized careers, offices, and budgets; prosecutors tally death sentences as professional achievements, and the fiscal premium the system consumes arrives as salaries, fees, and appropriations. Most participants could work ordinary criminal dockets instead, but the capital track concentrates prestige and promotion in major offices.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, capital_case_legal_apparatus, beneficiary,
    institutional, biographical, constrained, national).

% Organizations and individuals who hold that state killing is wrong in itself and campaign for repeal. They litigate, lobby, and testify, but their core argument - that the practice is impermissible regardless of consequences - has no slot in the cost-benefit accounting the arrangement's defenders use. The debate is conducted on ground where their premise cannot register as either a cost or a benefit.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolition_advocates, excluded,
    organized, generational, constrained, national).

% Drug makers whose products were repurposed for lethal injection. After export restrictions and reputational campaigns, major manufacturers refused sale for execution use, forcing corrections departments toward secret compounded sources and improvised drug combinations. They left the arrangement's supply chain entirely and at negligible cost to themselves.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, pharmaceutical_manufacturers, excluded,
    institutional, biographical, arbitrage, global).

% Academic economists and criminologists who test whether executions reduce homicide. The field's flagship review concluded in 2012 that existing studies could not establish any deterrent effect, leaving the arrangement's central premise unverified. They shape what counts as evidence but hold no enforcement, electoral, or fiscal stake in the outcome.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminologists_deterrence_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, capital_case_legal_apparatus).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a maximal-penalty backstop against homicidal violence: attaches the ultimate sanction to murder so that prospective killers face the highest possible price, administered through statute, capital litigation, and scheduled executions. The function operates only if prospective killers actually respond to the price signal - the coordination is attempted, not guaranteed.
% TRANSFER_FUNCTION: Moves the terminal sanction from convicted murderers to the state for administration; moves tax revenue from residents of retentionist jurisdictions into an enlarged capital-case apparatus (specialized trials, mandatory appeals, death-row confinement, execution logistics); moves asserted safety to the public contingent on the deterrent effect operating.
% ABSENT_VOICES: The condemned have no voice in frameworks that define them as objects of the calculation rather than parties to it. Abolition advocates hold a categorical objection that is structurally inadmissible inside the utilitarian frame - their premise cannot register as a cost or benefit, so unanimity within the deterrence debate partly reflects who the frame admits. Future victims are present only as proxies asserted by officials.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, retentionist statutes would be rewritten to life-without-parole, death rows would empty into general populations, capital-case budgets and specialized offices would contract, execution chambers would close, and crime politics would lose a recurring campaign asset. Whether murder rates would change is exactly the contested empirical question - but the institutional rearrangement itself is not in doubt.
% FOUNDING_PROBLEM: Early modern states confronted homicide with weak policing, low detection rates, and few reliable levers; severity was the one dial available, and a death penalty simultaneously marked the gravity of killing and supplied a credible threat where the probability of capture was low.
% FOUNDING_PROBLEM_CORROBORATION: Retentionist officials attest the problem is live (homicides continue) and the remedy effective - but they sit inside the benefiting set. Outside it: the National Research Council's 2012 committee found the existing evidence inadequate to show the remedy works as claimed; historical criminology attests the founding conditions no longer hold, since certainty of punishment now dominates severity in deterrence research; and survivors' organizations such as Murder Victims' Families for Reconciliation attest from the survivor seat that the arrangement serves neither prevention nor healing for many of those it claims to represent. External corroboration supports 'contested', not 'live'.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: certain extractions (the terminal sanction borne by the condemned, including the estimated few percent of death sentences involving innocent defendants, plus a fiscal premium over life imprisonment paid by taxpayers) stand against a safety return that is asserted but unverified; the value is moderate rather than extreme because the reading's own frame discounts the arrangement heavily if life-without-parole substitutes - the efficacious core is contingent. Suppression 0.75: absolute for the condemned (no exit exists), and the enforcement infrastructure has hardened over the interval - secrecy statutes, underground drug sourcing, improvised protocols - as resistance grew; the political suppression of the cheaper substitute (life-without-parole framed as weakness) is part of the same picture. Theater 0.58: deterrence logic demands swift, certain, consistent punishment, but actual practice is rare, delayed by decades, geographically concentrated, and applied to a sliver of homicides - most activity signals resolve rather than produces deterrence, and the ratio climbed as executions declined while statutes persisted. Accessibility_collapse 0.35: alternatives remain fully legible and operative - several jurisdictions repealed outright, others hold moratoria; understanding the arrangement does not foreclose exit. Resistance 0.6: sustained constitutional litigation, repeal waves, manufacturer refusal, prosecutorial declination, and gubernatorial moratoria. The three series share one time grid (t=0..48 in 8-step increments) so every metric is authored at every examined point; extractiveness peaks near t=24 (the peak-execution era) then declines as the regime contracts while error-rate knowledge accumulates.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently by construction. From the executive seat the arrangement is a legitimate protection policy it built and defends; from the condemned and wrongly-convicted seats the same statutes are the heaviest possible imposition; from the taxpayer seat it is a budget line with an unverified return; from the contingent-beneficiary seat it computes as nothing at all unless the efficacy premise holds - the same statute is protection, imposition, overhead, or noise depending on seat. The engine derives this divergence from the structural data; the authored claim does not adjudicate it. Coalition note: the payer class is not helpless in aggregate - taxpayers and voters have repealed the arrangement by ballot in multiple jurisdictions, which is why accessibility_collapse stays low despite concentrated executive power.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: future_potential_murder_victims and surviving_victims_families sit near the beneficiary end (low d, subsidized or damped chi); condemned_prisoners, wrongly_convicted_capital_defendants, and taxpayers_in_retentionist_states sit near the target end (high d, amplified chi - the trapped exit profile of the condemned pushes them furthest); retentionist_state_executives are dual-positioned (administer and collect, mid-low d); capital_case_legal_apparatus collects fiscally without setting policy (low-moderate d); observers and excluded suppliers sit outside the chi computation. Suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope; only extractiveness is scaled, by directionality and the national scope of the arrangement. No directionality overrides are used: the beneficiary/victim declarations plus exit profiles already yield the correct relationships, and no seat exhibits identity-lock - the executives' exit is arbitrage (they pivot when politics shift), the condemned's is material trap, not fused identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - severity as the only available lever against homicide under pre-modern detection conditions - is partially obsolete: modern deterrence research holds certainty of punishment, not severity, to be the operative variable, and detection is no longer structurally weak. The R5 consumer reads founding_problem_status='contested' against disappearance_verdict='world_rearranges': arrangements demonstrably depend on the constraint while its founding mechanism is disputed - the mismatch flags capture/zombie risk, cross-checked against the rising theater_ratio series (0.30 to 0.58), which independently tracks function decaying into signal. The tangled_rope classification prevents mislabeling in both directions: calling the arrangement pure extraction erases the sincere coordination attempt and the contingent beneficiary class that defines this reading; calling it pure coordination ignores the certain extractions (innocent deaths, fiscal premium) that persist even if efficacy fails. The omegas carry the resolution conditions: a null deterrence finding collapses the coordination half and forces reclassification; a confirmed, LWOP-surpassing effect legitimizes the premium and stabilizes the entangled reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_unverified,
    'Does execution actually prevent future murders relative to the next-best available penalty?',
    'Longitudinal panel designs comparing matched retentionist and abolitionist jurisdictions across policy transitions, meeting National Research Council methodological standards; natural experiments from state-level repeal and reinstatement.',
    'If the effect is null, the arrangement''s coordination function evaporates and it reclassifies toward pure extraction maintained by inertia and symbolism; if robust, the beneficiary ledger is confirmed and the rope-side reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_unverified, empirical, 'The arrangement''s central empirical premise - that executions deter homicide - remained unverified by the field''s flagship 2012 review.').

omega_variable(
    lwop_substitution_equivalence,
    'Would life-without-parole deter equally, making the incremental cost of execution pure overhead even inside this reading''s own utility calculus?',
    'Comparative deterrence research separating the severity dimension from the certainty dimension of punishment; jurisdiction pairs differing only in the ultimate sanction.',
    'If equivalent, epsilon rises sharply: every execution beyond the LWOP baseline is uncompensated cost by the reading''s own arithmetic, pushing the computed type toward snare; if execution adds deterrent force, the premium is partially justified coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lwop_substitution_equivalence, empirical, 'Whether the marginal deterrent contribution of death over life imprisonment is nonzero.').

omega_variable(
    wrongful_execution_tolerance_threshold,
    'What rate of wrongful execution is tolerable inside a utilitarian frame, and does the estimated rate among death sentences fall inside it?',
    'Systematic post-conviction exoneration audits scaled to the full death-sentenced population, combined with explicit statement of the error rate the utility trade-off can absorb given estimated deterrent gains.',
    'If the observed error rate exceeds the tolerable threshold, the reading''s own arithmetic condemns the arrangement it currently justifies; the classification flips from entangled coordination to extraction-with-cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_tolerance_threshold, empirical, 'Whether actual wrongful-execution incidence sits inside the error budget the deterrence justification implicitly assumes.').

omega_variable(
    phantom_beneficiary_conditionality,
    'Do future potential murder victims constitute a real beneficiary class, or a contingent one that exists only if the deterrence premise holds?',
    'Resolution rides on the efficacy determination: a confirmed deterrent effect converts the contingent class into actual beneficiaries; a null result reveals a beneficiary entry that never collected anything.',
    'If phantom, the entire beneficiary side of the ledger is retroactively restructured - the arrangement has extracted from identifiable payers while its named beneficiaries never existed as recipients, collapsing the coordination-function gate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phantom_beneficiary_conditionality, conceptual, 'The beneficiary set of this reading is defined prospectively and conditionally on an unverified empirical premise.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel state_execution_authority - what would the sibling readings change structurally, and where is the disagreement located?',
    'Not resolvable by data alone: the disagreement is located in the normative ground of state execution. The retributive_reading regrounds the arrangement in proportional desert (beneficiary becomes the moral order; efficacy drops out of the justification entirely). The abolition_reading installs a categorical bar (beneficiary set empties; every executed person enters the victim set regardless of procedural quality). Adopting either sibling replaces this file''s epsilon, beneficiary/victim structure, and classification wholesale.',
    'Cross-reading comparison is only valid at the kernel level; per-seat classifications computed from this file describe the deterrence instantiation exclusively and must not be projected onto the siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of a contested kernel; siblings are separate constraint files.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__deterrence_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(stat_tr_t8, observed).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__deterrence_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(stat_tr_t16, observed).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__deterrence_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement_basis(stat_tr_t24, observed).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__deterrence_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement_basis(stat_tr_t32, observed).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t48, state_execution_authority__deterrence_reading, theater_ratio, 48, 0.58).
narrative_ontology:measurement_basis(stat_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__deterrence_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(stat_be_t8, observed).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__deterrence_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(stat_be_t16, observed).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__deterrence_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement_basis(stat_be_t24, observed).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__deterrence_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement_basis(stat_be_t32, observed).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t48, state_execution_authority__deterrence_reading, base_extractiveness, 48, 0.62).
narrative_ontology:measurement_basis(stat_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__deterrence_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(stat_su_t8, observed).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__deterrence_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(stat_su_t16, observed).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__deterrence_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(stat_su_t24, observed).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__deterrence_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement_basis(stat_su_t32, observed).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(stat_su_t40, observed).
narrative_ontology:measurement(stat_su_t48, state_execution_authority__deterrence_reading, suppression_requirement, 48, 0.75).
narrative_ontology:measurement_basis(stat_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the kernel state_execution_authority. The colloquial label 'the death penalty debate' conflates three structurally distinct constraints: this file (deterrence_reading - instrumental justification, contingent beneficiary set, epsilon keyed to unverified efficacy), the retributive_reading (desert-based justification, moral order as beneficiary, efficacy irrelevant), and the abolition_reading (categorical prohibition, empty beneficiary set, all executed persons as victims). Same standing arrangement, three epsilons, three failure modes. The deterrence claim functions as upstream evidence in public argument - cited to legitimize retention generally - so this reading influences its siblings' operating environment without settling their disputes; all three remain live positions held by different actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
