% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-26
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Deterrence-Conditional Capital Punishment Authorization
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   In retentionist jurisdictions, capital punishment is authorized and
 *   administered under an explicitly conditional justification: the state
 *   kills convicted murderers because, and only insofar as, the practice
 *   prevents murders that would otherwise occur, at a cost the polity deems
 *   acceptable. This story instantiates the deterrence_instrument reading of
 *   the contested state-killing-authority kernel and takes the standing
 *   arrangement — the statutory machinery, death rows, execution protocols,
 *   and the political economy that maintains them — as epsilon's referent,
 *   assessed by the reading's own lights. Because the reading stakes
 *   legitimacy on demonstrated preventive return, the empirical record bears
 *   directly on its own ledger: costs (executions, including wrongful ones,
 *   and decades of specialized confinement) are certain, while the benefit
 *   beyond incapacitation is unverifiable and, by the weight of professional
 *   evidence, probably absent. The claim/metric stance is deliberate:
 *   claimed_type is tangled_rope (a genuinely posited coordination function
 *   coupled to asymmetric extraction), while the metrics describe an
 *   arrangement operating well past what its own justification standard
 *   certifies. Sibling readings are separate constraints with their own
 *   victim sets and epsilon values. KEY AGENTS (by structural relationship):
 *   - condemned_persons: Primary target (powerless/trapped) — bears the
 *   sanction itself - wrongfully_convicted_capital_defendants: Error-bearing
 *   target (powerless/trapped) — supplies the arrangement's terminal failure
 *   cost - families_of_the_executed: Collateral payers (moderate/constrained)
 *   — inherit losses they never consented to price -
 *   future_potential_murder_victims: Contingent beneficiary
 *   (powerless/constrained) — owed saved lives, only if the deterrent margin
 *   is real - tough_on_crime_officeholders: Agenda setter and rent collector
 *   (powerful/mobile) — administers the machinery and converts it into
 *   electoral standing - prosecutorial_offices: Option-value beneficiary
 *   (institutional/mobile) — monetizes death eligibility as plea leverage -
 *   appellate_judiciary: Administering agenda setter
 *   (institutional/constrained) — defines and polices the framework's
 *   boundaries - international_human_rights_monitors: Analytical observer
 *   (institutional/analytical) — documents from outside the coalition -
 *   abolitionist_legislative_minorities: Excluded voice
 *   (organized/constrained) — proposes repeal inside chambers they do not
 *   control
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.68).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.72).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Deterrence-Conditional Capital Punishment Authorization").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '618cf230-7fb1-4792-b46a-9b0a80caea03').
narrative_ontology:cs_kernel_codification('618cf230-7fb1-4792-b46a-9b0a80caea03', formalized).
narrative_ontology:cs_authority_grounding('618cf230-7fb1-4792-b46a-9b0a80caea03', expertise).
narrative_ontology:cs_interpretation_layer_present('618cf230-7fb1-4792-b46a-9b0a80caea03').
narrative_ontology:cs_reading_relation('618cf230-7fb1-4792-b46a-9b0a80caea03', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('618cf230-7fb1-4792-b46a-9b0a80caea03', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_axiom('618cf230-7fb1-4792-b46a-9b0a80caea03', foundational, punishment_justified_only_by_prevented_harm).
narrative_ontology:cs_axiom_status(punishment_justified_only_by_prevented_harm, holdable).
narrative_ontology:cs_axiom_grounding('618cf230-7fb1-4792-b46a-9b0a80caea03', punishment_justified_only_by_prevented_harm, instrumental).
narrative_ontology:cs_axiom('618cf230-7fb1-4792-b46a-9b0a80caea03', secondary, state_killing_subject_to_acceptable_cost_test).
narrative_ontology:cs_axiom_status(state_killing_subject_to_acceptable_cost_test, holdable).
narrative_ontology:cs_axiom_grounding('618cf230-7fb1-4792-b46a-9b0a80caea03', state_killing_subject_to_acceptable_cost_test, instrumental).
narrative_ontology:cs_reference_frame('618cf230-7fb1-4792-b46a-9b0a80caea03', efficacy_conditioned_capital_sanction).
narrative_ontology:cs_drift_state('618cf230-7fb1-4792-b46a-9b0a80caea03', post_nrc_meta_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('618cf230-7fb1-4792-b46a-9b0a80caea03', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, future_potential_murder_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, tough_on_crime_officeholders).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, prosecutorial_offices).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, wrongfully_convicted_capital_defendants).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, families_of_the_executed).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, incapacitation_guarantee_of_execution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The unidentifiable class of people who would be homicide victims in the absence of whatever preventive margin the sanction provides. Membership is knowable only retrospectively and never individually; each member's stake is prospective and arrives only if the deterrent effect over imprisonment is real. They bear no procedure, pay no fee, and appear in the ledger solely as the population the arrangement claims to protect.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, future_potential_murder_victims, beneficiary,
    powerless, generational, constrained, national).

% People convicted of capital-eligible murders and sentenced to death. They spend years to decades in specialized death-row confinement under a sentence that ends in execution or, rarely, commutation; their conduct after conviction changes the outcome only through clemency channels that almost never open. Once sentenced, no relocation, appeal strategy, or behavior removes the sentence's endpoint from the table.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, biographical, trapped, national).

% The subset sentenced to death for crimes they did not commit. Discovery of the error typically arrives through volunteer investigation or chance rather than through the process itself; since 1973 nearly two hundred US death-row prisoners have been exonerated, most after more than a decade under sentence. For this seat the sanction's irreversibility converts ordinary judicial error into a terminal outcome.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, wrongfully_convicted_capital_defendants, payer,
    powerless, biographical, trapped, national).

% Parents, children, and siblings of executed prisoners. They receive the loss without having priced it, are often barred from the room where it happens, and live afterward with both grief and the public record of the manner of death. Some organize and testify in hearings; none can reopen the outcome.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, families_of_the_executed, payer,
    moderate, biographical, constrained, national).

% Governors, attorneys general, and legislators who write the statutes, sign death warrants, and decide clemency. They campaign on willingness to carry the sanction out and draw measurable electoral returns from visible enforcement. Individual reversal is inexpensive — numerous officials have switched to opposition without career damage — yet the machinery is maintained because maintenance keeps paying.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, tough_on_crime_officeholders, agenda_setter,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__deterrence_instrument, tough_on_crime_officeholders, beneficiary).

% District attorneys' offices that hold charging discretion over death eligibility. A capital-eligible charge functions as bargaining leverage that produces pleas and cooperation across the whole docket, not only in the charged case. An office can decline to seek death at any time — several large jurisdictions have — so the benefit is an option exercised selectively rather than a wage depended upon.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, prosecutorial_offices, beneficiary,
    institutional, generational, mobile, national).

% State supreme courts, federal appellate panels, and the apex court as a set: they define which crimes, procedures, and offenders the framework reaches, and they operate the review layers that occupy most of the years between sentence and execution. Precedent and constitutions bind their choices; they cannot decline the docket their own earlier rulings created.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% UN treaty bodies, regional human-rights courts, and documentation NGOs. They publish annual counts, review retentionist states' records, and issue recommendations that carry no domestic enforcement power. Their archives constitute the principal evidentiary record assembled from outside the maintaining coalition.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, international_human_rights_monitors, observer,
    institutional, generational, analytical, global).

% Legislators in retentionist jurisdictions who introduce repeal bills year after year. They command floor speeches, occasional committee hearings, and increasingly favorable polls, but not committee chairs, agenda control, or veto-proof margins; their proposals are processed and defeated inside the very chambers they sit in.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolitionist_legislative_minorities, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, tough_on_crime_officeholders).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Posits a solution to the public-good problem of homicidal violence: by attaching the community's terminal sanction to murder and carrying it out visibly, the arrangement attempts to raise the expected price of killing above what imprisonment alone communicates, protecting a beneficiary class no other institution can name in advance.
% TRANSFER_FUNCTION: Moves the ultimate bodily sanction — the condemned person's life, plus decades of specialized confinement preceding it — from convicted capital defendants (and erroneously convicted defendants caught in the same net) into the state's sanction inventory, in exchange for a claimed diffuse reduction in homicide risk accruing to the public, and a concrete electoral and plea-leverage return accruing to officeholders and prosecutors.
% ABSENT_VOICES: The condemned shape nothing about the rules that sentence them and speak mainly through counsel; the wrongfully convicted enter the record mostly posthumously or through volunteer litigation; in retentionist jurisdictions, repeal-favoring legislators hold floor speech without committee or veto power, and the executed's survivors are heard as sentiment rather than agenda. Apparent unanimity in favor of maintenance therefore arises inside a coalition from which the arrangement's cost-bearers and its ballot-box losers are structurally absent.
% DISAPPEARANCE_RATIONALE: Overnight repeal would reroute sentencing for capital-eligible murders to life-without-parole, collapse plea-leverage pricing across prosecutions, strand existing death-row populations for resentencing, idle execution protocols and specialized facilities, and force officeholders who campaigned on the sanction to re-platform. The surrounding political economy would reorganize within a few legislative sessions, as it visibly has in each jurisdiction that has abolished.
% FOUNDING_PROBLEM: How should a sovereign that claims a monopoly on legitimate force answer deliberate killing by its own members — and, as crystallized in the Beccarian reform debates, should the state's lethal instrument be retained when, and only when, retaining it prevents more killing than it costs?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — persistent homicidal violence demanding sovereign response — is corroborated from well outside the arrangement's benefiting coalition: homicide epidemiology, the Beccarian reform tradition, and successive National Research Council panels (1978, 2012) all attest the problem's persistence and the legitimacy of asking the efficacy question. By contrast, no external body attests the instrument-specific claim: every major external review reports the deterrent margin unproven, and the corroboration that exists for maintenance comes from inside the retentionist coalition — which is itself signal.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.68 encodes the reading's own ledger: costs are certain and irreversible, the benefit beyond incapacitation is unverified and, per the accumulated panel literature, likely small or null — yet the guaranteed incapacitation return keeps the arrangement short of pure extraction from this seat. Suppression 0.72 is the raw structural coercion concentrated in the condemned's absolute entrapment; it is unscaled by design and is a different construct from the suppression_requirement series, which tracks enforcement-machinery capacity (built up from the 1970s reinstitution through the late-1990s peak, then decaying through drug-sourcing bans, botched protocols, and shortage moratoria). Theater 0.50 splits the apparatus between functional incapacitation and symbolic maintenance — deterrence rhetoric, campaign positioning, protocol ceremony. Accessibility_collapse is low (0.30) because abolitionist counterexamples keep the life-imprisonment alternative visibly workable; resistance is high (0.70) across litigation, clemency campaigns, and international pressure. The measurement series share one nine-point grid spanning the interval (roughly 1972-2024); the arc is a single rise-crest-decline cycle — reinstitution build-up, peak extraction and enforcement in the late 1990s, evidence-driven contraction after 2012 — not intermittent reinforcement; base_properties are measured at the end-state (t=52, declining phase). Coalition note: the payer class is fragmented procedurally (each condemned isolated by process), but cross-class coalitions of bereaved families and repeal advocates have repeatedly moved legislation, which is the observable coalition path the fragmentation leaves open.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is stark: the trapped payer seats compute an enforced-loss world in which the sanction is certain and its justification unverified; the officeholder and prosecutor seats compute a coordinated public-order investment that also pays them personally; the judiciary experiences administration and boundary-setting; the monitors register an unfalsified premise. Same-level differentiation: condemned_persons and wrongfully_convicted_capital_defendants share every positional atom and differ only in exposure to the arrangement's error term — a difference invisible at sentencing that nonetheless decides whether the sanction took an innocent. Inter-institutional: governorship, prosecution, and courts share the institutional power atom but hold different relationships to the machinery (electoral rents, option value, doctrinal custody), separated by the beneficiary declarations and exit profiles rather than by any authored override — the override surface keys on power_atom alone and could not split this trio more finely than the declarations already do.
 *
 * DIRECTIONALITY LOGIC:
 *   Future_potential_murder_victims sit near the beneficiary pole with zero verification: their entry into the beneficiary set is exactly the structural delta the kernel context prescribes for this reading, and their benefit materializes only if the efficacy omega resolves affirmatively. Tough_on_crime_officeholders derive low directionality through electoral capture — they collect standing from the machinery they administer. Prosecutorial_offices hold an option value: low directionality with exercised mobility, since several offices have simply stopped seeking death. Condemned_persons, wrongfully_convicted_capital_defendants, and families_of_the_executed sit at the full-target pole, amplified by trapped and constrained exits. International_human_rights_monitors take the analytical seat outside the directionality arithmetic. No directionality_overrides are authored: the beneficiary/victim declarations plus power and exit atoms already yield the correct ordering, and a power-atom-keyed override would smear across the institutional trio it cannot distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification disciplines four available mislabels. Not a mountain: the arrangement is enacted and repealing jurisdictions abound on every continent. Not a rope: its costs fall asymmetrically and irreversibly on a trapped class while identifiable seats collect electoral and plea-leverage returns. Not a scaffold: the conditional justification is epistemic, not temporal — no sunset clause governs the machinery, so the scaffold gate correctly fails. Not yet a piton: the function is not fully atrophied, since executions continue and claims remain operationally load-bearing. Tangled_rope holds both truths the public debate flattens: a real posited coordination function (population protection against homicidal violence) and real extraction (lives taken against an undelivered promise). The mandatrophy question is live but narrowing: the founding problem — sovereign response to homicide — persists, while the instrument-specific warrant erodes. If efficacy resolves null, this reading itself commands repeal, giving the constraint an internal termination condition most extraction-only structures lack. The mismatch consumer should watch founding_problem_status=live against a computed drift toward snare as enforcement capacity decays: a live problem served by a dying instrument is precisely the zombie configuration the genealogy interview exists to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_binding,
    'This classification binds to the deterrence_instrument reading of the state_killing_authority kernel; how would the sibling readings (retributive_desert, categorical_abolition) restructure the same arrangement?',
    'Author retributive_desert and categorical_abolition as separate stories over the same statutory referent and diff victim sets, beneficiary sets, and epsilon values. The disagreement is located in the ground of justification — consequence versus proportional desert versus inalienability — not in the arrangement described.',
    'Under retributive_desert the condemned cease to be instrumental costs and become rights-defaulting parties while future potential victims leave the beneficiary set; under categorical_abolition no beneficiary is admissible and the arrangement reads as pure rights violation. The standing referent is fixed; the reading-indexed values move.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_binding, conceptual, 'Committer structure: one reading of a three-way contested kernel over state killing authority.').

omega_variable(
    deterrence_efficacy_empirical_status,
    'Does capital punishment produce a marginal deterrent effect on homicide beyond life imprisonment, and at what magnitude?',
    'Panel and meta-analytic evidence meeting the identification standards set by the 2012 National Research Council review; long-run cross-jurisdiction comparisons with matched covariates and policy-discontinuity designs.',
    'A robust deterrent margin would validate the arrangement''s coordination function and lower effective extraction for the beneficiary-linked seats; a null result strips the coordination story to cover and drives the computed type toward snare — the condemned would have died for an effect that does not occur.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical_status, empirical, 'Whether the preventive return that licenses the arrangement exists.').

omega_variable(
    acceptable_cost_threshold,
    'What total cost — wrongful-execution rate, death-row confinement harms, administrative burden — does this reading count as ''acceptable'' for a verified deterrent benefit?',
    'Preference elicitation from deterrence-coalition members combined with revealed thresholds in statute, such as innocence-procedure adoption and method restrictions.',
    'A stringent threshold collapses justification even under modest efficacy and pushes the arrangement past its own license; a lax threshold sustains it at current error rates. Moves the computed classification between tangled_rope and a cleaner coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_threshold, preference, 'Where the reading''s own cost ceiling sits.').

omega_variable(
    wrongful_conviction_rate,
    'What fraction of capital sentences are erroneous?',
    'Exoneration-rate inference applied to death-row cohorts using the Gross et al. methodology, extended as new cohorts complete.',
    'Directly scales the certainty-of-cost term in the reading''s own ledger; higher error rates raise effective extraction and strengthen the case for innocence-procedure reforms as the arrangement''s internal corrective machinery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_conviction_rate, empirical, 'Size of the error term the arrangement imposes irreversibly.').

omega_variable(
    contingent_beneficiary_realization,
    'Can the beneficiary seat (future potential victims) realize its benefit at all if the deterrent margin is null, and does counting an unrealizable beneficiary inflate the arrangement''s coordination claim?',
    'Reclassify with and without the contingent beneficiary seat and observe whether coordination-function gates flip; correlate with the efficacy omega''s resolution.',
    'If the seat is removed under null efficacy, the arrangement loses its coordination claim entirely and computes as snare; retaining it preserves tangled_rope. This omega is the hinge between the two outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_beneficiary_realization, conceptual, 'Whether the prescribed beneficiary entry survives contact with the efficacy record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 52).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deterrence_instrument_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(deterrence_instrument_tr_t0, observed).
narrative_ontology:measurement(deterrence_instrument_tr_t4, state_killing_authority__deterrence_instrument, theater_ratio, 4, 0.27).
narrative_ontology:measurement_basis(deterrence_instrument_tr_t4, observed).
narrative_ontology:measurement(deterrence_instrument_tr_t10, state_killing_authority__deterrence_instrument, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(deterrence_instrument_tr_t10, observed).
narrative_ontology:measurement(deterrence_instrument_tr_t18, state_killing_authority__deterrence_instrument, theater_ratio, 18, 0.35).
narrative_ontology:measurement_basis(deterrence_instrument_tr_t18, observed).
narrative_ontology:measurement(deterrence_instrument_tr_t26, state_killing_authority__deterrence_instrument, theater_ratio, 26, 0.41).
narrative_ontology:measurement_basis(deterrence_instrument_tr_t26, observed).
narrative_ontology:measurement(deterrence_instrument_tr_t34, state_killing_authority__deterrence_instrument, theater_ratio, 34, 0.45).
narrative_ontology:measurement_basis(deterrence_instrument_tr_t34, observed).
narrative_ontology:measurement(deterrence_instrument_tr_t42, state_killing_authority__deterrence_instrument, theater_ratio, 42, 0.49).
narrative_ontology:measurement_basis(deterrence_instrument_tr_t42, observed).
narrative_ontology:measurement(deterrence_instrument_tr_t48, state_killing_authority__deterrence_instrument, theater_ratio, 48, 0.5).
narrative_ontology:measurement_basis(deterrence_instrument_tr_t48, observed).
narrative_ontology:measurement(deterrence_instrument_tr_t52, state_killing_authority__deterrence_instrument, theater_ratio, 52, 0.5).
narrative_ontology:measurement_basis(deterrence_instrument_tr_t52, observed).

% Extraction over time
narrative_ontology:measurement(deterrence_instrument_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(deterrence_instrument_be_t0, observed).
narrative_ontology:measurement(deterrence_instrument_be_t4, state_killing_authority__deterrence_instrument, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(deterrence_instrument_be_t4, observed).
narrative_ontology:measurement(deterrence_instrument_be_t10, state_killing_authority__deterrence_instrument, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(deterrence_instrument_be_t10, observed).
narrative_ontology:measurement(deterrence_instrument_be_t18, state_killing_authority__deterrence_instrument, base_extractiveness, 18, 0.64).
narrative_ontology:measurement_basis(deterrence_instrument_be_t18, observed).
narrative_ontology:measurement(deterrence_instrument_be_t26, state_killing_authority__deterrence_instrument, base_extractiveness, 26, 0.69).
narrative_ontology:measurement_basis(deterrence_instrument_be_t26, observed).
narrative_ontology:measurement(deterrence_instrument_be_t34, state_killing_authority__deterrence_instrument, base_extractiveness, 34, 0.71).
narrative_ontology:measurement_basis(deterrence_instrument_be_t34, observed).
narrative_ontology:measurement(deterrence_instrument_be_t42, state_killing_authority__deterrence_instrument, base_extractiveness, 42, 0.74).
narrative_ontology:measurement_basis(deterrence_instrument_be_t42, observed).
narrative_ontology:measurement(deterrence_instrument_be_t48, state_killing_authority__deterrence_instrument, base_extractiveness, 48, 0.71).
narrative_ontology:measurement_basis(deterrence_instrument_be_t48, observed).
narrative_ontology:measurement(deterrence_instrument_be_t52, state_killing_authority__deterrence_instrument, base_extractiveness, 52, 0.68).
narrative_ontology:measurement_basis(deterrence_instrument_be_t52, observed).

% Suppression requirement over time
narrative_ontology:measurement(deterrence_instrument_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(deterrence_instrument_su_t0, observed).
narrative_ontology:measurement(deterrence_instrument_su_t4, state_killing_authority__deterrence_instrument, suppression_requirement, 4, 0.44).
narrative_ontology:measurement_basis(deterrence_instrument_su_t4, observed).
narrative_ontology:measurement(deterrence_instrument_su_t10, state_killing_authority__deterrence_instrument, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(deterrence_instrument_su_t10, observed).
narrative_ontology:measurement(deterrence_instrument_su_t18, state_killing_authority__deterrence_instrument, suppression_requirement, 18, 0.63).
narrative_ontology:measurement_basis(deterrence_instrument_su_t18, observed).
narrative_ontology:measurement(deterrence_instrument_su_t26, state_killing_authority__deterrence_instrument, suppression_requirement, 26, 0.71).
narrative_ontology:measurement_basis(deterrence_instrument_su_t26, observed).
narrative_ontology:measurement(deterrence_instrument_su_t34, state_killing_authority__deterrence_instrument, suppression_requirement, 34, 0.64).
narrative_ontology:measurement_basis(deterrence_instrument_su_t34, observed).
narrative_ontology:measurement(deterrence_instrument_su_t42, state_killing_authority__deterrence_instrument, suppression_requirement, 42, 0.56).
narrative_ontology:measurement_basis(deterrence_instrument_su_t42, observed).
narrative_ontology:measurement(deterrence_instrument_su_t48, state_killing_authority__deterrence_instrument, suppression_requirement, 48, 0.49).
narrative_ontology:measurement_basis(deterrence_instrument_su_t48, observed).
narrative_ontology:measurement(deterrence_instrument_su_t52, state_killing_authority__deterrence_instrument, suppression_requirement, 52, 0.45).
narrative_ontology:measurement_basis(deterrence_instrument_su_t52, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% The colloquial label 'the capital punishment debate' conflates three structurally distinct constraints sharing one statutory referent: deterrence_instrument (this story — epsilon indexed to the efficacy ledger, with the wrongfully convicted as a distinct error-bearing victim seat), retributive_desert (epsilon indexed to desert-forfeiture symmetry), and categorical_abolition (epsilon indexed to inalienability, with no admissible beneficiary). Family links run through network.affects_constraints. Upstream/downstream pressure runs from the criminological research community's verdicts into this reading's legitimacy conditions: the same empirical literature that constitutes this reading's authority is the literature steadily eroding it — an authority structure consuming its own evidentiary base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
