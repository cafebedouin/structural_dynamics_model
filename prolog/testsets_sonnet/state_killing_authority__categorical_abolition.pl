% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: Categorical Abolition Reading of State Killing Authority
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the categorical-abolition reading of the
 *   state-killing-authority kernel: the claim that state killing is
 *   inherently impermissible regardless of crime or consequence, because life
 *   is inalienable. This is a distinct constraint from the
 *   deterrence_instrument reading (which conditions permissibility on
 *   demonstrated deterrent effect) and the retributive_desert reading (which
 *   grounds permissibility in forfeiture through proportional desert). Under
 *   this reading, the condemned person remains permanently within the
 *   rights-holder set — no act, however severe, removes their claim to life —
 *   and the state itself becomes a potential rights-violator the moment it
 *   exercises lethal sentencing power. This is a low-extraction, low-theater
 *   constraint precisely because its coordination function (removing
 *   irreversible sentencing error, providing a bright-line administrable
 *   limit) is doing real work; the extraction that exists sits with
 *   retributive-minded victims' families who experience the categorical
 *   foreclosure of their preferred outcome as a cost.
 *
 * KEY AGENTS:
 *   - condemned_persons: Primary beneficiary (powerless/trapped) — retains inalienable life claim under this reading
 *   - wrongfully_convicted_populations: Structural beneficiary (powerless/trapped) — irreversible error risk eliminated
 *   - abolitionist_victims_families: Beneficiary/excluded (moderate/constrained) — preference aligns with reading but voice often marginalized in proceedings
 *   - retributive_minded_victims_families: Payer (moderate/constrained) — categorically denied preferred outcome
 *   - the_state: Agenda-setter/excluded (institutional/constrained) — sentencing power capped; reclassified as potential violator if it executes
 *   - prosecutors_seeking_death: Excluded (organized/constrained) — institutional incentive to resist the categorical claim
 *   - constitutional_courts: Analytical observer (institutional/analytical) — adjudicates whether the reading is binding law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.28).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.42).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.28).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, rope).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolition Reading of State Killing Authority").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, 'd09a4755-9ba8-413c-86c1-2e874773eeab').
narrative_ontology:cs_kernel_codification('d09a4755-9ba8-413c-86c1-2e874773eeab', distributed).
narrative_ontology:cs_authority_grounding('d09a4755-9ba8-413c-86c1-2e874773eeab', distributed).
narrative_ontology:cs_reading_relation('d09a4755-9ba8-413c-86c1-2e874773eeab', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('d09a4755-9ba8-413c-86c1-2e874773eeab', state_killing_authority__deterrence_instrument, influences).
narrative_ontology:cs_axiom('d09a4755-9ba8-413c-86c1-2e874773eeab', foundational, life_is_inalienable_regardless_of_desert).
narrative_ontology:cs_axiom_status(life_is_inalienable_regardless_of_desert, holdable).
narrative_ontology:cs_axiom_grounding('d09a4755-9ba8-413c-86c1-2e874773eeab', life_is_inalienable_regardless_of_desert, deontological).
narrative_ontology:cs_axiom('d09a4755-9ba8-413c-86c1-2e874773eeab', secondary, irreversibility_of_execution_bars_state_sentencing_power).
narrative_ontology:cs_axiom_status(irreversibility_of_execution_bars_state_sentencing_power, holdable).
narrative_ontology:cs_axiom_grounding('d09a4755-9ba8-413c-86c1-2e874773eeab', irreversibility_of_execution_bars_state_sentencing_power, empirically_contingent).
narrative_ontology:cs_reference_frame('d09a4755-9ba8-413c-86c1-2e874773eeab', sovereign_capital_sentencing_power).
narrative_ontology:cs_drift_state('d09a4755-9ba8-413c-86c1-2e874773eeab', post_exoneration_evidence_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d09a4755-9ba8-413c-86c1-2e874773eeab', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, wrongfully_convicted_populations).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, abolitionist_victims_families).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, retributive_minded_victims_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits inside the criminal justice system as a convicted party facing the state's most severe available sanction. Under this reading, retains an inalienable claim to life regardless of the crime committed, meaning the constraint's operation directly withholds the state's power to end their life. Has no exit from the proceeding itself but the reading forecloses the specific outcome of execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, beneficiary,
    powerless, biographical, trapped, national).

% A statistically recurring subset of the condemned who did not commit the crime. Under any framework that permits execution, this population bears an irreversible risk; the categorical reading eliminates that irreversibility by removing execution as an available sanction, converting fatal error into a correctable one.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, wrongfully_convicted_populations, beneficiary,
    powerless, generational, trapped, national).

% Family members of murder victims who oppose capital punishment, often on the grounds that further killing does not restore their loss or that they do not want the state killing in their name. Their preference aligns with this reading's outcome, but prosecutors and victim-impact framing in retributive proceedings routinely marginalize or exclude their voice in favor of families seeking execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_victims_families, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, abolitionist_victims_families, excluded).

% Family members of murder victims who want the offender executed as proportional justice or closure. Under this reading, the state cannot deliver that outcome regardless of the crime's severity, which they experience as a categorical denial of what they see as deserved punishment and a cost imposed on them by the reading's premise.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_minded_victims_families, payer,
    moderate, biographical, constrained, national).

% Holds the machinery of prosecution, sentencing, and (where permitted) execution. Under this reading, the state is reclassified from a legitimate executor of ultimate punishment into a potential rights-violator should it kill a condemned person — its sentencing authority is categorically capped at life imprisonment. Legislatures and courts administer this cap; where it is not yet adopted, the state continues to exercise the killing power the reading argues it lacks legitimate warrant for.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, the_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, the_state, excluded).

% Elected or appointed officials who pursue capital charges as part of their prosecutorial strategy, often citing retributive or deterrent rationales. This reading forecloses the outcome they seek to obtain through capital proceedings; their institutional incentive is to resist or route around the categorical claim rather than adopt it.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, prosecutors_seeking_death, excluded,
    organized, biographical, constrained, regional).

% Adjudicates whether the state's killing power survives constitutional review under human-dignity, cruel-and-unusual, or right-to-life provisions. Determines, jurisdiction by jurisdiction, whether the categorical-abolition reading is binding law or merely a contested moral claim awaiting future recognition.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line, administrable limit on state sentencing power that removes the possibility of irreversible error and removes the state's need to justify, case by case, whether a particular killing is proportionate or deterrent-justified — a single categorical rule replaces a contestable, case-specific calculus.
% TRANSFER_FUNCTION: Moves the possibility of the ultimate sanction away from the state and toward permanent incarceration; correspondingly moves the perceived symbolic and retributive 'closure' good away from victims' families who sought execution, and moves risk-of-irreversible-error away from the pool of condemned persons including the wrongfully convicted.
% ABSENT_VOICES: Retributive-minded victims' families are structurally present in sentencing proceedings but this reading's categorical claim overrides what many would choose case by case; abolitionist victims' families are frequently absent from prosecutorial victim-impact framing, which tends to foreground families favoring execution. Neither voice is absent from public discourse, but both are unevenly represented within the formal proceedings that decide outcomes.
% DISAPPEARANCE_RATIONALE: If the categorical-abolition premise were formally repudiated overnight in a jurisdiction that had adopted it, executions could resume, capital charging practices by prosecutors would likely re-expand, and the risk of executing wrongfully convicted persons would return as a live possibility — sentencing regimes, appellate doctrine, and correctional administration would all reorganize around the restored killing power.
% FOUNDING_PROBLEM: The problem this reading was built to address: the demonstrated fallibility and irreversibility of capital sentencing (wrongful executions cannot be corrected), combined with a normative claim that no crime, however severe, forfeits the condemned person's claim to life.
% FOUNDING_PROBLEM_CORROBORATION: Innocence Project-style exoneration data and international human-rights bodies (outside the abolitionist advocacy movement itself) corroborate the irreversibility/error component of the founding problem. The inalienability premise itself is not corroborated by an outside empirical body — it is a normative claim asserted by abolitionist advocates, religious and philosophical traditions, and adopted by some national constitutional courts; retentionist states and retributive-minded families dispute that the problem it names is a problem at all.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).
:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because the dominant function of this reading is coordination-through-bright-line-rule: it resolves the genuine, recurring problem of irreversible sentencing error by removing execution from the available toolkit entirely, rather than by extracting rents from a governed population. The cost this reading imposes falls specifically on retributive-minded families who are denied the symbolic outcome of execution — a real but narrow victim class, not a broad extractive relationship. Suppression is moderate (0.42) because adoption of the categorical rule against a legal system historically built around capital sentencing does require active doctrinal and sometimes constitutional enforcement against prosecutorial and legislative resistance. Theater is low (0.2) because where this reading is adopted, it functions directly — it is not merely performed while executions continue in substance.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons and the wrongfully convicted are structural beneficiaries with essentially no exit from the underlying proceeding but full benefit from the specific outcome this reading forecloses (execution) — very low d. Retributive-minded victims' families bear the cost of the categorical foreclosure — higher d, though their exit options remain those of an organized political constituency rather than a trapped population. The state occupies a dual position: as agenda-setter it retains sentencing authority generally, but under this reading its authority is capped, and if it executes anyway it is reclassified as a rights-violator rather than a legitimate punisher — this is the reading's central structural move.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (irreversible execution of the wrongfully convicted; the normative claim that no crime forfeits life) remains contested rather than dead or resolved — retentionist jurisdictions continue to exercise the killing power this reading denies is legitimate, and the empirical error-rate evidence remains live and growing (DNA-based exonerations continuing to surface decades after conviction). This is not a case of an arrangement outliving its function; the arrangement has not been universally adopted, so mandatrophy in the classic sense (function dead, structure persists) does not apply. The interesting genealogical fact is asymmetric corroboration: the irreversibility/error component of the founding problem is corroborated by outside empirical bodies, while the inalienability premise itself remains a contested normative claim not resolvable by evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_claim_versus_contingent_practice,
    'Is inalienability of life a discoverable moral fact this reading tracks, or a contingent policy commitment dressed in categorical language to foreclose case-by-case retributive and deterrent calculation?',
    'Examine whether jurisdictions adopting this reading treat it as revisable by future legislative supermajority (suggesting contingent policy) or as entrenched constitutional/human-rights doctrine immune to ordinary revision (suggesting genuine categorical claim).',
    'If contingent, this reading functions more like a scaffold or rope built on shifting political consensus; if genuinely categorical and constitutionally entrenched, it functions closer to a mountain-adjacent commitment within the legal system, though still constructed rather than natural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_claim_versus_contingent_practice, conceptual, 'Whether the categorical claim is a discovered moral fact or an entrenched policy choice.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does adopting categorical_abolition in a given jurisdiction merely coexist with retributive_desert and deterrence_instrument as competing arguments in public discourse, or does formal legal adoption logically foreclose the other two readings from being simultaneously operative as sentencing law in that same jurisdiction?',
    'Track whether jurisdictions that constitutionally adopt this reading permit any residual capital sentencing regime to coexist (would indicate coexistence at the discourse level only) or whether adoption eliminates capital sentencing as a legal category entirely (would indicate operative foreclosure within that single legal framework).',
    'Determines whether this reading''s relationship to its siblings is best modeled as influences (shifting the burden of argument) or forecloses (eliminating the sibling reading''s operative content) within a specific adopting jurisdiction, even though at the level of global public discourse all three readings coexist across different jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether formal adoption forecloses sibling readings within a single legal framework versus merely coexisting in broader discourse.').

omega_variable(
    victims_family_voice_weighting,
    'Is the marginalization of abolitionist victims'' families in prosecutorial proceedings a structural suppression mechanism internal to this constraint, or an artifact of adversarial prosecutorial incentives external to the categorical-abolition claim itself?',
    'Compare victim-impact statement admissibility and prosecutorial charging patterns across jurisdictions with and without categorical abolition adopted, controlling for prosecutorial election incentives.',
    'If internal, the reading itself would need to be credited with (or blamed for) how victim voices are weighted; if external, the marginalization is a pre-existing feature of adversarial capital proceedings that this reading interrupts rather than causes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victims_family_voice_weighting, empirical, 'Whether victim-voice marginalization is caused by this reading or by pre-existing prosecutorial structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__categorical_abolition, theater_ratio, 10, 0.12).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__categorical_abolition, theater_ratio, 20, 0.14).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__categorical_abolition, theater_ratio, 30, 0.16).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.17).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__categorical_abolition, theater_ratio, 50, 0.19).
narrative_ontology:measurement(stat_tr_t60, state_killing_authority__categorical_abolition, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__categorical_abolition, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__categorical_abolition, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__categorical_abolition, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.26).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__categorical_abolition, base_extractiveness, 50, 0.27).
narrative_ontology:measurement(stat_be_t60, state_killing_authority__categorical_abolition, base_extractiveness, 60, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__categorical_abolition, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__categorical_abolition, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__categorical_abolition, suppression_requirement, 30, 0.37).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__categorical_abolition, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(stat_su_t60, state_killing_authority__categorical_abolition, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, deterrence_instrument).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the state_killing_authority kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. categorical_abolition (this story) claims life is inalienable regardless of crime; retributive_desert claims forfeiture through proportional desert; deterrence_instrument conditions permissibility on demonstrated deterrent effect and acceptable cost. The three readings produce different beneficiary/victim structures over the same underlying practice (state execution) and are linked here so contamination/coupling analysis can trace how adoption or erosion of one reading structurally pressures the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
