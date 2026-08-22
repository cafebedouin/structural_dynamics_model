% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority — Abolitionist Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the
 *   state-execution-authority kernel: the claim that execution is
 *   categorically impermissible regardless of the severity of the crime or
 *   the procedural safeguards surrounding conviction. Under this reading,
 *   every executed person — including the factually guilty — enters the
 *   victim set, because the wrong being identified is the irreversible
 *   exercise of lethal state power itself, not merely error-proneness. The
 *   retributive and deterrence readings are treated as illegitimate
 *   justifications rather than as competing empirical claims to be weighed;
 *   this reading does not attempt to refute their factual premises so much as
 *   deny their normative force even if true. Because the prohibition is
 *   categorical, there is no substitution or mitigation that reduces ε — a
 *   'more careful' execution process is, on this reading, still categorically
 *   wrong, which is why extraction is authored very high and
 *   accessibility_collapse is authored comparatively low (the abolitionist
 *   position holds that alternatives — life imprisonment — are always
 *   available and adequate, so alternatives have NOT collapsed; the
 *   constraint persists by state power, not by the absence of viable
 *   alternatives).
 *
 * KEY AGENTS:
 *   - executed_persons: primary target (powerless/trapped) — bears the categorical harm this reading identifies, guilty or not
 *   - wrongfully_convicted_death_row_inmates: irreversibility proof-point (powerless/trapped) — the empirical case for categorical prohibition
 *   - state_prosecutorial_apparatus: agenda-setter (institutional/arbitrage) — administers and could end the practice
 *   - abolitionist_advocacy_organizations: excluded challenger (organized/constrained) — argues the position this story instantiates, from outside the adjudicative seat
 *   - constitutional_courts: analytical observer (institutional/analytical) — could resolve the kernel dispute but has left it open
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.93).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.78).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.93).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority — Abolitionist Reading").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '8c52103b-6c02-4592-9ed6-b54582e9e46b').
narrative_ontology:cs_kernel_codification('8c52103b-6c02-4592-9ed6-b54582e9e46b', distributed).
narrative_ontology:cs_authority_grounding('8c52103b-6c02-4592-9ed6-b54582e9e46b', distributed).
narrative_ontology:cs_reading_relation('8c52103b-6c02-4592-9ed6-b54582e9e46b', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c52103b-6c02-4592-9ed6-b54582e9e46b', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('8c52103b-6c02-4592-9ed6-b54582e9e46b', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('8c52103b-6c02-4592-9ed6-b54582e9e46b', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('8c52103b-6c02-4592-9ed6-b54582e9e46b', secondary, irreversibility_forecloses_retributive_justification).
narrative_ontology:cs_axiom_status(irreversibility_forecloses_retributive_justification, holdable).
narrative_ontology:cs_axiom_grounding('8c52103b-6c02-4592-9ed6-b54582e9e46b', irreversibility_forecloses_retributive_justification, deontological).
narrative_ontology:cs_reference_frame('8c52103b-6c02-4592-9ed6-b54582e9e46b', inherent_dignity_prohibition_on_state_killing).
narrative_ontology:cs_drift_state('8c52103b-6c02-4592-9ed6-b54582e9e46b', contemporary_human_rights_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('8c52103b-6c02-4592-9ed6-b54582e9e46b', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, capital_defense_indigent_defendants).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, state_monopoly_on_lethal_force_claim).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, retributive_proportionality_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, deterrence_efficacy_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to the state's lethal authority following conviction. From this reading, categorically wronged regardless of guilt or the procedural care taken — the abolitionist premise treats the execution itself, not merely wrongful conviction, as the injury. Has no exit once sentence is finalized; appeals exhaust and clemency is discretionary and rare.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, national).

% Convicted in error and sentenced to death; the irreversibility of execution means any later exoneration comes too late. Stands as the clearest empirical proof-point the abolitionist reading points to: no procedural safeguard has prevented documented wrongful executions, and none can, because the error rate of any adjudicative system is nonzero while the remedy is irreversible.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, national).

% Bear the ongoing loss and stigma attached to a relative's execution, without standing to contest the state's action after the fact. Cannot exit the consequence; the harm compounds across generations through stigma and lost economic/relational support.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed_persons, payer,
    powerless, generational, trapped, national).

% Facing capital charges with under-resourced defense counsel; the deficit in legal resources maps directly onto exposure to the ultimate irreversible penalty. Cannot buy their way to the procedural quality that would (even under non-abolitionist readings) be considered necessary safeguards.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, capital_defense_indigent_defendants, payer,
    powerless, biographical, trapped, national).

% Seeks, obtains, and carries out death sentences; sets charging policy and controls whether capital punishment is sought in a given case. Retains full discretion over whether to invoke this authority and bears essentially no personal cost from doing so; can shift charging strategy without consequence to itself.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_prosecutorial_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Litigate, lobby, and publicize wrongful-execution cases, arguing the practice is categorically illegitimate independent of any individual case's facts. Structurally positioned outside the adjudicative process itself — they can influence policy and public opinion but hold no seat inside the charging or sentencing apparatus they are trying to change.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% Seek execution as closure or proportionate response to a loved one's murder; this reading's categorical rejection of execution does not engage their claim on its own terms, treating retributive and deterrence justifications as illegitimate regardless of the underlying case. Their preference is heard in sentencing proceedings but is not treated as dispositive under this reading.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, crime_victims_families_favoring_execution, excluded,
    moderate, biographical, constrained, local).

% Adjudicate Eighth Amendment and comparable constitutional challenges to capital punishment, weighing evolving standards of decency against precedent upholding the practice. Can, in principle, resolve the underlying kernel dispute by ruling execution categorically unconstitutional, but has so far left the kernel contested rather than settled.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None that this reading recognizes as legitimate. The retributive and deterrence readings claim coordination functions (proportionate justice, crime prevention); this reading holds both are illegitimate justifications for an irreversible state power, so there is no genuine coordination problem the practice solves that could not be solved by non-lethal incapacitation.
% TRANSFER_FUNCTION: Moves the ultimate discretionary power over life from the individual to the state, and moves the cost of that power's exercise — irreversibly, in the event of error — entirely onto the convicted person and their family, with no mechanism for restitution once carried out.
% ABSENT_VOICES: Executed persons cannot testify to their own case's disposition after the fact; wrongfully convicted persons executed before exoneration are permanently silenced as evidence; the abolitionist coalition is present in courts and legislatures but holds no seat in the charging or clemency process itself.
% DISAPPEARANCE_RATIONALE: If state execution authority disappeared, capital sentencing dockets would convert to maximum terms of imprisonment, resource allocation in capital defense and appellate review would shift entirely toward non-capital proceedings, and the population of death row would be resentenced — a substantial, concrete institutional rearrangement, not a null change.
% FOUNDING_PROBLEM: The historical founding problem was framed as the state's need for an ultimate sanction to express societal condemnation of the gravest crimes and to incapacitate the most dangerous offenders where no other remedy was thought sufficient.
% FOUNDING_PROBLEM_CORROBORATION: Independent penological research bodies and international human rights monitors (outside both the abolitionist advocacy coalition and the state prosecutorial apparatus) attest that life imprisonment without parole now provides equivalent incapacitation, and that comparative studies find no reliable marginal deterrent effect over long-term imprisonment — undercutting the founding justification from outside the interested parties on either side of the kernel dispute.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.93, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very high (0.93) because, under this reading, there is no legitimate offsetting benefit to weigh against the harm — retribution and deterrence are rejected as justifications, so the harm registers without counterweight. Suppression is authored high (0.78) and rising over the interval because maintaining capital punishment against a growing body of exoneration evidence and comparative-penology findings requires increasingly active political and judicial defense — courts uphold precedent against mounting empirical challenge, which this reading counts as suppression of an increasingly well-evidenced abolitionist case. Theater ratio is moderate and rising (0.25 to 0.42): procedural safeguards (extended appeals, clemency review, execution protocols) are real activity, but under this reading an increasing share of that activity functions to legitimate an outcome the reading holds is illegitimate regardless of procedure — process theater around a categorically impermissible act. accessibility_collapse is authored LOW (0.35) deliberately: this reading's entire premise is that a fully adequate alternative (life imprisonment) exists and always has, so alternatives have not collapsed — the constraint persists through state power and institutional inertia, not through the absence of options.
 *
 * PERSPECTIVAL GAP:
 *   The state_prosecutorial_apparatus and executed_persons/wrongfully_convicted_death_row_inmates seats compute this constraint completely differently under the engine's per-seat logic: the apparatus retains full discretion and bears no cost from exercising this authority (near-beneficiary positioning by default, though this reading declares no legitimate beneficiaries exist), while the payer seats experience irreversible, non-substitutable harm with zero possibility of correction after the fact. This divergence is exactly the seat-divergence the framework is built to surface — one seat administers a discretionary power at no personal cost, the other bears an outcome that cannot be undone if erroneous.
 *
 * DIRECTIONALITY LOGIC:
 *   No agent is declared a beneficiary under this reading — the abolitionist premise explicitly denies that retribution or deterrence constitute legitimate benefit, so there is no seat to place near d=0. All four payer groups (executed_persons, wrongfully_convicted_death_row_inmates, families_of_executed_persons, capital_defense_indigent_defendants) are declared victims and sit near the full-target end of directionality, amplified by trapped exit options. The state_prosecutorial_apparatus is authored as agenda_setter rather than beneficiary because it administers the power without personally collecting a rent from its exercise — it is the mechanism, not the recipient, which is why gain_flow is authored 'diffuse' rather than naming this seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem is authored as 'dead' with corroboration from penological researchers and human rights monitors outside both interested camps — the incapacitation rationale is functionally superseded by life imprisonment, and deterrence claims lack reliable empirical support in comparative studies. Under this reading, the arrangement persists past the point its founding justification held, which is precisely the mandatrophy the R5 genealogy interview is built to surface: an institution that administers a power (state_prosecutorial_apparatus) could change it, but political and institutional inertia — not continued function — sustains it. This is authored independently of the classification computation; the engine's own type verdict is a separate fact from this narrative claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_contingent_wrongness,
    'Is the wrongness of execution categorical (true regardless of any conceivable procedural safeguard or case facts) or contingent (a function of current error rates and procedural quality that could in principle be reduced to an acceptable residual)?',
    'This is not empirically resolvable — it is a normative-philosophical dispute about whether irreversibility itself is disqualifying or whether irreversibility is disqualifying only in proportion to residual error probability. Track whether any procedural reform regime (e.g., mandatory DNA review, elevated evidentiary standards) is ever treated by abolitionist advocates as sufficient; if none ever is, that is evidence the position is genuinely categorical rather than a proxy for current error-rate concerns.',
    'If the position is truly categorical, ε remains maximal regardless of any future procedural improvement, and no reform short of full abolition changes classification. If the position is actually contingent despite its categorical framing, a sufficiently reformed process could in principle reduce ε, which would functionally collapse this reading into a stringent-safeguards variant of the retributive or deterrence readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_contingent_wrongness, conceptual, 'Whether the abolitionist premise is genuinely categorical or a proxy for error-rate concerns.').

omega_variable(
    kernel_resolution_authority,
    'Which institutional actor, if any, has the authority to resolve the underlying kernel dispute (categorical impermissibility vs. legitimate retributive/deterrence justification) rather than merely adjudicate individual cases within it?',
    'Track whether constitutional courts ever issue a categorical Eighth Amendment (or equivalent) ruling foreclosing capital punishment entirely, versus continuing to adjudicate only procedural adequacy within an unresolved kernel dispute.',
    'A categorical constitutional ruling would settle the kernel in favor of this reading nationally, converting it from a contested reading into the sole surviving constraint; continued case-by-case adjudication leaves all three sibling readings live simultaneously, each governing different jurisdictions'' actual practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_resolution_authority, conceptual, 'Whether any institution can resolve the kernel dispute rather than merely litigate within it.').

omega_variable(
    wrongful_execution_evidentiary_base,
    'How large is the true rate of wrongful execution (as opposed to wrongful conviction later corrected before execution), and is it knowable given that execution forecloses further investigation?',
    'Forensic re-examination of closed capital cases using improved techniques (DNA, revised forensic science) not available at time of execution; statistical extrapolation from the known wrongful-conviction rate among death-row exonerations to estimate an undetected wrongful-execution rate.',
    'A higher documented or extrapolated wrongful-execution rate strengthens the empirical case underlying this reading''s categorical claim; a lower rate does not resolve the categorical/contingent question (per the first omega) but would weaken the practical urgency of the abolitionist position as commonly argued.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wrongful_execution_evidentiary_base, empirical, 'The true, likely undercounted rate of wrongful execution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__abolition_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__abolition_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__abolition_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__abolition_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__abolition_reading, base_extractiveness, 8, 0.87).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__abolition_reading, base_extractiveness, 16, 0.89).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__abolition_reading, base_extractiveness, 24, 0.9).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__abolition_reading, base_extractiveness, 32, 0.92).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.93).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__abolition_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__abolition_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__abolition_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__abolition_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the state_execution_authority kernel. The retributive_reading and deterrence_reading stories author very different beneficiary structures (victims' families and potential future crime victims, respectively, as beneficiaries) and substantially lower ε, because both accept a legitimate justificatory function this abolitionist reading categorically denies. All three stories share the same underlying institutional kernel (the state's claimed authority to execute) but diverge completely on beneficiary/victim structure, coordination function, and ε — per the ε-invariance principle, they are authored as three separate constraint files linked by network edges, not as one story with an averaged or parameterized ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
