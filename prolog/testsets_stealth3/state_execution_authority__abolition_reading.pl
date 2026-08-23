% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority — Abolition Reading (Categorical Impermissibility)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The colloquial label 'the capital punishment debate' covers one contested
 *   kernel — whether, and on what grounds, the state may execute —
 *   instantiated here as three structurally distinct constraints. This story
 *   is the ABOLITION READING: state execution is categorically impermissible
 *   regardless of crime severity or procedural safeguards. Per the
 *   kernel-reading epsilon-referent rule, the metrics assess the STANDING
 *   ARRANGEMENT UNDER CONTEST — the execution regime as practiced in
 *   retentionist jurisdictions — by this reading's own lights, never the
 *   abolitionist order it endorses. Every executed person enters the victim
 *   set including the guilty, because the categorical premise makes guilt
 *   irrelevant to the wrong; no beneficiaries are declared because the
 *   reading recognizes no legitimate beneficiary — desert-based and
 *   consequence-based justifications are rejected at the root, and any
 *   incidental political gains are products of the punitive framing this
 *   reading contests. KEY AGENTS (by structural relationship):
 *   condemned_prisoners: primary target (powerless/trapped) — bears the
 *   maximal irreversible cost; wrongly_convicted_condemned: primary target
 *   (powerless/trapped) — the proof-case the reading builds on;
 *   state_execution_authority: agenda-setter (institutional/constrained) —
 *   administers the practice and could end it by statute;
 *   elected_retentionist_officials: beneficiary seat (powerful/mobile) —
 *   converts maintenance into electoral capital; murder_victims_families:
 *   contested beneficiary (organized/constrained) — promised satisfaction,
 *   divided outcomes; execution_team_members: secondary payer
 *   (moderate/constrained) — bear the psychological cost of carrying it out;
 *   abolition_advocacy_movement: organized contestant in the observer seat
 *   (organized/analytical); international_human_rights_bodies: external
 *   observer raising the diplomatic price (institutional/analytical). The
 *   claim/metric pair is authored independently: claimed_type snare is this
 *   reading's structural judgment (cover-story coordination, coerced victims,
 *   no recognized benefit); the metrics are this reading's descriptive
 *   assessment of the standing regime; the engine computes per-seat
 *   classifications and owns any divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.95).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.88).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority — Abolition Reading (Categorical Impermissibility)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '53739f6c-cddb-4d2d-b85d-53ac166a7fcf').
narrative_ontology:cs_kernel_codification('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', formalized).
narrative_ontology:cs_authority_grounding('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', lineage).
narrative_ontology:cs_interpretation_layer_present('53739f6c-cddb-4d2d-b85d-53ac166a7fcf').
narrative_ontology:cs_reading_relation('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', foundational, categorical_prohibition_on_state_execution).
narrative_ontology:cs_axiom_status(categorical_prohibition_on_state_execution, holdable).
narrative_ontology:cs_axiom_grounding('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', categorical_prohibition_on_state_execution, deontological).
narrative_ontology:cs_axiom('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', secondary, wrongful_execution_proves_systemic_illegitimacy).
narrative_ontology:cs_axiom_status(wrongful_execution_proves_systemic_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', wrongful_execution_proves_systemic_illegitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', absolute_right_to_life_order).
narrative_ontology:cs_drift_state('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('53739f6c-cddb-4d2d-b85d-53ac166a7fcf', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, condemned_prisoners).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongly_convicted_condemned).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, elected_retentionist_officials).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, murder_victims_families).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, murder_victims_families).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, execution_team_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death by courts acting under state authority. Live confined on death rows, often for years or decades, while appeals proceed. If no clemency or court relief comes, the date of their death is set by government schedule. They cannot leave custody, decline the sentence, or relocate to a jurisdiction that would not impose it; commutation is discretionary and rare.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, condemned_prisoners, payer,
    powerless, biographical, trapped, national).

% People sentenced to death for crimes later shown — through recanted testimony, exposed official misconduct, or DNA and comparable evidence — to have been committed by someone else, or not to have occurred. Some are released after years on death row; others are executed before the error surfaces and cleared only afterward. They entered the process relying on safeguards that failed them.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongly_convicted_condemned, payer,
    powerless, biographical, trapped, national).

% The legislatures, governors, courts, and correctional agencies that define, sentence, warrant, and carry out executions under statute. Maintains death row facilities, execution protocols, clemency procedures, and the appellate pipeline that precedes each scheduled death. Can end the practice by legislation or moratorium at any time — several jurisdictions have — but continues where governing coalitions support it. Bears the direct operating costs: secure housing, litigation, staff, and the diplomatic criticism that follows each execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_authority, agenda_setter,
    institutional, generational, constrained, national).

% Governors, legislators, prosecutors, and attorneys general whose careers advance where supporting capital punishment signals firmness on violent crime. Campaign on maintaining or expanding the practice, sign warrants or defend the statute, and convert the arrangement's continuation into votes, donations, and office. Some reverse position when the politics shift, as a few have after innocence disclosures or botched executions.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, elected_retentionist_officials, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__abolition_reading, elected_retentionist_officials, agenda_setter).

% Relatives of homicide victims for whom the sentence is framed as recognition of their loss. Some report that an execution delivered the ending they were promised; others, publicly organized, report the opposite — decades of appeals reopening the case, hearings compelling them to relive the crime, and a conclusion that altered nothing in their grief. Their standing is invoked heavily by officials defending the practice, whatever their own division.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, murder_victims_families, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__abolition_reading, murder_victims_families, payer).

% Correctional officers, medical personnel, chaplains, and wardens who prepare for and carry out scheduled executions, some by assignment and some under institutional pressure. Psychological injury among execution staff — insomnia, dissociation, trauma diagnoses — is common enough that agencies rotate assignments and provide counseling. Their livelihoods sit inside the institutions that run the apparatus.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, execution_team_members, payer,
    moderate, biographical, constrained, regional).

% Defense lawyers, religious organizations, innocence projects, and campaign groups working to end capital punishment. They document wrongful convictions, litigate individual cases, lobby legislatures, press for moratoria, and cite the growing roster of abolitionist countries and treaty bodies. They hold no administrative control over the practice; their leverage is publicity, litigation, and election-cycle pressure.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolition_advocacy_movement, observer,
    organized, generational, analytical, global).

% Treaty-monitoring bodies, regional courts, and United Nations organs that treat capital punishment as inconsistent with the right to life and human dignity. Issue country reviews, findings, and protocol-accession campaigns directed at retaining states. Their rulings do not directly bind executing governments, but each adverse finding raises the diplomatic cost of the next execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, elected_retentionist_officials).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single settled terminal answer to what a jurisdiction does with its gravest offenders: a legally defined ceiling of punishment, administered through a standardized apparatus of charging, sentencing, appellate review, and scheduled execution, which closes the penal sequence for cases that would otherwise remain indefinitely open.
% TRANSFER_FUNCTION: Moves the condemned prisoner's remaining lifespan — wholly and irreversibly — from the person into the state's completed punishment record; moves symbolic satisfaction ('justice carried out') toward constituencies demanding the maximal response; and converts the arrangement's maintenance into electoral and professional advancement for the officials associated with it.
% ABSENT_VOICES: The executed themselves are the structurally absent voice: death ends testimony, so no one who underwent the arrangement's final act can address the bodies that maintain it, and the wrongfully executed are cleared only posthumously, by others. Death-row populations hold no seat in the legislative and procedural design that governs them, and in several retentionist jurisdictions foreign nationals have faced proceedings without consular representation.
% DISAPPEARANCE_RATIONALE: Death rows worldwide hold thousands whose sentences would convert to life imprisonment overnight; the execution apparatus — protocols, facilities, warrant schedules, secrecy procedures — would stand down; jurisdictions that organize crime politics around the penalty would lose a defining symbol and renegotiate their punitive settlements; and the state's claimed terminal authority over life would lapse wherever the practice lapsed. Concrete arrangements demonstrably depend on it.
% FOUNDING_PROBLEM: Sovereigns originally needed a penalty greater than every alternative in an era without long-term prisons: a way to remove the irreconcilable offender permanently, and to monopolize the vengeance that otherwise ran as open-ended kin-group blood-feud. Execution answered both at once — permanent elimination plus a public demonstration that the state, not the aggrieved family, commanded the ultimate response to killing.
% FOUNDING_PROBLEM_CORROBORATION: Penal-history scholarship traces the feud-monopoly and pre-incarceration origins and treats both founding conditions as superseded; criminological work finds life imprisonment achieves the removal function, and jurisdictions that abolished exhibit no return of private vengeance nor homicide increase attributable to abolition; international human rights jurisprudence treats the founding conditions as obsolete. Retentionist officials dispute all of this and assert a perennial need for a terminal penalty — the attestation of obsolescence sits entirely outside the arrangement's defenders.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.95, 'stealth/ox-alpha', 'none', direct).

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
 *   All scalars are reading-indexed over a fixed referent (the existing execution regime), per OQ-26. Extractiveness 0.95: the act removes an irreplaceable life irreversibly, and the categorical premise admits no substitution — life imprisonment is qualitatively different because it leaves the person alive and the error corrigible; the slight upward drift across the series reflects accumulating documentation of wrongful convictions among the death-sentenced, which raises the assessed wrong without changing the act. Suppression 0.88 is a raw structural property, unscaled by power or scope: custody is total, commutation discretionary, and the machinery has hardened toward method secrecy (compound sourcing, restricted witnesses) — the series models that enforcement hardening alongside the shrinkage of executing jurisdictions, a net intensification. Theater_ratio 0.52: the safeguard apparatus — layered appeals, humane-method refinement, closure rhetoric — grows as legitimation performance over an act this reading holds no procedure can justify, and the series tracks its expansion crossing the Goodhart line late in the interval. Accessibility_collapse 0.55 is deliberately bimodal: near-total for the condemned (no alternative exists from inside a death sentence) yet demonstrably open at the jurisdiction level, where well over a hundred countries and half of US states have abolished — averaged honestly rather than flattened. Resistance 0.7: sustained litigation campaigns, innocence-documentation, moratoria, and treaty-body pressure meet the practice continuously. The measurement series runs on one shared grid (points 0–8–16–24–32–40, approximately the last four decades of the modern contested era), every tracked metric authored at every point; jurisdiction-level moratorium/resumption oscillation is real but secondary to these reading-indexed levels and is noted rather than gridded. fixing_cost is authored cheap: the removal mechanism is ordinary legislation or executive moratorium, repeatedly exercised historically, and the benefit of ending irreversible killing dwarfs the political cost to whoever acts — the binding scarcity is willingness, not capability.
 *
 * PERSPECTIVAL GAP:
 *   From the governor's, prosecutor's, and legislator's seats the practice presents as lawful duty, public protection, and fulfilled promise — the engine should compute a coordinated or rope-flavored experience there, backed by mobile exit and biographical horizons. From the condemned prisoner's seat the identical schedule of warrants and appeals operates as the state arranging his death, with zero exit — a maximal snare-flavored experience. Murder-victims families split internally between delivered promise and reopened wound. Execution staff experience wage-bearing injury. The divergence is computed by the engine from the power, exit, and role data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   The victim-declared seats (condemned_prisoners, wrongly_convicted_condemned) sit at the full-target end: they surrender everything the arrangement takes, hold no exit, and are identity-unmoored from any benefit — trapped exit pushes them toward d≈1.0. No beneficiaries are declared in base_properties, and that omission is a substantive structural claim of this reading, not missing data: with no beneficiary declarations, derivation leans on the victim declarations and power atoms. The agenda-setter state occupies mid-range — it collects its own lethal prerogative (the capacity is the gain, on this reading's account of sovereignty) while paying fiscal and diplomatic costs, so its d sits well below the officials' and far above the condemned's. elected_retentionist_officials derive near the beneficiary end despite the absent declaration: they collect electoral capital at negligible personal cost and hold mobile exit. murder_victims_families sit mid-range with contested receipts. execution_team_members lean mildly target-side (injury borne against wages). The two observer seats are analytical and sit outside the chi arithmetic. No directionality_overrides were needed: the structural declarations plus power atoms produce the intended ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems — blood-feud monopoly and pre-prison permanent removal — are dead: modern incarceration achieves removal, and no abolitionist jurisdiction saw private vengeance return. Yet the arrangement persists and the world would rearrange without it, which is exactly the dead-problem-plus-dependence signature the R5 mismatch consumer flags (status=dead x verdict=world_rearranges). Classifying the regime as snare, rather than rope or tangled_rope, is what blocks the mislabel: it prevents the safeguard apparatus from being credited as a coordination function. On this reading the procedural growth is legitimation performance over an unjustifiable act — which is precisely what a rising theater_ratio operationalizes — so the mandatrophy resolution and the snare claim reinforce each other instead of competing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story is one reading of the state_execution_authority kernel — the abolition_reading, under which every executed person counts as wronged regardless of guilt and neither desert nor consequence legitimizes the act. How would the sibling readings, retributive_reading and deterrence_reading, alter the structural picture?',
    'Author the two sibling stories against the same standing arrangement and compare victim sets, beneficiary declarations, and epsilon. The disagreement is located in the foundational axiom: whether desert or consequences can license state killing at all.',
    'Under retributive_reading the victim set narrows toward the wrongfully executed and victims'' kin count as beneficiaries receiving restored balance; under deterrence_reading potential future victims count as beneficiaries. Either shift lowers epsilon and could move the computed classification from snare toward tangled_rope. Neither sibling is foreclosed as a live position — all three persist across actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this story is one reading of a three-reading kernel; sibling deltas recorded here rather than folded into the classification.').

omega_variable(
    deterrence_empirical_status,
    'Does execution in fact deter murder relative to life imprisonment?',
    'Matched-jurisdiction panel designs and meta-analysis; the existing National Research Council review found the evidence inconclusive.',
    'Robust deterrence would strengthen the deterrence_reading sibling but cannot dissolve this reading''s categorical axiom, which is deontological and indifferent to consequences; inconclusive deterrence strips the regime of its principal consequentialist defense and leaves the cover-story reading standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'Empirical status of the deterrence claim the abolition reading rejects.').

omega_variable(
    wrongful_execution_base_rate,
    'What fraction of people sentenced to death are in fact wrongfully convicted, and how many are executed before the error surfaces?',
    'Resampling analyses of death-row exoneration rates (estimates on the order of a few percent wrongfully convicted among the death-sentenced) together with posthumous clearing records.',
    'Higher rates sharpen the systemic-illegitimacy proof this reading builds on and push the extractiveness series upward; the categorical prohibition itself does not depend on the rate, since a single wrongful execution suffices under the axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_base_rate, empirical, 'Base rate of fatal error beneath the safeguard apparatus.').

omega_variable(
    categorical_exception_boundary,
    'Does ''categorically impermissible regardless of crime severity'' tolerate any exception at all — wartime treason, convictions for genocide or mass atrocity?',
    'Examine abolitionist doctrinal texts and draft instruments for carve-outs, and how fully abolitionist jurisdictions handled atrocity prosecutions.',
    'If any threshold exception exists, the constraint is thresholded rather than categorical and epsilon falls below the maximal band; if none exists, the prohibition is absolute exactly as claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_exception_boundary, conceptual, 'Where, if anywhere, the categorical claim could bend.').

omega_variable(
    substitution_disanalogy_weight,
    'Is irreversibility alone decisive against life imprisonment as substitution, or does this reading also rest on dignity and cruelty grounds that its critics reject?',
    'Analytic separation of the reversal argument (execution cannot be undone; imprisonment remains corrigible) from dignity-and-cruelty arguments, and a survey of which carries the movement''s official position.',
    'If irreversibility alone grounds the prohibition, the claim is narrower and the sibling readings retain more negotiating room; if dignity grounds dominate, the disagreement with the retributive reading is deeper than empirical and closer to irreducible preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_disanalogy_weight, preference, 'Which ground carries the categorical claim''s full weight.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__abolition_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(stat_tr_t8, observed).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__abolition_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(stat_tr_t16, observed).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__abolition_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(stat_tr_t24, observed).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__abolition_reading, theater_ratio, 32, 0.49).
narrative_ontology:measurement_basis(stat_tr_t32, observed).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__abolition_reading, base_extractiveness, 8, 0.91).
narrative_ontology:measurement_basis(stat_be_t8, observed).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__abolition_reading, base_extractiveness, 16, 0.92).
narrative_ontology:measurement_basis(stat_be_t16, observed).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__abolition_reading, base_extractiveness, 24, 0.94).
narrative_ontology:measurement_basis(stat_be_t24, observed).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__abolition_reading, base_extractiveness, 32, 0.95).
narrative_ontology:measurement_basis(stat_be_t32, observed).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__abolition_reading, suppression_requirement, 8, 0.74).
narrative_ontology:measurement_basis(stat_su_t8, observed).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__abolition_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement_basis(stat_su_t16, observed).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__abolition_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement_basis(stat_su_t24, observed).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__abolition_reading, suppression_requirement, 32, 0.86).
narrative_ontology:measurement_basis(stat_su_t32, observed).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel state_execution_authority. The single colloquial label 'capital punishment' covers three structurally distinct claims with different epsilon, victim sets, and beneficiary structures. This story (abolition_reading) assesses the standing execution regime with maximal epsilon and an all-executed-persons victim set; the retributive and deterrence readings assess the same arrangement with narrower victim sets and recognized beneficiaries, yielding lower epsilon. The readings are parallel rather than ordered — no upstream/downstream dependency is asserted among them — so the family is linked symmetrically through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
