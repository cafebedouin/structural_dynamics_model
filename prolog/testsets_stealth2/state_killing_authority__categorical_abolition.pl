% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: State Killing Authority — Categorical Abolition Reading
 *   domain: criminal justice/political philosophy/constitutional law
 *
 * SUMMARY:
 *   This story instantiates ONE reading — categorical_abolition — of the
 *   contested kernel state_killing_authority. Per the kernel-reading rules,
 *   the contest is not described inside the constraint: the sibling readings
 *   (retributive_desert, deterrence_instrument) are separate files linked
 *   through the network. The epsilon referent follows the fixed rule for
 *   kernel-reading stories: the standing arrangement under contest — the
 *   operative state-killing apparatus (capital statutes, death rows,
 *   warrant-signing, execution protocols) — assessed by THIS reading's own
 *   lights, in which every executed person was and remained a full
 *   rights-holder and the state that kills enters the violator set. Epsilon
 *   is therefore authored HIGH for the existing arrangement; the reading's
 *   endorsed alternative (universal abolition) is NOT the referent and would
 *   score near zero. The expected structural delta is authored throughout:
 *   the condemned stay in the rights-holder set, the state sits in the
 *   potential-violator set whenever it executes, and victims' families split
 *   into opposed camps with the abolitionist camp marginalized by the
 *   prosecutors who claim to speak for all of them.
 *
 * KEY AGENTS:
 *   - condemned_prisoners: primary target (powerless/trapped) — bears the ultimate taking; no exit except through the system that sentenced them
 *   - wrongfully_convicted_condemned: target subset — their existence demonstrates the irreversibility the categorical reading treats as decisive
 *   - families_of_the_executed: secondary targets — bear the killing's aftermath inside communities that endorse it
 *   - abolitionist_victims_families: excluded objectors — would veto executions done in their name; kept out of the rooms by the agenda-setter
 *   - retribution_seeking_victims_families: positioned beneficiaries — receive the promised execution and the standing that comes with demanding it
 *   - death_penalty_prosecutors: agenda-setter and collector — run the machinery day-to-day and convert it into career capital and plea leverage
 *   - tough_on_crime_elected_officials: beneficiaries — harvest electoral returns; mobile exit lets them abandon the position when opinion shifts
 *   - execution_participant_staff: instrument-bearers — assigned participation, career-constrained refusal, documented psychological injury
 *   - abolition_advocacy_and_defense_organizations: adversarial participant seat — litigates and documents; identity-fused with opposition
 *   - international_human_rights_bodies: analytical observer — monitors and pressures without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.88).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.85).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.88).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, snare).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "State Killing Authority — Categorical Abolition Reading").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal justice/political philosophy/constitutional law").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '73816476-3116-42b0-a1fc-d89ceb3de58b').
narrative_ontology:cs_kernel_codification('73816476-3116-42b0-a1fc-d89ceb3de58b', formalized).
narrative_ontology:cs_authority_grounding('73816476-3116-42b0-a1fc-d89ceb3de58b', lineage).
narrative_ontology:cs_interpretation_layer_present('73816476-3116-42b0-a1fc-d89ceb3de58b').
narrative_ontology:cs_reading_relation('73816476-3116-42b0-a1fc-d89ceb3de58b', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('73816476-3116-42b0-a1fc-d89ceb3de58b', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('73816476-3116-42b0-a1fc-d89ceb3de58b', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('73816476-3116-42b0-a1fc-d89ceb3de58b', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('73816476-3116-42b0-a1fc-d89ceb3de58b', foundational, no_consequence_justifies_state_killing).
narrative_ontology:cs_axiom_status(no_consequence_justifies_state_killing, holdable).
narrative_ontology:cs_axiom_grounding('73816476-3116-42b0-a1fc-d89ceb3de58b', no_consequence_justifies_state_killing, deontological).
narrative_ontology:cs_reference_frame('73816476-3116-42b0-a1fc-d89ceb3de58b', inalienable_life_prohibition).
narrative_ontology:cs_drift_state('73816476-3116-42b0-a1fc-d89ceb3de58b', contemporary_retentionist_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('73816476-3116-42b0-a1fc-d89ceb3de58b', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, death_penalty_prosecutors).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, tough_on_crime_elected_officials).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, retribution_seeking_victims_families).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_prisoners).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, wrongfully_convicted_condemned).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, families_of_the_executed).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, abolitionist_victims_families).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, execution_participant_staff).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under sentence of death in specialized housing, often for decades, while appeals proceed. Every aspect of daily life — movement, contact, property, medical care — is administered by the offices that sought their sentences. Exit from the sentence runs only through the same court system that imposed it, and clemency petitions are decided by executives advised by the prosecuting office. Some are later found to have been innocent; the sentence, once carried out, cannot be recalled.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_prisoners, payer,
    powerless, biographical, trapped, national).

% Were sentenced to death for crimes they did not commit and were exonerated only after years on death row — through witness recantations, forensic advances, or investigative journalism rather than the routine operation of the appeals system. Several were freed weeks or months before scheduled execution dates. Their cases supply the concrete record that the fact-finding process produces fatal errors.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, wrongfully_convicted_condemned, payer,
    powerless, biographical, trapped, national).

% Lose a parent, child, or sibling to a state-administered killing, typically after years of public proceedings that broadcast the loved one's worst act. They attend or are barred from the execution, receive the body, and live afterward inside communities that often regard the killing as deserved. Their objections, where they have them, are voiced mainly through advocacy organizations.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, families_of_the_executed, payer,
    moderate, biographical, identity_locked, national).

% Lost relatives to homicide and oppose execution, arguing it is carried out in their name without their consent. Prosecutors routinely present 'the victims' families' as supporting the death sentence, select which family members appear at sentencing and clemency hearings, and treat opposing members as disloyal to their own dead. Their participation is channeled away from the rooms where the decisions are made.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_victims_families, excluded,
    organized, biographical, identity_locked, national).

% Demand and await the execution of their relatives' killers, attend sentencings and executions, and are afforded standing and speaking roles that opposing family members are denied. The arrangement promises them vindication; whether the carrying-out delivers the relief they seek is disputed among them — some report closure, others report emptiness and renewed grief.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retribution_seeking_victims_families, beneficiary,
    organized, biographical, identity_locked, national).

% Decide which cases to charge capitally, try them, defend the resulting sentences through decades of appeals, and advise governors against clemency. Capital-case wins drive promotion within offices and reputations beyond them, and the filed death charge functions as leverage in plea negotiations across the whole docket. Senior capital litigators have spent entire careers mastering this body of law; abandoning it would concede that their life's work was error.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, death_penalty_prosecutors, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, death_penalty_prosecutors, beneficiary).

% Campaign on support for executions, sign death warrants, and run advertisements citing opponents' abolition positions. The arrangement yields durable electoral returns at low personal risk because its costs fall on others. When public opinion shifts, these same officials pivot to moratoria or silence without institutional penalty — their stake is positional, not structural.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, tough_on_crime_elected_officials, beneficiary,
    powerful, immediate, mobile, national).

% Corrections officers, medical personnel, and chaplains who prepare and carry out executions under departmental orders. Participation is assigned, refusal carries career consequences, and post-execution psychological injury among team members is common enough that agencies rotate staff and screen for trauma. They can request transfers but cannot opt out of the institution they serve.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, execution_participant_staff, payer,
    moderate, biographical, constrained, local).

% Litigate capital cases, document conditions on death rows, lobby legislatures, and publicize exonerations. They neither administer the arrangement nor bear its sentences, but their institutional identities are fused with opposition to it — redirecting resources away from capital defense would dissolve the organizations as constituted. International human rights bodies amplify their findings.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolition_advocacy_and_defense_organizations, observer,
    organized, generational, identity_locked, national).

% Monitor, report, and urge moratoria; condition memberships and agreements on suspension of executions; and publish jurisprudence treating the arrangement as a rights violation in progress. They hold no enforcement power over retentionist states, and their findings bind only signatories that accept them.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, death_penalty_prosecutors).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels society's response to the gravest crimes through exclusive state procedure — replacing private vengeance and blood feud with a single lawful terminal sanction, and marking the community's severest moral condemnation through its highest sanctioned penalty.
% TRANSFER_FUNCTION: Moves life itself — and, before that, years of condemned existence under sentence of death — from convicted persons to the state's punitive account; moves promised vindication to victims' family members who demand execution; moves career and electoral capital to the prosecutors and officials associated with it.
% ABSENT_VOICES: Abolitionist victims' family members are formally present but procedurally marginalized — prosecutors speak 'on behalf of' victims' families and select which family voices appear at sentencing and clemency hearings. The condemned are silenced at the end (last statements filtered, witnesses limited). Future persons who will be wrongly convicted have no seat at all until they exist, which is precisely the voice the arrangement's finality forecloses.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, prosecutors would restructure charging strategy around life-without-parole, victims'-family expectations would re-form around the substitute sanction, death-row infrastructure would convert to general maximum-security housing, and the electoral market for execution promises would close — the criminal justice economy visibly reorganizes, as it has in every jurisdiction that abolished.
% FOUNDING_PROBLEM: How should the polity respond to crimes so grave that lesser sanctions seem disproportionate — providing a terminal lawful penalty that satisfies retributive demand, deters the gravest offenses, and monopolizes lethal response in the state so that private vengeance and blood feud end.
% FOUNDING_PROBLEM_CORROBORATION: Retentionist legislatures, prosecutors, and retribution-seeking family members attest the founding problem is live. Outside the benefiting parties, the National Research Council's 2012 deterrence review found the deterrent premise unestablished, exoneree testimony attests the error rate, and international human rights jurisprudence attests that life imprisonment serves both incapacitation and condemnation — corroboration exists on both sides, and no source outside the disputing camps certifies the founding problem as settled-live.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.88: under this reading the arrangement takes the maximal good — life — from persons it holds to be inviolable; the taking is not a marginal cost of coordination but the categorical thing itself, so the reading-indexed value sits near the top of the scale. Suppression 0.85: the arrangement persists through finality rules, clemency decided on the advice of the seeking office, restricted post-conviction review, execution secrecy (drug sourcing, witness limits), and the procedural marginalization of dissenting family voices — structural coercion, with an internalized component flagged in its own omega. Theater 0.50: real incapacitation and real adjudication sit alongside heavy staging — medicalized execution protocols performing care, clemency processes that almost never grant, appeals presented as safeguards that function largely as delay-to-execution; the ratio rises over the interval as the medical and procedural staging thickens. Accessibility_collapse 0.35: alternatives do NOT collapse — life without parole exists everywhere the arrangement operates and achieves the incapacitation function; the arrangement persists by political incentive and concentrated defense, not by closed alternatives, which is why this value sits low. Resistance 0.60: sustained litigation, moratoria, exoneration-driven opinion movement, and international pressure meet the arrangement continuously. Claim and metrics are independent authored facts: snare is my structural belief from this reading's seat; the numbers above are my descriptive beliefs about the arrangement's operation. The temporal series share one grid (t=0..50, decade steps); the shape is rise-peak-partial-retreat-with-hardening, not oscillation — enforcement built up through the middle decades, contracted in volume recently, while the surviving core hardened (secrecy, compressed review), which is why suppression_requirement plateaus high rather than falling with volume.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the prosecutor's chair the arrangement is a lawful, carefully safeguarded channel for the community's gravest condemnation — decades of mastered procedure, real deliberation, genuine safeguards; from the condemned prisoner's chair it is a waiting room for a killing administered by the office that sought it. The retribution-seeking family experiences promise and standing; the abolitionist family experiences erasure — cited as 'the victims' families' while barred from the hearing. The elected official holds a positional stake with mobile exit; the corrections officer holds an assigned one with constrained exit. Identity-lock mechanisms differ by seat: prosecutors are bound by professional identity (career path dependence — the life's-work concession problem), both family camps by relational identity (self-concept constituted through the lost relative and loyalty to them), the advocacy organizations by institutional identity (the organization has become its opposition). If the prosecutor's frame broke — if the work were redescribed as killing rather than justice — exit would suddenly look available; the lock is the frame, not the labor market.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Condemned prisoners and the wrongfully convicted: victims with trapped exit — nearest the full-target end; the wrongfully convicted carry the sharpest d because their harm is unrecoverable. Families of the executed: victims, identity-locked — high d. Abolitionist victims' families: listed among the harmed and role-excluded — high d despite formal standing elsewhere, because the arrangement's costs land on them as silencing plus the killing itself. Execution staff: victims with constrained exit — elevated d; their salaries do not offset the assigned participation. On the beneficiary side: prosecutors (agenda-setter, identity-locked) derive low d but not the lowest — they bear career-risk exposure to error revelations; elected officials (mobile, immediate horizon) sit nearest the beneficiary end, harvesting returns while able to walk; retribution-seeking families derive low d as positioned beneficiaries, with the closure-dispute omega flagging that the promised benefit may not arrive. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is preventing two opposite mislabels. First, the arrangement's coordination story (monopolizing vengeance, marking ultimate condemnation, deterring the worst crimes) could pull it toward rope or tangled_rope; the structural data defeat that pull: the coordination function is historically real but the modern arrangement adds no coordination that life imprisonment lacks, its enforcement is active and directional, and its gains concentrate in identifiable seats — cover-story extraction, not hybrid coordination. Second, mandatrophy guardrails block a piton misread: the arrangement is not inertially maintained by nobody — it is actively defended by capturers, and the receipt surface shows it (gain_flow names the prosecutor seat; fixing_cost is cheap, meaning the administrator could change it at modest cost and does not, which is the capture signature, not neglect). The founding-problem interview supports the contested verdict: the original problem (ending private vengeance) is partly obsolete — the monopoly-on-lethal-response goal is achieved by any terminal state sanction — but the parties dispute whether the proportionality and deterrence halves remain live, so status is contested rather than dead, and no zombie flag fires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is the categorical_abolition reading of the state_killing_authority kernel — how would the sibling readings (retributive_desert, deterrence_instrument) restructure the victim and beneficiary sets if instantiated instead?',
    'Comparative authoring of the sibling stories: retributive_desert moves the condemned out of the rights-holder set (forfeited life) and casts execution as owed rather than taken; deterrence_instrument makes the condemned''s status contingent on an empirical deterrence result and casts the state as a cost-benefit calculator rather than a potential violator.',
    'Sibling instantiation changes epsilon and classification wholesale: the retributive reading authors the condemned as outside protection (low measured extraction against them), the deterrence reading authors extraction as contingent on unresolved empirics. This file''s high epsilon is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one kernel, three readings, this file instantiates categorical_abolition only.').

omega_variable(
    disagreement_location_forfeitability_of_life,
    'Where exactly do the three readings disagree? Is the dispute located in the forfeitability of the right to life (categorical_abolition: never forfeitable; retributive_desert: forfeited by heinous crime) and in whether consequences can enter the permissibility calculus (categorical_abolition: never; deterrence_instrument: decisively)?',
    'Conceptual analysis of the axioms: the disagreement is not about facts (error rates, deterrence data) but about which structural element — rights-holder membership or consequence-sensitivity — governs the kernel.',
    'If the dispute is located in forfeitability, no empirical finding can resolve it between categorical_abolition and retributive_desert (deontological axioms do not route to foreclosure-by-evidence); if located in consequence-sensitivity, the deterrence sibling is hostage to empirical resolution while this reading is not.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_forfeitability_of_life, conceptual, 'The specific structural element on which the sibling readings diverge.').

omega_variable(
    closure_delivery_dispute,
    'Does carrying out the execution actually deliver the vindication the arrangement promises retribution-seeking victims'' families?',
    'Longitudinal study of family members before and after executions, comparing reported relief, grief trajectories, and regret against matched cohorts whose offenders received life sentences.',
    'If execution systematically fails to deliver the promised relief, the beneficiary status of retribution-seeking families is hollow — the arrangement would then extract from every family it touches, including those it claims to serve, strengthening the pure-extraction reading of the whole structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_delivery_dispute, empirical, 'Whether the promised beneficiary actually receives what the arrangement promises her.').

omega_variable(
    wrongful_conviction_irreducible_rate,
    'What is the true rate at which condemned persons are innocent, given that detection of innocence is itself imperfect and post-conviction review is restricted?',
    'Convergent estimation from exoneration records, matched-cohort comparison of capital versus non-capital case error rates, and statistical modeling of undetected errors among the executed.',
    'Under this reading any nonzero rate is categorically intolerable because the sentence is irreversible, so the omega changes the strength of the evidentiary record but not the classification; a demonstrated zero-error regime would remove the strongest abolitionist evidence while leaving the inalienability axiom untouched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_irreducible_rate, empirical, 'Irreducible uncertainty in the error rate that makes the arrangement''s finality categorically dangerous.').

omega_variable(
    internalized_suppression_component,
    'Is part of the measured suppression structural (statutes of finality, clemency control, execution secrecy) or internalized (victims'' families accepting the prosecutorial script as the only loyal response, condemned persons'' resigned acquiescence after decades, staff normalizing participation)?',
    'Post-abolition trajectory comparison: in jurisdictions that abolished, do formerly marginalized family voices and former capital prosecutors reorganize quickly (structural suppression) or does the loyalty script and professional identity persist (internalized component)?',
    'If a substantial share is internalized, effective suppression exceeds the structural measure — the arrangement''s enforcement is partly carried by its targets and beneficiaries themselves, and abolition would not immediately release the locked positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_component, empirical, 'Structural versus internalized shares of the suppression holding the arrangement in place.').

omega_variable(
    drift_vector_bidirectionality,
    'Is the dominant drift of the arrangement practice_drift (severe departure from this reading''s reference frame, as retentionist practice continues) or revival_pressure (global abolition momentum reconstructing the frame — majority of states abolished in law or practice, death sentences at historic lows in formerly leading jurisdictions)?',
    'Jurisdiction-level decomposition: classify each retentionist jurisdiction''s trajectory (expanding, stable, moratorium, abolishing) and weight by execution volume; the net vector determines which direction label governs.',
    'If revival_pressure dominates, the arrangement is a contracting remnant whose remaining seats harden (consistent with the rising theater ratio and secrecy measures); if practice_drift dominates, the arrangement is a stable extractive equilibrium. The two readings imply different predictions about which seats defect first.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_vector_bidirectionality, conceptual, 'Bidirectional drift: the frame is simultaneously massively violated and gaining adherents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__categorical_abolition, theater_ratio, 10, 0.34).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__categorical_abolition, theater_ratio, 20, 0.38).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__categorical_abolition, theater_ratio, 30, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.46).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__categorical_abolition, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__categorical_abolition, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__categorical_abolition, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__categorical_abolition, base_extractiveness, 30, 0.91).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.89).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__categorical_abolition, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__categorical_abolition, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__categorical_abolition, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__categorical_abolition, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__categorical_abolition, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'capital punishment debate' decomposes into three structurally distinct constraints sharing one kernel (state_killing_authority). This file is the categorical_abolition reading — epsilon authored high for the standing arrangement, condemned persons retained in the rights-holder set, the state in the potential-violator set. The retributive_desert sibling removes the condemned from the rights-holder set (epsilon collapses against them; the state becomes an obligated executor); the deterrence_instrument sibling makes the condemned's status contingent on unresolved deterrence empirics (epsilon hostage to the NRC-class evidence base). The upstream/downstream structure differs from the BGS pattern: here no reading is empirically upstream of another — the deterrence reading rides on contested empirics, the retributive reading on a deontological forfeiture premise, this reading on a deontological inalienability premise — so the family links are peer-level co-contestation rather than evidence-flow edges. Each file links the other two via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
