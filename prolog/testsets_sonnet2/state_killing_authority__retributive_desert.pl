% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Retributive-Desert Reading of Capital Punishment Authority
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint isolates the retributive-desert reading of the
 *   state-killing-authority kernel: the claim that a murderer's own act
 *   forfeits their right to life, and that proportional justice (lex
 *   talionis) requires the state to impose death for death. This is distinct
 *   from the deterrence reading (which conditions the practice entirely on
 *   future-crime prevention and would collapse if deterrence evidence failed)
 *   and from the categorical-abolition reading (which denies the state may
 *   ever kill regardless of desert). The retributive reading does not need
 *   deterrence evidence to sustain itself — its justification is
 *   backward-looking moral balance, not forward-looking prevention — which is
 *   why its ε, victim set, and persistence conditions differ structurally
 *   from the sibling readings rather than being alternate measurements of the
 *   same claim.
 *
 * KEY AGENTS:
 *   - state_execution_authority: institutional agenda-setter administering capital statutes and executions
 *   - condemned_persons: powerless payers whose rights-holder status is declared forfeited
 *   - wrongfully_convicted_death_row_inmates: payers bearing the forfeiture cost without the triggering wrongdoing
 *   - victims_surviving_family: moderate-power beneficiaries of the promised proportional response
 *   - retributive_justice_tradition_adherents: organized beneficiaries whose moral framework is vindicated
 *   - constitutional_courts: institutional observers policing procedural application without touching the desert premise itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.58).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.72).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Retributive-Desert Reading of Capital Punishment Authority").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, 'acebde65-a4df-4e10-9c34-d23a634d6e6a').
narrative_ontology:cs_kernel_codification('acebde65-a4df-4e10-9c34-d23a634d6e6a', distributed).
narrative_ontology:cs_authority_grounding('acebde65-a4df-4e10-9c34-d23a634d6e6a', distributed).
narrative_ontology:cs_reading_relation('acebde65-a4df-4e10-9c34-d23a634d6e6a', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('acebde65-a4df-4e10-9c34-d23a634d6e6a', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom('acebde65-a4df-4e10-9c34-d23a634d6e6a', foundational, wrongdoing_forfeits_proportional_right).
narrative_ontology:cs_axiom_status(wrongdoing_forfeits_proportional_right, holdable).
narrative_ontology:cs_axiom_grounding('acebde65-a4df-4e10-9c34-d23a634d6e6a', wrongdoing_forfeits_proportional_right, deontological).
narrative_ontology:cs_axiom('acebde65-a4df-4e10-9c34-d23a634d6e6a', secondary, proportionality_ceiling_prevents_excess_punishment).
narrative_ontology:cs_axiom_status(proportionality_ceiling_prevents_excess_punishment, holdable).
narrative_ontology:cs_axiom_grounding('acebde65-a4df-4e10-9c34-d23a634d6e6a', proportionality_ceiling_prevents_excess_punishment, conventional).
narrative_ontology:cs_reference_frame('acebde65-a4df-4e10-9c34-d23a634d6e6a', lex_talionis_proportional_desert).
narrative_ontology:cs_drift_state('acebde65-a4df-4e10-9c34-d23a634d6e6a', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('acebde65-a4df-4e10-9c34-d23a634d6e6a', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_posthumous_vindication).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, victims_surviving_family).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, retributive_justice_tradition_adherents).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, capital_defense_indigent_population).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, moral_desert_proportionality_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, forfeiture_of_rights_through_wrongdoing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital statutes, sets charging and sentencing procedure, and carries out executions. Justifies the practice as restoring moral balance disturbed by murder rather than as crime prevention. Controls the entire apparatus from indictment through clemency review.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_execution_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Have been convicted of murder and sentenced to death under the proportionality rationale. Their claim to a right to life is declared forfeited by their own act. Exit is categorically foreclosed except through appeal, clemency, or exoneration, all of which run through the same authority that condemned them.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_persons, payer,
    powerless, immediate, trapped, local).

% Cannot act, but the retributive framework treats the execution as restoring the moral order the murder disrupted and as an acknowledgment of the wrong done to them specifically. Included for completeness as the entity whose desert-claim the punishment is said to satisfy; not a rights-bearing agent capable of collecting anything.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_posthumous_vindication, beneficiary,
    powerless, immediate, analytical, local).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murder_victims_posthumous_vindication).

% Receive the state's execution of the offender as the promised proportional response to their loss. Some report the closure narrative fits their experience; others report the years-long appeals process re-traumatizes them without delivering the finality the framework promises. Their standing to object to the process itself is limited to victim-impact statements.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victims_surviving_family, beneficiary,
    moderate, biographical, constrained, local).

% Bear the full weight of the forfeiture doctrine despite not having committed the underlying act that supposedly triggered forfeiture. The proportionality logic offers no internal correction for erroneous conviction — the forfeiture premise is applied categorically once guilt is adjudicated, and exoneration depends entirely on discovery of new evidence within the same system that convicted them.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, local).

% Face capital charges without resources to mount defenses comparable to well-funded prosecutions, meaning the actual application of the desert-forfeiture doctrine tracks wealth and jurisdiction as much as culpability. Their exit options are bounded by public-defender capacity, which is structurally under-resourced relative to capital case demands.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, capital_defense_indigent_population, payer,
    powerless, biographical, trapped, regional).

% Legal scholars, religious traditions, and segments of the public whose moral framework is affirmed and legitimized each time the state acts on the forfeiture premise. They benefit doctrinally — the constraint's continued operation vindicates a worldview they hold — without directly administering or paying for it.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, retributive_justice_tradition_adherents, beneficiary,
    organized, civilizational, mobile, national).

% Argue the forfeiture premise is a category error — that no wrongdoing forfeits the inalienable claim against being killed by the state — and would abolish the practice outright. Their objection is heard in courts and legislatures but does not control charging or sentencing decisions within retributive jurisdictions.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolitionist_and_reform_advocates, excluded,
    organized, generational, constrained, national).

% Review whether specific applications of the death penalty comport with constitutional guarantees against cruel punishment and with due process. They do not adjudicate the underlying desert premise itself but police its procedural application, occasionally narrowing or expanding its reach.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, publicly legible standard for the maximum proportional response to the gravest crime, so that punishment severity is anchored to a fixed desert-based ceiling rather than negotiated case by case or left to private vengeance.
% TRANSFER_FUNCTION: Moves the condemned person's continued existence to the state's disposal in exchange for a claimed restoration of moral balance to the victim and society; the wrongfully convicted and the indigent capital-defense population bear the same forfeiture cost without the underlying wrongdoing the doctrine requires to justify it.
% ABSENT_VOICES: Abolitionist and reform advocates who hold that no act forfeits the right against being killed by the state are heard in courts and legislatures but do not control sentencing outcomes within jurisdictions that retain the practice; wrongfully convicted individuals have no voice at all until post-conviction evidence surfaces.
% DISAPPEARANCE_RATIONALE: If the retributive-desert justification for capital punishment disappeared overnight, capital sentencing statutes grounded in forfeiture language would lose their doctrinal foundation, active death sentences would face immediate constitutional challenge, and the practice would either collapse toward life imprisonment or require re-justification on deterrence or incapacitation grounds — a structurally different constraint with a different beneficiary/victim map.
% FOUNDING_PROBLEM: Pre-modern and early-modern legal systems needed a principled ceiling on punishment for the gravest crimes to prevent both under-punishment (impunity) and unlimited private vengeance (blood feuds, disproportionate retaliation) — lex talionis offered a fixed, publicly declared proportionality rule.
% FOUNDING_PROBLEM_CORROBORATION: Retributive theorists and prosecutorial associations attest the desert-forfeiture problem remains live — that proportionate response to the gravest crimes is still a genuine unmet need absent capital punishment. Independent empirical criminology, exoneration registries (documenting wrongful capital convictions), and comparative-jurisdiction studies from abolitionist nations that report no resulting surge in either impunity or vigilante violence corroborate a different reading: the private-vengeance problem the doctrine was built to solve is largely absent in modern states with functioning incarceration systems, leaving the retributive premise doing moral work the founding problem no longer requires.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__retributive_desert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that the condemned population's life is taken as the constraint's central operation, moderated by the genuine coordination function the proportionality ceiling provides (preventing unlimited private vengeance). Suppression (0.72) is high because reversing a death sentence requires overturning the entire adjudicative apparatus that produced it — appeal, clemency, and exoneration all run through the same authority. Theater ratio (0.31) captures that procedural safeguards (appeals, competency review, clemency hearings) partly function as intended but increasingly serve to legitimize outcomes already substantially determined by initial trial resources and jurisdiction, rather than to catch error reliably. Accessibility collapse (0.62) is moderate-high: once convicted, alternatives for the condemned are structurally narrow, but the doctrine itself remains actively contested in legislatures and courts, unlike a true mountain. Resistance (0.68) is substantial and organized (abolitionist movements, defense bar, international human rights bodies).
 *
 * DIRECTIONALITY LOGIC:
 *   Victims' surviving families and retributive-tradition adherents are structural beneficiaries: the constraint's operation is claimed to vindicate them and their worldview, even though family members' actual experience of the process is mixed. Condemned persons sit at the full-target end — trapped exit, powerless, the constraint's entire operation is directed at them. Wrongfully convicted individuals are placed in the same victim category but represent a sharper structural asymmetry: they bear the cost of a doctrine whose own logic (forfeiture through wrongdoing) does not apply to them, since they did not commit the act. The murder victim is listed as a non-agent beneficiary (agent: false) because the vindication claimed on their behalf is doctrinal, not something a deceased person can collect — this prevents the schema from treating a dead person as an acting stakeholder while still capturing what the retributive framework claims to do for them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a public, principled ceiling on punishment to prevent unlimited private vengeance and blood feuds — is contested as still live. Retributive theorists corroborate it as unresolved; comparative evidence from abolitionist jurisdictions with functioning incarceration systems suggests the private-vengeance problem the doctrine addresses has been substantially solved by modern state monopoly on legitimate force independent of capital punishment specifically. This produces a status=contested, verdict=world_rearranges combination rather than a clean dead/unchanged pairing — the doctrine is neither obviously obsolete nor obviously still solving an unmet problem, and that ambiguity is the honest state, not a resolution to be forced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_versus_inalienability,
    'Can any act by a person forfeit a right (life) that the categorical_abolition reading holds to be inalienable, or does the forfeiture premise presuppose a rights framework the abolitionist reading denies from the outset?',
    'This is not empirically resolvable — it is a foundational disagreement about the structure of rights (alienable-through-desert vs. inalienable-by-definition) located in moral philosophy and constitutional theory, not in data about outcomes.',
    'If rights are genuinely inalienable regardless of desert, the retributive_desert reading''s core premise collapses and the constraint has no legitimate coordination function at all — it becomes pure extraction dressed in proportionality language. If forfeiture-through-desert is coherent, the reading retains its claimed tangled-rope structure (genuine proportionality coordination plus asymmetric extraction from the condemned).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(forfeiture_versus_inalienability, conceptual, 'Whether the forfeiture premise is coherent or presupposes what the abolitionist sibling reading denies.').

omega_variable(
    wrongful_conviction_rate_and_doctrine_validity,
    'What is the actual rate of wrongful capital conviction, and does the retributive-desert doctrine have any internal mechanism for discounting its confidence in light of that rate, or does it apply categorically once verdict is reached?',
    'Exoneration registry data cross-referenced with execution records; examination of whether retributive legal theory incorporates any confidence-discounting mechanism or treats post-verdict guilt as settled fact.',
    'A doctrine that applies forfeiture categorically without discounting for known wrongful-conviction base rates is extracting from a population that, by the doctrine''s own terms, should not be subject to forfeiture at all — this would sharpen the tangled_rope classification toward snare for the wrongfully-convicted subpopulation specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_rate_and_doctrine_validity, empirical, 'Whether the doctrine has an internal error-correction mechanism proportional to known wrongful-conviction rates.').

omega_variable(
    posthumous_beneficiary_status,
    'Can a deceased victim coherently be modeled as a ''beneficiary'' of vindication, or is the vindication claim entirely a construct serving the living (surviving family, tradition-adherents, the state) with the victim''s name attached?',
    'Philosophical analysis of posthumous interests and harm; comparison with how the framework treats other non-agent vindicated propositions versus actor beneficiaries.',
    'If posthumous vindication is incoherent, the murder_victims_posthumous_vindication entry should be reclassified entirely as a vindicated_proposition rather than even a non-agent beneficiary, shifting more of the claimed coordination function onto surviving family and tradition-adherents alone — a narrower and more clearly self-interested beneficiary set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posthumous_beneficiary_status, conceptual, 'Whether posthumous vindication is a coherent beneficiary category or a rhetorical construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.12).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__retributive_desert, theater_ratio, 10, 0.16).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__retributive_desert, theater_ratio, 20, 0.2).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__retributive_desert, theater_ratio, 30, 0.23).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.26).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__retributive_desert, theater_ratio, 50, 0.29).
narrative_ontology:measurement(stat_tr_t60, state_killing_authority__retributive_desert, theater_ratio, 60, 0.31).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__retributive_desert, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__retributive_desert, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__retributive_desert, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__retributive_desert, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(stat_be_t60, state_killing_authority__retributive_desert, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__retributive_desert, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__retributive_desert, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__retributive_desert, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__retributive_desert, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(stat_su_t60, state_killing_authority__retributive_desert, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, categorical_abolition).

% DUAL FORMULATION NOTE:
% Part of the state_killing_authority kernel family (3 stories). This story (retributive_desert) is authored independently with its own ε (0.58), beneficiary set (posthumous vindication, surviving family, tradition adherents), and victim set (condemned persons, wrongfully convicted, indigent capital defendants). The deterrence_instrument sibling conditions justification entirely on empirical crime-prevention efficacy and would carry a different ε tied to that evidence base. The categorical_abolition sibling treats the entire practice as illegitimate state extraction regardless of desert or deterrence, with ε near its ceiling and every condemned person as victim with no offsetting beneficiary. All three link to each other because their coexistence in live legal and political discourse means resolution or erosion of one reading's authority (e.g., empirical deterrence null results undermining deterrence_instrument) exerts structural pressure on the others' persistence and rhetorical support.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
