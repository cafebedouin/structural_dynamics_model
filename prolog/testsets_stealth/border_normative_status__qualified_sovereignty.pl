% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified Sovereignty Standard for Border Control
 *   domain: political philosophy/international law/migration
 *
 * SUMMARY:
 *   Since the post-WWII settlement, the operative international position on
 *   borders is neither absolute exclusion nor open movement: states retain
 *   border control authority, but each exercise of it must be justified as
 *   pursuing a legitimate aim, necessary, proportionate, and consistent with
 *   human rights obligations, with individual review available against
 *   removals and detention. This file instantiates the QUALIFIED SOVEREIGNTY
 *   reading of the border_normative_status kernel; the sovereignty_primary
 *   and freedom_primary readings are separate constraints in separate files
 *   and are not averaged into this one. The epsilon referent is the standing
 *   qualified-sovereignty arrangement itself, assessed by this reading's own
 *   lights: a regime that genuinely delivers reviewable protection to some,
 *   while a substantial population is excluded, detained, or pushed into
 *   externalized enforcement zones where review is weakest. The claim/metric
 *   independence rule applies: the claimed type is stated from the authoring
 *   seat, the metrics describe observed operation, and the engine computes
 *   per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - receiving_states: Agenda-setter and net beneficiary (institutional/constrained) — retain lawful control over admission, fund enforcement, bear the adjudication burden the standard imposes
 *   - - human_rights_courts: Agenda-setter (institutional/analytical) — administer the proportionality standard through individual complaints; their jurisprudence defines what counts as lawful exclusion
 *   - - excluded_migrants: Primary target (powerless/trapped) — refused entry, detained, or removed after process; bear the arrangement's costs directly
 *   - - displaced_citizens: Primary target (powerless/trapped) — displaced people outside the protected categories or defeated by deferential review; remain in protracted exile
 *   - - recognized_refugees: Beneficiary (moderate/constrained) — claims succeeded; carry the arrangement's demonstrated benefits
 *   - - transit_state_partners: Beneficiary (organized/constrained) — paid and equipped to intercept and host; absorb humanitarian costs of enforcement they did not design
 *   - - unhcr_and_rights_ngos: Observer (organized/analytical) — monitor, litigate, and publish the mortality and displacement data that anchor the record
 *   - - non_signatory_destination_states: Excluded voice (powerful/mobile) — wealthy destinations outside the treaty framework facing no review of their exclusion practices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.48).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.55).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.48).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty Standard for Border Control").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political philosophy/international law/migration").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '7741a01f-7703-4e5e-9251-1f2ba9f8dd32').
narrative_ontology:cs_kernel_codification('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', formalized).
narrative_ontology:cs_authority_grounding('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', lineage).
narrative_ontology:cs_interpretation_layer_present('7741a01f-7703-4e5e-9251-1f2ba9f8dd32').
narrative_ontology:cs_reading_relation('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', border_normative_status__freedom_primary, influences).
narrative_ontology:cs_axiom('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', foundational, exclusion_authority_conditionally_legitimate).
narrative_ontology:cs_axiom_status(exclusion_authority_conditionally_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', exclusion_authority_conditionally_legitimate, deontological).
narrative_ontology:cs_axiom('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', secondary, non_refoulement_binds_border_exercise).
narrative_ontology:cs_axiom_status(non_refoulement_binds_border_exercise, holdable).
narrative_ontology:cs_axiom_grounding('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', non_refoulement_binds_border_exercise, conventional).
narrative_ontology:cs_reference_frame('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', postwar_rights_answerable_exclusion_settlement).
narrative_ontology:cs_drift_state('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7741a01f-7703-4e5e-9251-1f2ba9f8dd32', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, receiving_states).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, recognized_refugees).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, transit_state_partners).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, receiving_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set immigration rules, operate border enforcement, and decide who may enter, stay, or be removed. Bound by treaty obligations they have ratified and subject to individual complaints when people challenge removals or detention. They fund detention estates, patrol forces, and externalized enforcement partnerships, and they bear the litigation and compliance costs the review system generates. Renegotiating or withdrawing from the instruments carries significant diplomatic and reputational cost, so they adjust practice at the margins instead.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, receiving_states, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, receiving_states, payer).

% Review individual complaints against removals, detention conditions, and border practices, deciding whether state action was justified, necessary, and proportionate. Their judgments bind member states and progressively define what counts as lawful exclusion. Caseload grows with every enforcement intensification; they shape the standard through doctrine but set no migration policy directly.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_courts, agenda_setter,
    institutional, generational, analytical, continental).

% People refused entry, denied protection after process, detained pending decision, or removed. Many crossed multiple countries and cannot safely return home; onward routes close as enforcement tightens, leaving them in transit zones, camps, or irregular status. Their participation in shaping the rules is limited to individual cases filed on their behalf, usually after the decisive decisions have been made.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, regional).

% People forced from their home countries by war, persecution, state collapse, climate disaster, or economic ruin who fall outside the categories that confer protection, or whose claims fail under deferential review. They remain citizens of states unable or unwilling to protect them and spend years or decades in protracted exile with no route that the reviewed systems recognize.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    powerless, generational, trapped, regional).

% People whose claims succeeded and who received residence, documentation, and a path to permanence. They hold enforceable protections against removal and become the visible proof offered when the system's continued operation is defended. Their position depends on the review machinery staying open to future claimants.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, recognized_refugees, beneficiary,
    moderate, biographical, constrained, national).

% Countries along migration corridors paid and equipped to intercept, host, or turn back people heading toward destination states. Enforcement funding is a significant budget line; declining it risks losing aid and diplomatic standing, while hosting large displaced populations strains local services. They enforce a standard they did not write and absorb its humanitarian residue.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, transit_state_partners, beneficiary,
    organized, immediate, constrained, regional).

% Agencies and organizations that monitor compliance, publish mortality and displacement statistics, litigate test cases, and advocate for broader protection categories. They hold no enforcement power, depend on state funding and access agreements, and supply much of the evidentiary record on which review depends.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, unhcr_and_rights_ngos, observer,
    organized, generational, analytical, global).

% Wealthy destination states outside the treaty framework that accept no review of their exclusion practices; labor migration there runs through sponsorship systems with no asylum channel. They bear none of the adjudication burden and none of the review, while the pressure the reviewed systems place on people seeking somewhere safe to go makes their unreviewed labor markets a downstream destination.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, non_signatory_destination_states, excluded,
    powerful, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, receiving_states).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, adjudicable standard that lets states, migrants, and courts predict which exclusions are lawful without first resolving the underlying philosophical dispute about borders: exclusion proceeds under published criteria, and objections are heard case-by-case rather than settled by force.
% TRANSFER_FUNCTION: Moves adjudication and compliance costs onto receiving states; moves exclusion risk onto migrants whose claims fail and onto displaced people outside the protected categories; moves enforcement funding to transit-state partners; and confers legitimacy on state border practices that survive review.
% ABSENT_VOICES: The people excluded and removed rarely participate in framing the proportionality standard — their experience enters as case facts, not as doctrine-shaping voices, and those removed before judgment are absent from the record entirely. Wealthy destination states outside the treaty framework never accepted the standard yet absorb its downstream pressure; their practices set a competitive floor the reviewed systems feel.
% DISAPPEARANCE_RATIONALE: If the qualified-sovereignty standard vanished overnight, exclusion would revert to unchecked state discretion: individual review of removals would collapse, externalization partnerships would lose their legal cover and their litigation exposure simultaneously, recognized refugees would hold protections no machinery defended, and the entire body of jurisprudence defining lawful exclusion would lose its object. Border governance would reorganize around raw bilateral bargaining within months.
% FOUNDING_PROBLEM: After the refoulement catastrophes of the 1930s and 1940s — the Évian conference, the St. Louis, wartime border closures — the founding problem was how to preserve state order and territorial self-determination while making exclusion answerable to human rights, so that flight from persecution could no longer be met with bare sovereign refusal.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UNHCR displacement statistics place forcibly displaced populations at historical highs; IOM's missing-migrants record documents continuing route deaths; ICRC field reporting and academic migration-law scholarship independently attest that the problem the standard was built for persists. No party to the arrangement disputes that displacement pressure is ongoing; the dispute is over whether the standard answers it adequately.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48): the arrangement delivers real, reviewable protection to recognized refugees, but a large population bears exclusion, prolonged detention, and externalized enforcement whose harms surface as route deaths and camp limbo — harm the framework's own success metrics do not count. Suppression (0.55) reflects actively maintained enforcement machinery: detention estates, carrier sanctions, patrol forces, and interception partnerships; the standard channels coercion as much as restraining it. Theater ratio (0.35) captures the growing share of performative compliance — formal conformity maintained while enforcement is outsourced to partners beyond easy review. Accessibility collapse is low (0.35): the two sibling readings remain live political alternatives; nothing about this arrangement forecloses them, and periodic crises revive both. Resistance (0.60) is real and bidirectional: states resist through margin-of-appreciation expansion, pushbacks, and non-compliance windows; advocates resist from the other side, pressing the standard as insufficient. The temporal series run on one shared eight-point grid (every tracked metric authored at every point). The suppression_requirement series is authored deliberately: the story's enforcement history is one of capacity build-up (joint patrol forces from the 1980s, dedicated border agencies from 2004, crisis surge and the 2024 pact), not a static picture. Extractiveness peaks around the 2015 crisis and partially corrects as courts constrain externalized practices.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the excluded migrant's position — powerless, trapped, already en route — the arrangement operates as managed exclusion with procedural decoration: process occurs, and removal frequently follows. From the receiving state's position, the same structure is a burdened entitlement: control retained, but each exercise now costs litigation, detention upkeep, and diplomatic exposure. From the court's position it is a functioning review system doing exactly what it was built to do. From the transit partner's position it is paid enforcement of someone else's standard, with the humanitarian residue left behind. One structure, four different lived types; the engine computes this divergence from power, exit, and directional position rather than from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for receiving_states, recognized_refugees, and transit_state_partners: the arrangement subsidizes their positions (lawful control, granted protection, enforcement revenue respectively). Victim declarations drive high directionality for excluded_migrants and displaced_citizens, and their trapped exit pushes them toward the full-target end — they cannot re-route, return safely, or opt out. Receiving_states are genuinely dual-positioned: the adjudication burden is a real cost to them, tempering their directionality above a pure beneficiary's, which is recorded through the secondary payer role rather than a numeric override. Courts and monitoring organizations occupy analytical seats: they administer and observe without collecting the arrangement's gains or bearing its extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making exclusion answerable after the refoulement catastrophes of the 1930s–40s — is live, not dead: displacement populations are at historical highs and the proportionality machinery is invoked daily. No mandatrophy resolution is declared, and the mismatch flag (dead status plus world_rearranges) does not fire. The classification discipline cuts both ways here: calling this a snare would erase the genuine coordination achievement — a shared, adjudicable standard that replaced unilateral discretion with reviewable justification and actually protects a class of people who would otherwise have none. Calling it a rope would erase the asymmetry — the legitimacy the arrangement produces for states rests partly on costs borne by people with no seat in its design. The tangled-rope reading holds both facts without letting either cancel the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is ONE reading (qualified_sovereignty) of the border_normative_status kernel. What structurally changes if a sibling reading prevails — sovereignty_primary (foundational authority to exclude) or freedom_primary (movement as a fundamental right that borders impermissibly restrict)?',
    'Comparative classification of the sibling stories: sovereignty_primary removes the adjudication burden from states and shrinks the protected class toward citizens only, raising epsilon for excluded migrants; freedom_primary flips receiving_states from beneficiaries to targets and expands the victim set to nearly all excluded movers.',
    'The victim set, the directionality of receiving_states, and the per-seat classifications all rotate with the prevailing reading; this story''s epsilon is valid only for the qualified-sovereignty referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega recording that this file instantiates one reading of a contested kernel and naming the sibling deltas.').

omega_variable(
    margin_of_appreciation_variance,
    'Is the proportionality standard''s bite stable, or does state deference expand systematically during declared crises (2015 arrivals, pandemic closures, 2022 displacement events), making effective extraction crisis-indexed?',
    'Panel-level analysis of court outcomes across crisis and non-crisis periods: compare strike-down rates and deference language in removal and detention judgments before, during, and after declared emergencies.',
    'If deference is systematically crisis-indexed, the standing epsilon understates extraction during exactly the periods of largest flows, and the theater_ratio should be read as cyclical rather than stationary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(margin_of_appreciation_variance, empirical, 'Whether proportionality review tightens or loosens under crisis conditions.').

omega_variable(
    externalization_responsibility_traceability,
    'Does legal responsibility follow enforcement across externalization arrangements (offshore processing, third-country interception deals), or does the chain of custody break at the border of the partner state?',
    'Track litigation outcomes on extraterritorial jurisdiction (sea pushback cases versus inland partner-state arrangements) and measure whether remedy rates diverge between internal and externalized enforcement.',
    'If responsibility does not trace, the authored theater_ratio understates the gap between formal compliance and actual practice, and effective extraction concentrates on people in transit zones where review is weakest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_responsibility_traceability, empirical, 'Whether accountability follows outsourced border enforcement.').

omega_variable(
    displaced_persons_coverage_boundary,
    'Do people displaced by climate disaster or economic collapse count within this reading''s victim set, or does the proportionality framework''s protection extend only to the persecution-based categories inherited from the 1951 settlement?',
    'Doctrinal analysis of whether necessity-based claims survive proportionality review in any forum, plus legislative developments creating complementary protection categories.',
    'If coverage stays persecution-bound, the displaced_citizens victim group is larger than the framework acknowledges and the measured extraction understates the population bearing costs; if coverage expands, part of the current victim load converts into beneficiary status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_persons_coverage_boundary, conceptual, 'Boundary of the protected class under the qualified reading.').

omega_variable(
    adjudication_burden_character,
    'Is the compliance and litigation burden this standard places on states a legitimate price of rights-answerable exclusion, or friction the system tolerates and states recoup elsewhere (deterrence design, procedural delay)?',
    'Value-theoretic analysis combined with behavioral evidence: whether states treat adverse judgments as binding corrections or as pricing inputs absorbed into deterrence strategy.',
    'If the burden is recouped through deterrence design, receiving_states sit closer to the target end than their beneficiary declaration suggests, and the seat divergence between states and migrants narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudication_burden_character, preference, 'How the state-side cost of the standard should be characterized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 1951, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_normative_status__qualified_sovereignty, theater_ratio, 1951, 0.15).
narrative_ontology:measurement(bord_tr_t1967, border_normative_status__qualified_sovereignty, theater_ratio, 1967, 0.16).
narrative_ontology:measurement(bord_tr_t1985, border_normative_status__qualified_sovereignty, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(bord_tr_t1999, border_normative_status__qualified_sovereignty, theater_ratio, 1999, 0.24).
narrative_ontology:measurement(bord_tr_t2011, border_normative_status__qualified_sovereignty, theater_ratio, 2011, 0.28).
narrative_ontology:measurement(bord_tr_t2015, border_normative_status__qualified_sovereignty, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(bord_tr_t2020, border_normative_status__qualified_sovereignty, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(bord_tr_t2025, border_normative_status__qualified_sovereignty, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_normative_status__qualified_sovereignty, base_extractiveness, 1951, 0.22).
narrative_ontology:measurement(bord_be_t1967, border_normative_status__qualified_sovereignty, base_extractiveness, 1967, 0.26).
narrative_ontology:measurement(bord_be_t1985, border_normative_status__qualified_sovereignty, base_extractiveness, 1985, 0.31).
narrative_ontology:measurement(bord_be_t1999, border_normative_status__qualified_sovereignty, base_extractiveness, 1999, 0.36).
narrative_ontology:measurement(bord_be_t2011, border_normative_status__qualified_sovereignty, base_extractiveness, 2011, 0.42).
narrative_ontology:measurement(bord_be_t2015, border_normative_status__qualified_sovereignty, base_extractiveness, 2015, 0.51).
narrative_ontology:measurement(bord_be_t2020, border_normative_status__qualified_sovereignty, base_extractiveness, 2020, 0.49).
narrative_ontology:measurement(bord_be_t2025, border_normative_status__qualified_sovereignty, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_normative_status__qualified_sovereignty, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement(bord_su_t1967, border_normative_status__qualified_sovereignty, suppression_requirement, 1967, 0.32).
narrative_ontology:measurement(bord_su_t1985, border_normative_status__qualified_sovereignty, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(bord_su_t1999, border_normative_status__qualified_sovereignty, suppression_requirement, 1999, 0.43).
narrative_ontology:measurement(bord_su_t2011, border_normative_status__qualified_sovereignty, suppression_requirement, 2011, 0.48).
narrative_ontology:measurement(bord_su_t2015, border_normative_status__qualified_sovereignty, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(bord_su_t2020, border_normative_status__qualified_sovereignty, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(bord_su_t2025, border_normative_status__qualified_sovereignty, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate border control' decomposes into three structurally distinct readings of the border_normative_status kernel, each with its own epsilon, victim set, and classification. This file (qualified_sovereignty) is the operative legal standard; it sits downstream of the postwar settlement's empirical premise (unreviewed exclusion produced catastrophic refoulement) and exerts structural influence on both siblings: sovereignty_primary defenders must now argue in the language of proportionality compliance, and freedom_primary advocates litigate their claims through this reading's adjudication machinery. The sibling files document the reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
