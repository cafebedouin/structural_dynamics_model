% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_feudal_obsolescence, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta Authority Structure (Feudal Obsolescence Reading)
 *   domain: constitutional_history/legal_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the feudal-obsolescence reading of
 *   Magna Carta's constitutional authority. Under this reading, Magna Carta
 *   is a 13th-century baronial compact addressing feudal-era grievances
 *   (feudal incidents, arbitrary feudal justice, feudal taxation)—specific to
 *   that social structure and binding only on feudal relationships. Because
 *   feudalism has dissolved, the reading asserts Magna Carta has no binding
 *   force over modern sovereignty structures or executive authority. The
 *   feudal-obsolescence reading functions as a doctrinal clearing mechanism:
 *   it removes Magna Carta from the active restraint toolkit, allowing
 *   executive discretion to expand without historical legitimacy claims. The
 *   measurement series show extractiveness and theater rising over time: the
 *   reading's strategic use intensifies as competing constitutional authority
 *   claims (popular constitutionalism, juridical precedent-based restraint)
 *   strengthen.
 *
 * KEY AGENTS:
 *   - Executive discretion maximalists — institutional agenda-setters administering the feudal-obsolescence reading; benefit from freedom from historical restraint claims
 *   - Popular constitutionalism advocates — organized payers seeking to invoke Magna Carta as living restraint; marginalized by the obsolescence frame
 *   - Juridical restraint defenders — powerful payers (judges, constitutional scholars) whose tools are undermined when Magna Carta is declared feudal-obsolete
 *   - Parliamentary sovereignty interpreters — institutionally positioned between the reading's benefits (cleared field of competing historical restraints) and costs (denial of inherited constitutional duty)
 *   - Monarchy/executive authority — institutional beneficiary; released from historical restraint claims by the reading's doctrine
 *   - Academic observer — scholars analyzing whether feudal-obsolescence accurately represents operative authority or functions as cover story
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.72).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta Authority Structure (Feudal Obsolescence Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__feudal_obsolescence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'edb9e15e-3446-4a6b-808f-be90bb8c4a9d').
narrative_ontology:cs_kernel_codification('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', fixed_text).
narrative_ontology:cs_authority_grounding('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', extraction).
narrative_ontology:cs_interpretation_layer_present('edb9e15e-3446-4a6b-808f-be90bb8c4a9d').
narrative_ontology:cs_reading_relation('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', foundational, feudal_governance_structurally_obsolete).
narrative_ontology:cs_axiom_status(feudal_governance_structurally_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', feudal_governance_structurally_obsolete, empirically_contingent).
narrative_ontology:cs_axiom('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', foundational, context_dependent_law_dies_with_context).
narrative_ontology:cs_axiom_status(context_dependent_law_dies_with_context, holdable).
narrative_ontology:cs_axiom_grounding('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', context_dependent_law_dies_with_context, deontological).
narrative_ontology:cs_reference_frame('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', feudal_text_feudal_authority).
narrative_ontology:cs_drift_state('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', post_enlightenment_modern_sovereignty_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('edb9e15e-3446-4a6b-808f-be90bb8c4a9d', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_discretion_maximalists).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_sovereignty_apparatus).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_interpreters).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, monarchy_and_executive_authority).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_interpreters).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, feudal_governance_obsolescence).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_prerogative_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and defend the doctrine that Magna Carta was a feudal-era compact binding only on 13th-century baronial relationships and has no force over modern executive authority. They administer this interpretation through executive practice, judicial nominees who adopt it, and legislative doctrine. They benefit from the freedom this reading provides to exercise discretionary power without historical restraint claims.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_discretion_maximalists, agenda_setter,
    institutional, generational, arbitrage, national).

% Seek to invoke Magna Carta as a living restraint on executive power through popular sovereignty doctrine. They argue the charter's principles are inherited through the people and bind all rulers. They are marginalized when the feudal-obsolescence reading dominates: their restraint appeals are ruled out of order as historically anachronistic, and they bear the cost of losing an inherited legitimacy frame for their constitutional arguments.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates, payer,
    organized, generational, constrained, national).

% Include judges, constitutional scholars, and legal institutions that believe prior written compacts should bind subsequent rulers through precedent and interpretation. They pay by losing a canonical text foundation for restraint doctrine when Magna Carta is declared feudal-obsolete. Their principal tool—historical authority—is removed from the toolkit.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_defenders, payer,
    powerful, generational, constrained, national).

% Hold that Parliament inherits and supersedes Magna Carta authority through statute law. They benefit from the feudal-obsolescence reading insofar as it clears the field of competing historical restraints; they pay if the reading also undermines parliamentary legitimacy by denying any inherited constitutional duty. Their position is unstable under this reading.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_interpreters, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_interpreters, payer).

% As the de facto administrator of executive discretion, the monarchy/executive benefits from the feudal-obsolescence reading by being released from historical restraint claims. The reading functions as a legitimacy clearance for prerogative power: the reading asserts Magna Carta was feudal law, therefore modern executive discretion is not bound by it.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, monarchy_and_executive_authority, beneficiary,
    institutional, civilizational, arbitrage, national).

% Historical and constitutional scholars analyzing whether the feudal-obsolescence reading accurately represents Magna Carta's operative authority in modern law, or whether it is a cover story for executive power maximization.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, academic_observer_seat, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_discretion_maximalists).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies which historical texts bind modern rulers: the feudal-obsolescence reading asserts that feudal-era compacts bind only feudal relationships and are displaced by modern sovereignty structures. This provides a stable rule for interpreting when historical claims on authority are legitimate and when they are superseded.
% TRANSFER_FUNCTION: Transfers legitimacy authority from inherited historical texts (Magna Carta, popular sovereignty doctrine, juridical precedent) to modern executive discretion and statutory law. The reading moves the ground of legitimate restraint away from 'what the charter says' toward 'what contemporary authority decides'.
% ABSENT_VOICES: Living constitutionalists and juridical restraint scholars who would argue that Magna Carta's principles transcend feudal context and remain binding through interpretation and precedent. They are excluded from this reading's framework by the very premise of feudal obsolescence. Populist movements invoking Magna Carta against executive power are systematically delegitimized as appealing to dead law.
% DISAPPEARANCE_RATIONALE: If the feudal-obsolescence reading disappeared overnight, Magna Carta would re-enter debate as a potential restraint on executive authority (competing readings would resurface). Executive discretion would face renewed historical legitimacy challenges. The constraint's disappearance would rearrange the interpretive landscape of constitutional authority, though modern executive institutions would likely adopt alternative legitimacy framings (pragmatism, efficiency, democratic delegation) rather than reverting to feudal-era authority.
% FOUNDING_PROBLEM: The problem Magna Carta was built to solve: specific baronial grievances against 13th-century feudal exploitation—excessive feudal incidents, arbitrary justice, arbitrary taxation within feudal hierarchy. The charter addressed these by stipulating specific duties of the king to barons and establishing a procedure for enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Historians and medieval scholars (outside the executive authority apparatus) agree that the specific feudal grievances Magna Carta addressed—feudal incidents, feudal wardship abuse, feudal justice procedures—are no longer operative in modern legal systems. The feudal relationship itself has dissolved. However, competing readings dispute whether this death of the founding problem means Magna Carta's principles are obsolete (feudal-obsolescence reading) or whether the principles evolved to address new forms of tyranny (living constitutionalism reading). Academic historians provide the corroboration that the original feudal grievances are dead; the executive-maximalist seat provides only that first half, while denying principle evolution.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 final) and rising because the feudal-obsolescence reading progressively removes constraint sources from the arena: each time the reading is successfully deployed to disqualify a historical restraint claim, executive discretion expands. Suppression is also high (0.72) because the reading must actively suppress competing historical authority frameworks—without continuous suppression effort, living constitutionalism or juridical restraint readings would resurface. Theater is high and rising (0.58 final, from 0.35 at t0) because much of the enforcement activity is doctrinal performance: ceremonial affirmations that feudalism is dead and therefore Magna Carta is dead, repeated often enough to maintain the fiction. The rising theater trajectory marks the constraint as a piton: the original coordination problem (clarifying which historical texts bind modern rulers) has atrophied, but the doctrinal structure persists through theatrical maintenance. The measurements are aligned on a single time grid: every metric is authored at every examined point (t=0,10,20,30,40,50).
 *
 * PERSPECTIVAL GAP:
 *   From the executive-maximalist seat, the feudal-obsolescence reading is genuine constitutional clarification: it accurately describes Magna Carta as a feudal-era compact and therefore correctly assigns it no authority over modern governance. From this seat, the constraint computes as low-extraction coordination (clarifying authority boundaries). From the popular-constitutionalist and juridical-restraint seats, the same reading is experienced as doctrinal suppression: a deliberate redefinition of Magna Carta to strip it of restraint force. From these seats, the constraint computes as high-extraction snare (suppressing alternative authority frameworks). The engine computes this divergence from the structural data—beneficiary vs. victim positioning, exit options, power atoms—without needing to adjudicate whose reading is 'correct'.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive-discretion-maximalist seat sits at near-zero directionality (d→0.0, beneficiary): the reading benefits them by removing historical restraint claims. The popular-constitutionalism seat sits at high directionality (d→1.0, target): the reading extracts from them by delegitimizing their constitutional vocabulary. The juridical-restraint seat also sits high (d→0.85, target): their tool (historical textual authority) is removed from the toolkit. The parliamentary-sovereignty seat is unstable and dual-positioned: they benefit from the feudal-obsolescence clearing (competing historical restraints removed), but they pay if the reading also undermines the inherited authority that would justify parliamentary supremacy. They occupy d≈0.50, symmetric. The monarchy/executive benefits unconditionally (d→0.0). Directionality overrides are not needed: the structural data (beneficiary/victim + exit + power) produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The feudal-obsolescence reading exhibits classic mandatrophy (mandate outliving function). The original mandate—clarifying that feudal-era compacts bind only feudal relationships—is genuine and was arguably useful when feudalism's dissolution was contested (14th-18th centuries). But by the 20th-21st century, no serious scholar disputes that feudalism is dead; the clarification is accomplished and the mandate is satisfied. Yet the doctrinal reading persists and intensifies, not because it continues to solve a coordination problem, but because it functions to maximize executive discretion. The measurement series capture mandatrophy in the rising theater_ratio: as the clarification problem becomes dead (founding_problem_status: dead), more effort goes into theatrical maintenance (doctrinal repetition) and less into functional work. This marks the constraint as a piton: atrophied mandate, persistent structure, high performance overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_obsolescence_vs_principle_continuity,
    'Is Magna Carta''s authority genuinely defunct because feudalism is defunct, or do its principles (rule of law, due process, limited authority) transcend feudal context and remain binding through interpretation and evolution?',
    'Comparative constitutional analysis: do other historical texts (Habeas Corpus, Bill of Rights) considered binding by the same authority also dissolve when their original context becomes obsolete? Or do courts and legislatures selectively declare some historical texts dead (Magna Carta) while preserving others (Habeas Corpus)? Selective application reveals whether feudal-obsolescence is principle or pretext.',
    'If principle continuity holds (principles evolve but bind across eras), then feudal-obsolescence is a strategic reading that sacrifices legitimate restraint to maximize discretion—a snare. If true obsolescence (context-bound law dies with its context), then feudal-obsolescence is genuine clarification—coordination. The classification swings from snare toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_obsolescence_vs_principle_continuity, conceptual, 'Whether feudal obsolescence of context implies obsolescence of principle.').

omega_variable(
    beneficiary_identity_under_reading_collapse,
    'If the feudal-obsolescence reading is abandoned and Magna Carta re-enters debate as a restraint on executive power, who would be identified as the constraint''s beneficiary? Does the beneficiary shift reveal whether executive discretion is the actual beneficiary today, or whether the reading merely redirects the benefit-flow?',
    'Hypothetical scenario analysis: if a new constitutional movement successfully reinstates living constitutionalism as the dominant reading, what institutional actors would gain? Do they overlap with today''s feudal-obsolescence beneficiaries, or are they a different set?',
    'If the beneficiary set shifts entirely (living constitutionalism benefits popular-sovereignty advocates, not executives), then executive discretion is the true current beneficiary of feudal-obsolescence, not a side effect. This would elevate the constraint''s classification toward snare (deliberate extraction via doctrinal reframing). If beneficiaries remain the same, the reading is more genuinely coordinative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_under_reading_collapse, conceptual, 'Whether beneficiary identity under competing readings reveals extraction vs. coordination.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of popular-constitutionalism and juridical-restraint voices structural (they are excluded from courts, legislatures, executive doctrine-setting by institutional design) or internalized (they accept the feudal-obsolescence premise and self-suppress their historical claims)?',
    'Post-suppression trajectory: if the feudal-obsolescence reading is suddenly abandoned (in a jurisdiction that adopts living constitutionalism), do these voices immediately re-emerge with full force, or do they remain suppressed? If they re-emerge, suppression was structural; if they remain muted, suppression is internalized (embedded in scholarly training, professional norms, accepted jurisprudence).',
    'If suppression is structural, exit is constrained and the constraint''s effective extraction is as measured. If suppression is internalized, the constraint''s effective extraction is higher than measured—the targets carry the suppression with them even after the feudal-obsolescence reading is removed, because it has become cognitive canon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of restraint-doctrine voices is structural or internalized.').

omega_variable(
    alternative_legitimacy_ground_if_feudal_obsolescence_collapses,
    'If the feudal-obsolescence reading is abandoned, does executive discretion persist via different legitimacy grounds (pragmatism, efficiency, democratic delegation) or would constitutional restraint claims genuinely constrain modern executive power?',
    'Historical analysis: do modern executives in jurisdictions that reject feudal-obsolescence (e.g., those with living constitutionalism traditions) actually face measurable restraint from Magna Carta invocations, or do they invoke alternative legitimacy grounds and operate the same way? Empirical observation of executive behavior under competing readings.',
    'If executives persist via alternative grounds, feudal-obsolescence is merely one legitimacy vector among several—removing it does not constrain power. If Magna Carta invocations actually constrain (as competitors claim), then feudal-obsolescence is a key extraction mechanism. If intermediate (some constraint effect, but not decisive), the reading''s strategic value is moderate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_legitimacy_ground_if_feudal_obsolescence_collapses, empirical, 'Whether abandoning feudal-obsolescence would generate measurable executive restraint or executives would simply adopt alternative legitimacy framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t10, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(magn_tr_t10, observed).
narrative_ontology:measurement(magn_tr_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(magn_tr_t20, observed).
narrative_ontology:measurement(magn_tr_t30, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 30, 0.54).
narrative_ontology:measurement_basis(magn_tr_t30, observed).
narrative_ontology:measurement(magn_tr_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 40, 0.57).
narrative_ontology:measurement_basis(magn_tr_t40, observed).
narrative_ontology:measurement(magn_tr_t50, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement_basis(magn_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t10, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(magn_be_t10, observed).
narrative_ontology:measurement(magn_be_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(magn_be_t20, observed).
narrative_ontology:measurement(magn_be_t30, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(magn_be_t30, observed).
narrative_ontology:measurement(magn_be_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(magn_be_t40, observed).
narrative_ontology:measurement(magn_be_t50, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(magn_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t10, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(magn_su_t10, observed).
narrative_ontology:measurement(magn_su_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(magn_su_t20, observed).
narrative_ontology:measurement(magn_su_t30, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(magn_su_t30, observed).
narrative_ontology:measurement(magn_su_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(magn_su_t40, observed).
narrative_ontology:measurement(magn_su_t50, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(magn_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.08).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel magna_carta_constraint_authority. The kernel is the stabilized text of Magna Carta and its operative authority in modern law. The feudal_obsolescence_reading asserts Magna Carta is feudal-era law with no binding force on modern executive authority. Sibling readings (living_constitutionalism_reading, parliamentary_sovereignty_reading) would instantiate different constraints from the same kernel, with different beneficiary/victim structures and different computed types. Each reading is authored as a separate constraint story with its own ε, beneficiaries, victims, and classification. The three stories are linked via network.affects_constraints to enable corpus analysis of how constraint readings diverge on the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
