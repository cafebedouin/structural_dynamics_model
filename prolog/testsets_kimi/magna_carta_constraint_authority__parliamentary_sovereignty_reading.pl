% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta Constraint Authority â Parliamentary Sovereignty Reading
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary sovereignty reading
 *   of the Magna Carta constraint authority kernel. Under this reading, Magna
 *   Carta's restraints on the Crown survive only insofar as they have been
 *   absorbed into ordinary parliamentary statute law. Parliament inherits the
 *   authority to constrain the Crown but also possesses the unlimited power
 *   to revise or repeal any charter provision. The constraint is therefore a
 *   tangled rope: it coordinates lawful restraint on arbitrary executive
 *   power, but simultaneously extracts from minorities and dissenters who
 *   lack protection against majoritarian repeal.
 *
 * KEY AGENTS:
 *   - uk_parliament (agenda_setter/institutional) â holds inherited constraint authority and controls revision
 *   - parliamentary_majorities (beneficiary/powerful) â exercise unconstrained lawmaking capacity
 *   - minorities_unprotected (payer/powerless) â bear cost of absent entrenched rights
 *   - crown_executive (payer/powerful) â prerogative constrained contingent on parliamentary will
 *   - uk_judiciary (observer/institutional) â interprets but cannot override parliamentary statute
 *   - rights_advocates (excluded/organized) â argue for entrenched review but lack institutional traction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.48).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Constraint Authority â Parliamentary Sovereignty Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'e05424b2-2091-41cd-bc09-ebc37b68e2b7').
narrative_ontology:cs_kernel_codification('e05424b2-2091-41cd-bc09-ebc37b68e2b7', fixed_text).
narrative_ontology:cs_authority_grounding('e05424b2-2091-41cd-bc09-ebc37b68e2b7', lineage).
narrative_ontology:cs_interpretation_layer_present('e05424b2-2091-41cd-bc09-ebc37b68e2b7').
narrative_ontology:cs_reading_relation('e05424b2-2091-41cd-bc09-ebc37b68e2b7', magna_carta_constraint_authority__living_constitutionalism_reading, influences).
narrative_ontology:cs_reading_relation('e05424b2-2091-41cd-bc09-ebc37b68e2b7', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_axiom('e05424b2-2091-41cd-bc09-ebc37b68e2b7', foundational, parliamentary_sovereignty_inherited).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_inherited, holdable).
narrative_ontology:cs_axiom_grounding('e05424b2-2091-41cd-bc09-ebc37b68e2b7', parliamentary_sovereignty_inherited, conventional).
narrative_ontology:cs_axiom('e05424b2-2091-41cd-bc09-ebc37b68e2b7', foundational, no_entrenched_legal_restraint).
narrative_ontology:cs_axiom_status(no_entrenched_legal_restraint, holdable).
narrative_ontology:cs_axiom_grounding('e05424b2-2091-41cd-bc09-ebc37b68e2b7', no_entrenched_legal_restraint, conventional).
narrative_ontology:cs_reference_frame('e05424b2-2091-41cd-bc09-ebc37b68e2b7', westminster_sovereignty_framework).
narrative_ontology:cs_drift_state('e05424b2-2091-41cd-bc09-ebc37b68e2b7', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e05424b2-2091-41cd-bc09-ebc37b68e2b7', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minorities_unprotected).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majoritarian_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherits Magna Carta's historical constraint authority over the Crown but holds it as ordinary statutory power; can revise or repeal any charter provision by simple majority legislation. No external legal body can invalidate its enactments.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_parliament, agenda_setter,
    institutional, generational, constrained, national).

% Control the legislative agenda and enjoy unconstrained lawmaking capacity subject only to political accountability; benefit from the absence of entrenched constitutional limits that would block majoritarian preferences.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majorities, beneficiary,
    powerful, biographical, mobile, national).

% Lack structural protection when parliamentary majorities decline to encode, or choose to repeal, Magna Carta-derived procedural safeguards; exposed to majoritarian legislation without judicial override or entrenched rights review.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minorities_unprotected, payer,
    powerless, biographical, trapped, national).

% Historical royal prerogative is constrained by charter provisions absorbed into statute, but those constraints exist only at parliamentary pleasure; modern executive authority is delegated and contingent rather than inherent.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, crown_executive, payer,
    powerful, biographical, constrained, national).

% Interprets statutes and acknowledges Magna Carta solely as incorporated into positive law; cannot enforce charter provisions or common-law rights against express parliamentary enactment.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_judiciary, observer,
    institutional, generational, constrained, national).

% Argue for entrenched fundamental rights and judicial review of primary legislation; structurally sidelined by the doctrine that Parliament alone is the guardian of inherited constitutional restraints.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, rights_advocates, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majorities).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels restraint on arbitrary executive power through a single, electorally accountable legislature rather than leaving it to feudal baronial compact or royal self-limitation, solving the coordination problem of who authoritatively interprets and updates the constraint.
% TRANSFER_FUNCTION: Moves constraint authority from the Crown to Parliament; moves vulnerability from the Crown to minorities and dissenters when Parliament chooses not to preserve or chooses to repeal charter-derived protections.
% ABSENT_VOICES: Minorities without effective parliamentary representation; future generations who cannot bind present legislatures; rights advocates arguing for entrenched judicial review; devolved nations whose autonomy rests on revocable statutes.
% DISAPPEARANCE_RATIONALE: If Parliament lost this inherited constraint authority, the UK constitutional order would shift toward either entrenched rights with judicial supremacy or unchecked executive prerogative; the existing balance between Crown, Parliament, and courts would reorganize.
% FOUNDING_PROBLEM: Baronial revolt against arbitrary royal imprisonment, taxation, and forest encroachment; establishing that the Crown is subject to lawful restraint and consent.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians attest the original feudal grievance is obsolete. Parliamentary sovereignty theorists (e.g., Dicey) attest the problem transformed into ensuring democratic rather than royal accountability. Rights advocates and jurists outside the majority-party beneficiary set attest the transformed arrangement now fails to protect against majoritarian overreach, citing periodic suspensions of protections and absence of entrenched review.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint genuinely restrains arbitrary Crown action â a real coordination function â but simultaneously leaves minorities exposed to majoritarian repeal. Suppression (0.48) is moderate: alternatives such as entrenched judicial review or constitutional supremacy are not violently suppressed but are politically and doctrinally blocked by parliamentary sovereignty orthodoxy. Theater ratio (0.50) reflects heavy symbolic maintenance of Magna Carta mythology while actual constraint authority has migrated to statute and majority will. Accessibility collapse (0.45) captures that legal alternatives exist in comparative constitutional models but are institutionally inaccessible within the Westminster framework. Resistance (0.38) is present but diffuse, coming from rights advocates and minority communities without veto power.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (parliamentary_majorities) experiences this constraint as genuine coordination: a inherited framework that channels popular will into lawful authority and prevents executive overreach. The payer seats (minorities_unprotected, crown_executive) experience the same structure as contingent and extractive: their protections exist only so long as a parliamentary majority chooses to maintain them. The judiciary, operating under the same doctrine, computes as an observer because it cannot resolve the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliamentary majorities are the structural beneficiaries: the constraint vests them with unlimited revisory power over historical restraints, yielding low directionality. Minorities unprotected by majoritarian legislation are the primary victims: the constraint's architecture exposes them to repeal without recourse, yielding high directionality. The Crown occupies an intermediate position â its prerogative is constrained, but that constraint is itself a weapon Parliament holds.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as tangled rope prevents the mandatrophy error of treating parliamentary sovereignty as either a pure mountain (inevitable constitutional logic) or a pure snare (majoritarian tyranny). The coordination function â lawful restraint on arbitrary executive power â is real and historically significant. The extraction â minority vulnerability to repeal â is simultaneously real and structurally asymmetric. Neither reading alone captures the constraint; the engine measures the coexistence of both functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the parliamentary sovereignty reading the only coherent modern instantiation of Magna Carta authority, or do the living constitutionalism and feudal obsolescence readings describe structurally distinct constraints that should be modeled separately?',
    'Corpus-level family analysis comparing epsilon, beneficiary/victim structures, and network coupling across the three sibling constraints.',
    'If sibling readings are structurally distinct constraints with non-overlapping victim sets and different epsilon profiles, the kernel decomposition is validated; if they collapse into similar metrics, the readings may be observer-axis variations rather than distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committing omega: this constraint is one reading of a contested kernel; sibling readings may instantiate different constraints.').

omega_variable(
    majoritarian_extraction_vs_democratic_accountability,
    'Does the absence of entrenched rights under parliamentary sovereignty constitute structural extraction from minorities, or merely the democratically legitimate price of popular self-government?',
    'Comparative analysis of minority protection outcomes across Westminster and entrenched-rights systems; historical instances of majoritarian repeal or suspension of Magna Carta-derived protections.',
    'If minorities systematically suffer repeal of protections that comparable entrenched systems preserve, the extraction reading is supported; if outcomes are statistically similar, the coordination (democratic accountability) reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_extraction_vs_democratic_accountability, preference, 'Whether the constraint''s majoritarian vulnerability is extraction or legitimate democratic cost.').

omega_variable(
    crown_contingent_constraint,
    'Is the Crown''s contingent constraint under parliamentary sovereignty a genuine victimhood, or has the modern executive effectively captured parliamentary majorities and thereby the constraint authority itself?',
    'Analysis of executive dominance metrics (whip control, delegated legislation volume, prerogative reform blockage) to determine whether the Crown/executive is genuinely constrained or has merged with the beneficiary seat.',
    'If the executive captures majorities, the victim set shrinks and the constraint moves toward snare; if executive and majority remain distinct, the tangled rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crown_contingent_constraint, empirical, 'Whether the Crown is a distinct payer or has merged with the parliamentary beneficiary seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(magn_tr_t20, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(magn_tr_t40, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(magn_tr_t60, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 60, 0.43).
narrative_ontology:measurement(magn_tr_t80, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 80, 0.47).
narrative_ontology:measurement(magn_tr_t100, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(magn_be_t20, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(magn_be_t40, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(magn_be_t60, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(magn_be_t80, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(magn_be_t100, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(magn_su_t20, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(magn_su_t40, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(magn_su_t60, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(magn_su_t80, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 80, 0.47).
narrative_ontology:measurement(magn_su_t100, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Magna Carta constraint authority kernel. The parliamentary sovereignty reading decomposes the natural-language 'Magna Carta' into a specific structural claim: charter restraints survive only as revocable parliamentary statute. Sibling readings instantiate different epsilon values and different stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
