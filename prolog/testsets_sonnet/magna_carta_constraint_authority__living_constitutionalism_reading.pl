% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta as Living Constitutional Restraint (evolutionary due-process reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading among three contested readings of the
 *   Magna Carta kernel: the living-constitutionalism reading, which holds
 *   that the 1215 charter established an inherited principle of lawful
 *   restraint on sovereign power that binds all subsequent rulers through
 *   juridical precedent and evolutionary interpretation. Under this reading,
 *   royal prerogative and executive discretion enter the victim set (their
 *   arbitrary latitude is narrowed), subjects gain a due-process shield, and
 *   the constraint functions as a genuine coordination mechanism (rope)
 *   around inherited restraint, with low-to-moderate extractiveness
 *   reflecting the real but modest ongoing cost imposed on executive actors.
 *   This reading is generated independently of its siblings
 *   (feudal_obsolescence_reading, which denies binding modern authority
 *   entirely, and parliamentary_sovereignty_reading, which relocates
 *   constraint authority to Parliament's revisable statute law) — per the
 *   ε-invariance principle, each reading is a structurally distinct
 *   constraint with its own stable epsilon, not a measurement of the same
 *   constraint from a different angle.
 *
 * KEY AGENTS:
 *   - subjects_and_citizens: primary beneficiary (powerless/trapped) — protected by the inherited due-process shield but cannot personally invoke it
 *   - common_law_judiciary: agenda_setter (institutional/analytical) — carries and extends the doctrine through precedent
 *   - constitutional_courts: agenda_setter/beneficiary (institutional/analytical) — applies the doctrine's interpretive authority
 *   - royal_prerogative_holders: primary target (powerful/constrained) — bound by restraint that narrows historical unbounded power
 *   - executive_discretion_actors: primary target (powerful/constrained) — bears ongoing cost of judicial review descended from the doctrine
 *   - legal_historians: analytical observer — assesses whether the doctrine's continuity is genuine inheritance or retrospective construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.22).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta as Living Constitutional Restraint (evolutionary due-process reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '52230f54-916a-4a62-b252-047015a34ef9').
narrative_ontology:cs_kernel_codification('52230f54-916a-4a62-b252-047015a34ef9', fixed_text).
narrative_ontology:cs_authority_grounding('52230f54-916a-4a62-b252-047015a34ef9', lineage).
narrative_ontology:cs_interpretation_layer_present('52230f54-916a-4a62-b252-047015a34ef9').
narrative_ontology:cs_reading_relation('52230f54-916a-4a62-b252-047015a34ef9', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('52230f54-916a-4a62-b252-047015a34ef9', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('52230f54-916a-4a62-b252-047015a34ef9', foundational, charter_binds_through_unbroken_juridical_lineage).
narrative_ontology:cs_axiom_status(charter_binds_through_unbroken_juridical_lineage, holdable).
narrative_ontology:cs_axiom_grounding('52230f54-916a-4a62-b252-047015a34ef9', charter_binds_through_unbroken_juridical_lineage, conventional).
narrative_ontology:cs_axiom('52230f54-916a-4a62-b252-047015a34ef9', secondary, restraint_authority_independent_of_legislative_will).
narrative_ontology:cs_axiom_status(restraint_authority_independent_of_legislative_will, holdable).
narrative_ontology:cs_axiom_grounding('52230f54-916a-4a62-b252-047015a34ef9', restraint_authority_independent_of_legislative_will, conventional).
narrative_ontology:cs_reference_frame('52230f54-916a-4a62-b252-047015a34ef9', baronial_charter_as_living_precedent).
narrative_ontology:cs_drift_state('52230f54-916a-4a62-b252-047015a34ef9', post_habeas_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('52230f54-916a-4a62-b252-047015a34ef9', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_and_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_courts).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_holders).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion_actors).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, no_punishment_without_lawful_judgment).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_binds_the_sovereign).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals subject to state power who benefit from the inherited principle that no one, including the sovereign, may imprison or dispossess them except by lawful judgment. They cannot personally invoke or enforce the charter's descendants without courts and counsel, but the doctrine's persistence shapes every arrest, seizure, and trial they might face.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_and_citizens, beneficiary,
    powerless, civilizational, trapped, national).

% Courts that carry forward and reinterpret the due-process principle across centuries, extending it to new contexts (habeas corpus, administrative detention, procedural fairness) through precedent. They administer the doctrine's evolutionary interpretation and decide, case by case, how far it restrains executive action.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Higher courts and constitutional bodies that draw on the Magna Carta lineage as interpretive authority when adjudicating disputes between citizens and the state. Their institutional standing is reinforced by the perceived antiquity and continuity of the restraint they apply.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_courts, beneficiary).

% Historical monarchs and their modern executive successors (heads of state, crown authority) who are bound by the inherited restraint against arbitrary detention and seizure. What was once unbounded prerogative power is narrowed by juridical precedent that traces its legitimacy to the charter; they cannot simply declare the restraint void.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_holders, payer,
    powerful, generational, constrained, national).

% Modern executive agencies, ministers, and security services whose discretionary powers (detention, seizure, emergency action) are checked by due-process doctrine descended from the charter. They experience judicial review and habeas challenges as a cost of governing under this inherited restraint, and periodically seek to narrow its reach through emergency legislation or executive assertion.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion_actors, payer,
    powerful, biographical, constrained, national).

% Scholars who trace how a 13th-century baronial settlement became cited as foundational authority across common-law jurisdictions, assessing whether the doctrine's continuity is genuine juridical inheritance or retrospective myth-construction serving present institutions.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, cross-generational baseline restraining arbitrary state power against individuals — a shared reference point that lets courts, legislators, and citizens coordinate expectations about lawful process without renegotiating first principles in every dispute.
% TRANSFER_FUNCTION: Moves discretionary latitude away from the sovereign/executive and toward judicial process and the individual subject: what would otherwise be unchecked prerogative becomes reviewable action, at the cost of executive speed and flexibility.
% ABSENT_VOICES: The original barons who extracted the 1215 charter for their own feudal interests are absent from this reading's beneficiary framing — the living-constitutionalism reading repurposes their instrument for a universalist due-process claim they did not intend. Contemporary executive actors who view the doctrine as anachronistic overreach are represented here as payers but would object more sharply if given voice.
% DISAPPEARANCE_RATIONALE: If the inherited due-process restraint vanished, executive and sovereign power over detention and seizure would lose its primary juridical check in common-law systems; courts would need to construct entirely new doctrinal foundations for restraining arbitrary state action, and the predictability that citizens and institutions currently rely on would collapse into ad hoc adjudication.
% FOUNDING_PROBLEM: In 1215, rebellious barons sought to check King John's arbitrary taxation, seizure of property, and extralegal punishment of feudal tenants — a narrow problem of baronial protection from royal overreach.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and common-law judiciaries (direct institutional beneficiaries) attest the founding problem has been generalized and remains live as a universal due-process principle. Legal historians, standing outside the beneficiary set, corroborate that the original founding problem (baronial feudal grievance) was narrow and specific, and that its extension to a universal citizen-versus-state restraint is a much later doctrinal construction — the corroboration is partial, not full, on the living-constitutionalism claim of continuous binding authority.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.22 at present) because the coordination function (predictable restraint on arbitrary state power) is genuine and the cost to executive/prerogative actors is real but bounded — judicial review is a friction cost, not wholesale expropriation. Suppression is moderate and has DECLINED over the measured interval (0.35 to 0.28) as the doctrine has become normalized rather than requiring active baronial-style coercion to maintain; enforcement now runs through ordinary judicial process rather than armed confrontation. Theater ratio is low but has crept upward slightly (0.10 to 0.20) reflecting some increase in ceremonial invocation of Magna Carta's 800th-anniversary symbolism relative to its operative doctrinal content. All three metrics share one time grid across 1215-2025.
 *
 * DIRECTIONALITY LOGIC:
 *   Subjects and citizens are the diffuse beneficiary class — the doctrine subsidizes their protection from arbitrary state action, though they rarely invoke it directly (low d). Common-law judiciary and constitutional courts are agenda-setters who administer and extend the doctrine, benefiting institutionally from the interpretive authority it grants them. Royal prerogative holders and executive discretion actors are the targets: the doctrine narrows their historically unbounded latitude, and their exit options are constrained (they operate within legal systems that have absorbed the restraint as foundational, and cannot simply repudiate it without a legitimacy crisis).
 *
 * MANDATROPHY ANALYSIS:
 *   The living-constitutionalism reading is precisely a mandatrophy question: does the founding problem (baronial protection from royal overreach in 1215) remain live in its generalized form, or has the doctrine's mandate been extended far beyond its original scope while retaining borrowed legitimacy from that scope? The founding_problem_status is authored as contested rather than clearly live or dead — legal historians (external corroborators) confirm the original problem was narrow and feudal, while the doctrine's beneficiaries (courts) have generalized it into a universal due-process claim. This divergence is exactly the kind of claim/metric gap the framework is built to surface, not to resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_reinvention,
    'Is the doctrinal line from the 1215 charter to modern due-process jurisprudence a genuine unbroken juridical inheritance, or a retrospectively constructed continuity that courts invoke for legitimacy while actually building new doctrine to meet new problems?',
    'Detailed doctrinal history tracing citation chains and actual holdings across centuries, distinguishing genuine precedential reliance from post-hoc symbolic invocation (e.g., checking whether courts cite Magna Carta as operative authority or as rhetorical flourish in judgments that rest on other grounds).',
    'If largely reinvented, this reading''s claim to inherited binding authority weakens substantially and the constraint''s coordination story becomes closer to convenient myth-making than genuine continuity — potentially shifting classification toward a constructed legitimacy device rather than organic coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_reinvention, empirical, 'Whether the claimed juridical continuity from 1215 to present is genuine or retrospectively constructed.').

omega_variable(
    committer_framing_selection,
    'Among the three declared kernel readings (living_constitutionalism, feudal_obsolescence, parliamentary_sovereignty), what determines which reading a given court or scholar adopts, and is the living-constitutionalism reading''s dominance in Anglo-American jurisprudence a function of its truth or its institutional convenience for judiciaries seeking interpretive authority?',
    'Comparative analysis of which reading is invoked by which institutional actors in which contexts — courts asserting independent constitutional authority tend toward living_constitutionalism; legislatures asserting supremacy tend toward parliamentary_sovereignty; historians emphasizing rupture tend toward feudal_obsolescence.',
    'If the reading selected correlates strongly with which actor benefits from that reading''s authority allocation, this is evidence the kernel contest is partly a proxy fight over institutional power rather than purely an interpretive/historical question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_selection, conceptual, 'Whether reading-selection tracks institutional self-interest rather than historical or doctrinal accuracy — the committer-axis framing question for this kernel.').

omega_variable(
    prerogative_narrowing_degree,
    'How much has royal prerogative and executive discretion actually been narrowed by this doctrine in practice, versus how much narrowing is nominal (courts defer to executive claims of necessity, especially in emergency/security contexts)?',
    'Empirical review of judicial deference rates in habeas corpus and executive detention cases across jurisdictions and eras, especially during declared emergencies.',
    'High deference rates would suggest the extractiveness/suppression figures authored here overstate the doctrine''s actual bite on executive power, and that the coordination function is more symbolic than the metrics assume.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prerogative_narrowing_degree, empirical, 'Whether measured restraint on executive discretion reflects real judicial enforcement or largely deferential theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1500, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement_basis(magn_tr_t1500, observed).
narrative_ontology:measurement(magn_tr_t1700, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1700, 0.14).
narrative_ontology:measurement_basis(magn_tr_t1700, observed).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1900, 0.16).
narrative_ontology:measurement_basis(magn_tr_t1900, observed).
narrative_ontology:measurement(magn_tr_t1980, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(magn_tr_t1980, observed).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2025, 0.2).
narrative_ontology:measurement_basis(magn_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.08).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1500, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1500, 0.1).
narrative_ontology:measurement_basis(magn_be_t1500, observed).
narrative_ontology:measurement(magn_be_t1700, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1700, 0.13).
narrative_ontology:measurement_basis(magn_be_t1700, observed).
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1900, 0.16).
narrative_ontology:measurement_basis(magn_be_t1900, observed).
narrative_ontology:measurement(magn_be_t1980, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1980, 0.19).
narrative_ontology:measurement_basis(magn_be_t1980, observed).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2025, 0.22).
narrative_ontology:measurement_basis(magn_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.35).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1500, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1500, 0.32).
narrative_ontology:measurement_basis(magn_su_t1500, observed).
narrative_ontology:measurement(magn_su_t1700, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement_basis(magn_su_t1700, observed).
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1900, 0.29).
narrative_ontology:measurement_basis(magn_su_t1900, observed).
narrative_ontology:measurement(magn_su_t1980, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement_basis(magn_su_t1980, observed).
narrative_ontology:measurement(magn_su_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2025, 0.28).
narrative_ontology:measurement_basis(magn_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__living_constitutionalism_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the magna_carta_constraint_authority kernel. feudal_obsolescence_reading denies binding modern authority (treats the charter as historically exhausted). parliamentary_sovereignty_reading agrees restraint persists but relocates its source of authority entirely to revisable parliamentary statute. living_constitutionalism_reading (this file) claims an independent, continuous juridical inheritance binding rulers directly. Each carries its own epsilon and stakeholder structure; they are not measurements of one constraint but three structurally distinct constraints sharing a contested textual origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
