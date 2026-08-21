% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta's Authority under Parliamentary Sovereignty
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint describes the authority of Magna Carta within a system of
 *   parliamentary sovereignty. Under this reading, Magna Carta's provisions
 *   are not supreme law but are absorbed into the body of statute law,
 *   subject to parliamentary revision or repeal. Parliament inherits the
 *   authority to constrain, but also the power to define the scope of those
 *   constraints. This creates a 'tangled rope' where coordination (democratic
 *   legislative process) is intertwined with extraction (potential for
 *   majoritarian erosion of minority rights).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.6).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.7).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta's Authority under Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'ba4b093c-b5d6-49bd-a36b-a0a0860b7863').
narrative_ontology:cs_kernel_codification('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', formalized).
narrative_ontology:cs_authority_grounding('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', lineage).
narrative_ontology:cs_interpretation_layer_present('ba4b093c-b5d6-49bd-a36b-a0a0860b7863').
narrative_ontology:cs_reading_relation('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', magna_carta_constraint_authority__feudal_obsolescence_reading, influences).
narrative_ontology:cs_axiom('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', foundational, parliamentary_supremacy_over_common_law).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_over_common_law, holdable).
narrative_ontology:cs_axiom_grounding('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', parliamentary_supremacy_over_common_law, conventional).
narrative_ontology:cs_axiom('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', foundational, magna_carta_as_ordinary_statute).
narrative_ontology:cs_axiom_status(magna_carta_as_ordinary_statute, holdable).
narrative_ontology:cs_axiom_grounding('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', magna_carta_as_ordinary_statute, conventional).
narrative_ontology:cs_reference_frame('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', glorious_revolution_settlement).
narrative_ontology:cs_drift_state('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', contemporary_human_rights_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('ba4b093c-b5d6-49bd-a36b-a0a0860b7863', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_electorate).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, individuals_unprotected_by_statute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the supreme legal authority, Parliament can absorb, revise, or repeal any provision of Magna Carta through statute. It benefits from the flexibility to adapt law to contemporary needs, but also from the historical legitimacy Magna Carta provides.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the principle that law reflects the will of the elected representatives, allowing for democratic change. Their interests are generally protected by the legislative process, but they are also subject to its limitations.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_electorate, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of a system where their rights are not entrenched beyond parliamentary reach. They are vulnerable to majoritarian legislation that may erode protections historically associated with Magna Carta, lacking direct recourse outside the legislative process.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups, payer,
    powerless, generational, constrained, national).

% Individuals whose specific rights or protections, though historically linked to Magna Carta, have not been explicitly codified or are subject to parliamentary revision. They have limited means to challenge parliamentary decisions.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, individuals_unprotected_by_statute, payer,
    powerless, biographical, trapped, local).

% Interprets and applies statute law, including those derived from or superseding Magna Carta. Under parliamentary sovereignty, the judiciary cannot strike down primary legislation, but can interpret its application and influence public discourse on rights.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unified source of legal authority (Parliament) for all laws, including those historically derived from Magna Carta, ensuring legal certainty and adaptability.
% TRANSFER_FUNCTION: Transfers ultimate legal authority from historical charter (Magna Carta) to contemporary statute, from the Crown to Parliament, and from entrenched rights to revisable legislative will. This transfers power to the elected legislature, potentially at the expense of minority protections.
% ABSENT_VOICES: Advocates for entrenched constitutional rights, particularly those of minority groups, who would argue for a higher legal status for fundamental principles beyond parliamentary repeal. Their arguments are often heard in academic or advocacy circles but lack direct legislative power.
% DISAPPEARANCE_RATIONALE: If the principle of parliamentary sovereignty over Magna Carta vanished, it would create profound legal uncertainty, potentially elevating historical charter provisions to a higher constitutional status, leading to judicial challenges against existing statutes and a re-evaluation of the balance of power between Parliament and the judiciary.
% FOUNDING_PROBLEM: The problem of arbitrary royal power and the need to establish a clear, supreme source of law that could evolve with society, while still drawing on historical legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and political theorists outside Parliament corroborate that the tension between historical rights and legislative flexibility remains a live issue, particularly concerning human rights and constitutional reform debates.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.6) because while Parliament can revise, it generally operates within a tradition of rights. Suppression is high (0.7) because there are few formal checks on parliamentary power to repeal or modify statutes, making it difficult for affected groups to resist. Theater ratio is low (0.2) as Parliament's legislative function is genuine, though the symbolic invocation of Magna Carta can sometimes be performative.
 *
 * PERSPECTIVAL GAP:
 *   Parliament views this as a legitimate, flexible system of democratic governance, where historical documents like Magna Carta inform but do not bind. Minority groups, however, experience it as a system where fundamental protections can be eroded by a simple majority vote, leading to a sense of vulnerability and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament and the majority electorate are beneficiaries, as the system grants them ultimate legislative authority. Minority groups and individuals unprotected by specific statutes are payers, as their rights are contingent on parliamentary will. The judiciary acts as an observer, interpreting but not overriding parliamentary decisions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entrenchment_of_rights,
    'To what extent are fundamental rights, historically associated with Magna Carta, effectively entrenched through other mechanisms (e.g., Human Rights Act, common law principles) despite parliamentary sovereignty?',
    'Comparative legal analysis of judicial decisions and legislative practice in cases involving rights derived from Magna Carta, assessing the practical limits on parliamentary power.',
    'If rights are found to be strongly entrenched, the effective extractiveness from minority groups would be lower, potentially shifting the constraint closer to a ''rope'' or ''scaffold'' (if temporary entrenchment). If not, the ''tangled rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_of_rights, empirical, 'The actual degree of protection for rights under parliamentary sovereignty.').

omega_variable(
    democratic_legitimacy_vs_minority_protection,
    'Is the democratic legitimacy derived from parliamentary sovereignty inherently in tension with the protection of minority rights, or can these be reconciled within this framework?',
    'Conceptual analysis of political philosophy and constitutional theory, examining whether a majoritarian system can adequately safeguard fundamental rights without formal entrenchment.',
    'If an inherent tension is found, the ''tangled rope'' classification is strengthened, highlighting the structural trade-off. If reconciliation is possible, the constraint might be re-evaluated as a more benign ''rope'' with a higher coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_minority_protection, conceptual, 'The philosophical compatibility of parliamentary sovereignty with robust minority rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1688, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1688, 0.1).
narrative_ontology:measurement(magn_tr_t1788, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1788, 0.12).
narrative_ontology:measurement(magn_tr_t1888, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1888, 0.15).
narrative_ontology:measurement(magn_tr_t1988, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1988, 0.18).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(magn_be_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1688, 0.4).
narrative_ontology:measurement(magn_be_t1788, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1788, 0.45).
narrative_ontology:measurement(magn_be_t1888, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1888, 0.5).
narrative_ontology:measurement(magn_be_t1988, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1988, 0.55).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1688, 0.5).
narrative_ontology:measurement(magn_su_t1788, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1788, 0.55).
narrative_ontology:measurement(magn_su_t1888, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1888, 0.6).
narrative_ontology:measurement(magn_su_t1988, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1988, 0.65).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'magna_carta_constraint_authority' kernel. This 'parliamentary_sovereignty_reading' emphasizes Parliament's ultimate authority, contrasting with the 'living_constitutionalism_reading' (evolving juridical precedent) and the 'feudal_obsolescence_reading' (historical irrelevance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
