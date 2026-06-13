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
 *   This constraint represents the 'parliamentary sovereignty' reading of
 *   Magna Carta's authority, where its principles survive only insofar as
 *   they are absorbed into and upheld by parliamentary statute. Parliament,
 *   as the supreme legal authority, can revise or repeal any charter
 *   provision. This reading contrasts with views that see Magna Carta as an
 *   entrenched, higher law or as entirely obsolete. The constraint is a
 *   tangled_rope because it provides a framework for governance
 *   (coordination) but allows for extraction from minority groups whose
 *   protections are not guaranteed by majoritarian legislation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.55).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.65).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta's Authority under Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '81cd9ecf-0c53-485b-a237-4fdb312f203d').
narrative_ontology:cs_kernel_codification('81cd9ecf-0c53-485b-a237-4fdb312f203d', formalized).
narrative_ontology:cs_authority_grounding('81cd9ecf-0c53-485b-a237-4fdb312f203d', lineage).
narrative_ontology:cs_interpretation_layer_present('81cd9ecf-0c53-485b-a237-4fdb312f203d').
narrative_ontology:cs_reading_relation('81cd9ecf-0c53-485b-a237-4fdb312f203d', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('81cd9ecf-0c53-485b-a237-4fdb312f203d', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_axiom('81cd9ecf-0c53-485b-a237-4fdb312f203d', foundational, parliamentary_supremacy_over_common_law).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_over_common_law, holdable).
narrative_ontology:cs_axiom_grounding('81cd9ecf-0c53-485b-a237-4fdb312f203d', parliamentary_supremacy_over_common_law, conventional).
narrative_ontology:cs_axiom('81cd9ecf-0c53-485b-a237-4fdb312f203d', secondary, statute_as_sole_source_of_constitutional_law).
narrative_ontology:cs_axiom_status(statute_as_sole_source_of_constitutional_law, holdable).
narrative_ontology:cs_axiom_grounding('81cd9ecf-0c53-485b-a237-4fdb312f203d', statute_as_sole_source_of_constitutional_law, conventional).
narrative_ontology:cs_reference_frame('81cd9ecf-0c53-485b-a237-4fdb312f203d', glorious_revolution_settlement).
narrative_ontology:cs_drift_state('81cd9ecf-0c53-485b-a237-4fdb312f203d', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('81cd9ecf-0c53-485b-a237-4fdb312f203d', '').
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

% As the supreme legal authority, Parliament can legislate on any matter, including revising or repealing provisions originally derived from Magna Carta. It benefits from the flexibility to adapt law to contemporary needs, but also from the historical legitimacy Magna Carta provides.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from a system where their elected representatives can enact laws reflecting the popular will, including those that might modify or supersede older constitutional principles. Their interests are directly represented in the legislative process.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_electorate, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs when parliamentary legislation, reflecting majority will, overrides protections that might otherwise be derived from Magna Carta. Their rights are subject to statutory enactment and can be revised or repealed by Parliament.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups, payer,
    powerless, generational, constrained, national).

% Lack inherent constitutional protections beyond what Parliament chooses to grant or preserve in statute. They are vulnerable to legislative changes that might diminish traditional liberties if not explicitly codified and defended by current law.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, individuals_unprotected_by_statute, payer,
    powerless, biographical, trapped, national).

% Interprets and applies parliamentary statutes, including those that incorporate or modify Magna Carta's principles. While bound by parliamentary sovereignty, the judiciary can influence the practical application of these laws through interpretation.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary, observer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unified source of legal authority (Parliament) for the nation, ensuring that laws can be adapted and revised to meet evolving societal needs without being rigidly bound by ancient texts.
% TRANSFER_FUNCTION: Transfers ultimate legal authority from historical documents and inherent rights to the legislative body, allowing for the revision or repeal of traditional constraints on power in favor of contemporary statutory law.
% ABSENT_VOICES: Advocates for entrenched constitutional rights and those who believe in a higher law that binds Parliament would object. Their voices are often heard in public discourse and academic debate but lack direct legislative power to prevent parliamentary action.
% DISAPPEARANCE_RATIONALE: If the principle of parliamentary sovereignty vanished overnight, the entire legal and constitutional framework would collapse. Courts would lack a clear hierarchy of laws, the authority of Parliament to legislate would be challenged, and the relationship between the Crown, Parliament, and the people would be fundamentally destabilized.
% FOUNDING_PROBLEM: The problem of establishing a clear, adaptable, and ultimately democratic source of legal authority that could evolve beyond feudal constraints and royal prerogative, while still acknowledging historical foundations.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political theorists, and historical documents corroborate the ongoing challenge of balancing historical constitutionalism with modern democratic governance. The debate over the extent of parliamentary power and the protection of fundamental rights remains central to UK constitutional discourse, attested by ongoing legislative and judicial debates, and academic commentary.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.55) because while Parliament can act in the public interest, it can also legislate in ways that disproportionately affect minorities. Suppression (0.65) is present as there are no entrenched constitutional mechanisms to prevent Parliament from altering or removing protections. Theater ratio is low (0.20) as the constraint's operation is genuinely functional in establishing legislative authority, though the historical reverence for Magna Carta can sometimes be performative when its substance is overridden. The historical trajectory shows a gradual increase in both extractiveness and suppression as parliamentary power consolidated and the scope of its legislative authority expanded over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's perspective, this is a legitimate and necessary framework for democratic governance, allowing for adaptation and progress. From the perspective of minority groups, it can be seen as a mechanism that permits the erosion of fundamental rights when those rights are not politically expedient for the majority. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament and the majority electorate are beneficiaries, as they wield and benefit from the ultimate legislative authority. Minority groups and individuals unprotected by specific statutes are payers, as their rights and protections are contingent on parliamentary will and can be diminished. The judiciary acts as an observer, bound by the framework but interpreting its application.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_sovereignty_limits,
    'Are there any unwritten constitutional conventions or common law principles that, in practice, limit Parliament''s ability to repeal or revise fundamental Magna Carta principles, even under a doctrine of sovereignty?',
    'Analysis of judicial review decisions and parliamentary practice in cases where fundamental rights derived from Magna Carta have been challenged or modified by statute. Examination of ''constitutional statutes'' doctrine.',
    'If such limits are found to be effective, the constraint''s suppression and extractiveness would be lower, potentially shifting its classification towards a more balanced ''rope'' or even a ''mountain'' for certain core principles. If no such limits exist, the ''tangled_rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_sovereignty_limits, empirical, 'Whether unwritten constitutional principles effectively constrain parliamentary sovereignty regarding Magna Carta.').

omega_variable(
    popular_will_vs_minority_rights,
    'To what extent does the ''popular will'' expressed through Parliament genuinely represent the interests of all citizens, including minorities, when it legislates on matters touching on Magna Carta''s legacy?',
    'Empirical studies of legislative outcomes, public opinion surveys across diverse demographics, and analysis of the impact of parliamentary legislation on minority groups'' rights and protections.',
    'If parliamentary action consistently disadvantages minorities without adequate redress, the ''tangled_rope'' classification is strengthened, with higher effective extraction for minority groups. If mechanisms for minority protection within the parliamentary system are robust, extractiveness would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_will_vs_minority_rights, empirical, 'The balance between majoritarian parliamentary power and minority rights in practice.').

omega_variable(
    reading_framing_choice,
    'Is the ''parliamentary sovereignty'' reading the most appropriate framing for Magna Carta''s contemporary authority, or does it obscure a deeper, more entrenched constitutionalism (living_constitutionalism_reading) or an outright irrelevance (feudal_obsolescence_reading)?',
    'Conceptual analysis of legal philosophy, historical evidence, and the practical effects of each reading on governance and rights. The choice depends on which set of normative and descriptive claims about the UK constitution is prioritized.',
    'Adopting the ''living_constitutionalism_reading'' would imply a more entrenched, less revisable constraint, potentially shifting it towards a ''mountain'' or ''rope'' for core principles. Adopting the ''feudal_obsolescence_reading'' would render Magna Carta a ''piton'' or ''world_unchanged'' constraint. This choice fundamentally alters the constraint''s structural properties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'The choice of framing for Magna Carta''s constitutional authority.').


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
narrative_ontology:measurement(magn_be_t1988, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1988, 0.53).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1688, 0.5).
narrative_ontology:measurement(magn_su_t1788, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1788, 0.55).
narrative_ontology:measurement(magn_su_t1888, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1888, 0.6).
narrative_ontology:measurement(magn_su_t1988, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1988, 0.63).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
