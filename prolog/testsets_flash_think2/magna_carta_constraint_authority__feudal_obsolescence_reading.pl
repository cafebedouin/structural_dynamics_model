% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta Authority: Feudal Obsolescence Reading
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'feudal obsolescence' reading of Magna
 *   Carta's authority, asserting that the 13th-century baronial compact has
 *   no binding force over modern sovereignty. This reading functions as a
 *   snare by actively dismissing historical constraints, thereby enabling
 *   greater discretion for modern executive and parliamentary powers. The
 *   claim of obsolescence is actively enforced through legal and political
 *   discourse, suppressing alternative interpretations that would impose
 *   limits on contemporary governance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.65).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.55).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, snare).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta Authority: Feudal Obsolescence Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__feudal_obsolescence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'd38bf78c-ea3c-4701-978a-5f6d3ca1712f').
narrative_ontology:cs_kernel_codification('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', fixed_text).
narrative_ontology:cs_authority_grounding('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', extraction).
narrative_ontology:cs_interpretation_layer_present('d38bf78c-ea3c-4701-978a-5f6d3ca1712f').
narrative_ontology:cs_reading_relation('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', foundational, historical_specificity_limits_modern_authority).
narrative_ontology:cs_axiom_status(historical_specificity_limits_modern_authority, holdable).
narrative_ontology:cs_axiom_grounding('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', historical_specificity_limits_modern_authority, empirically_contingent).
narrative_ontology:cs_axiom('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', foundational, sovereignty_is_unfettered_by_ancient_charters).
narrative_ontology:cs_axiom_status(sovereignty_is_unfettered_by_ancient_charters, holdable).
narrative_ontology:cs_axiom_grounding('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', sovereignty_is_unfettered_by_ancient_charters, conventional).
narrative_ontology:cs_reference_frame('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', thirteenth_century_feudal_compact).
narrative_ontology:cs_drift_state('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', contemporary_sovereignty_discourse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d38bf78c-ea3c-4701-978a-5f6d3ca1712f', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_power).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereigns).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, citizens_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the claim that Magna Carta holds no binding authority over modern structures, allowing for greater executive discretion and fewer historical constraints on action. Actively promotes this reading in legal and political discourse.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_power, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the argument that Parliament is the supreme legal authority, unconstrained by ancient charters. This reading reinforces their ability to legislate without external historical checks.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereigns, beneficiary,
    institutional, generational, arbitrage, national).

% Advocate for a constitution that derives its authority from the people and evolves through popular engagement, often citing Magna Carta as a foundational text. This reading undermines their claims by dismissing the charter's modern relevance.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists, payer,
    organized, generational, constrained, national).

% Seek to limit governmental power through judicial review and constitutional principles, frequently drawing on Magna Carta as a source of inherited rights and due process. This reading weakens their legal arguments and reduces the scope for judicial intervention.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_advocates, payer,
    powerful, biographical, constrained, national).

% Provide academic analysis of Magna Carta's original context and historical impact. While their scholarship may support the 'feudal compact' aspect, they often remain neutral on its modern legal authority, observing how different parties interpret it.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, legal_historians_feudal_scholars, observer,
    analytical, civilizational, analytical, universal).

% Indirectly bear the costs of potentially unchecked executive and parliamentary power, as the historical constraints on government are dismissed. Their ability to appeal to foundational rights derived from Magna Carta is diminished.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, citizens_at_large, payer,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_power).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the *absence* of constraint on modern executive and parliamentary power, allowing these bodies to operate with greater perceived discretion by dismissing ancient legal checks.
% TRANSFER_FUNCTION: Transfers authority and discretion from historical constitutional precedent (as embodied by Magna Carta) to modern political actors, specifically the executive and Parliament.
% ABSENT_VOICES: Those who believe in an evolving, living constitution, or those who see Magna Carta as a foundational document for parliamentary power, are marginalized by this reading. Their arguments for inherited rights or constitutional limits are dismissed as anachronistic.
% DISAPPEARANCE_RATIONALE: If the claim of Magna Carta's feudal obsolescence vanished overnight, modern executive and parliamentary powers would face renewed pressure to justify their actions against constitutional principles. This would likely lead to a re-evaluation of the scope of governmental authority and potentially empower juridical and popular constitutionalist movements, reorganizing the legal and political landscape.
% FOUNDING_PROBLEM: The problem of reconciling an ancient, historically specific document with the demands and structures of modern sovereign governance, or more critically, the problem of justifying maximal executive and parliamentary discretion in the face of historical claims of restraint.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists, some political scientists, and historians who emphasize the specific historical context of Magna Carta often corroborate the 'feudal compact' aspect. However, the claim that this compact has *no binding authority* over modern structures is primarily asserted by those who benefit from such a reading, with limited corroboration from genuinely disinterested parties regarding the *lack* of modern authority.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is moderate-to-high because the claim of obsolescence directly enables the extraction of discretion and power by modern political actors. Suppression (0.55) is also moderate, as this reading must actively counter and dismiss 'living constitutionalist' or 'inherited rights' arguments. The theater ratio (0.20) is low because the constraint's function is primarily to *deny* the charter's modern relevance, rather than to theatrically maintain a defunct function. The increasing extractiveness and suppression over the interval reflect a growing assertiveness of this reading in political and legal debates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of modern executive and parliamentary power, this reading is a pragmatic recognition of historical fact, enabling efficient governance. From the perspective of popular constitutionalists and juridical restraint advocates, it is an active suppression of foundational rights and a justification for unchecked power. The engine's classification as a Snare reflects the latter, identifying the active extraction enabled by the obsolescence claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Modern executive power and parliamentary sovereigns are the primary beneficiaries, as this reading maximizes their discretion (low directionality). Popular constitutionalists, juridical restraint advocates, and citizens at large are victims, as their claims for constitutional limits are undermined (high directionality). Legal historians act as observers, providing context without necessarily endorsing a modern legal interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_vs_modern_relevance,
    'Is Magna Carta''s historical context truly exhaustive of its modern relevance, or does it contain principles (e.g., due process, rule of law) that transcend its feudal origins and apply to modern sovereignty structures?',
    'Comparative legal analysis of constitutional development in common law jurisdictions, examining how similar historical documents have been reinterpreted or absorbed into modern legal frameworks, and the impact of such interpretations on governmental power.',
    'If principles transcend its origins, the ''feudal obsolescence'' reading''s claim of non-binding authority is weakened, potentially reclassifying it from a Snare (active extraction) to a Piton (atrophied function) or even a Tangled Rope (if a coordination function for modern rights is identified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_vs_modern_relevance, conceptual, 'Whether historical specificity fully negates modern legal applicability.').

omega_variable(
    obsolescence_claim_motivation,
    'Is the claim of Magna Carta''s feudal obsolescence a genuine historical-legal conclusion, or is it primarily a rhetorical strategy employed by modern political actors to justify increased discretion and reduce accountability?',
    'Analysis of the political and legal contexts in which this reading is asserted, examining who benefits from its adoption and whether alternative, historically plausible readings that impose greater restraint are systematically suppressed in public discourse.',
    'If primarily a rhetorical strategy for power, the ''snare'' classification is strongly reinforced, highlighting the active, extractive nature of the claim. If a genuine, disinterested historical-legal conclusion, the extractiveness might be lower, leaning towards a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obsolescence_claim_motivation, preference, 'Motivation behind the claim of feudal obsolescence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1980, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(magn_tr_t1988, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1988, 0.19).
narrative_ontology:measurement(magn_tr_t1996, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1996, 0.2).
narrative_ontology:measurement(magn_tr_t2004, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2004, 0.2).
narrative_ontology:measurement(magn_tr_t2012, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(magn_tr_t2020, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(magn_be_t1980, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(magn_be_t1988, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1988, 0.58).
narrative_ontology:measurement(magn_be_t1996, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1996, 0.61).
narrative_ontology:measurement(magn_be_t2004, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2004, 0.63).
narrative_ontology:measurement(magn_be_t2012, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2012, 0.64).
narrative_ontology:measurement(magn_be_t2020, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1980, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(magn_su_t1988, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1988, 0.48).
narrative_ontology:measurement(magn_su_t1996, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1996, 0.5).
narrative_ontology:measurement(magn_su_t2004, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2004, 0.52).
narrative_ontology:measurement(magn_su_t2012, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2012, 0.54).
narrative_ontology:measurement(magn_su_t2020, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
