% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__state_enforced_creation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: State-Enforced Created Qur'an Doctrine (Mihna)
 *   domain: islamic_theology/political_authority/philosophy_of_language
 *
 * SUMMARY:
 *   This constraint describes the historical period of the Mihna (833-847
 *   CE), an inquisition initiated by the Abbasid Caliphate to enforce the
 *   Mu'tazilite doctrine that the Qur'an was created in time, rather than
 *   being uncreated and co-eternal with God. This reading focuses on the
 *   transformation of a theological dispute into a state-enforced snare,
 *   where caliphal authority leveraged a specific doctrine to purge
 *   traditionalist scholars and consolidate political control. The
 *   constraint's persistence depended entirely on active state coercion and
 *   the suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.85).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.92).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Created Qur'an Doctrine (Mihna)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "islamic_theology/political_authority/philosophy_of_language").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, 'ca6c3a42-4d0c-4590-a37b-4bce39616caf').
narrative_ontology:cs_kernel_codification('ca6c3a42-4d0c-4590-a37b-4bce39616caf', formalized).
narrative_ontology:cs_authority_grounding('ca6c3a42-4d0c-4590-a37b-4bce39616caf', lineage).
narrative_ontology:cs_interpretation_layer_present('ca6c3a42-4d0c-4590-a37b-4bce39616caf').
narrative_ontology:cs_reading_relation('ca6c3a42-4d0c-4590-a37b-4bce39616caf', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('ca6c3a42-4d0c-4590-a37b-4bce39616caf', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('ca6c3a42-4d0c-4590-a37b-4bce39616caf', foundational, quran_is_created_and_temporal).
narrative_ontology:cs_axiom_status(quran_is_created_and_temporal, holdable).
narrative_ontology:cs_axiom_grounding('ca6c3a42-4d0c-4590-a37b-4bce39616caf', quran_is_created_and_temporal, conventional).
narrative_ontology:cs_reference_frame('ca6c3a42-4d0c-4590-a37b-4bce39616caf', caliphal_doctrinal_supremacy).
narrative_ontology:cs_drift_state('ca6c3a42-4d0c-4590-a37b-4bce39616caf', post_mihna_reversal, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ca6c3a42-4d0c-4590-a37b-4bce39616caf', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, umma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Abbasid Caliphate, which initiated and enforced the Mihna. It sought to consolidate its religious and political authority by imposing a specific theological doctrine, viewing doctrinal control as a tool for state unity and legitimacy. It directly benefited from the suppression of dissenting views.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% The rationalist theological school whose doctrine (Qur'an is created) was adopted and enforced by the Caliphate. They gained significant state backing, prestige, and influence, with their opponents facing persecution. Their position was temporarily secured by state power.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_scholars, beneficiary,
    organized, biographical, mobile, national).

% Scholars who adhered to the traditionalist view (Qur'an is uncreated) and refused to affirm the state-imposed doctrine. They faced imprisonment, torture, public humiliation, and loss of livelihood, exemplified by Ahmad ibn Hanbal. Their only 'exit' was recantation or death.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    powerless, biographical, trapped, national).

% The broader Muslim populace and local communities who largely followed traditionalist interpretations. They were subjected to the caliph's religious authority and forced to conform to the state-imposed dogma, leading to intellectual and spiritual coercion.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    powerless, biographical, constrained, local).

% The diversity of theological thought and open intellectual debate within Islamic scholarship. This was a victim of the Mihna, as state enforcement of a single dogma suppressed alternative interpretations and critical inquiry, leading to a chilling effect on intellectual freedom.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism, excluded,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

% The general Muslim community, whose religious life and intellectual discourse were directly impacted by the state's imposition of a specific theological doctrine. They bore the cost of enforced conformity and the suppression of respected scholars.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, umma, payer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a unified theological position on the ontological status of the Qur'an, thereby consolidating the Caliph's religious authority and preventing perceived doctrinal fragmentation.
% TRANSFER_FUNCTION: Transfers religious legitimacy and political control from diverse scholarly interpretations to the Caliphal authority and its favored Mu'tazilite scholars, at the cost of traditionalist scholars' freedom and intellectual pluralism.
% ABSENT_VOICES: The voices of independent traditionalist scholars and their followers were actively silenced and suppressed through imprisonment and torture. They would have argued for the uncreated nature of the Qur'an and the illegitimacy of state interference in theological matters.
% DISAPPEARANCE_RATIONALE: If the state's enforcement of the created Qur'an doctrine (Mihna) had vanished overnight, the immediate persecution of traditionalist scholars would have ceased, allowing for a more open theological debate. The Caliphate's religious authority would have been significantly weakened, and the intellectual landscape of early Islam would have developed differently, likely with greater pluralism.
% FOUNDING_PROBLEM: A theological dispute regarding the ontological status of the Qur'an (whether it was created in time or co-eternal with God), which the Caliphate perceived as a threat to its religious and political unity.
% FOUNDING_PROBLEM_CORROBORATION: The Caliphate and Mu'tazilite sources attested that the problem was live and required resolution for religious unity. However, traditionalist accounts and later historical analyses (from outside the benefiting parties) attest that the problem was primarily a political tool for consolidating power, and the theological debate continued long after the Mihna's end, indicating the 'problem' was not truly 'solved' by coercion.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the state directly extracted conformity and suppressed dissent, using the theological claim as justification for political control. Suppression is extremely high (0.92) due to the use of imprisonment, torture, and public trials to force affirmation of the doctrine, effectively collapsing alternatives for scholars. Theater ratio is moderate (0.40) because while there was a genuine theological claim, a significant portion of the enforcement became performative, aimed at public displays of loyalty rather than pure theological debate. Accessibility collapse is high (0.88) as the state actively eliminated any safe space for dissenting views. Resistance is also high (0.75) due to the steadfast opposition of figures like Ahmad ibn Hanbal, despite severe persecution.
 *
 * PERSPECTIVAL GAP:
 *   From the Caliphate's perspective, the Mihna was a necessary measure to maintain religious unity and state legitimacy, a 'rope' of doctrinal coordination. From the perspective of traditionalist scholars, it was a 'snare' of pure extraction and persecution, leveraging state power to impose a contested theological view. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority and Mu'tazilite scholars are the primary beneficiaries, gaining political control and doctrinal supremacy, respectively. Traditionalist scholars, literalist communities, and scholarly pluralism are the clear targets/victims, bearing the full cost of persecution and suppression. The general Umma also bears costs through enforced conformity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_motivation,
    'To what extent was the Mihna driven by genuine theological conviction (Mu''tazilite belief) versus political consolidation (Caliphal authority)?',
    'Analysis of primary sources, caliphal decrees, and scholarly writings for explicit statements of intent, and comparison with other instances of caliphal intervention in religious affairs.',
    'If primarily theological, the constraint might lean more towards a ''tangled_rope'' (coordination with extraction). If primarily political, it reinforces the ''snare'' classification, highlighting the instrumentalization of theology for power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_vs_political_motivation, conceptual, 'Ambiguity of underlying motivation for state enforcement.').

omega_variable(
    long_term_doctrinal_impact,
    'Did the Mihna permanently settle the theological debate over the Qur''an''s ontological status, or did it merely suppress it temporarily?',
    'Examination of post-Mihna theological developments, the resurgence of traditionalist views, and the eventual abandonment of the Mu''tazilite position as state doctrine.',
    'If the debate continued and traditionalism eventually prevailed, it suggests the Mihna''s suppression was ultimately ineffective in shaping long-term doctrine, highlighting the resilience of intellectual resistance against state coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(long_term_doctrinal_impact, empirical, 'Effectiveness of state coercion in resolving theological disputes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 833, 847).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t833, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 833, 0.25).
narrative_ontology:measurement(qura_tr_t836, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 836, 0.3).
narrative_ontology:measurement(qura_tr_t839, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 839, 0.35).
narrative_ontology:measurement(qura_tr_t842, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 842, 0.45).
narrative_ontology:measurement(qura_tr_t845, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 845, 0.42).
narrative_ontology:measurement(qura_tr_t847, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 847, 0.4).

% Extraction over time
narrative_ontology:measurement(qura_be_t833, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 833, 0.7).
narrative_ontology:measurement(qura_be_t836, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 836, 0.8).
narrative_ontology:measurement(qura_be_t839, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 839, 0.88).
narrative_ontology:measurement(qura_be_t842, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 842, 0.9).
narrative_ontology:measurement(qura_be_t845, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 845, 0.87).
narrative_ontology:measurement(qura_be_t847, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 847, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t833, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 833, 0.8).
narrative_ontology:measurement(qura_su_t836, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 836, 0.88).
narrative_ontology:measurement(qura_su_t839, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 839, 0.95).
narrative_ontology:measurement(qura_su_t842, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 842, 0.98).
narrative_ontology:measurement(qura_su_t845, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 845, 0.94).
narrative_ontology:measurement(qura_su_t847, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 847, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_ontological_status' kernel, focusing on the state-enforced aspect of the 'created Qur'an' doctrine. It is distinct from a purely theological 'created_reading' and directly opposes the 'uncreated_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
