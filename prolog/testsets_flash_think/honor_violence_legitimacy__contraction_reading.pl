% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Redefined Honor Code Excluding Violence
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'contraction_reading' of the
 *   'honor_violence_legitimacy' kernel. It describes a historical period
 *   where the very definition of honor underwent a fundamental redefinition,
 *   shifting to explicitly exclude violence as a legitimate response to
 *   insult. This redefinition made dueling 'structurally unthinkable' for
 *   those adhering to the new code, rather than merely illegal or costly. The
 *   constraint operates as a deeply internalized social norm, coordinating
 *   society away from violence but imposing significant social costs on those
 *   who cling to older, violent conceptions of honor.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.6).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.8).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Redefined Honor Code Excluding Violence").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '3d9ede33-afeb-455d-b069-e395dd4758bb').
narrative_ontology:cs_kernel_codification('3d9ede33-afeb-455d-b069-e395dd4758bb', implicit).
narrative_ontology:cs_authority_grounding('3d9ede33-afeb-455d-b069-e395dd4758bb', practice).
narrative_ontology:cs_interpretation_layer_present('3d9ede33-afeb-455d-b069-e395dd4758bb').
narrative_ontology:cs_reading_relation('3d9ede33-afeb-455d-b069-e395dd4758bb', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d9ede33-afeb-455d-b069-e395dd4758bb', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('3d9ede33-afeb-455d-b069-e395dd4758bb', foundational, honor_excludes_violence).
narrative_ontology:cs_axiom_status(honor_excludes_violence, holdable).
narrative_ontology:cs_axiom_grounding('3d9ede33-afeb-455d-b069-e395dd4758bb', honor_excludes_violence, deontological).
narrative_ontology:cs_axiom('3d9ede33-afeb-455d-b069-e395dd4758bb', secondary, social_order_prioritizes_life).
narrative_ontology:cs_axiom_status(social_order_prioritizes_life, holdable).
narrative_ontology:cs_axiom_grounding('3d9ede33-afeb-455d-b069-e395dd4758bb', social_order_prioritizes_life, conventional).
narrative_ontology:cs_reference_frame('3d9ede33-afeb-455d-b069-e395dd4758bb', honor_as_non_violent_integrity).
narrative_ontology:cs_drift_state('3d9ede33-afeb-455d-b069-e395dd4758bb', contemporary_social_norms, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3d9ede33-afeb-455d-b069-e395dd4758bb', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, society_at_large).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, individuals_avoiding_duels).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, traditional_honor_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals and groups actively uphold and transmit the redefined honor code, ensuring that violence is excluded from legitimate responses to insult. Their social standing and identity are deeply intertwined with this new definition, making deviation unthinkable for them.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, honor_code_adherents, agenda_setter,
    powerful, generational, identity_locked, global).

% Benefits from the reduction in violence, loss of life, and social disruption previously caused by dueling. The collective peace and stability are enhanced, though the redefinition of honor itself is a deeply ingrained social norm that is difficult to consciously exit.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, society_at_large, beneficiary,
    moderate, generational, constrained, global).

% Individuals who, under the old honor code, might have felt compelled to duel, now have a socially legitimate path to maintain their honor without resorting to violence. They avoid the personal risk and legal repercussions of dueling.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, individuals_avoiding_duels, beneficiary,
    moderate, biographical, mobile, local).

% Individuals or small groups who still believe that violence is a legitimate or necessary response to certain insults to honor. They are marginalized, socially ostracized, and their views are considered anachronistic or illegitimate by the dominant social order, effectively denying them a path to honor as they understand it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, traditional_honor_advocates, payer,
    powerless, biographical, trapped, local).

% State and judicial bodies that enforce laws against violence. The redefinition of honor aligns with their mandate to maintain public order and prosecute violent acts, reinforcing the social shift away from dueling.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social behavior by establishing a new, shared understanding of honor that explicitly excludes violence, thereby preventing costly and dangerous duels and promoting non-violent conflict resolution.
% TRANSFER_FUNCTION: Transfers the social cost, personal risk, and legal repercussions associated with dueling from individuals and society to a conceptual space where such actions are deemed 'unthinkable' or illegitimate, effectively eliminating them as options for honor disputes.
% ABSENT_VOICES: Traditional honor advocates who believed violence was a legitimate or even necessary response to insult are now largely absent from mainstream discourse, their views having been rendered anachronistic by the redefinition of honor itself. They would argue for the historical legitimacy of dueling as a means of honor defense.
% DISAPPEARANCE_RATIONALE: If the redefinition of honor to exclude violence vanished overnight, the social fabric around honor would be destabilized. Dueling or similar forms of violence might re-emerge as legitimate responses to insult, leading to increased social conflict, personal risk, and a need for society to re-establish new boundaries for acceptable conduct in honor disputes.
% FOUNDING_PROBLEM: The high social cost, frequent loss of life, and legal challenges associated with dueling as a prevalent means of resolving honor disputes, which undermined public order and individual safety.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal reforms, and sociological analyses from outside the immediate adherents of the new honor code confirm the societal shift away from dueling due to its destructive consequences. The ongoing need for non-violent mechanisms for honor is attested by contemporary legal frameworks and social norms.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.6) and suppression (0.8) reflect the profound social pressure and ostracism faced by those who might still adhere to older, violent honor codes. While the new honor code itself doesn't extract resources, the social enforcement of its redefinition is highly coercive. Accessibility collapse is very high (0.9) because dueling truly became 'unthinkable' within the dominant social frame. Resistance is low (0.1) as open opposition to this redefinition was largely marginalized. The claimed type is 'tangled_rope' because it genuinely coordinates society away from violence (beneficiaries) but does so through strong social coercion that extracts from those whose honor concepts are superseded (victims), requiring active social enforcement.
 *
 * PERSPECTIVAL GAP:
 *   For adherents of the redefined honor code, the constraint is a beneficial social norm that prevents destructive violence. For traditional honor advocates, it is a deeply extractive and suppressive force that denies them a legitimate means of defending their honor, effectively trapping them in a marginalized position.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and individuals avoiding duels are beneficiaries, gaining from reduced violence and risk. Honor code adherents act as agenda-setters, actively maintaining and transmitting the new norm. Traditional honor advocates are victims, as their understanding of honor is delegitimized and suppressed. Legal authorities observe and reinforce this shift through formal law.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing violence in honor disputes) remains live, but its mechanism has shifted from external prohibition to internalized social redefinition. The 'tangled_rope' classification prevents mislabeling this as pure coordination, acknowledging the coercive social pressure and extraction from those whose honor concepts are superseded, while also recognizing its genuine coordination function in reducing violence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''contraction_reading'' of the ''honor_violence_legitimacy'' kernel?',
    'Further historical and sociological analysis comparing the relative weight of conceptual redefinition versus external costs in the decline of dueling, particularly in different cultural contexts.',
    'If the ''drop_reading'' or ''composite_reading'' is found to be more accurate, the primary causal mechanism for dueling''s decline would shift, potentially altering the classification of the underlying constraint (e.g., towards a ''snare'' if external costs were purely extractive, or a ''rope'' if they were genuinely coordinative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the primary driver of dueling''s decline: conceptual redefinition vs. external costs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (social ostracism, legal penalties) or internalized (self-censorship, psychological aversion to violence)?',
    'Analysis of personal diaries, literature, and legal records from the period to gauge the extent of internal conviction versus external pressure. Post-exit suppression trajectory: if individuals who leave the dominant social sphere still avoid dueling, it suggests internalized suppression.',
    'If primarily internalized, the constraint''s effective suppression is higher than a purely structural measure suggests, as the ''unthinkable'' nature is deeply embedded. If primarily structural, the constraint relies more heavily on active social enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the redefinition of honor.').

omega_variable(
    scope_of_unthinkability,
    'How universal was the ''unthinkability'' of dueling? Did pockets of society or specific subcultures retain a more traditional, violent conception of honor, even if marginalized?',
    'Detailed micro-historical studies of specific communities and social strata, examining instances of dueling or challenges to the new honor code.',
    'If significant pockets of resistance or alternative honor codes existed, the ''accessibility_collapse'' and ''resistance'' metrics might need adjustment, and the ''victims'' group (''traditional_honor_advocates'') would be more clearly defined and potentially larger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_unthinkability, empirical, 'The actual reach and completeness of the redefinition of honor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__contraction_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(hono_tr_t1820, honor_violence_legitimacy__contraction_reading, theater_ratio, 1820, 0.13).
narrative_ontology:measurement(hono_tr_t1840, honor_violence_legitimacy__contraction_reading, theater_ratio, 1840, 0.12).
narrative_ontology:measurement(hono_tr_t1860, honor_violence_legitimacy__contraction_reading, theater_ratio, 1860, 0.11).
narrative_ontology:measurement(hono_tr_t1880, honor_violence_legitimacy__contraction_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.7).
narrative_ontology:measurement(hono_be_t1820, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1820, 0.68).
narrative_ontology:measurement(hono_be_t1840, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1840, 0.65).
narrative_ontology:measurement(hono_be_t1860, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1860, 0.63).
narrative_ontology:measurement(hono_be_t1880, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1880, 0.61).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(hono_su_t1820, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1820, 0.83).
narrative_ontology:measurement(hono_su_t1840, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1840, 0.82).
narrative_ontology:measurement(hono_su_t1860, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1860, 0.81).
narrative_ontology:measurement(hono_su_t1880, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1880, 0.8).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_violence_legitimacy' kernel, focusing on the redefinition of honor itself. It is linked to sibling readings that emphasize external costs or a composite of factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
