% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Authority (Strict Orthodox Reading)
 *   domain: systematic_theology/ecclesiology/history_of_christian_doctrine
 *
 * SUMMARY:
 *   This constraint describes the Nicene Creed as interpreted by a strict
 *   orthodox reading, where it functions as a binding metaphysical ontology
 *   for all believers. Deviation from this ontology is considered heresy,
 *   warranting ecclesiastical sanction. This reading emphasizes doctrinal
 *   uniformity and the authority of hierarchical clergy in defining and
 *   enforcing theological truth. This is one reading of the
 *   'nicene_creed_authority' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.85).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.9).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, snare).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Authority (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/history_of_christian_doctrine").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, 'd7af718d-cf5b-415b-a1b9-156a6bd27592').
narrative_ontology:cs_kernel_codification('d7af718d-cf5b-415b-a1b9-156a6bd27592', fixed_text).
narrative_ontology:cs_authority_grounding('d7af718d-cf5b-415b-a1b9-156a6bd27592', lineage).
narrative_ontology:cs_interpretation_layer_present('d7af718d-cf5b-415b-a1b9-156a6bd27592').
narrative_ontology:cs_reading_relation('d7af718d-cf5b-415b-a1b9-156a6bd27592', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7af718d-cf5b-415b-a1b9-156a6bd27592', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('d7af718d-cf5b-415b-a1b9-156a6bd27592', foundational, creed_as_metaphysical_truth).
narrative_ontology:cs_axiom_status(creed_as_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('d7af718d-cf5b-415b-a1b9-156a6bd27592', creed_as_metaphysical_truth, theological).
narrative_ontology:cs_axiom('d7af718d-cf5b-415b-a1b9-156a6bd27592', foundational, hierarchical_interpretive_authority).
narrative_ontology:cs_axiom_status(hierarchical_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('d7af718d-cf5b-415b-a1b9-156a6bd27592', hierarchical_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('d7af718d-cf5b-415b-a1b9-156a6bd27592', patristic_doctrinal_uniformity).
narrative_ontology:cs_drift_state('d7af718d-cf5b-415b-a1b9-156a6bd27592', contemporary_pluralistic_theology, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d7af718d-cf5b-415b-a1b9-156a6bd27592', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, orthodox_theologians).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, academic_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the creed as a non-negotiable metaphysical statement, defining orthodoxy and sanctioning deviation. Their authority and institutional power are directly derived from and maintained by this strict interpretation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the clear boundaries and established framework for theological discourse. Their careers and intellectual work are validated by the strict adherence to the creed's metaphysical claims, providing a stable foundation for their scholarship.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, orthodox_theologians, beneficiary,
    organized, biographical, constrained, global).

% Face excommunication, social ostracism, and spiritual condemnation for deviating from the creed's prescribed metaphysical ontology. Their attempts at alternative interpretations are suppressed, leading to marginalization or forced conformity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    powerless, biographical, trapped, local).

% Are expected to assent to the creed's metaphysical claims without extensive theological training, often leading to cognitive dissonance or superficial adherence. Their personal spiritual journeys are constrained by the rigid interpretive framework.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    moderate, immediate, constrained, local).

% Those outside the strict orthodox tradition find their scholarship dismissed or condemned if it challenges the creed's metaphysical claims. They face pressure to conform or risk losing institutional standing and funding within certain religious academic contexts.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, academic_theologians, payer,
    organized, generational, constrained, global).

% Often recite the creed as part of worship without necessarily assenting to its strict metaphysical claims, or even fully understanding them. Their experience of the creed as a communal ritual is overridden by the strict reading's insistence on cognitive assent to specific ontology.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, liturgical_participants, excluded,
    moderate, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a singular, universally binding metaphysical framework for Christian belief, ensuring doctrinal uniformity and preventing theological fragmentation across diverse communities.
% TRANSFER_FUNCTION: Transfers interpretive authority over core theological concepts from individual believers and local communities to a centralized, hierarchical clergy, in exchange for a clear, stable, and enforced doctrinal boundary.
% ABSENT_VOICES: Those who view the creed as a symbolic statement of faith, a liturgical practice, or a historically contingent document are excluded from the interpretive process; they would argue for a more expansive understanding of theological truth and less rigid enforcement.
% DISAPPEARANCE_RATIONALE: If the strict enforcement of the Nicene Creed as a metaphysical ontology vanished, the hierarchical clergy would lose a primary source of their authority, heterodox communities would flourish, and theological discourse would fragment into diverse interpretations, fundamentally altering the structure of orthodox Christianity.
% FOUNDING_PROBLEM: The early Christian church faced widespread doctrinal disputes regarding the nature of Christ and the Trinity, threatening its unity and coherence.
% FOUNDING_PROBLEM_CORROBORATION: Hierarchical clergy and orthodox theologians attest that doctrinal unity remains a live problem, citing ongoing theological debates and challenges to traditional interpretations. Academic historians and theologians outside the strict tradition corroborate the historical problem but contest its contemporary 'live' status as a justification for strict enforcement, arguing the context has changed.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading demands strict cognitive assent to specific metaphysical claims, imposing significant costs on those with differing interpretations or spiritual experiences. Suppression is very high (0.90) due to the historical and ongoing mechanisms of heresy policing, excommunication, and social ostracism. Theater ratio is low (0.10) as the enforcement is direct and functional, not merely performative; the stakes of deviation are real. The temporal measurements reflect a period of increasing enforcement and extractiveness as the doctrine solidified, followed by a slight decrease in modern times due to external pressures, though still remaining high.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hierarchical clergy, this constraint is a necessary 'rope' for maintaining the integrity and unity of the faith. From the perspective of heterodox communities, it is a 'snare' designed to enforce conformity and suppress alternative spiritual paths. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hierarchical clergy and orthodox theologians are clear beneficiaries, as their authority and intellectual framework are validated and enforced by this reading. Heterodox communities, lay interpreters, and academic theologians (who challenge the strict reading) are victims, bearing the costs of suppression and marginalization. Liturgical participants are excluded, as their experience of the creed is often not aligned with the strict cognitive assent demanded.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_vs_symbolic_interpretation,
    'Is the Nicene Creed fundamentally a statement of metaphysical ontology requiring cognitive assent, or a symbolic confession of faith primarily for liturgical and identity formation?',
    'Historical-critical analysis of early Christian theological debates and contemporary empirical studies on how believers actually engage with creedal statements in practice.',
    'If primarily symbolic, the measured extractiveness and suppression of this reading would be reclassified as illegitimate, as the constraint would be enforcing a function (metaphysical assent) not inherent to the creed''s primary purpose. If primarily metaphysical, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_vs_symbolic_interpretation, conceptual, 'Ambiguity in the fundamental nature and purpose of the Nicene Creed.').

omega_variable(
    authority_source_ambiguity,
    'Does the authority for interpreting the Nicene Creed reside solely with hierarchical clergy, or is it distributed among the broader community of believers and scholarly interpreters?',
    'Sociological and historical analysis of interpretive practices across different Christian traditions, particularly those with non-hierarchical structures.',
    'If authority is distributed, the ''agenda_setter'' role of hierarchical clergy in this reading would be challenged, potentially reclassifying the constraint as a ''tangled_rope'' or ''piton'' where a smaller group extracts from a broader, disempowered interpretive community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_source_ambiguity, empirical, 'Ambiguity regarding the legitimate source of interpretive authority for the creed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nice_tr_t400, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 400, 0.08).
narrative_ontology:measurement(nice_tr_t800, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(nice_tr_t1200, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1200, 0.12).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1700, 0.1).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(nice_be_t400, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 400, 0.8).
narrative_ontology:measurement(nice_be_t800, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 800, 0.88).
narrative_ontology:measurement(nice_be_t1200, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1200, 0.92).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1700, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(nice_su_t400, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 400, 0.85).
narrative_ontology:measurement(nice_su_t800, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 800, 0.95).
narrative_ontology:measurement(nice_su_t1200, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1200, 0.98).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1700, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, liturgical_habituation_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nicene_creed_authority' kernel. This 'strict_orthodox_reading' emphasizes metaphysical ontology and hierarchical enforcement, contrasting with the 'symbolic_confessional_reading' (historical witness) and 'liturgical_habituation_reading' (ritual identity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
