% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Doctrinal Authority: Rupture-Progressive Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rupture-progressive' reading of
 *   Vatican II's doctrinal authority. From this perspective, Vatican II
 *   represented a necessary break with pre-conciliar rigidity, and the
 *   'spirit of the Council' authorizes ongoing reform beyond the literal
 *   textual limits of the documents. This reading emphasizes doctrinal
 *   development, particularly on issues like religious freedom (seen as a
 *   reversal of prior condemnations like the Syllabus of Errors), and
 *   interprets textual ambiguities as intentional openings for future
 *   theological and pastoral development. Post-conciliar implementation is
 *   viewed as the authentic realization of conciliar intent. The constraint
 *   is claimed as a Rope by its proponents, but its operation, from the
 *   perspective of those who bear its costs, is substantially extractive and
 *   actively enforced, leading to a computed Tangled Rope or Snare
 *   classification for those seats.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.82).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority: Rupture-Progressive Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, '85aa862a-07cf-44d7-b45a-c9447d40674e').
narrative_ontology:cs_kernel_codification('85aa862a-07cf-44d7-b45a-c9447d40674e', fixed_text).
narrative_ontology:cs_authority_grounding('85aa862a-07cf-44d7-b45a-c9447d40674e', lineage).
narrative_ontology:cs_interpretation_layer_present('85aa862a-07cf-44d7-b45a-c9447d40674e').
narrative_ontology:cs_reading_relation('85aa862a-07cf-44d7-b45a-c9447d40674e', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('85aa862a-07cf-44d7-b45a-c9447d40674e', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('85aa862a-07cf-44d7-b45a-c9447d40674e', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('85aa862a-07cf-44d7-b45a-c9447d40674e', foundational, doctrinal_development_is_dynamic).
narrative_ontology:cs_axiom_status(doctrinal_development_is_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('85aa862a-07cf-44d7-b45a-c9447d40674e', doctrinal_development_is_dynamic, deontological).
narrative_ontology:cs_axiom('85aa862a-07cf-44d7-b45a-c9447d40674e', foundational, spirit_of_council_guides_interpretation).
narrative_ontology:cs_axiom_status(spirit_of_council_guides_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('85aa862a-07cf-44d7-b45a-c9447d40674e', spirit_of_council_guides_interpretation, conventional).
narrative_ontology:cs_reference_frame('85aa862a-07cf-44d7-b45a-c9447d40674e', post_conciliar_renewal).
narrative_ontology:cs_drift_state('85aa862a-07cf-44d7-b45a-c9447d40674e', contemporary_church, gap(stable, minor, true)).
narrative_ontology:cs_created_at('85aa862a-07cf-44d7-b45a-c9447d40674e', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians_and_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_partners).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy_and_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, conservative_catholic_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, catholic_laity_at_large).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, catholic_laity_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and implement reforms based on the 'spirit of the Council,' interpreting ambiguities as intentional openings for further development. They benefit from the expanded scope for theological innovation and institutional adaptation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians_and_bishops, agenda_setter,
    institutional, generational, mobile, global).

% Experience the 'rupture' as a loss of traditional practices, doctrinal clarity, and institutional stability. They bear the cost of adapting to new norms or face marginalization for resisting. Their identity is often deeply tied to pre-conciliar forms.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy_and_laity, payer,
    organized, biographical, identity_locked, global).

% Officially promulgates and interprets the Council's documents, navigating internal tensions while generally affirming the progressive trajectory. They enforce the new interpretive framework, balancing continuity with perceived necessity for change.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, magisterium_of_the_church, agenda_setter,
    institutional, civilizational, constrained, universal).

% Experience the Church's adaptation to modern society, including changes in liturgy, ecumenical outreach, and social teaching. Some embrace these changes as beneficial, others find them disorienting or alienating, indirectly bearing the cost of internal division.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, catholic_laity_at_large, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, catholic_laity_at_large, payer).

% Benefit from the increased openness, dialogue, and shared theological ground fostered by the Council's progressive interpretation, which facilitates inter-Christian and inter-religious engagement.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_partners, beneficiary,
    organized, generational, mobile, global).

% Analyze the historical development and impact of Vatican II, often providing critical perspectives on both the 'rupture' and 'continuity' claims, without being directly subject to the Church's disciplinary authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, historical_theologians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To adapt the Catholic Church to the challenges and opportunities of the modern world, foster Christian unity, and renew its internal life and mission.
% TRANSFER_FUNCTION: Transfers interpretive authority from strict textual literalism and pre-conciliar rigidity to a dynamic, contextual understanding guided by the 'spirit of the Council'; shifts power towards those advocating for ongoing reform and away from traditionalist factions.
% ABSENT_VOICES: Those who formally separated from the Catholic Church due to perceived doctrinal rupture, or those whose deeply traditionalist views are systematically marginalized within official discourse. They would argue for a return to pre-conciliar norms and condemn the 'spirit of the Council' as a source of error.
% DISAPPEARANCE_RATIONALE: If the 'rupture-progressive' interpretation of Vatican II vanished, the Catholic Church's identity, mission, and relationship with the modern world would be fundamentally altered. The ongoing reforms would halt, traditionalist views would gain prominence, and ecumenical and inter-religious dialogue would likely diminish, leading to a significant reorganization of institutional priorities and theological discourse.
% FOUNDING_PROBLEM: The Catholic Church was perceived as rigid, isolated, and irrelevant in a rapidly changing modern world, lacking engagement with other Christian traditions and experiencing internal stagnation.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians, many bishops, and a significant portion of the Catholic laity attest that the founding problems were real and the Council's progressive interpretation continues to address them. Traditionalist groups and some historical analyses, however, contest the necessity or wisdom of the 'rupture' approach, arguing it created new problems while failing to fully resolve the old ones. Independent sociological studies of religious practice offer mixed corroboration, showing both adaptation and decline in different contexts.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.82, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.82) because this reading imposes significant doctrinal and practical changes on those who adhere to pre-conciliar traditions, demanding adaptation or marginalization. Suppression is also high (0.78) as the institutional authority actively enforces this interpretive framework, limiting the space for traditionalist dissent or alternative interpretations. The theater ratio is moderate (0.40), reflecting that while genuine theological and pastoral reforms occurred, some aspects of 'reform' may also serve to manage internal dissent or maintain institutional control under the guise of progress. Accessibility collapse is moderate-high (0.65) as pre-conciliar theological and liturgical forms become less accessible or officially discouraged. Resistance is high (0.75) from traditionalist groups who actively oppose this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The 'rupture-progressive' reading is experienced as a necessary and beneficial adaptation by its proponents (progressive theologians, ecumenical partners), who see it as a Rope or Scaffold. However, for traditionalist clergy and laity, the same constraint operates as a Snare or Tangled Rope, extracting their preferred forms of worship and belief, and suppressing their resistance. The Magisterium, while generally aligning with the progressive trajectory, must also manage the internal tensions, experiencing the constraint as an institutional Rope or Tangled Rope with significant enforcement costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians and ecumenical partners are clear beneficiaries, as the constraint enables their theological and pastoral agendas. Traditionalist clergy and laity are victims, bearing the costs of doctrinal and liturgical shifts, often feeling identity-locked due to their deep commitment to the Church. The Magisterium acts as the agenda-setter, enforcing the progressive interpretation while also being constrained by the need to maintain institutional unity. Catholic laity at large are a mixed group, some benefiting from perceived relevance, others paying the cost of internal division and alienation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_necessity_of_rupture,
    'Was the ''rupture'' with pre-conciliar rigidity truly historically necessary for the Church''s survival and mission, or were alternative paths of organic development viable?',
    'Counterfactual historical analysis comparing the Church''s trajectory with and without the ''rupture-progressive'' interpretation, or comparative studies of other religious traditions'' adaptations to modernity.',
    'If alternative paths were viable, the ''necessity'' claim underpinning this reading''s legitimacy weakens, potentially reclassifying aspects of its enforcement as pure extraction rather than coordination for survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_necessity_of_rupture, conceptual, 'Contestability of the ''necessary break'' claim.').

omega_variable(
    spirit_vs_letter_interpretive_drift,
    'To what extent has the ''spirit of the Council'' interpretation genuinely remained faithful to the Council''s textual ambiguities and intentions, versus drifting into interpretations not supported by the documents?',
    'Detailed textual analysis of conciliar documents against post-conciliar theological developments, and historical studies of the Council Fathers'' stated intentions.',
    'If significant drift beyond textual support is demonstrated, the legitimacy of reforms based solely on the ''spirit'' would be challenged, potentially increasing the perceived extractiveness for those adhering to textual fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_vs_letter_interpretive_drift, empirical, 'Fidelity of ''spirit'' interpretation to Council texts.').

omega_variable(
    legitimacy_of_doctrinal_reversal,
    'Is the ''rupture-progressive'' reading''s assertion of doctrinal development, even to the point of reversing prior condemnations (e.g., religious freedom vs. Syllabus of Errors), consistent with the Catholic understanding of unchanging truth?',
    'Deep theological and philosophical analysis of the nature of doctrinal development, infallibility, and the sensus fidelium, engaging with both progressive and traditionalist hermeneutics.',
    'If deemed inconsistent, the foundational axioms of this reading would be severely undermined, potentially leading to a reclassification of its claims as a Snare for those committed to doctrinal immutability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_doctrinal_reversal, conceptual, 'Consistency of doctrinal reversal with unchanging truth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1962, 0.5).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.65).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1962, 0.45).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, liturgical_reform_norms).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_dialogue_protocols).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'vatican_ii_doctrinal_authority' kernel. This 'rupture-progressive' reading emphasizes a necessary break with the past and ongoing reform, differing significantly in its interpretation and implications from the 'continuity', 'rupture-traditionalist', and 'composite_overdetermination' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
