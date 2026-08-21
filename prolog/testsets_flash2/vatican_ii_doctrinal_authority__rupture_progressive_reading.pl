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
 *   This constraint story models the 'rupture-progressive' reading of Vatican
 *   II's doctrinal authority, which views the Council as a necessary break
 *   with pre-conciliar rigidity and uses the 'spirit of the Council' to
 *   authorize ongoing reform beyond textual limits. This reading emphasizes
 *   doctrinal development, religious freedom (seen as a reversal of the
 *   Syllabus of Errors), and interprets textual ambiguities as intentional
 *   openings for further development. Post-conciliar implementation is
 *   treated as the authentic realization of conciliar intent. The constraint
 *   is classified as a Tangled Rope due to its genuine coordination function
 *   (modernization) coupled with asymmetric extraction from traditionalist
 *   elements.
 *
 * KEY AGENTS:
 *   - progressive_theologians: Agenda setter (organized/mobile) — drives interpretive framework
 *   - reform_minded_clergy: Beneficiary (powerful/constrained) — implements reforms
 *   - laity_seeking_modernization: Beneficiary (moderate/mobile) — embraces changes
 *   - traditionalist_clergy: Payer (organized/identity_locked) — resists changes, bears costs
 *   - conservative_laity: Payer (moderate/constrained) — disoriented by changes, bears costs
 *   - doctrinal_conservatives: Payer (powerful/constrained) — intellectual opponents, bears costs
 *   - magisterium: Agenda setter (institutional/constrained) — navigates interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority: Rupture-Progressive Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'd14f14f4-5d2d-432e-86f1-201cc1c7a32a').
narrative_ontology:cs_kernel_codification('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', fixed_text).
narrative_ontology:cs_authority_grounding('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', lineage).
narrative_ontology:cs_interpretation_layer_present('d14f14f4-5d2d-432e-86f1-201cc1c7a32a').
narrative_ontology:cs_reading_relation('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', foundational, spirit_of_council_authorizes_development).
narrative_ontology:cs_axiom_status(spirit_of_council_authorizes_development, holdable).
narrative_ontology:cs_axiom_grounding('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', spirit_of_council_authorizes_development, conventional).
narrative_ontology:cs_axiom('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', foundational, religious_freedom_as_doctrinal_progress).
narrative_ontology:cs_axiom_status(religious_freedom_as_doctrinal_progress, holdable).
narrative_ontology:cs_axiom_grounding('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', religious_freedom_as_doctrinal_progress, deontological).
narrative_ontology:cs_reference_frame('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', post_conciliar_reform_mandate).
narrative_ontology:cs_drift_state('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', contemporary_hermeneutical_contest, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d14f14f4-5d2d-432e-86f1-201cc1c7a32a', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_minded_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_seeking_modernization).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, conservative_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, doctrinal_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Vatican II as a mandate for ongoing reform, emphasizing the 'spirit of the Council' over strict textual literalism. They gain influence and academic freedom by pushing for doctrinal and pastoral development.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians, agenda_setter,
    organized, generational, mobile, global).

% Implement reforms in liturgy, ecumenism, and social teaching, finding justification and authority in the progressive reading. They benefit from a more adaptable and relevant church, but are constrained by hierarchical structures.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_minded_clergy, beneficiary,
    powerful, biographical, constrained, national).

% Embrace the changes and find the Church more aligned with contemporary values. They benefit from a less rigid and more inclusive religious experience, but their influence on doctrinal matters is limited.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_seeking_modernization, beneficiary,
    moderate, biographical, mobile, local).

% Experience the progressive reading as a betrayal of tradition, leading to doctrinal confusion and liturgical abuses. They bear the cost of feeling alienated within their own institution, often facing marginalization or disciplinary action for resisting reforms.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    organized, generational, identity_locked, global).

% Are disoriented by rapid changes and perceive a loss of clear doctrinal identity. They bear the cost of spiritual unease and often seek solace in traditionalist communities outside mainstream structures.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, conservative_laity, payer,
    moderate, biographical, constrained, local).

% Argue that the progressive reading undermines the Church's perennial teaching and leads to relativism. They bear the cost of constant intellectual and pastoral struggle against what they perceive as heterodoxy, often losing institutional battles.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, doctrinal_conservatives, payer,
    powerful, generational, constrained, global).

% The teaching authority of the Church, which attempts to navigate between progressive and traditionalist interpretations. While officially promoting a 'hermeneutic of reform in continuity,' its actions are often perceived as favoring one reading over another, leading to ongoing contestation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, magisterium, agenda_setter,
    institutional, civilizational, constrained, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Church's adaptation to the modern world, allowing for engagement with contemporary thought and social issues, and fostering ecumenical dialogue, thereby maintaining relevance and preventing alienation of a significant portion of the faithful.
% TRANSFER_FUNCTION: Transfers interpretive authority from a strict, literal reading of conciliar texts and pre-conciliar tradition to a more dynamic, 'spirit-driven' interpretation, empowering progressive theological currents and disempowering traditionalist ones.
% ABSENT_VOICES: Those who left the Church due to perceived doctrinal laxity or liturgical changes, as well as those who were marginalized or silenced for advocating for a more traditional interpretation, are absent. They would argue that the 'spirit of the Council' has been used to justify deviations from core Catholic teaching.
% DISAPPEARANCE_RATIONALE: If this progressive reading of Vatican II's authority vanished, the Church would face an immediate crisis of relevance and internal coherence. Progressive theologians would lose their interpretive framework, reform efforts would halt, and the Church's engagement with modernity would be severely curtailed, leading to a significant reorganization of its intellectual and pastoral life.
% FOUNDING_PROBLEM: The Church faced a crisis of relevance in the modern world, perceived as rigid, anachronistic, and isolated, leading to declining engagement and intellectual stagnation among many Catholics.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and many lay Catholics attest that the problem of modern relevance remains live, requiring ongoing adaptation. Traditionalist critics, however, argue that the 'solution' created new problems of doctrinal confusion and internal division, making the original problem's status contested.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) reflects the significant cost borne by traditionalist and conservative elements who feel alienated and disempowered by the progressive interpretation. Suppression (0.55) is present through institutional pressure and marginalization of dissenting voices, though not outright coercion. Theater ratio (0.25) is moderate; while there's genuine reform, some appeals to the 'spirit' are performative justifications for pre-determined outcomes. Accessibility collapse (0.45) is moderate, as traditionalist alternatives exist but are often outside mainstream structures. Resistance (0.70) is high, reflecting ongoing internal conflict and organized opposition from traditionalist groups. The temporal measurements show a rise in extractiveness and suppression in the decades following the Council, as the progressive reading gained institutional traction, then stabilized as the contest became entrenched.
 *
 * PERSPECTIVAL GAP:
 *   Progressive theologians and reform-minded clergy experience this as a Rope, enabling necessary adaptation and growth. Traditionalist clergy and conservative laity experience it as a Snare, extracting their adherence to tradition and imposing an alien interpretive framework. The Magisterium, as the institutional agenda-setter, attempts to frame it as a Rope (continuity in reform), but its actions often contribute to the Tangled Rope experience for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians and reform-minded clergy are beneficiaries (low d) as their interpretive framework gains institutional validation and power. Traditionalist clergy and conservative laity are victims (high d) as their positions are marginalized and they bear the costs of doctrinal shifts. The Magisterium, while an agenda-setter, is also constrained by the need to maintain unity, leading to a more symmetric d, though its actions often benefit the progressive reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling the progressive reading as pure extraction, acknowledging its genuine coordination function in adapting the Church to modernity. However, it also highlights the asymmetric costs imposed on those who do not align with this interpretive shift, preventing it from being mislabeled as a pure Rope. The ongoing contestation over the 'spirit' versus the 'letter' of the Council indicates that the mandate is not fully resolved, as the 'founding problem' of relevance is still live, but the 'solution' itself generates new forms of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_vs_letter_ambiguity,
    'To what extent does the ''spirit of the Council'' genuinely reflect the Council Fathers'' intent, versus serving as a rhetorical device to justify post-conciliar developments?',
    'Historical-theological analysis of conciliar debates, periti (expert) writings, and subsequent magisterial interpretations, seeking convergence or divergence on specific points of ''spirit'' versus ''letter''.',
    'If primarily a rhetorical device, the extractiveness of the progressive reading increases, as it relies more on power than genuine interpretive authority. If it genuinely reflects intent, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_vs_letter_ambiguity, conceptual, 'Ambiguity in the ''spirit of the Council'' as interpretive authority.').

omega_variable(
    doctrinal_reversal_or_development,
    'Are specific post-conciliar doctrinal shifts (e.g., religious freedom, ecumenism) genuine developments of prior teaching, or do they represent a reversal of previously held doctrines?',
    'Systematic theological comparison of pre- and post-conciliar magisterial documents on specific points, assessing for logical contradiction versus organic growth.',
    'If reversals, the extractiveness from traditionalists is higher, as they are forced to accept what they perceive as contradiction. If developments, the coordination function is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_reversal_or_development, empirical, 'Whether doctrinal changes are development or reversal.').

omega_variable(
    institutional_resistance_sustainability,
    'Can traditionalist and conservative resistance to the progressive reading be sustained long-term within the institutional Church, or will it eventually be suppressed or co-opted?',
    'Longitudinal study of traditionalist movements'' growth, institutional recognition, and disciplinary actions against them over several decades.',
    'If resistance is suppressed, the overall suppression metric for the progressive reading increases. If sustained, it indicates a more persistent contestation and limits the progressive reading''s totalizing power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_resistance_sustainability, empirical, 'Sustainability of traditionalist resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.23).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2015, 0.69).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.57).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vatican_ii_doctrinal_authority' kernel. Its progressive interpretation directly influences and is influenced by other readings, particularly the continuity and rupture-traditionalist readings, as they contest the same interpretive space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
