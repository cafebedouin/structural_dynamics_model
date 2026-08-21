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
 *   This constraint represents the 'rupture-progressive' reading of Vatican
 *   II, which interprets the Council as a necessary break with pre-conciliar
 *   rigidity and authorizes ongoing reform beyond the literal text, guided by
 *   the 'spirit of the Council.' This reading is a specific instantiation of
 *   the broader 'vatican_ii_doctrinal_authority' kernel, distinct from
 *   'continuity' or 'rupture-traditionalist' interpretations. It emphasizes
 *   the Council's intent to open the Church to the modern world, even if this
 *   means re-evaluating or re-interpreting prior doctrinal statements.
 *
 * KEY AGENTS:
 *   - progressive_theologians: Primary agenda-setter (organized/mobile) — drives interpretation and reform.
 *   - reform_minded_clergy: Beneficiary (institutional/constrained) — implements changes at local level.
 *   - laity_seeking_modernization: Beneficiary (moderate/mobile) — finds validation and relevance.
 *   - traditionalist_clergy: Primary payer (organized/identity_locked) — resists changes, faces marginalization.
 *   - conservative_laity: Payer (powerless/constrained) — feels alienated, seeks alternatives.
 *   - doctrinal_conservatives: Payer (institutional/identity_locked) — structurally disadvantaged in shaping official teaching.
 *   - magisterium: Agenda-setter (institutional/constrained) — navigates and sometimes endorses progressive interpretations.
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
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'a6e4489a-a11a-48ed-913c-7525a071b3e6').
narrative_ontology:cs_kernel_codification('a6e4489a-a11a-48ed-913c-7525a071b3e6', formalized).
narrative_ontology:cs_authority_grounding('a6e4489a-a11a-48ed-913c-7525a071b3e6', lineage).
narrative_ontology:cs_interpretation_layer_present('a6e4489a-a11a-48ed-913c-7525a071b3e6').
narrative_ontology:cs_reading_relation('a6e4489a-a11a-48ed-913c-7525a071b3e6', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6e4489a-a11a-48ed-913c-7525a071b3e6', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6e4489a-a11a-48ed-913c-7525a071b3e6', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('a6e4489a-a11a-48ed-913c-7525a071b3e6', foundational, spirit_of_council_guides_development).
narrative_ontology:cs_axiom_status(spirit_of_council_guides_development, holdable).
narrative_ontology:cs_axiom_grounding('a6e4489a-a11a-48ed-913c-7525a071b3e6', spirit_of_council_guides_development, conventional).
narrative_ontology:cs_axiom('a6e4489a-a11a-48ed-913c-7525a071b3e6', foundational, doctrinal_adaptation_to_modernity_is_necessary).
narrative_ontology:cs_axiom_status(doctrinal_adaptation_to_modernity_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a6e4489a-a11a-48ed-913c-7525a071b3e6', doctrinal_adaptation_to_modernity_is_necessary, instrumental).
narrative_ontology:cs_reference_frame('a6e4489a-a11a-48ed-913c-7525a071b3e6', post_conciliar_renewal).
narrative_ontology:cs_drift_state('a6e4489a-a11a-48ed-913c-7525a071b3e6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a6e4489a-a11a-48ed-913c-7525a071b3e6', '').
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

% Interpret Vatican II as a mandate for ongoing reform, emphasizing the 'spirit of the Council' over strict textual literalism. They gain influence and academic legitimacy by driving theological innovation and adapting doctrine to contemporary thought.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians, agenda_setter,
    organized, generational, mobile, global).

% Find justification for pastoral and liturgical changes in their dioceses, aligning with modern sensibilities. They benefit from the perceived legitimacy of the Council to implement reforms that might otherwise face significant resistance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_minded_clergy, beneficiary,
    institutional, biographical, constrained, national).

% Experience the Church as more relevant and open to the modern world, validating their desire for a less rigid religious experience. They benefit from a sense of inclusion and intellectual compatibility with their faith.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_seeking_modernization, beneficiary,
    moderate, biographical, mobile, local).

% Resist changes they perceive as a betrayal of immutable tradition, often facing marginalization or disciplinary action for non-compliance. They bear the cost of being out of step with the dominant interpretive current, risking their careers and standing.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    organized, generational, identity_locked, global).

% Feel alienated by changes they view as undermining the faith, often seeking out traditionalist communities or leaving the Church. They pay with a loss of spiritual home and a sense of betrayal, with limited options for recourse within the mainstream.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, conservative_laity, payer,
    powerless, biographical, constrained, local).

% Are structurally disadvantaged in shaping official Church teaching and practice, as their interpretations are often dismissed as 'pre-conciliar' or 'rigid'. They bear the cost of seeing their theological framework systematically undermined by the progressive reading.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, doctrinal_conservatives, payer,
    institutional, civilizational, identity_locked, global).

% The teaching authority of the Church, which attempts to navigate and sometimes adjudicate between competing interpretations. While officially promoting 'continuity in reform,' its actions are often read by progressives as endorsing further development beyond strict textual limits, thereby enabling the progressive agenda.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, magisterium, agenda_setter,
    institutional, civilizational, constrained, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the Catholic Church to engage with modernity, adapting its pastoral approach and theological expression to contemporary cultural and intellectual currents, thereby maintaining relevance and attracting new adherents.
% TRANSFER_FUNCTION: Transfers interpretive authority from strict adherence to pre-conciliar texts and traditions to a dynamic, 'spirit-led' hermeneutic, empowering progressive theological and pastoral initiatives while marginalizing traditionalist ones.
% ABSENT_VOICES: The voices of those who left the Church due to perceived doctrinal rupture or who were excommunicated for rejecting Vatican II are absent from the official discourse, but their dissent continues to fuel traditionalist movements outside the mainstream.
% DISAPPEARANCE_RATIONALE: If the rupture-progressive reading of Vatican II vanished, the Church would face an immediate crisis of legitimacy regarding its post-conciliar reforms. Progressive theologians and clergy would lose their primary justification for ongoing change, leading to a significant re-entrenchment of traditionalist positions and a potential schism as the 'spirit of the Council' interpretation is no longer available to bridge perceived contradictions.
% FOUNDING_PROBLEM: The Church faced a crisis of relevance in the modern world, perceived as rigid, outmoded, and unable to engage with contemporary intellectual, scientific, and social developments, leading to declining engagement and intellectual alienation.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and many clergy attest that the problem of relevance remains live, arguing for continued adaptation. Traditionalist clergy and conservative laity, however, contend that the 'solution' itself created new problems of doctrinal confusion and loss of identity, making the original problem's status contested.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.68) reflects the cost borne by traditionalists who are marginalized or disciplined for resisting the progressive interpretation, and the re-interpretation of doctrines that were previously considered settled. Suppression (0.55) is moderate but present, as official channels often favor progressive interpretations, making it difficult for traditionalist views to gain traction or be officially endorsed. Theater ratio (0.25) indicates that while genuine theological development occurs, some arguments for 'continuity' are performative, masking a more fundamental shift. The temporal measurements show a rise in extractiveness and suppression as the progressive reading gained dominance and enforced its interpretation, with a slight leveling off in recent years as resistance solidified.
 *
 * PERSPECTIVAL GAP:
 *   Progressive agents experience this as a liberating and necessary adaptation, a 'rope' pulling the Church forward. Traditionalist agents experience it as a 'snare' that extracts their doctrinal certainty and marginalizes their identity. The Magisterium, while officially seeking 'continuity,' often acts in ways that enable the progressive reading, making it an 'agenda_setter' that benefits from the flexibility this reading provides.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians and reform-minded clergy are beneficiaries, as this reading empowers their agenda and provides legitimacy for their work. Traditionalist clergy and conservative laity are victims, as their adherence to pre-conciliar norms is undermined and they face pressure to conform or be marginalized. The Magisterium, while a complex actor, benefits from the interpretive flexibility that allows it to navigate modern challenges, even as it attempts to balance competing readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to make the Church relevant to the modern world) is still live, but the method (rupture-progressive interpretation) has become a source of internal extraction. The classification as a Tangled Rope prevents mislabeling it as pure coordination (ignoring the victims) or pure extraction (ignoring the genuine coordination function of adapting to modernity). The ongoing contestation over the 'spirit' versus the 'letter' of the Council is central to its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_vs_letter_ambiguity,
    'To what extent does the ''spirit of the Council'' genuinely reflect the Council Fathers'' intentions, versus serving as a rhetorical device to justify post-conciliar innovations?',
    'Historical-theological analysis of conciliar debates, periti (expert) writings, and subsequent magisterial interpretations, seeking to delineate the boundaries of legitimate development from novel interpretations.',
    'If the ''spirit'' is found to be largely a post-hoc justification, the extractiveness on traditionalists would be higher, as the coordination story would be weaker. If it genuinely reflects a broader intent, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_vs_letter_ambiguity, conceptual, 'Ambiguity between the literal text of Vatican II documents and their ''spirit''.').

omega_variable(
    doctrinal_reversal_or_development,
    'Are specific post-conciliar doctrinal shifts (e.g., religious freedom, ecumenism) genuine ''development of doctrine'' or a ''rupture'' with prior infallible teaching?',
    'Deep theological and historical analysis comparing pre-conciliar and post-conciliar magisterial documents, seeking to establish logical consistency or identify explicit contradictions.',
    'If deemed a rupture, the extractiveness on traditionalists is higher, and the constraint''s legitimacy as a ''rope'' is severely undermined, pushing it closer to a ''snare.'' If deemed development, the coordination function is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_reversal_or_development, conceptual, 'Whether post-conciliar changes represent development or reversal of doctrine.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of traditionalist views structural (institutional policies, academic gatekeeping) or internalized (self-censorship, fear of marginalization)?',
    'Sociological studies of clerical careers and theological publishing, combined with qualitative interviews with traditionalist clergy and laity regarding their experiences of expressing dissent.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — traditionalists carry the suppression with them after exit or even within the Church.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditionalist views.').


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
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2015, 0.69).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vatican_ii_doctrinal_authority' kernel. It represents the rupture-progressive interpretation, which emphasizes a break with pre-conciliar rigidity and ongoing reform. It is linked to other readings (continuity, rupture-traditionalist, composite-overdetermination) as part of a constraint family where different interpretations of the same historical event lead to structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
