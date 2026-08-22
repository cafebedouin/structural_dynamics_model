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
 *   II, which views the Council as a necessary break with pre-conciliar
 *   rigidity and interprets the 'spirit of the Council' as authorizing
 *   ongoing reform beyond the strict textual limits of the documents. This
 *   reading emphasizes the Council's innovations (e.g., religious freedom,
 *   ecumenism) as reversals of prior teaching, and treats post-conciliar
 *   implementation as the authentic realization of conciliar intent. It is
 *   one reading of the broader 'vatican_ii_doctrinal_authority' kernel,
 *   alongside 'continuity_reading' and 'rupture_traditionalist_reading'.
 *
 * KEY AGENTS:
 *   - progressive_theologians: Primary beneficiary (organized/mobile) — provides theological justification for ongoing reforms.
 *   - reform_minded_clergy: Primary beneficiary (powerful/constrained) — implements changes in practice and liturgy.
 *   - traditionalist_clergy: Primary payer (organized/identity_locked) — marginalized for resisting reforms.
 *   - roman_curia: Agenda setter (institutional/constrained) — navigates and enforces interpretations, shaping policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.7).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.6).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority: Rupture-Progressive Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'a91d0b70-7b4e-4e16-ae69-17608c1238aa').
narrative_ontology:cs_kernel_codification('a91d0b70-7b4e-4e16-ae69-17608c1238aa', fixed_text).
narrative_ontology:cs_authority_grounding('a91d0b70-7b4e-4e16-ae69-17608c1238aa', lineage).
narrative_ontology:cs_interpretation_layer_present('a91d0b70-7b4e-4e16-ae69-17608c1238aa').
narrative_ontology:cs_reading_relation('a91d0b70-7b4e-4e16-ae69-17608c1238aa', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a91d0b70-7b4e-4e16-ae69-17608c1238aa', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('a91d0b70-7b4e-4e16-ae69-17608c1238aa', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('a91d0b70-7b4e-4e16-ae69-17608c1238aa', foundational, spirit_of_council_authorizes_development).
narrative_ontology:cs_axiom_status(spirit_of_council_authorizes_development, holdable).
narrative_ontology:cs_axiom_grounding('a91d0b70-7b4e-4e16-ae69-17608c1238aa', spirit_of_council_authorizes_development, conventional).
narrative_ontology:cs_axiom('a91d0b70-7b4e-4e16-ae69-17608c1238aa', foundational, doctrinal_rupture_with_pre_conciliar_rigidity).
narrative_ontology:cs_axiom_status(doctrinal_rupture_with_pre_conciliar_rigidity, holdable).
narrative_ontology:cs_axiom_grounding('a91d0b70-7b4e-4e16-ae69-17608c1238aa', doctrinal_rupture_with_pre_conciliar_rigidity, empirically_contingent).
narrative_ontology:cs_reference_frame('a91d0b70-7b4e-4e16-ae69-17608c1238aa', post_conciliar_aggiornamento).
narrative_ontology:cs_drift_state('a91d0b70-7b4e-4e16-ae69-17608c1238aa', contemporary_pontificate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a91d0b70-7b4e-4e16-ae69-17608c1238aa', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_minded_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_seeking_modernization).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, conservative_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the 'spirit of the Council' interpretation, which provides theological justification for ongoing reforms and allows for development beyond strict textual limits. Their careers and influence are often tied to this progressive hermeneutic.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians, beneficiary,
    organized, generational, mobile, global).

% Utilize the progressive reading to implement changes in liturgy, pastoral practice, and ecumenical relations. They find their authority and mandate strengthened by this interpretation, often facing resistance from traditionalist elements.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_minded_clergy, beneficiary,
    powerful, biographical, constrained, national).

% Experience the Church as more open, inclusive, and relevant to modern life under this interpretation. They are often the recipients of pastoral innovations and find their concerns addressed, but their influence is often mediated through clergy.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_seeking_modernization, beneficiary,
    moderate, biographical, constrained, local).

% Bear the costs of perceived doctrinal and liturgical changes, feeling that their understanding of tradition is undermined. They are often marginalized or disciplined for resisting reforms, with their identity deeply tied to pre-conciliar forms.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    organized, generational, identity_locked, global).

% Experience alienation and confusion due to changes they perceive as ruptures with established faith. They often seek out traditionalist communities or leave the Church, feeling that their spiritual needs are not met by the progressive interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, conservative_laity, payer,
    moderate, biographical, constrained, local).

% Institutions (e.g., religious orders, seminaries) founded on pre-conciliar norms and practices face pressure to adapt or risk suppression. Their very existence is challenged by the progressive reading's emphasis on rupture and ongoing reform.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_institutions, payer,
    institutional, generational, trapped, global).

% The central administrative body of the Catholic Church, which often navigates between different hermeneutics. While officially promoting 'continuity in reform,' elements within it actively enforce or permit the progressive reading's implementation, shaping policy and appointments.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_curia, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the Catholic Church to engage with modernity, fostering ecumenical dialogue, religious freedom, and liturgical adaptation, thereby coordinating the Church's mission in a changing world.
% TRANSFER_FUNCTION: Transfers interpretive authority from strict adherence to pre-conciliar texts and practices towards a dynamic, evolving understanding of tradition, empowering those who advocate for ongoing reform and modernization.
% ABSENT_VOICES: Those who believe the Council itself was illegitimate or invalid are entirely excluded from the interpretive debate, their positions deemed outside the bounds of Catholic discourse. They would argue for a return to pre-conciliar doctrine and practice.
% DISAPPEARANCE_RATIONALE: If the progressive reading of Vatican II vanished, the Church would face an immediate crisis of identity and mission. Many reforms would be called into question, leading to widespread dissent among clergy and laity who have embraced modernization, and a significant reorganization of theological and pastoral priorities.
% FOUNDING_PROBLEM: The Catholic Church faced increasing irrelevance and alienation from modern society, with rigid structures and doctrines perceived as out of touch with contemporary intellectual, social, and political developments.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and many bishops attest that the problem of engaging modernity remains live, requiring ongoing adaptation. Traditionalist critics, however, argue that the Council exacerbated the problem by introducing confusion and weakening doctrinal clarity; independent sociological studies of religious adherence offer mixed corroboration, showing both increased engagement in some areas and decline in others.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.7) is high because this reading imposes significant costs on those who adhere to pre-conciliar forms, demanding adaptation or marginalization. Suppression (0.6) is present through institutional pressure and disciplinary actions against traditionalist dissent. Theater ratio (0.4) reflects that while genuine reform efforts exist, a portion of the 'spirit of the Council' rhetoric serves to legitimize ongoing power shifts rather than purely functional development. Accessibility collapse (0.4) is moderate, as alternatives (e.g., traditionalist groups) exist but are constrained. Resistance (0.7) is high, indicating active opposition from those who feel extracted from.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of progressive theologians and reform-minded clergy, this reading is a necessary Rope or Scaffold, enabling the Church to fulfill its mission in the modern world. For traditionalist clergy and conservative laity, it operates as a Snare or Tangled Rope, extracting their adherence to tradition and imposing unwanted changes. The Roman Curia, as agenda-setter, often attempts to frame it as a Rope (continuity in reform) while selectively enforcing the progressive elements.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians and reform-minded clergy are beneficiaries, as this reading empowers their agenda and provides a framework for their work. Traditionalist clergy and conservative laity are payers, bearing the costs of doctrinal and liturgical shifts. The Roman Curia, while officially promoting a nuanced view, often acts as an agenda-setter that facilitates the progressive interpretation, thus benefiting from the resulting institutional dynamism.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the 'spirit of the Council' as pure coordination. While it genuinely coordinates a response to modernity, the high extractiveness and suppression indicate that it also functions to reallocate power and legitimate specific theological agendas, making it a Tangled Rope rather than a pure Rope. The ongoing contestation over its meaning prevents it from becoming a Piton, as its function is still actively debated and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_vs_letter_ambiguity,
    'To what extent does the ''spirit of the Council'' genuinely reflect the Council Fathers'' intent, versus being a post-conciliar construct used to justify further reforms?',
    'Historical-theological analysis of conciliar debates, periti (expert) writings, and early post-conciliar interpretations, seeking convergence or divergence from later progressive readings.',
    'If largely a post-conciliar construct, the extractiveness of this reading increases, as its claims to authority are weakened. If it genuinely reflects intent, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_vs_letter_ambiguity, conceptual, 'Ambiguity between the explicit text of Vatican II and its ''spirit''.').

omega_variable(
    doctrinal_reversal_or_development,
    'Are specific conciliar teachings (e.g., religious freedom) genuine doctrinal reversals of prior magisterial teaching, or can they be reconciled as organic developments?',
    'Systematic theological comparison of conciliar documents with pre-conciliar magisterial texts (e.g., Syllabus of Errors), seeking logical contradiction or hermeneutical continuity.',
    'If genuine reversals, this reading''s claim of ''rupture'' is strengthened, justifying its high extractiveness from traditionalists. If organic developments, the ''continuity_reading'' gains ground, reducing the perceived extractiveness of this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_reversal_or_development, conceptual, 'Whether conciliar teachings represent rupture or development.').

omega_variable(
    institutional_enforcement_legitimacy,
    'Is the Roman Curia''s enforcement of the progressive reading perceived as legitimate by all parties, or is it seen as an exercise of raw power?',
    'Sociological studies of clerical and lay attitudes towards curial authority, analysis of disciplinary actions, and patterns of episcopal appointments.',
    'If widely seen as illegitimate power, the suppression metric for this reading increases, and its classification shifts closer to a Snare. If legitimate, its coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_legitimacy, empirical, 'Legitimacy of institutional enforcement of the progressive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.69).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
