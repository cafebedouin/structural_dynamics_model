% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Doctrinal Authority — Rupture-Progressive Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) was convened to address the Catholic Church's
 *   perceived rigidity and isolation. The Council promulgated 16 documents
 *   that were genuinely ambiguous on fundamental questions: How far should
 *   the Church adapt to the modern world? Can doctrinal positions reverse, or
 *   only develop? What is the relationship between continuity and change? The
 *   'rupture-progressive reading' claims Vatican II represents a necessary
 *   break with preconciliar rigidity, that the Council intentionally left
 *   textual ambiguities as openings for ongoing reform, and that the 'spirit
 *   of the Council' — the lived experience of change and the direction of
 *   historical adaptation — is the authoritative hermeneutic key. This
 *   reading was institutionalized in post-conciliar theology, seminary
 *   formation, and episcopal practice, especially in Western European and
 *   North American dioceses. It benefited progressive theologians and
 *   reform-minded bishops; it disoriented traditionalist clergy and lay
 *   faithful who experienced radical change without explicit textual warrant.
 *   The reading extracts from those who preferred stability and doctrinal
 *   fixity, while delivering coordination benefits (pastoral flexibility,
 *   ecumenical rapprochement) to those who embrace adaptive interpretation.
 *
 * KEY AGENTS:
 *   - progressive_reform_theologians (institutional, agenda-setter): interpret the Council's 'spirit' as authoritative; shape post-conciliar theology and episcopal practice
 *   - bishops_implementing_liberal_council_interpretation (institutional, beneficiary): benefit from interpretive flexibility to reform liturgy, decentralize authority, and adapt pastoral practice
 *   - traditionalist_clergy_resisting_implementation (moderate, payer, identity-locked): experience the reading as delegitimizing their preconciliar fidelity; cannot exit without abandoning their vocational identity
 *   - lay_faithful_disoriented_by_rapid_change (powerless, payer, trapped): experience radical discontinuity; lack recourse to shape the Council's interpretation
 *   - vatican_conservative_curia (powerful, payer, partially excluded): sees the reading as overreach; their interpretive authority is subordinated to the progressive narrative
 *   - pope_paul_vi_and_successors (institutional, agenda-setter/observer): navigate the hermeneutical tension; their authority is contested by the reading's claim to interpret the Council beyond papal guidance
 *   - ecumenical_dialogue_partners (institutional, beneficiary, mobile): benefit from the reading's authorization of rapprochement and doctrinal softening
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.52).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority — Rupture-Progressive Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'b842b86c-a1ce-4b6a-81c9-fb3b75719d9c').
narrative_ontology:cs_kernel_codification('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', fixed_text).
narrative_ontology:cs_authority_grounding('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', lineage).
narrative_ontology:cs_interpretation_layer_present('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c').
narrative_ontology:cs_reading_relation('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', foundational, doctrinal_positions_reversible_under_development).
narrative_ontology:cs_axiom_status(doctrinal_positions_reversible_under_development, holdable).
narrative_ontology:cs_axiom_grounding('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', doctrinal_positions_reversible_under_development, deontological).
narrative_ontology:cs_axiom('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', foundational, spirit_of_council_authorizes_ongoing_reform).
narrative_ontology:cs_axiom_status(spirit_of_council_authorizes_ongoing_reform, holdable).
narrative_ontology:cs_axiom_grounding('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', spirit_of_council_authorizes_ongoing_reform, conventional).
narrative_ontology:cs_reference_frame('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', post_conciliar_adaptive_reform).
narrative_ontology:cs_drift_state('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', contemporary_restorationist_pushback, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b842b86c-a1ce-4b6a-81c9-fb3b75719d9c', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_reform_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, bishops_implementing_liberal_council_interpretation).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy_resisting_implementation).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, lay_faithful_disoriented_by_rapid_change).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).

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
 *   The extractiveness (0.68) is high because the reading transfers interpretive authority from fixed doctrinal frameworks and hierarchical certainty toward adaptive, historically-conscious reinterpretation. Beneficiaries (progressive theologians, reform bishops) gain authority and flexibility; payers (traditionalists, disoriented lay faithful) lose doctrinal certainty and practice continuity. The reading's persistence depends on active enforcement: it must continuously override and delegitimize traditionalist interpretations, marginalize preconciliar theological schools, and reframe resistance to change as fidelity to the Council's true spirit. The theater_ratio (0.41) reflects the reading's growing investment in performative maintenance: by 2020, repeated invocations of the 'spirit of the Council' perform hermeneutical legitimacy rather than establishing it through new evidence or argument. The accessibility_collapse (0.62) is moderate: traditionalist alternatives remain available (SSPX, Tridentine communities, conservative bishops) but are marginal and socially costly; once the progressive reading becomes institutionalized in seminaries and dioceses, alternatives are harder to access. The resistance (0.71) is substantial because traditionalist clergy and intellectuals actively contest the reading, maintain parallel communities and interpretations, and mount theological arguments against the rupture thesis. The measured suppression (0.52) reflects the reading's reliance on institutional authority (removing traditionalist scholars from influential positions, marginalizing preconciliar liturgy, controlling seminary formation) rather than on coercive force — the suppression is institutional and professional, not violent.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive theologian and reform bishop seats, the reading is a liberation narrative: the Council freed the Church from preconciliar rigidity and authorized ongoing, responsible reform adapted to each generation's needs. The readings' truth is evident in the Council's own openness, in the pastoral benefits of reformed liturgy and ecumenical dialogue, and in the Council's implicit authorization of the 'spirit' over the letter. From the traditionalist clergy seat, the reading is a heretical innovation that overrides explicit Council texts to justify novelties the Council never authorized. The suppression they experience is unjust silencing of faithful doctrine-keeping. From the lay faithful's perspective, the reading created discontinuity and disorientation without consent. From the Vatican conservative curia's seat, the reading is an illegitimate appropriation of the Council's authority, subordinating papal magisterium to the preferences of a theological elite. The engine computes these per-seat divergences from the structural data (power differentials, exit options, beneficiary/victim positioning, institutional authority flow). The authored claim (tangled_rope) expects the computed types to diverge: agenda-setters and beneficiaries should compute toward rope or coordination; payers toward snare or extraction. The metrics are authored independently of any predicted type — the divergence is the signal the reading produces.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive reform theologians (institutional power, beneficiary role, agenda-setter position) sit at d ≈ 0.15 (full beneficiary end): the reading elevates their authority, validates their interpretive methods, and rewards their innovation. They exercise interpretive authority with minimal constraint — the 'spirit of the Council' language gives them hermeneutical flexibility. Bishops implementing liberal interpretation (institutional power, beneficiary role) sit at d ≈ 0.25 (beneficiary-weighted): they benefit from the interpretive flexibility to reform their dioceses, but they remain subject to papal guidance and face resistance from traditionalist clergy. Traditional clergy (moderate power, payer role, identity-locked exit) sit at d ≈ 0.78 (heavily targeted): they pay through loss of institutional authority, marginalization, and forced choice between abandoning preconciliar fidelity or maintaining it in tension with the dominant reading. Their identity-lock (vocational commitment to priesthood understood through preconciliar lens) makes exit costly and leaves them dependent on institutional sufferance. Lay faithful disoriented by change (powerless, payer, trapped) sit at d ≈ 0.85 (full target): they pay through discontinuity and loss of familiar practice; they have no institutional power to shape the reading and cannot easily exit. The Vatican conservative curia (institutional power, payer role with some excluded positionality) sits at d ≈ 0.65 (heavily targeted but with some power to resist): their interpretive authority is subordinated to the progressive narrative, but they retain some institutional voice and can push back. Ecumenical dialogue partners (institutional power, beneficiary role, mobile exit) sit at d ≈ 0.10 (full beneficiary): the reading benefits them by opening Catholic teaching and inviting engagement; they can engage or withdraw based on the Church's trajectory. No overrides are needed; the structural data yields coherent directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's mandate (Vatican II authorized rupture and ongoing adaptive reform) has not outlived its function. The Church continues to face modernity and remains ambiguous about how far to adapt. The 'spirit of the Council' continues to authorize interpretive flexibility in Pope Francis's pontificate (2013–), with his emphasis on mercy, accompaniment, and pastoral discernment over doctrinal rigidity. However, the reading has accumulated performative characteristics: the 'spirit of the Council' is invoked repeatedly without new interpretive content, suggesting theater ratio growth. The omega on suppression ambiguity is critical: if traditionalist resistance is primarily internalized (traditionalists have come to doubt their own reading), the reading's hold is stable even if institutional enforcement loosened. If suppression is structural, the reading is fragile to institutional change (a traditionally-inclined pope could begin dismantling the reading's infrastructure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_vs_letter_boundary,
    'Where does the legitimate extrapolation from the Council''s ''spirit'' end and unauthorized innovation begin? Are the Council''s textual ambiguities intentional openings or unresolved tensions?',
    'Hermeneutical analysis of Vatican II''s preparatory materials (the preparatory commissions'' debates, draft votes, final revisions) to establish authorial intent on key contested passages. Historical-theological reconstruction of what the Council''s participants believed they authorized.',
    'If ambiguities were intentional and authorized ongoing reform, the progressive reading''s hermeneutic is vindicated. If ambiguities were unresolved compromises the Council expected subsequent magisterium to settle definitively, the progressive reading overreaches its warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_vs_letter_boundary, empirical, 'Whether textual ambiguities in Vatican II are intentional openings or unresolved tensions awaiting settlement.').

omega_variable(
    doctrinal_development_vs_reversal,
    'Is religious freedom (Dignitatis Humanae) and ecumenical openness authentic development of preconciliar doctrine, or a reversal that contradicts the Syllabus of Errors and prior magisterium?',
    'Doctrinal genealogy: trace the logical chain from preconciliar doctrine to Vatican II teaching. If the chain is continuous (Vatican II reframes and develops preconciliar premises), development holds. If Vatican II reverses explicit prior doctrinal positions (e.g., the Church''s right to coerce non-Catholics), reversal holds. Historical documentation of whether Vatican II''s architects consciously embraced reversal or believed they were developing.',
    'If development, the progressive reading''s framing of rupture as merely apparent difference in context is supported. If reversal, the traditionalist reading''s claim that Vatican II abandoned core doctrine gains empirical support. The two readings'' axioms hinge on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_development_vs_reversal, empirical, 'Whether Vatican II teaching on religious freedom and ecclesiology is development or reversal of preconciliar doctrine.').

omega_variable(
    magisterial_authority_over_spirit,
    'Does the Pope (the current magisterium) have authority to settle the interpretation of Vatican II''s ''spirit,'' or does the ''spirit'' operate as an independent hermeneutical principle that can override papal clarification?',
    'Observe instances where a Pope explicitly interpreted a contested Council passage and assess whether the progressive theologians and bishops accept, resist, or reinterpret the papal clarification. If papal guidance is consistently treated as binding by progressive theologians, the magisterium retains interpretive authority. If papal guidance is routinely reinterpreted or subordinated to the ''spirit of the Council,'' the reading has severed magisterial authority from doctrinal interpretation.',
    'If the magisterium retains interpretive authority, the progressive reading is constrained and subject to papal correction. If the ''spirit'' operates independently, the reading has fundamentally redistributed ecclesiastical authority toward the theological community and away from the hierarchical magisterium.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magisterial_authority_over_spirit, empirical, 'Whether papal magisterium retains definitive interpretive authority over Vatican II or whether the ''spirit of the Council'' operates as an independent hermeneutic principle.').

omega_variable(
    identity_fusion_suppression,
    'Is the suppression of traditionalist resistance structural (institutional authority actively overriding competing interpretations) or internalized (traditionalists have come to doubt their own reading and accept the progressive narrative as authoritative)?',
    'Post-suppression trajectory analysis: traditionalist clergy and intellectuals who were marginalized after Vatican II — did they maintain conviction in their reading in private while capitulating outwardly, or did they internalize the progressive reading as superior? Examination of traditionalist communities (SSPX, sede vacantism, conservative parishes) that maintained parallel structures: do they persist out of conviction or out of institutionalized habit and identity-fusion?',
    'If suppression is primarily structural, traditionalists could re-emerge and contest the progressive reading if institutional constraints loosened. If internalized, traditionalists carry the suppression with them even after institutional barriers fall — the reading''s hold persists through psychological identification. The distinction affects the estimate of the reading''s fragility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_suppression, empirical, 'Whether traditionalist suppression is structural or internalized.').

omega_variable(
    kernel_reading_contest,
    'Which of the four competing readings of the vatican_ii_doctrinal_authority kernel — rupture_progressive, continuity, rupture_traditionalist, or composite_overdetermination — correctly represents what Vatican II was and what it authorized?',
    'This is the foundational omega that cannot be resolved by new data alone. The contest turns on hermeneutical frameworks (how to interpret ambiguous texts), on philosophical assumptions (whether doctrine can truly reverse or only develop), and on authority claims (whose reading of the Council''s intent is legitimate). Resolution requires adjudication across competing epistemologies of magisterial authority, which is a theological and philosophical question, not an empirical one.',
    'If continuity_reading is correct, the progressive reading is heretical overreach. If rupture_traditionalist is correct, the Council is contaminated by error and reform cannot proceed on its authority. If rupture_progressive is correct, the Council genuinely authorized ongoing, adaptive reform. If composite_overdetermination is correct, all three are partially right because the Council bundled incompatible changes without resolving their interaction. The entire ecclesiastical trajectory of the past 60 years is at stake.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of Vatican II is hermeneutically and theologically correct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1962, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(vati_tr_t2020, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2020, 0.41).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.61).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(vati_be_t2020, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1962, 0.25).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.44).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2000, 0.49).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2010, 0.51).
narrative_ontology:measurement(vati_su_t2020, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2020, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The constraint family 'vatican_ii_doctrinal_authority' decomposes into four structurally distinct constraints, each instantiating a different hermeneutical reading of the same kernel texts. The rupture_progressive_reading (this story) claims Vatican II authorized doctrinal reversal and ongoing adaptive reform. The continuity_reading claims Vatican II represents organic development within tradition. The rupture_traditionalist_reading claims Vatican II contains errors that enabled heterodox implementation. The composite_overdetermination_reading claims Vatican II bundled incompatible structural changes without resolving their interaction. These are NOT the same constraint viewed from different angles; they have different ε values (extractiveness scales with how thoroughly the reading overrides prior doctrine), different beneficiary/victim structures (beneficiaries differ by reading), different authority flows, and different persistence conditions. Each story's epsilon is stable under its own description and would change if a different reading were adopted, which violates the ε-invariance principle if they were merged into one story. They must be authored as four separate constraint stories, linked by network edges representing doctrinal influence and family kinship. The rupture_progressive_reading influences the continuity_reading (by claiming rupture, it pressures continuity reading to justify apparent changes as genuine developments), and influences the rupture_traditionalist_reading (by claiming authorized reform, it provokes the traditionalist claim that authorization never existed and implementation is corrupted).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
