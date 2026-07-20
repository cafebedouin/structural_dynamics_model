% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Rupture-Progressive Hermeneutic: Spirit Authorizes Reform Beyond Text
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint instantiates the rupture-progressive reading of the
 *   Vatican II doctrinal authority kernel. It treats the Second Vatican
 *   Council not merely as a past event but as an ongoing source of authority
 *   mediated by a 'spirit of the Council' that legitimates reform beyond the
 *   conciliar texts. The kernel is contested: the continuity reading holds
 *   that the Council was organic development within unchanging tradition,
 *   while the rupture-traditionalist reading holds that the rupture was real
 *   but resulted in doctrinal error. This reading claims the rupture was
 *   necessary and the 'spirit' authorizes continuing development. The
 *   constraint shapes who may speak, worship, and teach within the Catholic
 *   Church by binding institutional advancement to adherence to a hermeneutic
 *   that deliberately transcends textual positivism.
 *
 * KEY AGENTS:
 *   - progressive_hierarchy (agenda_setter/institutional/constrained) â sets and enforces the hermeneutic, benefits from expanded doctrinal authority
 *   - progressive_theologians (beneficiary/organized/constrained) â produce legitimating discourse, career-identified with reform
 *   - traditionalist_clergy (payer/organized/trapped) â bear costs of liturgical and doctrinal suppression
 *   - traditionalist_laity (payer/moderate/constrained) â experience parish-level destabilization
 *   - pre_conciliar_theologians (excluded/moderate/trapped) â structurally excluded from contemporary theological conversation
 *   - church_historians (observer/organized/analytical) â analyze the gap between texts and implementations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.72).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Rupture-Progressive Hermeneutic: Spirit Authorizes Reform Beyond Text").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, '38b3bdcd-e9cc-4fd3-ba57-0c358dce0858').
narrative_ontology:cs_kernel_codification('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', fixed_text).
narrative_ontology:cs_authority_grounding('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', lineage).
narrative_ontology:cs_interpretation_layer_present('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858').
narrative_ontology:cs_reading_relation('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', foundational, extratextual_spirit_authority).
narrative_ontology:cs_axiom_status(extratextual_spirit_authority, holdable).
narrative_ontology:cs_axiom_grounding('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', extratextual_spirit_authority, theological).
narrative_ontology:cs_axiom('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', foundational, religious_liberty_as_reversal).
narrative_ontology:cs_axiom_status(religious_liberty_as_reversal, holdable).
narrative_ontology:cs_axiom_grounding('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', religious_liberty_as_reversal, theological).
narrative_ontology:cs_reference_frame('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', conciliar_renewal_authority).
narrative_ontology:cs_drift_state('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', post_traditionis_custodes_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('38b3bdcd-e9cc-4fd3-ba57-0c358dce0858', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_hierarchy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, modernizing_laity).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_partners).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, episcopal conferences, and curial officials who interpret the Council as a mandate for ongoing reform. They invoke the 'spirit of Vatican II' to authorize liturgical experimentation, ecumenical outreach, and doctrinal development beyond the conciliar texts. Their institutional authority depends on maintaining this hermeneutic against textual positivist and traditionalist challenges.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_hierarchy, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_hierarchy, beneficiary).

% Academic theologians whose careers and intellectual frameworks are built on the progressive conciliar hermeneutic. They produce the interpretive literature that legitimizes extratextual development and train clergy in the new theological paradigm. Their professional identity is fused with the post-conciliar reform project.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians, beneficiary,
    organized, biographical, constrained, global).

% Lay Catholics who experience the post-conciliar reforms (vernacular liturgy, expanded lay roles, ecumenical openness) as genuine spiritual liberation from pre-conciliar rigidity. They benefit from the progressive reading's authorization of continued adaptation to modern culture.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, modernizing_laity, beneficiary,
    moderate, biographical, mobile, national).

% Non-Catholic Christian communities and other religious groups who benefit from Dignitatis Humanae's affirmation of religious freedom and Unitatis Redintegratio's ecumenical opening. The progressive reading's expansion of these conciliar impulses beyond their textual limits creates space for further interfaith accommodation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_partners, beneficiary,
    moderate, biographical, mobile, global).

% Clergy attached to the pre-conciliar liturgical and theological forms who experience the progressive hermeneutic as the suppression of their spiritual and sacramental world. They face disciplinary restrictions, exclusion from preferment, and institutional marginalization for maintaining Latin liturgy or questioning extratextual reforms. Many are trapped by ordination vows and dependency on diocesan structures.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy, payer,
    organized, biographical, trapped, global).

% Lay Catholics who find spiritual nourishment in traditional liturgy and theology and experience the progressive implementation as the destruction of their parochial communities and catechetical stability. They bear the costs of liturgical experimentation, declining sacramental practice, and the erosion of distinctively Catholic identity in their parishes.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_laity, payer,
    moderate, biographical, constrained, local).

% Theologians formed in the Thomistic and anti-modernist frameworks of the pre-conciliar Church whose intellectual tradition was systematically displaced from seminaries and Catholic universities after the Council. They are excluded from the contemporary theological conversation not by argument but by institutional memory-holeing; their frameworks are treated as superseded rather than refuted.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_theologians, excluded,
    moderate, generational, trapped, global).

% Professional historians of the Church who analyze the conciliar event and its aftermath without institutional stake in either the progressive or traditionalist program. They document the gap between conciliar texts and post-conciliar implementations, and trace the genealogies of the competing hermeneutics.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, church_historians, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_hierarchy).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the mid-twentieth-century crisis of Catholic engagement with modernity, ecumenism, and religious pluralism by providing a hermeneutic framework that authorizes the Church to adapt its doctrine, liturgy, and institutional practice to contemporary culture without being bound to explicit conciliar textual limits.
% TRANSFER_FUNCTION: Moves doctrinal and disciplinary authority from fixed textual and traditional anchors to a dynamic 'spirit of the Council' interpreted by progressive institutional actors; transfers liturgical and catechetical stability from traditionalist clergy and laity to modernizing reformers and ecumenical partners.
% ABSENT_VOICES: Pre-conciliar theologians and strict textual positivists who would insist that the Council documents alone bind and cannot authorize developments beyond their explicit provisions; traditionalist bishops and clergy who have been marginalized from episcopal conferences and curial posts; secular historians of theology who are not formed within the post-conciliar academic establishment.
% DISAPPEARANCE_RATIONALE: Without this hermeneutic framework, progressive post-conciliar reforms (experimental liturgies, ecumenical accommodations, doctrinal developments beyond the texts) would lose their primary authority claim. Traditionalist alternatives would resurge institutionally; the post-Vatican II Church would revert to textual-legal or tradition-bound hermeneutics, and the current progressive episcopal and theological establishment would face delegitimation.
% FOUNDING_PROBLEM: Pre-conciliar rigidity and the Church's inability to engage modernity, ecumenism, and religious freedom without doctrinal self-contradiction; the anti-modernist enclosure and suspicion of contemporary culture creating pastoral and evangelical paralysis.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and ecumenical leaders attest the problem was real and required rupture. Traditionalist historians and conservative bishops attest the problem was exaggerated or that the rupture cure was worse than the rigidity disease. Academic church historians outside the immediate beneficiary set are divided; sociological data on Catholic disaffiliation post-conciliar is cited by both sides with contested causality.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because the 'spirit of the Council' authorizes doctrinal and disciplinary moves with no explicit textual warrant, concentrating interpretive power in progressive institutional actors. Suppression (0.68) is high because the constraint's persistence requires active exclusion of textual positivism and traditionalist practice (e.g., Traditionis Custodes, seminary exclusions). Theater_ratio (0.48) reflects substantial performative maintenance: much post-conciliar rhetoric invokes the Council's spirit to justify arrangements the documents never envisioned. The temporal series shows a Benedictine dip (2005) when the 'hermeneutic of continuity' temporarily dampened extraction, followed by resurgence under the current pontificate. Accessibility_collapse (0.60) measures how thoroughly pre-conciliar theological alternatives have been displaced from seminaries and universities; resistance (0.55) captures persistent traditionalist noncompliance.
 *
 * PERSPECTIVAL GAP:
 *   The progressive hierarchy experiences this constraint as genuine coordination: it solves the pre-conciliar impasse and authorizes necessary engagement with modernity. From the traditionalist clergy and laity seats, the identical structure reads as extraction: they pay in lost liturgy, doctrinal stability, and institutional standing for reforms they never accepted. The engine computes this divergence from the structural data â the agenda_setter/beneficiary seats face constrained exit (they could theoretically revert, but it would destroy their authority and identity), while payer seats are trapped or highly constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   The progressive hierarchy sits near the beneficiary end (d low): they collect expanded doctrinal authority and institutional control. Progressive theologians and modernizing laity also sit beneficiary-side, though theologians are identity-locked to the reform project. Traditionalist clergy and laity sit near the full-target end (d high): the constraint extracts their liturgical and catechetical stability, and their exit options are trapped or constrained by ordination, parish dependency, and family ties. Pre-conciliar theologians are excluded victims whose structural target status is amplified by their trapped exit from the academy. Ecumenical partners are beneficiaries with mobile exit â they gain from Catholic opening without bearing its internal costs.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the Tangled Rope classification, this constraint could be misread as a Rope (pure coordination of necessary modernization) or a Snare (pure extraction masquerading as theology). The Tangled Rope typing is warranted because there was a real coordination problem â the pre-conciliar Church's anti-modernist rigidity genuinely impeded pastoral engagement â but the solution was captured by asymmetric extraction: traditionalists pay for progressive institutional advancement. It is not a Piton because the progressive hierarchy and theologians are concentrated beneficiaries who actively maintain the constraint; it is not a Scaffold because no credible sunset clause exists (the 'spirit' is by definition open-ended). The founding problem status is contested, and the mandate has clearly outgrown its original conciliar textual limits, but the constraint persists because beneficiaries remain powerful and organized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_vs_text_authority,
    'Is the ''spirit of the Council'' a theologically legitimate authority locus separable from the conciliar texts, or is it an interpretive construct that enables unbounded doctrinal drift?',
    'Comparative magisterial analysis examining whether post-conciliar ''spirit'' claims can be constrained by textual limits; natural experiment from jurisdictions or religious orders that adhere strictly to the texts versus those that follow the ''spirit.''',
    'If the spirit is not separably authoritative, the progressive reading collapses into either continuity (text alone binds) or rupture-traditionalist (text insufficient and potentially erroneous). If it is authoritative, the high extractiveness is the legitimate price of living tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_vs_text_authority, conceptual, 'Whether spirit authority is structurally separable from conciliar text').

omega_variable(
    structural_suppression_mechanism,
    'Is the suppression of traditionalist alternatives driven by hierarchical enforcement (structural) or by ideological consensus within the episcopate (internalized)?',
    'Analysis of disciplinary actions, seminary admissions policies, and liturgical restrictions versus voluntary abandonment of traditional practice by clergy not subject to direct penalty.',
    'If suppression is internalized, the constraint''s effective coercive force exceeds formal metrics because traditionalist clergy carry the suppression with them even where explicit penalties are absent. If purely structural, removal of hierarchical penalties might permit traditionalist resurgence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_suppression_mechanism, empirical, 'Structural vs internalized suppression of traditionalist alternatives').

omega_variable(
    founding_problem_misdiagnosis,
    'Was pre-conciliar rigidity the actual problem requiring rupture, or was the crisis of modernity external to the Church''s doctrinal framework?',
    'Historical analysis of Catholic intellectual and pastoral life in the 1940s-1950s; sociological comparison of Catholic retention rates in traditionalist versus progressive jurisdictions post-conciliar.',
    'If the founding problem was misdiagnosed, the rupture is iatrogenic: the constraint''s coordination function is cover for institutional self-dissolution, and its classification as Tangled Rope overstates the genuine coordination component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_misdiagnosis, empirical, 'Whether pre-conciliar rigidity was the true founding problem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2drp_tr_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(v2drp_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(v2drp_tr_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(v2drp_tr_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(v2drp_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(v2drp_tr_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(v2drp_tr_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(v2drp_be_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1965, 0.25).
narrative_ontology:measurement(v2drp_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(v2drp_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(v2drp_be_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(v2drp_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(v2drp_be_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(v2drp_be_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(v2drp_su_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(v2drp_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(v2drp_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(v2drp_su_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(v2drp_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(v2drp_su_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(v2drp_su_t2024, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Vatican II doctrinal authority kernel. The label 'Vatican II' conflates structurally distinct claims: the conciliar texts as historical event (continuity reading), the conciliar event as revolutionary authorization (this reading), and the conciliar event as doctrinal rupture and error (traditionalist reading). Each reading emits a different constraint with different Îµ, beneficiary/victim structures, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
