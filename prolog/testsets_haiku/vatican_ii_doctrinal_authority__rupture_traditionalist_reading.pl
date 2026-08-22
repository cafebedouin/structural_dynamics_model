% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority (Rupture/Traditionalist Reading)
 *   domain: ecclesiastical/doctrinal/hermeneutical
 *
 * SUMMARY:
 *   This is ONE READING of the contested kernel:
 *   vatican_ii_doctrinal_authority. The reading I am instantiating is the
 *   rupture_traditionalist reading: Vatican II represents a rupture with
 *   pre-conciliar doctrine, enabled by deliberate ambiguities in the
 *   conciliar documents and implemented as heterodox reform by postconciliar
 *   leadership. From this reading's perspective, the constraint operates to
 *   suppress pre-conciliar practice (Latin liturgy, doctrinal clarity on
 *   salvation outside the Church, Scholastic seminary formation) while
 *   enabling progressive reinterpretation of conciliar texts as authorization
 *   for changes the documents do not explicitly mandate. The ε is high
 *   because the reading assesses the doctrinal shift as substantial and
 *   extractive (traditionalist practitioners lose their spiritual language
 *   and institutional standing). The suppression is high because resistance
 *   is actively marginalized through institutional pressure and obedience
 *   norms. The theater ratio is moderate-to-high because postconciliar
 *   leadership performs reform as continuous development while implementing
 *   rupture. This reading DOES NOT describe the progressive reading or the
 *   continuity reading — those are other constraints, other files. This story
 *   generates the rupture_traditionalist reading alone, with ε,
 *   beneficiary/victim structure, and stakeholder situation as that reading
 *   sees them.
 *
 * KEY AGENTS:
 *   - conciliar_reformers: Bishops and theologians who voted Vatican II documents; from this reading, either complicit in rupture or failed to foresee its consequences
 *   - postconciliar_institutional_leadership: Pope Paul VI and successors, Vatican theologians, reform bishops; benefit from interpretive authority over ambiguous texts
 *   - progressive_reform_advocates: Theologians and clergy who embrace Vatican II as mandate for ongoing reform; benefit from doctrinal pluralism and liturgical simplification
 *   - traditional_liturgy_practitioners: Clergy and laity experiencing displacement of Latin Mass and pre-conciliar practice; trapped by identity fusion with pre-conciliar forms
 *   - doctrinal_traditionalists: Theologians and bishops defending pre-conciliar doctrine; marginalized in postconciliar seminary and institutional structures
 *   - conciliar_conservatives: Minority bishops who opposed rupture; excluded from postconciliar hermeneutics authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority (Rupture/Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiastical/doctrinal/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '5cbf186c-f582-42b1-be9c-bc00a90b6b23').
narrative_ontology:cs_kernel_codification('5cbf186c-f582-42b1-be9c-bc00a90b6b23', fixed_text).
narrative_ontology:cs_authority_grounding('5cbf186c-f582-42b1-be9c-bc00a90b6b23', lineage).
narrative_ontology:cs_interpretation_layer_present('5cbf186c-f582-42b1-be9c-bc00a90b6b23').
narrative_ontology:cs_reading_relation('5cbf186c-f582-42b1-be9c-bc00a90b6b23', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cbf186c-f582-42b1-be9c-bc00a90b6b23', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cbf186c-f582-42b1-be9c-bc00a90b6b23', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('5cbf186c-f582-42b1-be9c-bc00a90b6b23', foundational, doctrinal_rupture_from_preconciliar_teaching).
narrative_ontology:cs_axiom_status(doctrinal_rupture_from_preconciliar_teaching, holdable).
narrative_ontology:cs_axiom_grounding('5cbf186c-f582-42b1-be9c-bc00a90b6b23', doctrinal_rupture_from_preconciliar_teaching, empirically_contingent).
narrative_ontology:cs_axiom('5cbf186c-f582-42b1-be9c-bc00a90b6b23', foundational, ambiguities_enable_heterodox_drift).
narrative_ontology:cs_axiom_status(ambiguities_enable_heterodox_drift, holdable).
narrative_ontology:cs_axiom_grounding('5cbf186c-f582-42b1-be9c-bc00a90b6b23', ambiguities_enable_heterodox_drift, deontological).
narrative_ontology:cs_reference_frame('5cbf186c-f582-42b1-be9c-bc00a90b6b23', preconciliar_doctrinal_clarity).
narrative_ontology:cs_drift_state('5cbf186c-f582-42b1-be9c-bc00a90b6b23', contemporary_postconciliar_period, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('5cbf186c-f582-42b1-be9c-bc00a90b6b23', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reform_advocates).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_institutional_leadership).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_practitioners).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_traditionalists).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_missionary_forms).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_development_through_historical_discontinuity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, magisterial_authority_through_conciliar_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops convened at Vatican II who voted through the sixteen documents; tasked with modernizing church engagement with the contemporary world. From the traditionalist reading, they either compromised dangerously with ambiguous language (enabling heterodox implementation) or failed to foresee the consequences of doctrinal shifts they authorized. Their authority derives from conciliar succession and magisterial standing; they cannot escape the consequences of their legislation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conciliar_reformers, agenda_setter,
    institutional, generational, trapped, universal).

% Vatican officials, theologians, and bishops who interpret Vatican II in a progressive direction post-1965; they benefit from the ambiguities in the conciliar texts as authorization for reforms the documents do not explicitly mandate. They exercise authority through implementation, interpretation, and teaching; they are incentivized to read the documents as opening to ongoing reform rather than settling doctrine.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_institutional_leadership, beneficiary,
    institutional, generational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_institutional_leadership, agenda_setter).

% Theologians, lay activists, and parish clergy who embrace Vatican II as mandate for liturgical simplification, doctrinal pluralism, and structural decentralization. They benefit from the interpreted texts as authorization for changes they believe are necessary; the ambiguities give them cover for positions that exceed explicit conciliar language. Exiting would require rejecting the postconciliar authority structure they depend on.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reform_advocates, beneficiary,
    organized, biographical, constrained, universal).

% Clergy and laity who experienced the pre-conciliar Latin Mass, sacramental theology, and liturgical forms as normative expressions of faith. Post-Vatican II, they bear the systematic displacement of these forms by the reformed (Novus Ordo) rite; many experience this as loss of their primary spiritual language. Their identity is fused with pre-conciliar practice; leaving the Church is not a realistic exit option, but internal resistance is constrained by obedience norms and institutional pressure toward conformity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_practitioners, payer,
    moderate, biographical, identity_locked, global).

% Theologians, catechists, and bishops who hold that pre-conciliar doctrine (particularly on religious freedom, salvation outside the Church, and relations with non-Catholic faiths) was clear and binding, and that Vatican II's ambiguous restatements enable dissolution of these teachings. They experience the post-conciliar period as doctrinal drift; their institutional standing is diminished as they are increasingly marginalized in seminaries and teaching positions. Their exit is constrained by religious identity and institutional dependence.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_traditionalists, payer,
    moderate, generational, identity_locked, global).

% Class of institutional practices: Latin-based missionary catechesis, monastic observance, parish sodality structure, seminary formation in Scholastic philosophy. These forms are systematically de-authorized post-Vatican II; they are replaced by vernacular-language catechesis, liturgical simplification, and lay-led pastoral models. From the traditionalist reading, this displacement is enabled by conciliar ambiguities about tradition and continuity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_missionary_forms, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_missionary_forms).

% Minority bishops and theologians at Vatican II who opposed doctrinal rupture and advocated for explicit continuity with pre-conciliar teaching. They lost the conciliar vote; they remain within the institutional structure but are largely excluded from post-conciliar hermeneutics and implementation authority. Their voice is preserved in the minority reports and in certain conciliar document phrasings but is overridden in implementation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, conciliar_conservatives, excluded,
    institutional, generational, identity_locked, universal).

% The question of how conciliar texts bind subsequent doctrine: whether they settle doctrine or open it to ongoing development; whether ambiguities are productive (enabling living tradition) or destructive (enabling heterodoxy). This is an analytical seat mapping onto the hermeneutic dispute between readings.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, magisterial_hermeneutics_authority, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, magisterial_hermeneutics_authority).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_institutional_leadership).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the entire communion of the Catholic Church around a single magisterial authority expressing doctrine through a formal ecumenical council. Vatican II was convoked to update this coordination for the modern world: a necessary collective decision about how the institutional Church would engage contemporary society.
% TRANSFER_FUNCTION: Transfers interpretive authority from pre-conciliar doctrinal formulations (in Latin, systematized Scholastically, defining boundaries against error) to post-conciliar institutional leadership and theologians (authorized to read conciliar documents as mandating ongoing reform). The ambiguities in the texts enable a transfer of closure: pre-conciliar doctrine was posed as settled and binding; post-conciliar doctrine is posed as developing and subject to reinterpretation by the magisterium and theological community. This reading treats the transfer as extraction of closure from those whose spiritual practice depended on doctrinal finality.
% ABSENT_VOICES: The pre-conciliar missionary and liturgical forms themselves cannot speak. Their practitioners (traditional liturgy adherents, Scholastic theologians, parish sodality members) are marginalized in post-conciliar implementation and excluded from hermeneutic authority over how Vatican II is read. They would object that the conciliar documents were sold to them as continuity but are being implemented as discontinuity, and that the ambiguities are cover for predecided rupture.
% DISAPPEARANCE_RATIONALE: If Vatican II and its magisterial authority disappeared overnight, the Church would revert to pre-conciliar doctrine and practice (Latin Mass, doctrinal clarity on salvation outside the Church, missionary rigidity on non-Catholic faiths, Scholastic seminary formation). The entire postconciliar institutional restructuring — lay participation, vernacular liturgy, ecumenical outreach, revised canon law — depends on conciliar authorization. The constraint is not a natural fact but an act of institutional will codified in conciliar decrees and implemented through successive papal and episcopal leadership.
% FOUNDING_PROBLEM: The pre-conciliar Church was perceived (by the convening Pope John XXIII and most conciliar fathers) as institutionally sclerotic, alienated from modern culture, defensive and closed to dialogue with the contemporary world. Vatican II was convoked to address this by 'opening the windows' to modern thought while preserving core doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Progressive postconciliar authorities (Pope Paul VI, later Pope Francis, reform theologians like Rahner and Küng) attest the founding problem was real and required conciliar remedy. Traditionalist observers (Archbishop Lefebvre and later SSPX adherents, conservative theologians like Msgr. Strickland) attest that the postconciliar implementation far exceeded what was necessary to address the founding problem and that the documents themselves were compromised by ambiguous language that enabled doctrinal rupture. Independent historical scholarship (Melloni, Komonchak) documents that the conciliar process involved complex coalition-building and that ambiguous formulations were often deliberate compromises allowing opposing conciliar factions to claim victory. This corroboration comes from outside the benefiting parties: the conciliar historians are analytical, not invested in postconciliar progressivism, and they document that the founding problem and its scope remain contested among contemporaries.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   From the traditionalist reading, the constraint measures as high extractiveness (0.78 at interval end) because the doctrinal shift is assessed as substantial and negatively valued: pre-conciliar doctrine on religious freedom, salvation outside the Church, and missionary method was clear; postconciliar reinterpretation is assessed as dissolution of these certainties. Suppression rises from 0.42 to 0.71 over the interval (1965–2025) as institutional mechanisms for enforcing postconciliar interpretation harden: seminary curricula shift away from Scholasticism, Latin Mass is marginalized (then gradually permitted again), and doctrinal traditionalists are increasingly excluded from teaching positions and theological authority. Theater ratio rises from 0.28 to 0.52, indicating that an increasing share of postconciliar activity is performative maintenance of reform rhetoric rather than functional coordination — the 'spirit of the Council' is invoked as authorization for positions the texts do not explicitly support. The measurements are authored on one shared time grid so every metric appears at every time point. The coercion grid shows resistance declining over the interval (individual-level resistance from 0.82 to 0.71; organizational resistance from 0.62 to 0.38) as traditionalist coalitions are depleted through attrition, death, and institutional integration pressure. Accessibility collapse rises (exit alternatives close for traditionalists as the pre-conciliar institutional forms are systematically displaced). Stakes inflation rises at organizational and structural levels (the cost of noncompliance with progressive reading increases) while declining at individual level (individual traditionalists find internal niches or exit the constraint through schism or retirement).
 *
 * PERSPECTIVAL GAP:
 *   The conciliar reformers' seat and the traditionalist victims' seat should compute differently from this reading's structural data. The reformers see themselves as coordinators addressing a real founding problem (pre-conciliar rigidity); traditionalists see institutional rupture and suppression of their practice. The postconciliar leadership seat experiences the constraint as openness and interpretive authority; traditionalists experience it as enforced displacement. The engine computes per-seat classification from the structural data (power, exit options, beneficiary/victim role) — the reformers and leaders occupy institutional power with arbitrage options (they can shift doctrine and maintain authority); traditionalists are moderate power with identity-locked exit, cast as payers. From the reformer seat the constraint may compute as rope (genuine coordination with minimal extraction). From the traditionalist seat it computes as tangled_rope or snare (asymmetric extraction suppressed by institutional pressure). This reading's authored metrics represent the traditionalist assessment; a different reading would author different metrics for the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Postconciliar institutional leadership = beneficiary (collects interpretive authority and implementation power; d near 0.1–0.2). Progressive reform advocates = beneficiary-adjacent (benefit from theological pluralism and liturgical opening; d near 0.2–0.3). Traditionalist practitioners and doctrinal defenders = victims (lose institutional standing, spiritual practice displaced, doctrine destabilized; d near 0.8–0.9). Conciliar conservatives = excluded but not directly targeted (their voice is suppressed but they were part of the decision; d near 0.6). Conciliar reformers = trapped in the constraint they created; their authority is invoked by both progressives (who exceed their texts) and traditionalists (who blame them for rupture); d near 0.5 (symmetric — they benefit from institutional continuity but are implicated in consequences). No directionality overrides are needed; the structural derivation captures the core asymmetry: institutional beneficiaries with arbitrage exit options versus traditionalist victims with identity-locked exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading presents a mandatrophy signal: the founding problem (pre-conciliar rigidity and world-alienation) may be substantially solved by the 1990s (new liturgical translations, John Paul II's theological restoration efforts, Ecclesia Dei provisions for traditionalist communities), yet the constraint persists and even intensifies (theater ratio rises to 0.52 by interval end). The postconciliar leadership persists in invoking 'the spirit of the Council' as authorization for ongoing reform even after the specific founding problem is addressed. This is characteristic of mandatrophy: the institutional arrangement outlives its functional justification and persists through inertia and performance. The recovery of Latin Mass permissions (2007, then restricted 2019–2021) shows institutional leadership trying to manage the mandatrophy signal without admitting it — performing continuity while enforcing change. From the rupture_traditionalist reading, this is evidence that the constraint is extractive (rent-seeking in interpretive authority) rather than coordinative (solving a real problem). The mandatrophy is real from this reading but contested from others: progressives argue the founding problem is never fully solved and ongoing openness is necessary; continuity readers argue Vatican II was legitimate development and the constraint was never extractive at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_reading_divergence,
    'Are the ambiguities in Vatican II documents evidence of deliberate theological compromise that was bound to enable divergent implementations, or are they the natural product of collective deliberation that permits legitimate doctrinal development?',
    'This is a conceptual divergence rooted in how one reads the hermeneutic authority of magisterial texts. Historico-critical analysis of conciliar process (Melloni, Komonchak) documents the coalitions and compromises; theological hermeneutics must assess whether such products are defective or productive. No empirical fact resolves this — different theological frameworks give different answers.',
    'If ambiguities are defects: Vatican II is read as a failed reform whose consequences are institutional drift and doctrinal dissolution — pushing the constraint toward snare classification (rupture enforced by suppressing traditionalist resistance). If ambiguities are productive: Vatican II is a successful opening to the tradition that permits living development — sustaining tangled_rope or even rope classification where progressives coordinate and traditionalists accept enforced change as legitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutic_reading_divergence, conceptual, 'Whether conciliar ambiguities are hermeneutic defects or productive openness.').

omega_variable(
    rupture_vs_development_axiom_foreclosure,
    'Does this reading''s core axiom (doctrinal rupture) foreclose or coexist with the continuity_reading''s axiom (organic development)?',
    'Logical analysis of the axioms: if ''doctrinal rupture'' is defined as ''discontinuous shift from prior teaching'' and ''organic development'' is defined as ''explication of implicit prior teaching,'' the axioms are contradictory only if these are treated as mutually exclusive. They need not be. One axiom can be overridden if the tradition formally rejects it; but within a single framework, both cannot be held. Resolution requires choosing which framework one accepts.',
    'If the axioms foreclose each other strictly: the two readings cannot coexist within any single coherent magisterium, and one must be wrong. If they coexist under different frameworks: they are readings available to different parties within a contested tradition, and classification diverges by seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rupture_vs_development_axiom_foreclosure, conceptual, 'Whether rupture and organic-development axioms strictly foreclose each other or coexist under different hermeneutic frames.').

omega_variable(
    identity_lock_mechanism_suppression,
    'For traditionalist practitioners bearing the constraint, is the measured suppression (0.71) structural (institutional pressure, marginalization, obedience norms) or internalized (they have fused their identity with pre-conciliar practice and cannot imagine legitimate exit)?',
    'Post-exit trajectory observation: traditionalists who have left (joining SSPX or sedevacantist groups) report whether their identity-fusion dissolves after institutional separation or persists. High persistence suggests substantial internalization; rapid dissolution suggests primarily structural suppression.',
    'If structural: the constraint''s extraction is reducible through institutional reform. If substantially internalized: traditionalist practitioners carry suppression with them even after institutional exit; the constraint''s cost is embedded in identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Whether suppression of traditionalist resistance is structural or internalized.').

omega_variable(
    gain_flow_institutional_capture,
    'Is the constraint primarily captured by postconciliar institutional leadership, or is it more diffuse among progressive reform advocates?',
    'Track resource flows: does the institutional hierarchy concentrate the gains of interpretation authority, or do these gains disperse to parish reformers and theologians?',
    'If concentrated in institutional leadership: the constraint is rentier (theater_ratio should be higher). If diffuse: it remains tangled_rope where institutional leadership gains authority while coordinating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gain_flow_institutional_capture, empirical, 'Whether constraint gains concentrate in institutional hierarchy or distribute among beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t6, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(vati_tr_t6, observed).
narrative_ontology:measurement(vati_tr_t12, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(vati_tr_t12, observed).
narrative_ontology:measurement(vati_tr_t24, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement_basis(vati_tr_t24, observed).
narrative_ontology:measurement(vati_tr_t36, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 36, 0.52).
narrative_ontology:measurement_basis(vati_tr_t36, observed).
narrative_ontology:measurement(vati_tr_t48, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 48, 0.52).
narrative_ontology:measurement_basis(vati_tr_t48, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t6, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(vati_be_t6, observed).
narrative_ontology:measurement(vati_be_t12, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(vati_be_t12, observed).
narrative_ontology:measurement(vati_be_t24, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement_basis(vati_be_t24, observed).
narrative_ontology:measurement(vati_be_t36, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 36, 0.76).
narrative_ontology:measurement_basis(vati_be_t36, observed).
narrative_ontology:measurement(vati_be_t48, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 48, 0.77).
narrative_ontology:measurement_basis(vati_be_t48, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t6, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(vati_su_t6, observed).
narrative_ontology:measurement(vati_su_t12, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement_basis(vati_su_t12, observed).
narrative_ontology:measurement(vati_su_t24, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(vati_su_t24, observed).
narrative_ontology:measurement(vati_su_t36, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement_basis(vati_su_t36, observed).
narrative_ontology:measurement(vati_su_t48, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 48, 0.71).
narrative_ontology:measurement_basis(vati_su_t48, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(vati_su_t60, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=60
narrative_ontology:measurement(vati_grid_01, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(class), 0, 0.62).
narrative_ontology:measurement(vati_grid_02, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(class), 60, 0.73).
narrative_ontology:measurement(vati_grid_03, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(vati_grid_04, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(individual), 60, 0.65).
narrative_ontology:measurement(vati_grid_05, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(vati_grid_06, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(organizational), 60, 0.71).
narrative_ontology:measurement(vati_grid_07, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(vati_grid_08, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse(structural), 60, 0.72).
narrative_ontology:measurement(vati_grid_09, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(class), 0, 0.75).
narrative_ontology:measurement(vati_grid_10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(class), 60, 0.58).
narrative_ontology:measurement(vati_grid_11, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(individual), 0, 0.82).
narrative_ontology:measurement(vati_grid_12, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(individual), 60, 0.71).
narrative_ontology:measurement(vati_grid_13, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(vati_grid_14, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(organizational), 60, 0.38).
narrative_ontology:measurement(vati_grid_15, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement(vati_grid_16, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance(structural), 60, 0.31).
narrative_ontology:measurement(vati_grid_17, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(class), 0, 0.64).
narrative_ontology:measurement(vati_grid_18, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(class), 60, 0.74).
narrative_ontology:measurement(vati_grid_19, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(individual), 0, 0.72).
narrative_ontology:measurement(vati_grid_20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(individual), 60, 0.68).
narrative_ontology:measurement(vati_grid_21, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(vati_grid_22, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(organizational), 60, 0.78).
narrative_ontology:measurement(vati_grid_23, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(vati_grid_24, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, stakes_inflation(structural), 60, 0.76).
narrative_ontology:measurement(vati_grid_25, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(class), 0, 0.52).
narrative_ontology:measurement(vati_grid_26, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(class), 60, 0.71).
narrative_ontology:measurement(vati_grid_27, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(individual), 0, 0.58).
narrative_ontology:measurement(vati_grid_28, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(individual), 60, 0.74).
narrative_ontology:measurement(vati_grid_29, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(organizational), 0, 0.45).
narrative_ontology:measurement(vati_grid_30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(organizational), 60, 0.72).
narrative_ontology:measurement(vati_grid_31, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(structural), 0, 0.38).
narrative_ontology:measurement(vati_grid_32, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression(structural), 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Vatican II doctrinal authority kernel generates four distinct constraint stories: rupture_traditionalist (this file), continuity_reading, rupture_progressive_reading, and composite_overdetermination_reading. Each reading instantiates a different ε, beneficiary/victim structure, and classification. The rupture_traditionalist reading assesses Vatican II as doctrinal discontinuity enabled by ambiguous texts and extractively implemented by postconciliar progressives, victimizing traditionalist practitioners. The sibling readings dispute the naturalness, beneficiary structure, and founding-problem status of the constraint. All four stories are linked via network.affects_constraints; they are one kernel family with readings that coexist across different parties within the Church.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
