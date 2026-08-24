% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Rupture Reading — Magisterial Authority as Break
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The rupture reading of Vatican II functions as a living constraint on
 *   Catholic magisterial authority: it asserts that the conciliar texts
 *   themselves encode a new ecclesiology fundamentally incompatible with
 *   prior teaching, and that this incompatibility is not a defect but the
 *   Council's achievement. The constraint coordinates the post-conciliar
 *   Church around authorized implementation (liturgy, religious freedom,
 *   ecumenism, collegiality) while extracting from those whose identity and
 *   formation are bound to the superseded forms. The constraint is actively
 *   enforced through liturgical norms, canonical discipline, episcopal
 *   appointments, and academic gatekeeping. The claimed type (tangled_rope)
 *   reflects the dual structure: genuine coordination of a massive
 *   institutional transition, plus asymmetric extraction from traditionalist
 *   constituencies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Rupture Reading — Magisterial Authority as Break").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '752dc7da-b251-42c9-b8ea-cf714c62bc08').
narrative_ontology:cs_kernel_codification('752dc7da-b251-42c9-b8ea-cf714c62bc08', fixed_text).
narrative_ontology:cs_authority_grounding('752dc7da-b251-42c9-b8ea-cf714c62bc08', lineage).
narrative_ontology:cs_interpretation_layer_present('752dc7da-b251-42c9-b8ea-cf714c62bc08').
narrative_ontology:cs_reading_relation('752dc7da-b251-42c9-b8ea-cf714c62bc08', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('752dc7da-b251-42c9-b8ea-cf714c62bc08', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('752dc7da-b251-42c9-b8ea-cf714c62bc08', foundational, conciliar_texts_authorize_rupture).
narrative_ontology:cs_axiom_status(conciliar_texts_authorize_rupture, holdable).
narrative_ontology:cs_axiom_grounding('752dc7da-b251-42c9-b8ea-cf714c62bc08', conciliar_texts_authorize_rupture, conventional).
narrative_ontology:cs_axiom('752dc7da-b251-42c9-b8ea-cf714c62bc08', foundational, doctrinal_progress_acknowledges_contradiction).
narrative_ontology:cs_axiom_status(doctrinal_progress_acknowledges_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('752dc7da-b251-42c9-b8ea-cf714c62bc08', doctrinal_progress_acknowledges_contradiction, conventional).
narrative_ontology:cs_reference_frame('752dc7da-b251-42c9-b8ea-cf714c62bc08', conciliar_texts_as_rupture_event).
narrative_ontology:cs_drift_state('752dc7da-b251-42c9-b8ea-cf714c62bc08', post_conciliar_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('752dc7da-b251-42c9-b8ea-cf714c62bc08', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, reformist_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, liturgical_reformers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_formation_adherents).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, conciliar_texts_authorize_rupture).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, doctrinal_progress_through_acknowledged_contradiction).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, religious_freedom_as_doctrinal_advance).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__rupture_reading, liturgical_experimentation_as_legitimate_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain interpretive authority and institutional positions by advancing the rupture reading; their careers and theological projects depend on the Council being read as authorizing change. Exit means leaving academic theology or accepting marginalization.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians, beneficiary,
    organized, biographical, mobile, global).

% Implement the rupture reading through diocesan policy, liturgical norms, and seminary formation. They set the agenda for how the Council is received locally. Exit is constrained by episcopal office and communion with Rome.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, reformist_bishops, agenda_setter,
    institutional, biographical, constrained, continental).

% The papal and curial offices that authoritatively promulgate the rupture reading through subsequent documents (e.g., Paul VI's reforms, John Paul II's hermeneutic of reform, Francis's synodality). They benefit from the rupture reading as it legitimizes their governance. Exit is arbitrage-grade — they define the constraint.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_magisterium, beneficiary).

% Use the rupture reading to justify liturgical experimentation and the Novus Ordo as legitimate development rather than rupture. Their professional identity and institutional projects depend on this reading. Exit means abandoning the reform project.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, liturgical_reformers, beneficiary,
    organized, biographical, mobile, global).

% Experience the rupture reading as the loss of the liturgical, devotional, and doctrinal certainties that formed their Catholic identity. They bear the cost of abandoned forms with no effective voice in the interpretation. Exit is identity-locked — leaving means surrendering the self-understanding constituted by the pre-conciliar Church.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_laity, payer,
    powerless, biographical, identity_locked, local).

% Clergy formed in the pre-conciliar seminary system who experience the rupture reading as invalidating their formation and priestly identity. They face disciplinary pressure if they resist implementation. Exit is constrained by priestly vows and canonical status.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy, payer,
    moderate, biographical, constrained, regional).

% Communities (e.g., SSPX, traditionalist religious orders) whose entire institutional existence is organized around the rejected forms. They bear the full extraction of the rupture reading — canonical irregularity, marginalization, denial of structural recognition. Exit is trapped; the constraint defines their structural position.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_formation_adherents, payer,
    moderate, generational, trapped, global).

% Theologians and bishops (e.g., Ratzinger/Benedict XVI school) who argue for hermeneutic of continuity. They are excluded from authoritative implementation of the rupture reading despite holding institutional positions. Their reading is tolerated as private opinion but not as governing hermeneutic.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, continuity_reading_proponents, excluded,
    organized, biographical, constrained, global).

% Scholars who see Vatican II as ambiguously overdetermined (e.g., O'Malley, Gaillardetz). They are excluded from both the rupture and continuity hegemonies; their reading has no institutional implementation path. Exit is mobile — they can pursue academic work without institutional adoption.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, composite_reading_proponents, excluded,
    moderate, biographical, mobile, global).

% Non-Catholic churches watching the Catholic reception of Vatican II. The rupture reading affects ecumenical dialogue (e.g., on religious freedom, ecclesiology) but they have no vote in Catholic hermeneutics. Analytical exit.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, ecumenical_partners, observer,
    moderate, generational, analytical, global).

% Study the Council as a historical event. Their analysis is unaffected by the constraint's enforcement but informs the broader cultural reception. Pure analytical seat.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, secular_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how to implement Vatican II: the rupture reading provides a hermeneutic key that authorizes concrete changes (liturgy, ecumenism, religious freedom, collegiality) without requiring each change to be individually justified against prior teaching. It coordinates the post-conciliar Church around a single interpretive principle: the Council as event of rupture.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy from pre-conciliar forms and their defenders to post-conciliar reformers and their projects. Transfers the cost of adaptation (abandoned certainties, disrupted formation, canonical marginalization) onto traditionalist laity, clergy, and communities.
% ABSENT_VOICES: The pre-conciliar magisterium itself (Pius XII, earlier popes) — their teaching is declared superseded but they cannot object. The laity of the 1950s-60s whose faith was formed by the rejected forms — they were not consulted. Future generations who will inherit a Church shaped by this reading without having chosen it.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished overnight, the post-conciliar liturgical, canonical, and pastoral edifice would lose its hermeneutic foundation. The Novus Ordo, religious freedom doctrine, ecumenical commitments, and collegial governance would need new justification or would be rolled back. The Church would reorganize around either continuity or composite readings, with radically different institutional consequences.
% FOUNDING_PROBLEM: The Council was convoked to address the Church's crisis of relevance in the modern world (aggiornamento). The rupture reading claims the Council solved this by authorizing a break with the pre-conciliar fortress mentality — error has no rights, religious freedom, liturgical adaptation, collegiality — and that this break is the only faithful reception.
% FOUNDING_PROBLEM_CORROBORATION: The rupture reading's claim that aggiornamento required rupture is attested by the Council's progressive majority (Cardinals Lercaro, Suenens, König) and implemented by Paul VI. The contested status is corroborated by Benedict XVI (continuity reading) who argued the Council solved aggiornamento without rupture, and by composite reading scholars (O'Malley, Gaillardetz) who argue the texts themselves are ambiguously overdetermined. No single reading commands consensus outside its own beneficiary set.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the rupture reading demands abandonment of prior certainties (error has no rights, integralist ecclesiology, Tridentine liturgy as normative) from those who bore no responsibility for the Council but whose lives are organized around them. Suppression (0.58) reflects active enforcement: the 1970 Missal imposition, suppression of the old rite (1988 Ecclesia Dei notwithstanding), disciplinary measures against traditionalist clergy, and the hermeneutic gatekeeping in seminaries and theology faculties. Theater ratio (0.35) acknowledges real coordination function (the Council did solve aggiornamento) but notes growing performativity — the rupture reading is increasingly invoked to justify innovations (synodality, blessings) that lack clear conciliar mandate. Accessibility collapse (0.72) is high: once the rupture reading is accepted as the authoritative hermeneutic, the continuity reading becomes structurally illegitimate within the system. Resistance (0.65) is significant and persistent: traditionalist communities, the Benedict XVI hermeneutic, and the composite reading scholarship all contest the rupture reading's authority.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (magisterium, reformist bishops), the constraint appears as rope: a genuine coordination solution to aggiornamento, minimally coercive, with alternatives (the old forms) not suppressed but simply superseded by legitimate development. From the payer seats (traditionalist laity, clergy, communities), the same constraint appears as snare: the coordination story is cover for the suppression of their form of life, alternatives are actively suppressed (Summorum Pontificum's restrictions, Traditionis Custodes), and they are identifiable victims. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-conciliar magisterium and reformist bishops are structural beneficiaries (d near 0.0) — they control the interpretation and collect the legitimacy gains. Progressive theologians and liturgical reformers are beneficiaries with mobile exit (d ~0.2). Traditionalist laity are identity-locked targets (d ~0.95) — their self-constitution is fused to the rejected forms, exit means identity death. Traditionalist clergy are constrained targets (d ~0.8) — canonical obligations trap them. Pre-conciliar adherents (SSPX etc.) are trapped targets (d ~1.0) — the constraint defines their structural position as irregular. Continuity and composite reading proponents are excluded — they have voice but no structural power in the implementation.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading prevents mislabeling by exposing the dual structure: the Council genuinely solved a coordination problem (Church modernity) — that is the rope function. But it did so by authorizing a break that extracts from those who did not consent and whose exit is blocked — that is the snare function. The tangled_rope classification captures both. The mandatrophy risk would be calling it pure rope (ignoring extraction) or pure snare (ignoring the real coordination of aggiornamento). The founding problem (aggiornamento) is contested as live/dead — the rupture reading says it remains live (modernity keeps changing), continuity says it was solved without rupture, composite says the texts don't decide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the rupture_reading a distinct constraint with its own ε, or merely a rhetorical emphasis within a single constraint (the Council''s reception)?',
    'Test ε-invariance: if measuring the constraint via liturgical enforcement yields ε≈0.7 but measuring via ecumenical dialogue yields ε≈0.3, then the label ''rupture reading'' covers multiple constraints. Decompose per DP-001.',
    'If multiple constraints, the rupture reading must be split (e.g., liturgical_rupture, ecumenical_rupture, doctrinal_rupture) each with own ε and stakeholders. Current single-story model would be ε-variant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the rupture reading is one constraint or a family of constraints sharing a label.').

omega_variable(
    rupture_vs_continuity_foreclosure,
    'Does the rupture reading''s core premise (texts authorize break) logically foreclose the continuity reading within a single magisterial framework, or do they coexist as live options for different parties?',
    'Examine whether any single magisterial act (papal document, canonical law) simultaneously affirms both rupture and continuity as authoritative. If yes → coexists_with. If the framework requires choosing one → forecloses.',
    'If forecloses, the continuity reading is structurally impossible within the rupture framework (relation = forecloses). If coexists, both remain live across different factions (relation = coexists_with). Current analysis: forecloses — the rupture reading treats continuity as hermeneutically invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rupture_vs_continuity_foreclosure, conceptual, 'Structural relationship between rupture and continuity readings within a single commitment framework.').

omega_variable(
    rupture_vs_composite_influence,
    'Does the rupture reading''s claim of clear textual meaning (rupture) create structural pressure on the composite reading''s claim of irreducible ambiguity?',
    'Track whether composite reading scholars are marginalized from magisterial advisory roles, whether ambiguity arguments are excluded from official hermeneutics, whether the rupture reading''s institutional dominance reduces resources for composite scholarship.',
    'If yes, relation = influences (rupture creates downstream pressure on composite without logically foreclosing it). If no, relation = coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_composite_influence, empirical, 'Whether rupture reading''s institutional dominance structurally pressures the composite reading''s viability.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.58) structural (canonical penalties, liturgical prohibitions, appointment gatekeeping) or internalized (traditionalists believe the old forms are illegitimate, self-censor)?',
    'Post-exit suppression trajectory: if traditionalists who leave the structural constraint (e.g., attend SSPX, leave Church) still experience the suppression as internalized illegitimacy, reclassify as partially internalized. Survey traditionalist communities on whether suppression persists after canonical exit.',
    'If internalized, effective suppression is higher than structural measure — the target carries the suppression with them after exit. This would increase χ for identity-locked targets beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditionalist targets.').

omega_variable(
    aggiornamento_as_founding_problem,
    'Is ''aggiornamento'' the genuine founding problem, or a post-hoc rationalization for a rupture that served other interests (liturgical modernism, ecclesiastical power redistribution)?',
    'Historical analysis of pre-conciliar preparatory commissions vs. conciliar floor interventions vs. post-conciliar implementation. Trace whether the rupture reading''s specific moves (novus ordo, DH, collegiality) were present in the aggiornamento mandate or emerged from conciliar dynamics.',
    'If aggiornamento is rationalization, the founding_problem_status ''contested'' understates the rupture reading''s extractive character — the coordination function would be cover for a power grab. If genuine, the coordination function is real and the extraction is the price of a necessary transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggiornamento_as_founding_problem, conceptual, 'Whether the stated founding problem (aggiornamento) genuinely motivated the rupture or rationalizes it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1988, 0.32).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2013, 0.33).
narrative_ontology:measurement(vati_tr_t2021, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2021, 0.36).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1978, 0.58).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1988, 0.62).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2013, 0.68).
narrative_ontology:measurement(vati_be_t2021, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2021, 0.7).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1978, 0.52).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1988, 0.58).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement(vati_su_t2021, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__rupture_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, religious_freedom_doctrine_implementation).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, ecumenical_dialogue_framework).

% DUAL FORMULATION NOTE:
% This constraint (rupture_reading) and its siblings (continuity_reading, composite_overdetermination_reading) form the vatican_ii_magisterial_authority constraint family. The rupture reading has ε=0.68 (substantial extraction from traditionalists), the continuity reading has ε≈0.15 (minimal extraction, claims organic development), the composite reading has ε≈0.35 (moderate extraction from all sides via ambiguity management). They share the kernel (conciliar texts as magisterial authority) but instantiate different constraints with different ε, different beneficiaries/victims, and different types. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, institutional, 0.05).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, organized, 0.15).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, powerless, 0.95).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
