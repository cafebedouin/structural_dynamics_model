% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority — Rupture Traditionalist Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The rupture traditionalist reading of Vatican II treats the Council not
 *   as a pastoral council but as a structural break: its sixteen documents
 *   contain deliberate ambiguities (e.g., 'subsistit in', 'seeds of the
 *   Word', 'religious freedom as civil right') that function as interpretive
 *   traps. Post-conciliar implementation (liturgical reform, catechetical
 *   collapse, disciplinary laxity) is read not as distortion but as the
 *   texts' logical unfolding. The constraint is the standing arrangement
 *   whereby the post-conciliar magisterium claims authority to bind
 *   consciences to a hermeneutic that the traditionalist reading judges
 *   discontinuous with the prior 1960 years of magisterial teaching. High
 *   extractiveness (0.82) reflects the transfer of doctrinal sovereignty from
 *   Tradition to a living magisterium; high suppression (0.78) reflects the
 *   canonical and administrative machinery that marginalizes, suppresses, or
 *   penalizes the traditionalist alternative (1962 Missal restrictions,
 *   doctrinal censorship, appointment control). Theater ratio (0.45) captures
 *   the performative maintenance of 'hermeneutic of continuity' rhetoric
 *   while practice diverges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.82).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, snare).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority — Rupture Traditionalist Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e371d036-343e-4cfc-a72b-8eda5f49bef6').
narrative_ontology:cs_kernel_codification('e371d036-343e-4cfc-a72b-8eda5f49bef6', fixed_text).
narrative_ontology:cs_authority_grounding('e371d036-343e-4cfc-a72b-8eda5f49bef6', lineage).
narrative_ontology:cs_interpretation_layer_present('e371d036-343e-4cfc-a72b-8eda5f49bef6').
narrative_ontology:cs_reading_relation('e371d036-343e-4cfc-a72b-8eda5f49bef6', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('e371d036-343e-4cfc-a72b-8eda5f49bef6', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('e371d036-343e-4cfc-a72b-8eda5f49bef6', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('e371d036-343e-4cfc-a72b-8eda5f49bef6', foundational, conciliar_texts_contain_doctrinal_errors).
narrative_ontology:cs_axiom_status(conciliar_texts_contain_doctrinal_errors, holdable).
narrative_ontology:cs_axiom_grounding('e371d036-343e-4cfc-a72b-8eda5f49bef6', conciliar_texts_contain_doctrinal_errors, empirically_contingent).
narrative_ontology:cs_axiom('e371d036-343e-4cfc-a72b-8eda5f49bef6', foundational, post_conciliar_heterodoxy_inevitable_from_texts).
narrative_ontology:cs_axiom_status(post_conciliar_heterodoxy_inevitable_from_texts, holdable).
narrative_ontology:cs_axiom_grounding('e371d036-343e-4cfc-a72b-8eda5f49bef6', post_conciliar_heterodoxy_inevitable_from_texts, empirically_contingent).
narrative_ontology:cs_axiom('e371d036-343e-4cfc-a72b-8eda5f49bef6', foundational, tradition_ruptured_at_vatican_ii).
narrative_ontology:cs_axiom_status(tradition_ruptured_at_vatican_ii, holdable).
narrative_ontology:cs_axiom_grounding('e371d036-343e-4cfc-a72b-8eda5f49bef6', tradition_ruptured_at_vatican_ii, deontological).
narrative_ontology:cs_reference_frame('e371d036-343e-4cfc-a72b-8eda5f49bef6', pre_conciliar_doctrinal_stability).
narrative_ontology:cs_drift_state('e371d036-343e-4cfc-a72b-8eda5f49bef6', post_conciliar_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('e371d036-343e-4cfc-a72b-8eda5f49bef6', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_hierarchy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, liberal_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, liturgical_reformers).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditionalist_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_form_adherents).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_continuity_advocates).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_bureaucracy).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tradition_as_immutable_deposit).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, liturgical_continuity_as_constitutive).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_zeal_requires_doctrinal_certainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Priests and bishops formed in pre-conciliar seminary system who cannot reconcile ordination vows with post-conciliar praxis. Exit means laicization or schism — both structurally costly. They bear the cost of implementing liturgical and doctrinal changes they judge erroneous, while their formational identity binds them to the very tradition the constraint disrupts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditionalist_clergy, payer,
    organized, biographical, identity_locked, global).

% Lay faithful attached to traditional liturgy, catechesis, and devotional life. Their parish communities were restructured, familiar forms suppressed, and objections dismissed as disobedience. Exit options: tolerate novel praxis, seek rare traditional enclaves (FSSP, ICKSP, SSPX), or leave the Church — each carries significant social and spiritual cost.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditionalist_laity, payer,
    moderate, biographical, constrained, global).

% Communities and individuals for whom the pre-conciliar form is not preference but existential structure — cloistered contemplatives using the 1962 Office, missionary orders founded on Tridentine spirituality, families built around the traditional calendar. They were not consulted; their form of life was administratively suppressed. No exit preserves their charism.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_form_adherents, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, pre_conciliar_form_adherents, excluded).

% Theologians, canonists, and teachers who argue conciliar texts contradict prior defined doctrine (e.g., on religious liberty, ecumenism, collegiality). Their publications are marginalized, appointments denied, and positions labeled 'pre-conciliar' as pejorative. Exit means academic exile or submission to hermeneutic they judge false.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_continuity_advocates, payer,
    moderate, generational, constrained, global).

% Orders founded on explicit mandate to convert non-Christians (e.g., early Jesuits, Franciscans, Dominicans, White Fathers, Holy Ghost Fathers). Post-conciliar shift to 'dialogue' and 'inculturation' replaced conversionary zeal. Vocations collapsed; founding charism administratively reinterpreted. Identity fused to missionary mandate — cannot exit without dissolving the order's reason for existence.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders, payer,
    organized, generational, identity_locked, global).

% Bishops and cardinals who drove conciliar reforms and control post-conciliar implementation. They interpret ambiguities expansively, appoint like-minded successors, and direct resources to reform structures. Benefit: institutional relevance, ecumenical prestige, alignment with modernity. Exit is trivial — they administer the constraint.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Academic theologians (Rahner, Küng, Schillebeeckx, later generations) whose projects were marginalized pre-conciliar and centered post-conciliar. Conciliar ambiguities legitimize their research programs; they staff pontifical universities, advise dicasteries, shape catechetical texts. Exit: tenured positions elsewhere; constraint subsidizes their careers.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, liberal_theologians, beneficiary,
    organized, biographical, mobile, global).

% Experts (Bugnini, Martimort, later ICEL/ICEL-type bodies) who designed the Novus Ordo and control its ongoing revision. Their professional standing depends on the reform's irreversibility. They benefit from consultancy, publication, and institutional gatekeeping. Exit: academic liturgical studies posts; constraint is their professional substrate.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, liturgical_reformers, beneficiary,
    organized, biographical, mobile, global).

% Roman Curia dicasteries (CDF, CDW, CCEE, etc.) that administer conciliar implementation. They gain expanded regulatory scope, personnel, and budget from managing the transition. The constraint's ambiguity expands their interpretive authority. Exit is notional — they are the constraint's enforcement arm.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_bureaucracy, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_bureaucracy, beneficiary).

% Historians, sociologists, canonists, and external analysts who study the post-conciliar transition without structural stake. They document the constraint's operation across seats but neither collect nor pay. Their analyses inform but do not determine classification.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecclesiastical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council presented itself as solving the coordination problem of 'aggiornamento' — updating Church discipline, liturgy, and pastoral approach to engage the modern world while preserving doctrinal integrity. The traditionalist reading judges this coordination as failed: the texts introduced ambiguities that made doctrinal integrity impossible to preserve.
% TRANSFER_FUNCTION: Moves doctrinal authority and liturgical sovereignty from the immutable Tradition (as guarded by the pre-conciliar magisterium) to a living magisterium that reads conciliar texts through a progressive hermeneutic. The transfer is: certainty → ambiguity; fixity → development; clerical/lay distinction in liturgy → universal lay activation; missionary mandate → dialogue mandate. Gains accrue to progressive hierarchy and their theological allies; costs fall on traditionalist clergy, laity, and missionary orders.
% ABSENT_VOICES: The global missionary field — bishops and priests in mission territories who pleaded for doctrinal clarity as prerequisite for conversion — were overridden by European conciliar fathers. The laity of the 'global South' whose Catholicism was shaped by Tridentine forms were not consulted. Persecuted Church behind Iron Curtain (whose witness relied on traditional forms) had no vote. These voices are excluded from the conciliar record and post-conciliar governance.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — meaning the progressive hermeneutic of conciliar texts lost institutional enforcement — traditional liturgy and catechesis would rapidly re-expand (already occurring where permissions granted), missionary orders would recover conversionary identity, doctrinal disputes would re-center on pre-conciliar definitions. The progressive theological establishment would lose its structural subsidy. The Church's institutional center of gravity would shift decisively toward Tradition.
% FOUNDING_PROBLEM: The Council was convoked by John XXIII to 'open the windows' and address 'the signs of the times' — perceived crisis of relevance, clericalism, anti-modernism, and missionary stagnation in the 1950s Church. The traditionalist reading argues the Council misdiagnosed: the crisis was not Tradition but its defective transmission; the remedy introduced the very evils it sought to cure.
% FOUNDING_PROBLEM_CORROBORATION: John XXIII's opening address and the preparatory schemata (rejected by conciliar fathers) attest the founding intent: pastoral updating without doctrinal change. The progressive hermeneutic's own historians (O'Malley, Alberigo) document the conciliar takeover by Northern European bishops. Benedict XVI's 2005 'hermeneutic of continuity' address implicitly concedes the founding problem (relevance) was live but argues the Council solved it correctly — a contested reading. No non-beneficiary source corroborates that the Council's texts successfully solved the founding problem without rupture.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness rises monotonically from 0.35 (1965, immediate post-conciliar optimism) to 0.82 (2025, Traditionis Custodes era) as the progressive hermeneutic consolidates control over seminaries, appointments, and liturgical law. Suppression requirement rises from 0.45 to 0.78 as enforcement shifts from persuasion to canonical restriction (1974 Missal suppression, 1988 Ecclesia Dei, 2007 Summorum Pontificum, 2021 Traditionis Custodes). Theater peaks mid-interval (0.45) as 'hermeneutic of continuity' discourse masks accelerating divergence. Accessibility collapse (0.72) reflects the near-total disappearance of pre-conciliar formation from ordinary ecclesiastical channels. Resistance (0.68) captures the persistent traditionalist movement (SSPX, Ecclesia Dei communities, lay associations) that the constraint must actively contain.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive hierarchy seat, the constraint is a rope: genuine coordination solving the Church's relevance crisis. From the traditionalist clergy seat, it is a snare: the coordination story (aggiornamento) is cover for doctrinal rupture; the constraint persists only by suppressing the traditional alternative. From the missionary order seat, it is a piton: the missionary mandate has atrophied but the order persists by inertia, performing dialogue while conversionary zeal evaporates. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive hierarchy and liberal theologians are structural beneficiaries (d ≈ 0.15): they control interpretation, appointments, and resources. Traditionalist clergy and laity are structural targets (d ≈ 0.85): they bear the cost of implementing a hermeneutic they judge false, with exit blocked by identity_lock (clergy) or constrained (laity). Missionary orders are identity-locked targets (d ≈ 0.9): their charism is structurally incompatible with the constraint but they cannot exit without self-dissolution. Vatican bureaucracy sits at d ≈ 0.2 — they administer the constraint and benefit from its ambiguity-expansion. Ecclesiastical observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (relevance/missionary stagnation) is dead: the post-conciliar Church in Europe shows catastrophic decline in practice, vocations, and belief — the very metrics the Council sought to improve. The constraint persists not because it solves the founding problem but because the progressive hierarchy that benefits from it controls the succession. This is classic mandatrophy: the arrangement outlives its function and becomes self-justifying. The 'hermeneutic of continuity' is the mandatrophic discourse — it claims the constraint is continuous with Tradition while the constraint's operation transfers authority away from Tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Vatican II doctrinal authority kernel one constraint with multiple readings, or are the readings themselves distinct constraints with different ε referents?',
    'Apply ε-invariance test: if measuring ''Vatican II authority'' from the traditionalist seat yields ε ≈ 0.82 and from the progressive seat yields ε ≈ 0.15, they are different constraints. The kernel_id groups them for committer-frame tracking; the engine classifies each reading''s constraint independently.',
    'If readings are distinct constraints, the kernel_id is a committer-frame grouping only, not a single classification target. The engine must not average or reconcile across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether kernel_id denotes one constraint or a family of reading-constraints').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditional forms structural (canonical restrictions, appointment control) or internalized (traditionalists self-censor because they accept the progressive frame as legitimate)?',
    'Post-exit suppression trajectory: traditionalists who achieve canonical regularization (Ecclesia Dei communities) still face liturgical restrictions and doctrinal marginalization — suggesting structural suppression persists. Those who schism (SSPX) face excommunication-level penalties — structural. Internalized component: many traditionalists accept ''obedience'' framing that limits resistance.',
    'If substantially internalized, effective suppression is higher than structural measure — the target carries suppression after exit. This amplifies χ for identity-locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in post-conciliar traditionalist marginalization').

omega_variable(
    conciliar_text_intent_vs_effect,
    'Are the conciliar ambiguities deliberate compromise-drafting (intended to permit progressive development) or unintentional errors (exploited post-hoc)?',
    'Conciliar acta and intervention records: the ''rhine flows into the tiber'' coalition deliberately drafted ambiguous formulas (e.g., Dignitatis Humanae §2, Unitatis Redintegratio §3) to secure majority while preserving progressive interpretive space. The traditionalist reading''s ''error'' claim is partly retrospective — the texts were engineered for the progressive reading.',
    'If deliberate, the constraint''s coordination function was always a cover for progressive capture (snare confirmed). If unintentional, the constraint drifted from rope to snare via progressive hermeneutic capture (tangled_rope → snare drift).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conciliar_text_intent_vs_effect, empirical, 'Whether conciliar ambiguities were engineered for progressive hermeneutic capture').

omega_variable(
    missionary_zeal_collapse_causality,
    'Did the post-conciliar shift from conversion to dialogue cause missionary collapse, or did secularization cause both the shift and the collapse?',
    'Compare missionary orders that retained traditional charism (e.g., FSSP missions, ICKSP Africa) vs. those that adopted dialogue model. Control for regional secularization. Early data: traditional charism missions show vocations growth; dialogue-model orders show collapse.',
    'If causal, the constraint extracts missionary vitality as direct cost. If secularization is primary driver, the constraint is a symptom, not cause — ε should be lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(missionary_zeal_collapse_causality, empirical, 'Causal attribution of missionary vitality collapse to conciliar hermeneutic shift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 50, 0.81).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_ecumenism).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_religious_liberty).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_collegiality).

% DUAL FORMULATION NOTE:
% This reading is one member of the vatican_ii_doctrinal_authority constraint family. The continuity_reading claims ε ≈ 0.05 (mountain/rope); rupture_progressive_reading claims ε ≈ 0.15 (rope/tangled_rope with positive valence); composite_overdetermination_reading decomposes into sub-constraints. This reading's high ε (0.82) reflects the same doctrinal change the progressive reading celebrates, negatively valued. The family shares the conciliar texts as kernel but disagrees on ε, beneficiaries, victims, and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
