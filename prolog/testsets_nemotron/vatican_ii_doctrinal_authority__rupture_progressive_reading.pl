% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Doctrinal Authority — Rupture Progressive Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story models the 'rupture progressive' reading of Vatican
 *   II's doctrinal authority: the Council constituted a necessary break with
 *   pre-conciliar rigidity, and its 'spirit' authorizes ongoing reform beyond
 *   the textual limits of the sixteen documents. This reading treats
 *   Dignitatis Humanae (religious liberty) as a genuine reversal of the
 *   Syllabus of Errors, reads textual ambiguities (e.g., 'subsists in' in
 *   Lumen Gentium 8, 'seeds of the Word' in Ad Gentes) as intentional
 *   openings for further development, and treats post-conciliar
 *   implementation (the liturgical reform, ecumenical dialogue, collegial
 *   episcopal conferences) as the authentic realization of conciliar intent
 *   rather than distortion. The constraint operates through the hermeneutic
 *   license: 'spirit of the Council' becomes a principle that authorizes
 *   development unmoored from textual anchors, generating extraction from
 *   those bound to the pre-conciliar framework while benefiting reformers who
 *   claim the Council's mandate.
 *
 * KEY AGENTS:
 *   - progressive_theologians: Primary beneficiary (institutional/biographical) — claim interpretive authority over the Council's meaning, receive institutional positions and intellectual prestige
 *   - post_conciliar_reformers: Primary beneficiary (organized/biographical) — implement changes authorized by 'spirit' reading, control pastoral and liturgical apparatus
 *   - traditionalist_faithful: Primary victim (organized/identity_locked) — bear doctrinal/liturgical displacement, experience suppression through marginalization and canonical restriction
 *   - pre_conciliar_doctrinal_framework: Victim (abstract, non-agent) — the doctrinal structure itself is overwritten; its coherence is the extraction referent
 *   - clergy_formed_pre_1962: Victim (organized/identity_locked) — formation rendered obsolete, ministerial identity disrupted, obedience demanded to changes they were not formed to understand
 *   - collegial_bishops: Beneficiary (institutional/generational) — gain governance authority via collegiality doctrine, exercise power previously reserved to Rome
 *   - ecumenical_dialogue_practitioners: Beneficiary (organized/biographical) — gain institutional mandate and resources for dialogue previously forbidden
 *   - magisterium_post_conciliar: Agenda setter (institutional/generational) — administers the hermeneutic, determines which 'spirit' developments are authentic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.78).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.52).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority — Rupture Progressive Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'c646e3b7-a1bc-4f41-acef-f93494a07733').
narrative_ontology:cs_kernel_codification('c646e3b7-a1bc-4f41-acef-f93494a07733', fixed_text).
narrative_ontology:cs_authority_grounding('c646e3b7-a1bc-4f41-acef-f93494a07733', lineage).
narrative_ontology:cs_interpretation_layer_present('c646e3b7-a1bc-4f41-acef-f93494a07733').
narrative_ontology:cs_reading_relation('c646e3b7-a1bc-4f41-acef-f93494a07733', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c646e3b7-a1bc-4f41-acef-f93494a07733', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c646e3b7-a1bc-4f41-acef-f93494a07733', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('c646e3b7-a1bc-4f41-acef-f93494a07733', foundational, spirit_authorizes_unbounded_development).
narrative_ontology:cs_axiom_status(spirit_authorizes_unbounded_development, holdable).
narrative_ontology:cs_axiom_grounding('c646e3b7-a1bc-4f41-acef-f93494a07733', spirit_authorizes_unbounded_development, deontological).
narrative_ontology:cs_axiom('c646e3b7-a1bc-4f41-acef-f93494a07733', foundational, religious_liberty_reverses_syllabus_authentically).
narrative_ontology:cs_axiom_status(religious_liberty_reverses_syllabus_authentically, holdable).
narrative_ontology:cs_axiom_grounding('c646e3b7-a1bc-4f41-acef-f93494a07733', religious_liberty_reverses_syllabus_authentically, deontological).
narrative_ontology:cs_reference_frame('c646e3b7-a1bc-4f41-acef-f93494a07733', conciliar_texts_as_springboard).
narrative_ontology:cs_drift_state('c646e3b7-a1bc-4f41-acef-f93494a07733', post_synodality_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c646e3b7-a1bc-4f41-acef-f93494a07733', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_dialogue_practitioners).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, collegial_bishops).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_faithful).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_doctrinal_framework).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, clergy_formed_pre_1962).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, spirit_of_the_council_hermeneutic).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, development_of_doctrine_as_rupture).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, religious_liberty_as_doctrinal_reversal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim interpretive authority over the Council's meaning through the 'spirit' hermeneutic. Receive academic positions, publication venues, advisory roles in Roman curia and episcopal conferences. Their career advancement depends on the hermeneutic remaining authoritative. Exit is mobile — they could work in secular academia or other traditions, but their specific capital is tied to this reading's dominance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theologians, beneficiary,
    institutional, biographical, mobile, global).

% Implement liturgical, catechetical, and pastoral changes justified by 'spirit of the Council'. Control diocesan offices, liturgical commissions, seminary formation. Their institutional position depends on the reform trajectory continuing. Exit is constrained — they have professional skills but their specific authority derives from the progressive reading's institutional capture.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_reformers, beneficiary,
    organized, biographical, constrained, global).

% Experience the Council's changes as loss of doctrinal certainty, liturgical form, and spiritual inheritance. Their identity is fused with the pre-conciliar framework — the Mass, the catechism, the moral theology of their formation. Structural exit exists (Ecclesia Dei communities, SSPX, sedevacantism) but is costly and incomplete; internal exit is nearly impossible because the framework constitutes their Catholic identity. They bear the extraction: their inheritance is overwritten, their obedience demanded to changes they experience as rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_faithful, payer,
    organized, generational, identity_locked, global).

% The doctrinal structure itself — the Syllabus of Errors, Quanta Cura, the Thomistic manual tradition, the pre-conciliar magisterial corpus — is the referent of extraction. It is overwritten by Dignitatis Humanae, the collegiality doctrine, the religious liberty reversal. As a non-agent, it cannot resist or exit; its coherence is simply displaced. Listed as victim to mark the structural extraction referent.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_doctrinal_framework, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_doctrinal_framework).

% Priests and bishops formed in the pre-conciliar seminary system (Thomistic philosophy, Trent catechism, Tridentine liturgy, anti-modernist oath). Their ministerial identity, intellectual formation, and spiritual vocabulary were constituted by the framework the Council displaced. They are required to implement and teach reforms they were not formed to understand, often experiencing this as betrayal of their ordination promises. Exit is identity_locked — leaving ministry means leaving their vocation; staying means administering a framework they experience as alien.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, clergy_formed_pre_1962, payer,
    organized, biographical, identity_locked, global).

% Gain governance authority through Lumen Gentium's collegiality doctrine — episcopal conferences, synods, shared responsibility with Rome. Their power is structurally enhanced by the Council. Exit is arbitrage-grade: they could operate within the continuity reading or traditionalist frameworks and retain episcopal authority, but the collegial structure gives them maximum leverage.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, collegial_bishops, beneficiary,
    institutional, generational, arbitrage, global).

% Receive institutional mandate, funding, and status for ecumenical and interreligious dialogue previously forbidden (Unitatis Redintegratio, Nostra Aetate). Their professional existence depends on the post-conciliar framework. Exit is constrained — dialogue skills transfer, but the Catholic institutional mandate is specific to this reading.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_dialogue_practitioners, beneficiary,
    organized, biographical, constrained, global).

% Administers the hermeneutic: determines which 'spirit' developments are authentic (e.g., approving the liturgical reform, restricting the traditional Mass via Traditionis Custodes, advancing synodality). Could change the hermeneutic (as Benedict XVI attempted with 'hermeneutic of continuity') but extracts legitimacy and governance capacity from maintaining the progressive reading's authority. Exit is analytical — as an institution it can reevaluate its own interpretive framework, but doing so risks its governing legitimacy.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, magisterium_post_conciliar, agenda_setter,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, magisterium_post_conciliar).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the Church's isolation from modernity: religious liberty enables engagement with pluralistic states; ecumenism ends confessional warfare; collegiality decentralizes governance; liturgical reform makes worship intelligible. The 'spirit' hermeneutic coordinates ongoing adaptation so the solution doesn't fossilize.
% TRANSFER_FUNCTION: Moves interpretive authority, governance power, liturgical control, and intellectual prestige from the pre-conciliar framework (papal monarchy, curial centralization, Tridentine liturgy, Thomistic manualism) to progressive reformers (theologians, bishops' conferences, liturgical experts, ecumenical offices). The extraction is the displacement of the old framework's coherence and the faithful's inheritance.
% ABSENT_VOICES: The pre-conciliar magisterium itself (Pius IX, Leo XIII, Pius X, Pius XII) — their teaching is the referent being overwritten, but they cannot speak. The faithful of 1960 who entered the Council with one framework and exited with another — they were not consulted. Future generations who inherit the 'spirit' license with no textual anchor — they are not yet present to object.
% DISAPPEARANCE_RATIONALE: If the 'spirit of the Council' hermeneutic vanished overnight, the Church would revert to textual conciliar documents only. The unbounded development license would cease. Liturgical, ecumenical, and governance reforms justified only by 'spirit' (not textual conciliar mandate) would lose authorization. Traditionalist communities would claim vindication. Progressive reformers would lose their primary legitimating principle. The magisterium would face a legitimacy crisis — its post-conciliar governance has been justified by this hermeneutic for 60 years.
% FOUNDING_PROBLEM: The Church's isolation from the modern world: condemnation of religious liberty (Syllabus of Errors), rejection of ecumenism (Mortalium Animos), centralized papal monarchy unable to govern a global Church, liturgy unintelligible to the faithful, anti-modernist oath suppressing theological engagement with contemporary thought.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and post-conciliar reformers attest the problem is live: new modernities (digital, bioethical, gender, ecological) require ongoing 'spirit'-authorized development. Traditionalist faithful and clergy formed pre-1962 attest the problem is dead: the Council solved the isolation, further development is extraction. No corroborating source outside the beneficiary set (progressive theologians, reformers, collegial bishops) confirms the problem remains live — independent historians of the modern Church (e.g., non-Catholic scholars like Brad Gregory, Carlos Eire) document the Church's successful engagement with modernity post-Vatican II but do not confirm the 'spirit' hermeneutic remains necessary for that engagement.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the hermeneutic license authorizes unbounded development: the 'spirit' principle has no internal stopping condition, so each generation of reformers can claim new reforms as conciliarly mandated. The religious liberty reversal alone is a massive extraction — it overturns a settled doctrinal framework (Syllabus of Errors, Quanta Cura) that structured Catholic political theology for a century, imposing costs on the faithful who held that framework as binding. Suppression (0.52) is moderate: structural suppression exists (canonical restrictions on traditionalist groups, liturgical uniformity enforcement via Traditionis Custodes), but much suppression is internalized — the pre-conciliar framework's collapse makes the old way unlivable from within. Theater ratio (0.28) is moderate: genuine coordination occurs (ecumenism, religious liberty engagement with modernity, collegial governance), but a growing share of 'spirit'-authorized changes serve institutional interests (bureaucratic expansion, relevance-seeking) rather than the coordination function. Accessibility collapse (0.35) is low for a tangled_rope: alternatives persist (traditionalist communities, continuity hermeneutics, sedevacantism) because the textual documents themselves are ambiguous enough to support rival readings. Resistance (0.62) is high: the continuity reading and traditionalist reading maintain institutional presence and intellectual coherence, contesting the progressive reading's claim to authenticity.
 *
 * PERSPECTIVAL GAP:
 *   From the progressive_theologian and post_conciliar_reformer seats, the constraint is genuine coordination (rope-like): the Council solved the Church's isolation from modernity, and 'spirit' is the principle that keeps the solution living. From the traditionalist_faithful and clergy_formed_pre_1962 seats, the same constraint is extractive (snare-like): the 'spirit' license extracts their doctrinal and liturgical inheritance without consent, and suppression enforces the extraction. The magisterium_post_conciliar seat experiences it as agenda-setting authority with extraction as a byproduct of governance. The engine computes this divergence from the structural data — the declared beneficiaries, victims, power levels, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (progressive_theologians, post_conciliar_reformers, collegial_bishops, ecumenical_dialogue_practitioners) collect interpretive authority, institutional positions, governance power, and intellectual prestige — they are near the beneficiary end (d ~ 0.15-0.25). Victims (traditionalist_faithful, clergy_formed_pre_1962) bear doctrinal displacement, liturgical loss, identity disruption, and canonical marginalization — they are near the target end (d ~ 0.85-0.95, identity_locked exit). The pre_conciliar_doctrinal_framework is a non-agent victim (the doctrinal structure overwritten). The magisterium_post_conciliar is agenda_setter with institutional power and analytical exit — it administers the hermeneutic and could change it but extracts legitimacy from maintaining it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Church's isolation from modernity, need for aggiornamento) was live in 1962. By 2025, the problem is contested: the Church is not isolated but the 'spirit' hermeneutic continues authorizing reforms. The constraint exhibits mandatrophy — the coordination function (engaging modernity) has been substantially achieved, but the extraction function (unbounded hermeneutic license) persists and expands. The founding_problem_status 'contested' reflects this: beneficiaries claim the problem is live (new modernities require new reforms); victims claim it is dead (the Council solved it, further 'development' is extraction). No external corroborator outside the beneficiary set confirms the problem remains live — the corroboration field notes this absence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this constraint represent one reading of the contested kernel ''vatican_ii_doctrinal_authority'', or an independent constraint?',
    'Compare structural metrics and beneficiary/victim sets across the declared sibling readings (continuity_reading, rupture_traditionalist_reading, composite_overdetermination_reading). If each yields distinct ε and distinct stakeholder structures, they are separate constraints linked by the kernel.',
    'If this is a kernel reading, the committer structure (which reading, which kernel, sibling relations) must be routed to omegas and cs_structure, not embedded in standard fields.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint instantiates the rupture_progressive_reading of the vatican_ii_doctrinal_authority kernel.').

omega_variable(
    spirit_vs_letter_extraction_boundary,
    'Where does the ''spirit of the Council'' hermeneutic transition from genuine development of doctrine into extraction — authorizing changes that have no textual anchor and serve institutional interests rather than the faithful?',
    'Trace specific post-conciliar changes (liturgical, ecumenical, disciplinary) to their conciliar textual basis. Where the chain of derivation breaks — where a change is justified only by ''spirit'' with no textual foothold — measure the extraction that change generates.',
    'If the spirit/letter gap is systematically extractive, the constraint''s claimed_type may be snare rather than tangled_rope. If the gap is genuinely developmental, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_vs_letter_extraction_boundary, conceptual, 'Boundary between authentic doctrinal development and extraction via hermeneutic license.').

omega_variable(
    religious_freedom_reversal_measurement,
    'How much of the measured extractiveness (ε=0.78) derives specifically from the religious liberty reversal (Dignitatis Humanae vs. Syllabus of Errors), versus other conciliar shifts?',
    'Decompose ε by doctrinal domain: religious liberty, ecumenism, collegiality, liturgy, world engagement. Compare pre/post conciliar teaching in each domain and measure the coercive/reform pressure each shift generated.',
    'If religious liberty reversal alone accounts for most extraction, the constraint may be more snare-like (targeted reversal with high coercion). If distributed across domains, tangled_rope is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_freedom_reversal_measurement, empirical, 'Attribution of extractiveness to specific doctrinal reversals within the Council.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression experienced by traditionalist_faithful and pre-1962 clergy primarily structural (canonical penalties, institutional marginalization) or internalized (identity-fusion with the pre-conciliar framework making the new framework unlivable)?',
    'Post-exit trajectory analysis: where traditionalist communities obtained structural exit (Ecclesia Dei, Summorum Pontificum, Traditionis Custodes phases), did suppression persist? Persistence after structural exit indicates internalized component.',
    'If substantially internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression for traditionalist stakeholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t15, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(vati_tr_t15, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t45, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement_basis(vati_tr_t45, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t15, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(vati_be_t15, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t45, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 45, 0.73).
narrative_ontology:measurement_basis(vati_be_t45, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t15, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(vati_su_t15, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t45, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement_basis(vati_su_t45, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_liturgical_reform).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, ecumenical_dialogue_framework).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, collegial_episcopal_governance).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the vatican_ii_doctrinal_authority kernel. The continuity_reading claims low ε (organic development); rupture_traditionalist_reading claims high ε but frames it as error rather than authentic development; composite_overdetermination_reading decomposes the Council into multiple constraints with distinct ε values. This reading (rupture_progressive) claims high ε as authentic development authorized by 'spirit'. The ε-invariance principle requires separate stories because each reading produces a different ε on the same referent — they are different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, institutional, 0.2).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
