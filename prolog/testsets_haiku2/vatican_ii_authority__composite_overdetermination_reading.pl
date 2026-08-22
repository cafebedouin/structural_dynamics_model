% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Authority Apparatus: Composite Overdetermination Reading
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   Vatican II (1962-1965) is presented institutionally as a single,
 *   univocally interpretable Council that modernized Catholic doctrine while
 *   maintaining continuity with tradition. This reading — the composite
 *   overdetermination reading — asserts that Vatican II is not a single
 *   interpretable event but an overdetermined composite of multiple distinct
 *   doctrinal shifts with incompatible theological rationales. The Council's
 *   documents encode genuine contradictions from factional compromise among
 *   bishops with incompatible visions of modernization and tradition. The
 *   ambiguities cannot be resolved into either a continuity framework (which
 *   requires harmonizing the documents with pre-conciliar doctrine) or a
 *   rupture framework (which requires identifying systematic doctrinal
 *   error). This reading makes the structural claim that the ambiguities are
 *   not accidental ambiguities awaiting better interpretation, but
 *   constitutive contradictions embedded in the factional origins of the
 *   compromise. The magisterium's insistence on univocal interpretation is
 *   therefore an extraction mechanism: it suppresses recognition of the
 *   contradictions to maintain institutional authority, while permitting both
 *   liberal and traditionalist factions to use the ambiguities as cover for
 *   their readings.
 *
 * KEY AGENTS:
 *   - conciliar_scholarship: beneficiary (livelihood depends on ongoing interpretive work on unresolved tensions)
 *   - magisterium_authority_apparatus: agenda_setter and payer (must enforce univocity; pays in enforcement costs)
 *   - traditionalist_theological_factions: payers (dissent constrained, reading not definitively validated)
 *   - liberal_reform_practitioners: beneficiaries (ambiguity permits progressive reading as institutionally legitimate)
 *   - vatican_archival_authority: agenda_setter (controls evidence base for what Council 'really intended')
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II Authority Apparatus: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'bdd902fb-cf1d-4db1-a4c3-52836e6be8f1').
narrative_ontology:cs_kernel_codification('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', fixed_text).
narrative_ontology:cs_authority_grounding('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', extraction).
narrative_ontology:cs_interpretation_layer_present('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1').
narrative_ontology:cs_reading_relation('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', foundational, vatican_ii_ambiguities_constitute_genuine_theological_contradiction).
narrative_ontology:cs_axiom_status(vatican_ii_ambiguities_constitute_genuine_theological_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', vatican_ii_ambiguities_constitute_genuine_theological_contradiction, empirically_contingent).
narrative_ontology:cs_axiom('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', foundational, univocal_authority_cannot_resolve_constitutive_contradiction).
narrative_ontology:cs_axiom_status(univocal_authority_cannot_resolve_constitutive_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', univocal_authority_cannot_resolve_constitutive_contradiction, deontological).
narrative_ontology:cs_reference_frame('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', vatican_ii_as_coherent_univocal_doctrine).
narrative_ontology:cs_drift_state('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', post_archival_scholarship_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('bdd902fb-cf1d-4db1-a4c3-52836e6be8f1', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholarship).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, post_conciliar_reform_practitioners).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, magisterium_authority_apparatus).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, traditionalist_theological_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, liberal_reform_practitioners).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, conservative_reform_resistors).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, pastoral_implementation_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic and theological scholarship communities that read Vatican II as a complex overdetermined event benefit from the ambiguities themselves: the unresolved tensions enable sustained interpretive work, grant-funded research into competing readings, and professional legitimacy in explicating the contradictions. The ambiguity is their subject matter and their livelihood. Scholars can exit by working on other topics, but the field of Vatican II studies is constituted by the undecidability.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholarship, beneficiary,
    organized, generational, mobile, global).

% The official teaching authority of the Catholic Church claims univocal interpretation of Vatican II's binding meaning. The apparatus must enforce a reading (continuity or rupture) to maintain institutional coherence, but the documents themselves embed contradictory theological rationales from factional compromise. The apparatus bears the cost of active enforcement to suppress recognition of the ambiguities. It cannot exit without dissolving its claim to authoritative teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, magisterium_authority_apparatus, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, magisterium_authority_apparatus, payer).

% Catholic theologians and communities committed to reading Vatican II as either continuous with tradition or as a rupture from tradition pay the cost of institutional conflict: their reading is not definitively validated by the magisterium, their dissent is administratively constrained, their interpretation competes for institutional authority without settling the question. They are caught in the unresolved tensions. Exit is constrained because leaving the Church means abandoning the institutional home of their faith.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_theological_factions, payer,
    moderate, generational, constrained, continental).

% Clergy, religious educators, and pastoral theologians who read Vatican II as legitimating progressive reforms (liturgical openness, ecumenism, lay engagement, engagement with modernity) benefit from the ambiguities: the overdetermination permits their reading without contradiction from the magisterium, since the magisterium cannot univocally close off their interpretation. They use the ambiguity as institutional cover. They have mobile exit because they can adopt traditional interpretations or leave institutional ministry.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, liberal_reform_practitioners, beneficiary,
    organized, generational, mobile, global).

% Clergy and theologians resistant to the progressive interpretation of Vatican II pay through institutional marginalization: their reading that the Council was misinterpreted is not definitively upheld by the magisterium, forcing them to dissent within the structure while bearing costs of administrative constraint and denial of advancement. Exit is constrained by institutional identity and the costs of leaving priesthood or religious life.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, conservative_reform_resistors, payer,
    moderate, generational, constrained, continental).

% National bishops' conferences and diocesan structures implementing Vatican II's decrees confront incompatible doctrinal rationales in the documents: they must choose a reading (continuity or rupture framework) to guide pastoral practice, but the documents provide no univocal resolution. They bear the cost of choosing while lacking authority to settle the interpretation. Their exit is constrained by their institutional role.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, pastoral_implementation_bodies, payer,
    moderate, biographical, constrained, regional).

% The Vatican's control over archival access to Vatican II preparatory materials and conciliar deliberations enables selective disclosure of the factional origins of the contradictions. This control shapes whose reading becomes historicized as 'what the Council really intended' and suppresses recognition of overdetermination by controlling the evidence base. The Vatican Archive remains the primary source for reconstructing the factional origins, so control of access is control over the knowledge base.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, vatican_archival_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Other Christian communities and world religions engaged in dialogue with the Catholic Church on the basis of Vatican II's ecumenical openness confront ambiguity about the Council's binding force: they are excluded from the magisterium's internal authority structure but affected by which reading (continuity or rupture) becomes institutionally dominant, as it determines the Church's doctrinal ground for dialogue. They cannot exit from the need to negotiate with the institutional Church.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, ecumenical_dialogue_partners, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, magisterium_authority_apparatus).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vatican II coordinates a single authoritative body (the magisterium) that resolves what is binding doctrine for Catholics globally. The Council's documents are meant to clarify and update that doctrine in response to modern conditions, providing a stable reference point for interpretation across the Church.
% TRANSFER_FUNCTION: The constraint transfers interpretive authority from the Council's texts to the magisterium's official reading of those texts. It also transfers institutional legitimacy from the Council's internal factional compromise (the real historical event) to a univocal doctrinal position claimed as the Council's true meaning. The cost is paid in unresolved theological conflict and the requirement of constant enforcement against alternative readings.
% ABSENT_VOICES: Non-Catholic theological traditions are excluded from magisterial authority but affected by which reading becomes dominant. Critical historians who have documented the factional origins of the contradictions are structurally excluded from the authority apparatus itself, permitted only as external observers whose scholarship threatens the univocal interpretation. Dissenting Catholic theologians are partially excluded: they can publish but cannot hold institutional teaching authority.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if the magisterium ceased enforcing a univocal reading and permitted Vatican II's ambiguities to stand as genuinely unresolved — the institutional structure of the Catholic Church would reorganize around acknowledged pluralism. Theological factions would cease dissenting against the magisterium and begin negotiating the boundaries of acceptable pluralism. Pastoral implementation would be freed to choose frameworks based on local conditions rather than enforcing a single reading globally.
% FOUNDING_PROBLEM: Vatican II was convoked to modernize the Church's relation to the contemporary world while preserving doctrinal continuity. The Council's documents were written through factional compromise among bishops with incompatible visions (modernizers vs. traditionalists, European vs. non-European, curial vs. reformist). The founding problem was genuine: how to teach binding doctrine while opening the Church to modernity and ecumenism.
% FOUNDING_PROBLEM_CORROBORATION: Vatican historians (Wiltgen, Komonchak, Alberigo) have documented the factional origins and incompatible theological rationales embedded in the Council's compromises from archival evidence and conciliar debates. The magisterium attests the founding problem remains live and requires univocal interpretation. Progressive and traditionalist Catholic theologians attest the problem is that the magisterium claims univocity where the documents encode genuine contradiction — their corroboration comes from textual analysis and historical evidence from outside the benefiting factions.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at endpoint) because the constraint extracts institutional legitimacy from the Council's ambiguities while suppressing recognition of the contradictions. It is not as high as a pure snare because genuine coordination value exists: the magisterium does coordinate a single authoritative voice, and that voice is binding for Catholics globally. But extractiveness rises over the 60-year interval (0.48→0.62) as the magisterium's effort to enforce univocity increases in response to growing scholarly documentation of the factional origins. Suppression is high (0.71) because maintaining the univocal reading requires actively suppressing alternative readings through administrative constraint and archival control. Theater is substantial (0.58) and rising: the magisterium's interpretive efforts increasingly consist of performative assertions of univocity (papal pronouncements, conciliar hermeneutics documents) that cannot resolve the underlying contradictions because the contradictions are structural, not interpretive. The measurement series show rising theater and suppression with extractiveness leveling off after year 40, consistent with the constraint reaching a stable enforcement equilibrium where the contradictions are openly acknowledged in scholarship but administratively suppressed from institutional doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium's seat, the constraint is genuine coordination: a single authoritative voice interpreting a Council whose ambiguities are exegetical challenges, not substantive contradictions. From the traditionalist and liberal faction seats, the constraint is enforced extraction: the magisterium claims univocity it cannot achieve, suppresses the scholarly documentation of contradictions, and marginalizes dissenting readings while permitting compatible readings through the ambiguities. From the scholarship seat, the constraint is an opportunity structure: the undecidability of Vatican II is what makes the work possible. The engine will compute these differences from the structural data (power, exit, role); the authored claim remains independent of this computation.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium_authority_apparatus benefits from the constraint as structured: it maintains monopoly interpretive authority over Vatican II while avoiding the institutional crisis of acknowledging the contradictions. The beneficiary power atom is institutional and the exit is trapped (the magisterium cannot exit its role as authoritative teacher). From this seat, d approaches 0.0 (full beneficiary). Traditionalist and conservative factions pay through institutional marginalization and constrained dissent: they are powerless-to-moderate, their exit options are constrained, they bear real costs in the unresolved theological conflict. From their seats, d approaches 0.85-0.95 (target seats). Conciliar scholarship and liberal practitioners are beneficiaries but at moderate power and with mobile exit: d for these seats is 0.2-0.35 (partial beneficiary, exit available). The constraint is therefore asymmetrically extractive across the seat structure: the institutional beneficiary is protected by administrative constraint on the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to teach binding doctrine while opening the Church to modernity and ecumenism) is contested: the magisterium attests it remains live and requires univocal interpretation; scholars and theologians attest it is partially dead — the Church has opened to modernity and ecumenism regardless of whether univocal doctrine exists. The constraint persists by suppressing the acknowledgment that the founding problem may be intractable because it was never solvable in the form posed: genuine theological pluralism cannot be governed by univocal magisterial doctrine. The mandatrophy is that the constraint enforces a univocality that suppresses recognition of the very pluralism it generates. This is not a case where the founding mandate has been achieved and the constraint persists inertially; it is a case where the constraint's persistence depends on administratively preventing the mandatrophy from being publicly acknowledged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    factional_origin_reconstruction,
    'Are the contradictions in Vatican II documents genuine theological contradictions from factional compromise, or are they apparent contradictions resolvable through better hermeneutics?',
    'Archival evidence from Vatican II preparatory materials (acta synodalia) showing which bishops held which positions and how compromises were reached. Critical textual analysis comparing the theological rationales offered for apparently contradictory positions in different conciliar documents.',
    'If genuine contradictions with identifiable factional origins, the composite_overdetermination reading is vindicated: the ambiguities are not interpretive challenges but constitutive contradictions. If resolvable through hermeneutics, the continuity or rupture reading remains viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(factional_origin_reconstruction, empirical, 'Whether the unresolved tensions in Vatican II documents trace to theological contradiction or hermeneutic ambiguity.').

omega_variable(
    magisterial_suppression_intentionality,
    'Does the magisterium suppress scholarly recognition of Vatican II''s contradictions as a deliberate strategy to maintain authority, or does the magisterium genuinely not perceive the contradictions?',
    'Analysis of magisterial responses to scholarly work documenting the contradictions (whether the magisterium engages the scholarship substantively or dismisses it administratively). Interviews or archival evidence from magisterial decision-makers about their understanding of the Council''s internal coherence.',
    'Deliberate suppression establishes the constraint as extractive (magisterium knows but enforces univocity anyway). Genuine non-perception would suggest the constraint operates differently — the magisterium is not extracting but mistaken. If the constraint is not extractive, the extraction component of the classification would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_suppression_intentionality, empirical, 'Whether suppression of contradictions is strategic or cognitive.').

omega_variable(
    reading_incommensurability,
    'Is it possible for a single party (e.g., a Catholic bishop) to hold both the continuity_reading and the rupture_reading simultaneously, or are they logically incommensurable such that a party holding one must reject the other?',
    'Detailed study of individual bishops'' conciliar positions and their post-conciliar theological work to determine whether any bishop adopted both readings or whether adoption of one reading logically precludes the other.',
    'If incommensurable, the two sibling readings foreclose each other, not coexist_with this reading — the reading relations in cs_structure would need to be revised. If commensurate, they coexist, and this reading''s claim to neither-nor status is stronger (it offers a third framework acknowledging the incommensurability without resolving it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether the continuity and rupture readings are logically incommensurable or can be held by a single party.').

omega_variable(
    ambiguity_as_institutional_feature,
    'Is the institutional suppression of Vatican II''s ambiguities a pathology of the magisterium''s response, or a necessary feature of any institution claiming univocal authority over contested doctrine?',
    'Comparative analysis of how other religious and secular institutions handle foundational texts with embedded contradictions (e.g., Quranic interpretation in Islamic jurisprudence, constitutional interpretation in plural democracies). Theoretical work on the structural requirements of institutional authority under conditions of textual undecidability.',
    'If suppression is pathological, the constraint is unnecessarily extractive and could be reformed without dissolving the magisterium''s authority. If suppression is necessary, the extraction is structural to any univocal authority claim, and the reform would require accepting institutional pluralism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguity_as_institutional_feature, conceptual, 'Whether suppression of ambiguity is pathological or necessary to univocal institutional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(vati_tr_t0, projected).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 20, 0.53).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 30, 0.56).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 50, 0.59).
narrative_ontology:measurement_basis(vati_tr_t50, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(vati_be_t0, projected).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement_basis(vati_be_t50, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(vati_su_t0, projected).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(vati_su_t50, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__composite_overdetermination_reading, 0.18).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, catholic_ecumenical_dialogue_legitimacy).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, traditionalist_institutional_dissent).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, post_conciliar_pastoral_pluralism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel vatican_ii_authority. The composite_overdetermination_reading asserts that Vatican II encodes genuine theological contradictions from factional compromise, and that institutional suppression of this recognition is the mechanism by which the magisterium maintains univocal authority. The sibling readings (continuity_reading, rupture_reading) offer frameworks that resolve the ambiguities into coherence; this reading asserts no coherent resolution is possible within the magisterium's univocal-authority model. All three readings share the referent (Vatican II as a historical event and set of documents) but differ in what ε represents: for the continuity reading, ε is low because the coordination function is real and extraction minimal; for the rupture reading, ε is high because doctrinal error and confusion are extracted from the Church; for this reading, ε measures the magisterium's suppression of the ambiguities to maintain authority, independent of whether the ambiguities are resolvable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, institutional, 0.15).
constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
