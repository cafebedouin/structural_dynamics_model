% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via UN Partition and Recognized Statehood (1948/1967 Lines)
 *   domain: political/legal/territorial sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the partition/international-legal reading of
 *   territorial legitimacy in the Israeli-Palestinian conflict: legitimacy
 *   derives from the UN partition process (Resolution 181, 1947) and the
 *   subsequent armistice and 1967 lines, under which both an Israeli state
 *   and a prospective Palestinian state hold legally cognizable claims within
 *   recognized borders, while territorial acquisition and settlement beyond
 *   the 1967 lines is treated as internationally illegitimate. This reading
 *   structurally supports a two-state framework as the coherent resolution of
 *   the underlying claim conflict. It is authored as one of three sibling
 *   readings of the territorial_legitimacy kernel — the
 *   indigenous_continuity_reading (which treats 1948 itself as an
 *   illegitimate colonial dispossession, the Nakba, rather than a legitimate
 *   partition) and the security_necessity_reading (which grounds legitimacy
 *   in defensible strategic control extending past 1967 lines). This
 *   reading's ε is assessed on its own terms: the standing
 *   partition/1967-lines legal architecture as it actually operates, not the
 *   two-state outcome it envisions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.55).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via UN Partition and Recognized Statehood (1948/1967 Lines)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/legal/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, 'e8434f2b-5b8f-493a-81dc-6fc21e139a86').
narrative_ontology:cs_kernel_codification('e8434f2b-5b8f-493a-81dc-6fc21e139a86', formalized).
narrative_ontology:cs_authority_grounding('e8434f2b-5b8f-493a-81dc-6fc21e139a86', lineage).
narrative_ontology:cs_interpretation_layer_present('e8434f2b-5b8f-493a-81dc-6fc21e139a86').
narrative_ontology:cs_reading_relation('e8434f2b-5b8f-493a-81dc-6fc21e139a86', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8434f2b-5b8f-493a-81dc-6fc21e139a86', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_axiom('e8434f2b-5b8f-493a-81dc-6fc21e139a86', foundational, international_partition_instrument_confers_legitimate_title).
narrative_ontology:cs_axiom_status(international_partition_instrument_confers_legitimate_title, holdable).
narrative_ontology:cs_axiom_grounding('e8434f2b-5b8f-493a-81dc-6fc21e139a86', international_partition_instrument_confers_legitimate_title, conventional).
narrative_ontology:cs_axiom('e8434f2b-5b8f-493a-81dc-6fc21e139a86', foundational, post_1967_territorial_acquisition_is_illegitimate_absent_negotiated_agreement).
narrative_ontology:cs_axiom_status(post_1967_territorial_acquisition_is_illegitimate_absent_negotiated_agreement, holdable).
narrative_ontology:cs_axiom_grounding('e8434f2b-5b8f-493a-81dc-6fc21e139a86', post_1967_territorial_acquisition_is_illegitimate_absent_negotiated_agreement, conventional).
narrative_ontology:cs_reference_frame('e8434f2b-5b8f-493a-81dc-6fc21e139a86', un_resolution_181_partition_framework).
narrative_ontology:cs_drift_state('e8434f2b-5b8f-493a-81dc-6fc21e139a86', post_oslo_stalemate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e8434f2b-5b8f-493a-81dc-6fc21e139a86', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state_within_1967_lines).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_order_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, prospective_palestinian_state).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, settlement_residents_beyond_1967_lines).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinians_under_occupation_beyond_1967_lines).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees_of_1948).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, prospective_palestinian_state).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, un_partition_resolution_legal_authority).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, two_state_framework_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds recognized statehood and UN membership grounded in the partition/armistice legal framework; enjoys diplomatic recognition, treaty capacity, and access to international institutions predicated on internationally acknowledged borders. Benefits from the legitimacy the partition framework confers, while facing pressure from settlement expansion that undermines the same framework's coherence.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state_within_1967_lines, beneficiary,
    institutional, generational, arbitrage, national).

% Its claim to statehood within 1967 lines derives legal standing from the same partition/UN-resolution framework, giving it a recognized legal path to sovereignty that indigenous-continuity or security-necessity readings do not equally offer. But its territory is fragmented and diminished by ongoing settlement activity the framework declares illegitimate yet cannot on its own halt.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, prospective_palestinian_state, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, prospective_palestinian_state, payer).

% The UN, ICJ, and treaty-based diplomatic architecture administer and periodically reaffirm the partition/1967-lines framework as the reference standard for legitimate statehood and territorial title. They issue resolutions, advisory opinions, and recognition votes that operationalize the framework, but possess no independent enforcement capacity to compel compliance.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_order_institutions, agenda_setter,
    institutional, civilizational, analytical, global).

% Live in communities the partition/1967-lines framework designates illegitimate under international law, despite domestic legal and political support. Face permanent legal precarity, potential future dismantlement or annexation-dependent futures, and international delegitimization of their claims to the land they inhabit.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, settlement_residents_beyond_1967_lines, payer,
    organized, biographical, constrained, regional).

% Live under military occupation and settlement expansion in territory the partition/1967-lines framework assigns to a prospective Palestinian state that has not materialized. Bear daily costs of movement restriction, land confiscation, and administrative uncertainty while the legal framework that names their claim as valid provides no enforcement mechanism to realize it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinians_under_occupation_beyond_1967_lines, payer,
    powerless, biographical, trapped, regional).

% Displaced in and after 1948, their claims (return, compensation, property restitution) exist mostly outside the partition/1967-lines framework's operative scope, since that framework is oriented toward two-state territorial division rather than the 1948 displacement itself. The framework offers them little direct legal traction.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees_of_1948, payer,
    powerless, generational, trapped, regional).

% Shape enforcement, aid flows, and diplomatic pressure around the framework from outside the formal partition instrument, using recognition and sanctions leverage to advance their own regional interests; not bound by the framework themselves but able to determine whether it is enforced.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, regional_and_great_power_states, excluded,
    powerful, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, internationally legible reference standard for what counts as legitimate sovereign territory in the conflict — a common legal grammar (partition lines, subsequent armistice/1967 lines) that other states, courts, and institutions can use to coordinate recognition, treaty relations, and dispute resolution without each having to independently adjudicate competing historical claims.
% TRANSFER_FUNCTION: Confers diplomatic recognition, institutional standing, and legal legitimacy on statehood claims that track the 1948 partition and 1967 lines, while withholding equivalent legitimacy from settlement expansion beyond those lines and from claims (Palestinian refugee return, unilateral annexation) that fall outside the framework's territorial logic. Legitimacy flows toward the two recognized statehood projects; legal disadvantage flows toward parties whose claims or presence sit outside the mapped lines.
% ABSENT_VOICES: Palestinian refugees of 1948, whose claims center on return and restitution rather than territorial partition, are largely unaddressed by a framework oriented around future state borders rather than backward-looking displacement remedies; their objection — that partition legitimacy was itself imposed without their consent — is structurally outside this reading's frame, not absent by oversight.
% DISAPPEARANCE_RATIONALE: If the partition/UN-resolution legal framework were withdrawn as a reference standard overnight, diplomatic recognition of Israeli statehood and any future Palestinian statehood would lose their principal internationally legible legal grounding; states and institutions would have to reconstruct legitimacy claims from alternative bases (security necessity, indigenous continuity, raw territorial control), materially altering recognition patterns, aid conditionality, and the legal status of settlements and occupied territory.
% FOUNDING_PROBLEM: In 1947-48, competing Jewish and Arab claims to Mandatory Palestine required an internationally sanctioned mechanism to allocate sovereignty and avert or manage armed conflict; UN Resolution 181 was built to provide a legal partition formula that great powers and the new UN system could endorse as a resolution mechanism.
% FOUNDING_PROBLEM_CORROBORATION: UN institutions and international law scholars outside both national projects continue to cite Resolution 181 and subsequent armistice/1967 lines as the operative legal reference in ICJ opinions and Security Council resolutions, treating the founding problem (need for an internationally legitimate partition formula) as still structurally live. Israeli settlement advocates and Palestinian indigenous-continuity advocates, by contrast, each argue from outside the benefiting recognition-seeking parties that the framework's founding premise was already overtaken by subsequent conquest, war, or the illegitimacy of imposed partition itself — corroboration for 'dead' or 'contested' comes from historians and legal scholars critical of the 1947 partition process, not from either state party's own institutions.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.55, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects a hybrid coordination/extraction structure: the framework genuinely coordinates international recognition and provides a workable legal reference point (real coordination function), but it also produces disproportionate costs for populations whose claims sit outside its territorial logic — settlement residents whose communities it delegitimizes, occupied Palestinians whose statehood claim it recognizes in principle but cannot realize in practice, and 1948 refugees whose displacement claims it does not address at all. Suppression (0.62) is moderate-high because enforcement depends on diplomatic and legal pressure (sanctions, non-recognition, ICJ opinions) rather than direct coercive capacity — the framework's persistence relies on states continuing to treat it as the reference standard despite decades of non-implementation. Theater ratio (0.45) has risen over time as the framework increasingly serves as a rhetorical and diplomatic touchstone (resolutions, statements, conferences) without corresponding implementation — reaffirmation has partly substituted for enforcement, a genuine Goodhart-style drift visible in the measurement series.
 *
 * DIRECTIONALITY LOGIC:
 *   The recognized Israeli state and prospective Palestinian state are the framework's principal beneficiaries: both derive legal standing and a path to international legitimacy from the same partition/1967-lines architecture, even though only one has fully realized statehood. International legal institutions administer the framework without directly extracting from it. The clearest victims are populations whose position sits outside the framework's clean territorial logic: settlement residents (whom the framework delegitimizes but cannot itself remove), occupied Palestinians (whose statehood claim the framework recognizes but cannot enforce, leaving them to bear the practical costs of occupation), and 1948 refugees (whose displacement claims the framework was never built to address). This asymmetry — real coordination benefit for two state projects, real unaddressed cost for three population groups — is why the story is authored as tangled_rope rather than a clean rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for an internationally sanctioned mechanism to allocate contested sovereignty in 1947-48) remains cited as live by international institutions, yet the framework has not produced the two-state outcome it structurally implies for over seven decades, while settlement expansion has proceeded largely unchecked by the framework's own delegitimization of it. This is the classic mandatrophy signature: a legal architecture whose original coordination purpose (peaceful territorial allocation) is increasingly performed rhetorically (repeated UN resolutions, diplomatic statements) rather than substantively (no enforced settlement freeze, no realized Palestinian sovereignty), while continuing to confer real recognition benefits on the party (Israel) that has achieved the statehood the framework was designed to allocate to both parties. The rising theater_ratio and stagnant real-world implementation are exactly the drift this classification exists to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_instrument_legitimacy_ambiguity,
    'Was UN Resolution 181 itself a legitimate exercise of international authority to partition Mandatory Palestine, or an imposed settlement made without the consent of the majority population then present, such that everything built on it inherits a legitimacy defect?',
    'No empirical resolution exists; this is a foundational normative disagreement between the partition_reading and the indigenous_continuity_reading about whether an international body can legitimately allocate sovereign territory absent the consent of existing inhabitants. Historical and legal scholarship on decolonization-era self-determination norms bears on it but does not settle it.',
    'If the partition instrument itself is held illegitimate, this reading''s entire legal architecture — and the legitimacy it confers on the state project built from it — is undermined at the root, which is precisely the indigenous_continuity_reading''s core objection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_instrument_legitimacy_ambiguity, conceptual, 'Whether Resolution 181''s partition authority was legitimately exercised, contested between partition and indigenous-continuity readings.').

omega_variable(
    enforcement_capacity_gap,
    'Is the framework''s persistent non-implementation (no realized two-state outcome after seven decades) evidence that the coordination function has substantially atrophied into theater, or evidence that implementation is merely delayed by contingent political obstacles while the underlying legal architecture remains sound?',
    'Track whether renewed diplomatic initiatives (e.g., future peace processes) produce implementation movement correlated with the framework''s legal terms, versus continued rhetorical reaffirmation without corresponding territorial or political change.',
    'If atrophy, the tangled_rope classification should trend toward piton as coordination function fades further into pure legitimacy theater; if contingent delay, the coordination function remains substantively live and the tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'Whether non-implementation reflects structural atrophy (piton drift) or contingent political delay.').

omega_variable(
    sibling_reading_framing_selection,
    'Is the partition/1967-lines framework the natural default legal reading, or does treating it as ''the'' international-law baseline already favor the two recognized-state parties over the indigenous-continuity and security-necessity framings, which read the same historical record differently?',
    'Compare how international courts, UN bodies, and legal scholarship outside any single national tradition characterize the relative authority of the partition instrument versus subsequent self-determination and security doctrines when they conflict.',
    'If the partition framing is itself a contestable choice among framings rather than a neutral legal baseline, then declaring it the reference standard privileges the two-state coordination story analytically over rival readings before any facts are examined — a conceptual under-determination this story routes here rather than resolving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framing_selection, conceptual, 'Whether treating the partition framework as the default legal baseline already favors this reading over its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__partition_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy__partition_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__partition_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__partition_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy__partition_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__partition_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__partition_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy__partition_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__partition_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__partition_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the territorial_legitimacy kernel. partition_reading (this story) authors ε=0.55 as tangled_rope: genuine two-state coordination function, but asymmetric costs on settlement residents, occupied Palestinians, and 1948 refugees. security_necessity_reading authors a structurally distinct ε and beneficiary/victim set grounded in strategic depth past 1967 lines. indigenous_continuity_reading authors yet another ε and victim set grounded in treating 1948 itself as the Nakba rather than legitimate partition. All three share the same underlying territorial dispute but are ε-invariant separate constraints per the decomposition principle — none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
