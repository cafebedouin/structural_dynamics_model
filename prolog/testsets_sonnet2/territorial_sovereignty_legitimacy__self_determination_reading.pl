% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Sovereignty Legitimacy via Modern Self-Determination of the Demographic-Majority Arab Population
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   territorial-sovereignty-legitimacy kernel: the self-determination
 *   reading, under which legitimate sovereignty over the territory derives
 *   from the modern (19th-20th century) principle of national
 *   self-determination applied to the population holding demographic majority
 *   and continuous residence — identified in this reading as the Arab
 *   population. The reading treats the 1947 Partition Plan and the 1948
 *   establishment of Israel as an imposition by external powers (Britain, the
 *   UN) that overrode this self-determination claim rather than fulfilling
 *   it, and treats the right of return as restoration of a pre-existing
 *   status quo rather than a novel remedy. This is a clean, ε-invariant
 *   instantiation of one reading only; the covenant-continuity reading and
 *   the existential-matrix reading are separate constraints with their own ε,
 *   their own beneficiary/victim structure, and their own type, linked here
 *   only via network edges and cs_structure.reading_relations — this file
 *   does not average across them or hedge its ε.
 *
 * KEY AGENTS:
 *   - palestinian_arab_population: primary claimed beneficiary of the reading's legitimacy logic — bears the practical costs of the unresolved contest
 *   - arab_national_movements: agenda_setter articulating and pressing the self-determination framing diplomatically and politically
 *   - palestinian_refugees_and_descendants: payer — direct human cost of the seven-decade unresolved status, powerless, trapped exit
 *   - jewish_residents_of_mandate_palestine: payer under this reading — their own continuous presence during the same period is treated as subordinate by the demographic-majority test
 *   - israeli_state_and_citizenry: payer — the reading frames the existing sovereign state's founding as illegitimate colonial imposition
 *   - external_mandate_and_partition_powers: excluded — historical actors whose decisions are the object of the reading's contest, absent from the present dispute
 *   - international_legal_and_human_rights_bodies: observer — apply and elaborate the self-determination doctrine the reading rests on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.72).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Sovereignty Legitimacy via Modern Self-Determination of the Demographic-Majority Arab Population").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '868cda11-3df0-4c8a-995e-c90dada17f45').
narrative_ontology:cs_kernel_codification('868cda11-3df0-4c8a-995e-c90dada17f45', distributed).
narrative_ontology:cs_authority_grounding('868cda11-3df0-4c8a-995e-c90dada17f45', distributed).
narrative_ontology:cs_reading_relation('868cda11-3df0-4c8a-995e-c90dada17f45', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('868cda11-3df0-4c8a-995e-c90dada17f45', territorial_sovereignty_legitimacy__existential_matrix_reading, influences).
narrative_ontology:cs_axiom('868cda11-3df0-4c8a-995e-c90dada17f45', foundational, demographic_continuity_grounds_sovereign_title).
narrative_ontology:cs_axiom_status(demographic_continuity_grounds_sovereign_title, holdable).
narrative_ontology:cs_axiom_grounding('868cda11-3df0-4c8a-995e-c90dada17f45', demographic_continuity_grounds_sovereign_title, conventional).
narrative_ontology:cs_axiom('868cda11-3df0-4c8a-995e-c90dada17f45', foundational, external_imposition_voids_partition_legitimacy).
narrative_ontology:cs_axiom_status(external_imposition_voids_partition_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('868cda11-3df0-4c8a-995e-c90dada17f45', external_imposition_voids_partition_legitimacy, deontological).
narrative_ontology:cs_axiom('868cda11-3df0-4c8a-995e-c90dada17f45', secondary, modern_period_is_sole_relevant_temporal_frame).
narrative_ontology:cs_axiom_status(modern_period_is_sole_relevant_temporal_frame, holdable).
narrative_ontology:cs_axiom_grounding('868cda11-3df0-4c8a-995e-c90dada17f45', modern_period_is_sole_relevant_temporal_frame, conventional).
narrative_ontology:cs_reference_frame('868cda11-3df0-4c8a-995e-c90dada17f45', post_ottoman_self_determination_baseline).
narrative_ontology:cs_drift_state('868cda11-3df0-4c8a-995e-c90dada17f45', post_oslo_contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('868cda11-3df0-4c8a-995e-c90dada17f45', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_population).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_national_movements).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, post_colonial_self_determination_doctrine_advocates).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_and_descendants).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, jewish_residents_of_mandate_palestine).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state_and_citizenry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_population).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, self_determination_as_sovereignty_ground).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, demographic_continuity_as_legitimacy_test).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, anti_colonial_territorial_restitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held demographic majority and continuous residence in Mandate Palestine through the 19th-20th centuries under this reading's evidentiary frame. The reading grounds their claim to sovereignty in that continuity and majority status rather than in prior legal title, religious claim, or international grant. They bear the ongoing costs of statelessness, occupation, and displacement that the reading identifies as the consequence of partition being imposed against their demonstrated self-determination claim.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_population, beneficiary,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_population, payer).

% Articulate and press the self-determination claim in diplomatic, legal, and armed contexts (Arab Higher Committee, PLO, Arab League). They administer the political framing that continuous demographic presence during the modern period is the operative legitimacy test, and press for its enforcement through UN resolutions, boycotts, and negotiation postures.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_national_movements, agenda_setter,
    organized, generational, constrained, regional).

% Displaced in 1948 and 1967 and their descendants, most without citizenship in any state, living in camps or diaspora. Under this reading, the right of return is a restoration of status quo ante rather than a novel humanitarian ask — but the underlying sovereignty dispute has left this population's claim unresolved for over seven decades, and they bear the concrete cost of the unresolved contest regardless of which reading of the kernel prevails.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_and_descendants, payer,
    powerless, generational, trapped, regional).

% Present in the territory in smaller but growing numbers during the same modern period, including communities with residence predating and continuous through the period. This reading's demographic-majority test treats their claim as subordinate to the Arab majority claim; from their position, the same continuous-residence logic that grounds Arab sovereignty is applied selectively to exclude their own century-plus presence and pre-modern historical connection.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jewish_residents_of_mandate_palestine, payer,
    moderate, generational, trapped, regional).

% The existing sovereign state whose founding this reading frames as a colonial project imposed by external powers (Balfour, UN Partition) against the self-determination rights of the demographic majority. Under this reading, the state's legitimacy is structurally contested at the root; the state bears diplomatic isolation, contested legal status in international fora, and an unresolved refugee-return claim as direct consequences of the reading's application.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state_and_citizenry, payer,
    institutional, generational, constrained, national).

% Britain (Mandate authority) and the UN General Assembly (1947 Partition Plan) made the determinations this reading identifies as the unjust imposition overriding the demographic majority's self-determination claim. They are largely absent from the present-day contest but their historical decisions are the object the reading treats as illegitimate.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, external_mandate_and_partition_powers, excluded,
    institutional, biographical, analytical, global).

% UN bodies, ICJ proceedings, and human rights organizations that assess claims under self-determination doctrine, occupation law, and refugee law. They apply and elaborate the self-determination principle this reading rests on, without being party to the underlying territorial contest.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_legal_and_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a juridical-normative framework — the post-1945/post-colonial self-determination principle applied via demographic majority and continuous residence — for adjudicating which population's national claim should ground sovereign title over a contested territory, in place of ad hoc power politics or purely historical-religious claims.
% TRANSFER_FUNCTION: Moves normative and diplomatic legitimacy (recognition, standing in international fora, moral weight in negotiations, and the basis for the right-of-return claim) toward the population identified as the continuous demographic majority, and away from claims grounded in covenant, prior sovereignty, or post-1948 established-state facts.
% ABSENT_VOICES: The external mandate and partition powers whose decisions this reading contests are analytically named but not present as live parties. Sephardi and Mizrahi Jewish populations displaced from Arab states after 1948 are not addressed by this reading's demographic-continuity test at all, since their claims run through different territories. Multiple Palestinian factions with divergent views on negotiated versus restorative outcomes are compressed into a single population seat here.
% DISAPPEARANCE_RATIONALE: If this specific reading of the legitimacy kernel vanished — if self-determination-via-demographic-continuity ceased to be an available or persuasive legitimacy argument — the parties dispute what would follow. Advocates of the reading hold that removing it would strip the Palestinian claim of its principal juridical anchor, reshaping negotiating postures, UN voting patterns, and the right-of-return demand. Critics of the reading hold that the underlying territorial and demographic facts, and the competing claims, would persist regardless of which legitimacy vocabulary is used to describe them — the conflict is not created by the argument, only articulated through it.
% FOUNDING_PROBLEM: The problem of adjudicating rival claims to sovereignty over Mandate Palestine after the collapse of Ottoman rule, when Britain's Mandate ended without an agreed successor arrangement and the UN Partition Plan was rejected by Arab states and Palestinian Arab leadership as an imposition rather than a negotiated self-determination outcome.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars outside the immediate parties (writing on self-determination doctrine post-Mandate systems, e.g. in the context of decolonization jurisprudence) and UN human rights bodies attest that the underlying sovereignty and refugee-status questions remain formally unresolved in international law. Israeli governmental and academic sources dispute that the self-determination reading's demographic-majority test is the correct legal frame at all, treating the 1947 Partition Plan and 1948 recognition as the operative legitimating acts instead — so corroboration of the founding problem's continued liveness is available from outside the Palestinian/Arab beneficiary set, but corroboration of THIS READING's specific framing is largely confined to advocates of the reading itself.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 for the standing arrangement under contest — the currently existing state of Israel and its territorial control — assessed by this reading's own lights: the reading holds that sovereignty was extracted from the rightful self-determining population via externally imposed partition, and that the ongoing occupation and refugee non-return represent continued extraction from that population. Suppression is high (0.72) because, under this reading, the demographic-majority claim's political and legal expression has been actively constrained — through the 1948 displacement, subsequent occupation, and diplomatic non-recognition dynamics — rather than simply losing a fair contest of legitimacy claims. Theater ratio is moderate (0.3): the coordination function (a genuine international legal principle — self-determination — applied to a genuine historical population) is real, but a meaningful share of its invocation in diplomatic fora is also performative positioning rather than substantive legal adjudication. Accessibility collapse is moderate (0.5), not high, because alternative legitimacy framings (covenant-continuity, existential-matrix, negotiated two-state frameworks) remain live and contested rather than fully foreclosed — this is precisely why the kernel has three surviving readings rather than one settled answer. Resistance is very high (0.85) because the reading is fiercely contested by the Israeli state, substantial international constituencies, and by the covenant-continuity and existential-matrix readings themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab population and refugee descendants sit closest to the target/payer end of directionality: under this reading they are owed sovereignty and have borne the cost of not receiving it, but structurally they are also named beneficiaries of the LEGITIMACY ARGUMENT itself even while paying the material cost of the unresolved dispute — hence the dual role. Arab national movements are the agenda-setters who press the claim in international and diplomatic venues. Jewish residents of Mandate Palestine and the present Israeli state and citizenry are payers under this reading specifically because the reading's demographic-majority test treats their century-plus presence and existing sovereignty as subordinate or illegitimate — this is a direct structural cost imposed by the reading's own logic, not an incidental side effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adjudicating rival sovereignty claims after the collapse of Ottoman and then British Mandate authority — is authored as status=live because the underlying territorial and refugee-status questions remain formally unresolved in international law and in practice on the ground; this is corroborated by international legal scholarship and UN bodies outside the Palestinian/Arab beneficiary set, even though the SPECIFIC self-determination-and-demographic-continuity framing is contested by others (Israeli sources) who agree the problem is live but reject this reading's proposed test for resolving it. This prevents the mandatrophy mislabeling in both directions: the reading is not dismissed as a dead or purely rhetorical claim (founding_problem_status is not 'dead'), but its self-serving potential is also not laundered as settled fact — the corroboration field explicitly flags that only the underlying liveness, not the specific framing, is attested from outside the reading's own advocates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_scope_selection_as_framing_choice,
    'Is the modern-period (19th-20th century) temporal bound the natural scope for legitimacy adjudication, or is bounding the analysis to this period itself a substantive choice that pre-selects this reading''s outcome by excluding pre-modern historical claims?',
    'Comparative analysis of international legal doctrine''s own temporal scoping conventions (e.g., uti possidetis, critical-date doctrine in territorial disputes) against how this reading applies the bound; examine whether the reading''s choice of start-date (Ottoman-era demographic surveys vs. earlier periods) is independently justified or selected to produce a particular demographic outcome.',
    'If the temporal bound is itself contestable rather than doctrinally compelled, the reading''s demographic-majority conclusion is less an application of neutral self-determination principle and more a framing choice that determines its own result — this would not change this reading''s authored ε, but would sharpen the omega documenting why sibling readings reject the bound entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_scope_selection_as_framing_choice, conceptual, 'Whether the modern-period temporal scope is a neutral application of doctrine or a selection effect.').

omega_variable(
    self_determination_doctrine_generality_vs_selective_application,
    'Does the self-determination principle as applied in this reading generalize consistently to the Jewish population''s own continuous presence during the same modern period, or does the reading apply demographic-continuity logic asymmetrically?',
    'Apply the reading''s own stated test (demographic majority + continuous residence in the modern period) symmetrically to both populations'' presence data and compare the reading''s stated criteria against its practical application.',
    'If the criteria are applied asymmetrically, this reading''s claim to derive from a general, principled doctrine (rather than a particularized political claim dressed in general doctrinal language) weakens — relevant to how the reading''s axioms should be read as holdable versus contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_doctrine_generality_vs_selective_application, conceptual, 'Whether the demographic-continuity test is applied as a general principle or selectively.').

omega_variable(
    colonial_characterization_contestability,
    'Is the characterization of the Israeli state''s founding as a ''colonial project'' an accurate structural description (external power settling a population to extract resources/control without the settled population''s own self-determination claim) or a contested rhetorical framing that elides the Jewish population''s own indigenous historical connection and refugee-driven migration patterns?',
    'Comparative historical analysis against settler-colonialism criteria (metropole relationship, extraction structure, settler population''s own claimed indigeneity) versus alternative national-liberation-movement historiography of Zionism.',
    'This directly affects whether the extraction ε=0.68 authored here is best characterized as colonial extraction (supporting tangled_rope/snare framing under this reading) or whether the underlying historical claim is itself the locus of the kernel-level contest — which is precisely why this is one reading among three rather than a settled fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colonial_characterization_contestability, conceptual, 'Whether the colonial characterization is a settled historical description or a contested framing central to the kernel dispute.').

omega_variable(
    right_of_return_restoration_vs_novel_remedy,
    'Is the right of return correctly characterized as ''restoration of status quo ante'' (implying a clean historical baseline to which return would return things) or is any return necessarily a novel remedy given seven-plus decades of demographic, political, and physical transformation of the territory?',
    'Examine whether comparable post-conflict restitution frameworks (post-WWII population transfers, post-colonial repatriation cases) treat multi-generational displacement as restorable to a prior status quo or as requiring novel remedial design.',
    'If return cannot be a true restoration given the scale of transformation, the reading''s framing of the remedy as merely restorative (rather than as a new political settlement requiring fresh negotiation) may overstate the simplicity of implementing this reading''s implied resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_restoration_vs_novel_remedy, conceptual, 'Whether the right-of-return framing as ''restoration'' understates the novelty of any actual remedy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(terr_tr_t2005, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(terr_be_t2005, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement(terr_su_t2005, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_sovereignty_legitimacy kernel: covenant_continuity_reading (legitimacy from ancient covenant + continuous Jewish presence + modern international recognition), existential_matrix_reading (legitimacy is existential/zero-sum, not juridical), and this self_determination_reading (legitimacy from modern self-determination applied to demographic-majority Arab population). Each reading authors its own ε against the same standing arrangement (the existing state of Israel and its territorial control) assessed by that reading's own lights — the three ε values differ substantially and are NOT to be averaged or reconciled; the divergence itself is the structural fact the kernel-decomposition records. All three link to each other via affects_constraints as members of one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
