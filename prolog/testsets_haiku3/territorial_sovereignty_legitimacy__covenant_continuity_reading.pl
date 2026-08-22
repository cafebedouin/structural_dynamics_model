% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty Legitimacy: Covenant-Continuity Reading
 *   domain: political/legal/theological
 *
 * SUMMARY:
 *   This constraint instantiates the covenant-continuity reading of
 *   territorial sovereignty legitimacy — a framework that treats Jewish
 *   historical claim as grounded in ancient divine covenant, surviving
 *   through diaspora via non-residential continuity (textual, legal,
 *   theological), and validated by modern international instruments (Balfour
 *   Declaration, UN Partition Plan). The reading extends legitimacy's
 *   temporal scope backward to the biblical period and treats modern
 *   partition as recognition rather than creation. This is one of three
 *   readings of a contested kernel: two siblings are the self-determination
 *   reading (modern principle applied to Arab majority) and the
 *   existential-matrix reading (survival-based zero-sum framing). This story
 *   generates only the covenant-continuity reading as a clean, ε-invariant
 *   constraint. The other readings are separate stories. The kernel contest
 *   itself — whether these readings coexist as live alternatives or foreclose
 *   each other — is routed to omega variables (Rule 2), not embedded in this
 *   story's structure.
 *
 * KEY AGENTS:
 *   - Jewish diaspora return advocates (identity-locked organized agents): ground legitimacy in covenant; treat return as restoration; maintain theological/legal tradition; benefit from covenant-continuity framing
 *   - Israeli state apparatus (institutional agenda-setter): administers territorial control; enforces boundaries; codifies covenant-continuity reading into law and policy; arbitrages between justification frames
 *   - Palestinian residents 1948 onwards (powerless trapped agents): bear displacement and governance subordination under this constraint's logic; territorial claim is subordinated by temporal-scope choice that privileges ancient covenant over modern residence
 *   - Palestinian Authority (moderate constrained agent): attempts governance under constraints; operates within territory whose legitimacy this constraint defines; sovereignty bounded by the prior-rights framing
 *   - International law community (institutional observer): interprets Partition Plan differently depending on reading adopted; can validate or contest this constraint's historical claims; internally divided
 *   - Diaspora theological authorities (organized identity-locked agents): provide covenant grounding; maintain interpretive lineage; benefit from reading that treats non-residential continuity as legitimate
 *   - Regional Arab states (excluded organized agents): contest the reading; assert self-determination priority; structurally marginalized by framework that treats partition as compromise on pre-existing right
 *   - Critical historians and scholars (excluded analytical agents): produce scholarship questioning historical/theological coherence; marginal to authority-setting despite methodological rigor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.71).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy: Covenant-Continuity Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political/legal/theological").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '23220de4-5c28-4639-8af9-bc142aed6530').
narrative_ontology:cs_kernel_codification('23220de4-5c28-4639-8af9-bc142aed6530', fixed_text).
narrative_ontology:cs_authority_grounding('23220de4-5c28-4639-8af9-bc142aed6530', lineage).
narrative_ontology:cs_interpretation_layer_present('23220de4-5c28-4639-8af9-bc142aed6530').
narrative_ontology:cs_reading_relation('23220de4-5c28-4639-8af9-bc142aed6530', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('23220de4-5c28-4639-8af9-bc142aed6530', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('23220de4-5c28-4639-8af9-bc142aed6530', foundational, ancient_covenant_confers_persistent_right).
narrative_ontology:cs_axiom_status(ancient_covenant_confers_persistent_right, holdable).
narrative_ontology:cs_axiom_grounding('23220de4-5c28-4639-8af9-bc142aed6530', ancient_covenant_confers_persistent_right, theological).
narrative_ontology:cs_axiom('23220de4-5c28-4639-8af9-bc142aed6530', foundational, legitimacy_survives_demographic_absence).
narrative_ontology:cs_axiom_status(legitimacy_survives_demographic_absence, holdable).
narrative_ontology:cs_axiom_grounding('23220de4-5c28-4639-8af9-bc142aed6530', legitimacy_survives_demographic_absence, deontological).
narrative_ontology:cs_reference_frame('23220de4-5c28-4639-8af9-bc142aed6530', jewish_diaspora_restoration_framework).
narrative_ontology:cs_drift_state('23220de4-5c28-4639-8af9-bc142aed6530', contemporary_contested_authorization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23220de4-5c28-4639-8af9-bc142aed6530', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora_return_advocates).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_residents_1948_onwards).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_authority_governance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_theological_authorities).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, divine_covenant_legitimacy).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, return_as_restoration_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, historical_continuity_despite_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for territorial restoration based on divine covenant and historical continuity. Frame the Balfour Declaration and UN Partition Plan as international recognition of pre-existing right, not creation of new right. Identity fused with territorial return narrative; exit from this framing would require abandoning core narrative of historical legitimacy and restoration destiny.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora_return_advocates, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora_return_advocates, agenda_setter).

% Administers and enforces territorial control; claims legitimacy from covenant-continuity reading combined with international recognition. Controls military enforcement, legal codification, settlement policy, and historical narrative production. Can arbitrage between different justification frames (covenant, international law, existential security) depending on audience.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Bear territorial displacement, governance subordination, and access restrictions justified under this constraint's logic. Their continuous residence during the relevant modern period (19th-20th centuries) is subordinated to the covenant-continuity reading's longer temporal scope. Exit means displacement; remaining means accepting governance by the agenda_setter under rules justified by a historical claim they dispute.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_residents_1948_onwards, payer,
    powerless, biographical, trapped, local).

% Attempts to govern Palestinian territory but operates under constraints imposed by Israeli state apparatus, which exercises enforcement authority over the territorial domain. Governance capacity is bounded; sovereignty claims are contested and subordinated under the covenant-continuity reading's framework that assigns prior rights to the Jewish diaspora return.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_authority_governance, payer,
    moderate, generational, constrained, regional).

% Interprets UN Partition Plan (1947) and subsequent international law; negotiates between the covenant-continuity reading (which treats Partition as recognition of pre-existing right) and self-determination reading (which treats Partition as creation of new states with equal standing). Produces analysis and mediation that can validate or contest this constraint's framing.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_law_community, observer,
    institutional, generational, analytical, global).

% Provide theological and historical grounding for the covenant narrative. Affirm the reading that divine promise and historical continuity survive demographic absence and diaspora, legitimating return as restoration. Identity and institutional continuity depend on maintaining this interpretive framework.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_theological_authorities, beneficiary,
    organized, civilizational, identity_locked, global).

% Would strongly contest the covenant-continuity reading and assert self-determination priority. Excluded from the authoritative interpretation-setting process during the 1947-1948 period and structurally marginalized by the reading's framing that treats partition as a compromise on pre-existing right rather than equal creation of two new states. Their voice enters only as resistance, not as co-interpreter.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, regional_arab_states, excluded,
    organized, generational, constrained, regional).

% Produce scholarship questioning the historical and theological claims underlying the covenant-continuity reading (demographic discontinuity, archaeological interpretation contests, documentary dating disputes). Excluded from institutional authority-setting in this constraint's framing; their analysis remains marginal to the dominant narrative even when methodologically rigorous.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, critical_historians_and_scholars, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a singular legitimate territorial authority based on a combined covenant-historical-international framework, providing stable jurisdiction, law, and governance structure. Solves the problem of competing claims to the same territory by treating one lineage (Jewish diaspora return) as having superior historical precedence that international instruments (Balfour, Partition) recognize rather than create.
% TRANSFER_FUNCTION: Moves territorial control, governance authority, and resource allocation rights from Palestinian residents and the Palestinian Authority to the Israeli state apparatus, justified by a historical claim based on ancient covenant and continuous (though non-residential) Jewish connection to the territory. The transfer is framed as restoration of pre-existing right rather than new allocation.
% ABSENT_VOICES: Palestinian residents' own narrative of continuous presence during the modern territorial period; Arab states' objections to the framework that subordinates Arab self-determination to Jewish covenant-historical claims; critical historians who question the historical coherence of the covenant-continuity narrative; alternative theological readings (Christian, Muslim, secular) of the same territorial domain.
% DISAPPEARANCE_RATIONALE: If this constraint's framing of legitimacy (covenant + continuity + international recognition) vanished, the territorial arrangement would immediately face alternative legitimacy claims (self-determination of Palestinian residents, existential security of both populations) with no clear authority to adjudicate between them. The entire governance structure and its normative grounding would require renegotiation.
% FOUNDING_PROBLEM: Competing claims to the same small territory by two populations: one diaspora population claiming historical continuity through covenant and religious/legal tradition, one resident population claiming modern self-determination. How to establish legitimate sovereign authority over territory claimed by both?
% FOUNDING_PROBLEM_CORROBORATION: Jewish diaspora advocates and Israeli state institutions affirm the founding problem and assert the covenant-continuity reading solves it. Palestinian authority and Arab states dispute both the problem framing (treating them as less-rooted claims) and the solution (treating covenant-historical claims as inferior to modern self-determination). International law scholars are internally divided on whether Partition created two equal new entities or recognized pre-existing Jewish rights. No consensus corroboration exists outside the benefiting parties; the problem itself is framed differently by different stakeholders.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint moves territorial control, governance authority, and resource allocation from Palestinian residents to Israeli state apparatus, justified by a historical claim that invokes a 2500-year-old covenant. The claim is coherent within its own frame but depends on treating ancient covenant as conferring legitimacy that survives demographic absence — a premise the self-determination and existential-matrix readings reject. Suppression is high (0.71) because the constraint's persistence requires active enforcement: military control of territory, legal subordination of Palestinian governance, exclusion of rival legitimacy claims from authoritative interpretation-setting. Theater ratio is moderate (0.42) because the covenant-historical narrative is genuine — it is maintained through education, institutional practice, and theological scholarship — but an increasing share of enforcement activity appears to consist of containment and boundary maintenance rather than active assertion of covenant legitimacy. The measurements show a gradual rise in extractiveness from 0.52 to 0.68 over the interval, indicating that the extraction dimension has grown (increasing settlement expansion, increasing resource disparity, increasing governance subordination) while the coordination dimension has remained constant or declined. The theater ratio also rises, suggesting that the narrative-maintenance component has become more salient relative to functional governance as the founding problem (establishing legitimate authority) has receded and the arrangement has shifted toward rent collection and control.
 *
 * PERSPECTIVAL GAP:
 *   The covenant-continuity reading appears to the Israeli state apparatus and diaspora advocates as a genuine coordination mechanism solving the problem of competing territorial claims through historical precedence and international recognition — they perceive it as the discovery of a true legitimacy hierarchy. From the Palestinian residents and Palestinian Authority seats, the same constraint appears as enforced territorial extraction justified by a contested historical claim that privileges a diaspora population's ancient connection over their own modern continuous residence. The engine computes this perspectival gap from the structural data: the Israeli institutional seat has high directionality toward beneficiary (d near 0.1–0.2, arbitrage exit, organized power); the Palestinian resident seat has high directionality toward target (d near 0.9, trapped exit, powerless). The claimed type (tangled_rope) reflects the covenant-continuity reading's internal logic: it provides genuine coordination (unified authority over contested territory) AND asymmetric extraction (Palestinian displacement and governance subordination). An observer at the Israeli institutional seat would compute rope (coordination with acceptable costs); an observer at the Palestinian powerless seat would compute snare (extraction justified by contested historical claim).
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state apparatus: d ≈ 0.15 (near beneficiary end). Agenda-setter role; institutional power; arbitrage exit (can arbitrage between justification frames depending on audience); collects the extraction (territorial control, governance authority, resource allocation). Diaspora advocates: d ≈ 0.25 (beneficiary side). Organized power; identity-locked exit (core identity fused with territorial return narrative); benefit from the reading without running it; derive validation for diaspora restoration. Palestinian residents: d ≈ 0.88 (near target end). Powerless; trapped exit (displacement means leaving; remaining means accepting governance under rules justified by contested claim); bear the extraction (territorial loss, governance subordination, resource constraints). Palestinian Authority: d ≈ 0.75 (target side). Moderate power; constrained exit (governance capacity bounded; exit means abandoning claim to Palestinian statehood); operates under constraints this constraint imposes; governance authority limited by prior-rights framing. International law community: d ≈ 0.5 (observer, analytical exit, no power atom or extraction relationship). Arab states: d ≈ 0.7 (near target side in the sense that the reading's temporal-scope choice subordinates Arab self-determination claims; excluded so no direct extraction relationship but structurally marginalized). The directionality overrides are not needed here; the derivation from beneficiary/victim declarations and exit options produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The covenant-continuity reading claims to solve the founding problem (establishing legitimate authority over contested territory) via historical precedence and international recognition. The founding_problem_status is contested: diaspora advocates affirm the problem is solved (legitimate authority established through covenant-historical framework); Palestinian authority and Arab states dispute both the problem framing and the solution. The disappearance_verdict is world_rearranges: if the covenant-continuity legitimacy framing vanished, the territorial arrangement would immediately face alternative claims (self-determination, existential security) with no clear arbiter. The constraint does NOT exhibit classic mandatrophy (where the founding problem is dead but the arrangement persists through inertia). Instead, it exhibits what might be called contested mandatrophy: the founding problem remains live for some parties (diaspora advocates affirm it; settlement expansion indicates ongoing assertion of the legitimacy claim) but appears dead for others (Palestinian residents and Arab states see the founding problem as having been solved in the opposite direction — Palestinian self-determination is the legitimate answer, not Jewish return). The theater_ratio rising from 0.28 to 0.42 suggests that narrative maintenance (covenant affirmation, historical education, theological scholarship) is becoming more salient relative to functional governance, which could indicate emerging mandate fatigue — the arrangement persists not because it is solving the founding problem (which all parties affirm remains contested) but because the Israeli institutional seat has the power to maintain it. However, this does not meet the classic piton criterion (beneficiaries would abandon it if they could); the Israeli state and diaspora advocates clearly benefit and show no sign of abandoning the framing. The constraint is best modeled as tangled_rope with contested founding problem: genuine coordination (unified authority; stable law) AND asymmetric extraction (displacement, governance subordination) AND active enforcement AND contestation about whether the founding problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_historical_continuity_claim,
    'Is the claim of historical continuity from ancient covenant through diaspora to modern return structurally coherent, or does demographic absence (two millennia) constitute a break that resets the legitimacy baseline?',
    'This is fundamentally a question of how legitimacy survives rupture — whether a claim can persist through a period of zero enforcement and zero residence, or whether legitimacy must be continuously performed and embodied. Critical historical and theological scholarship produces contradictory assessments; no empirical fact settles the structural question because it depends on which continuity model is adopted a priori.',
    'If continuity can survive diaspora, the covenant-continuity reading''s temporal scope is valid and ancient claims supersede modern self-determination. If continuity requires continuous practice/residence, the modern temporal scope becomes primary and self-determination reading gains structural authority. This omega is conceptual, not empirical: it concerns what kind of claim can persist through rupture, not whether empirical facts obtain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_historical_continuity_claim, conceptual, 'Whether legitimacy derived from ancient covenant survives complete demographic absence.').

omega_variable(
    partition_interpretation_delta,
    'Did the 1947 UN Partition Plan create two new sovereign entities with equal standing, or recognize pre-existing rights with one party (Jewish diaspora) having superior historical claim and the plan serving as international validation of that prior right?',
    'The text of UNSC Resolution 181 and preparatory documents; historical testimony from the drafting parties; subsequent international law jurisprudence on how partition is treated in comparable territorial disputes. Different parties read the same documents as supporting opposite framings, and the doctrinal dispute is live among international law scholars.',
    'The covenant-continuity reading treats Partition as recognizing pre-existing right; the self-determination reading treats it as creating two equal new entities. If Partition is read as recognition, settlements on Palestinian land can be framed as return; if Partition is read as creation of equal states, settlements are framed as colonization. The empirical fact of what the text says exists; the structural interpretation of what kind of act Partition constitutes depends on which reading''s framework is adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_interpretation_delta, empirical, 'Interpretive status of UN Partition Plan: recognition of pre-existing right vs. creation of new equal entities.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the resistance by Palestinian residents and Arab states primarily suppressed by external force (military, legal coercion, resource control), or have interpretive frameworks and historical narratives become internalized such that alternative legitimacy claims are treated as structurally incoherent rather than merely contested?',
    'Post-suppression trajectory analysis: if suppression is purely structural, removing enforcement would immediately surface alternative legitimacy claims with equal force. If internalization has occurred, alternative claims remain marginal even after enforcement relaxation. Comparative analysis of how thoroughly the covenant-continuity reading is embedded in education, institutional practice, and daily narrative across different populations.',
    'If suppression is primarily structural, the measured suppression value (0.71) understates effective suppression because it does not capture internalized frame-adoption. If suppression has become partially internalized, the constraint''s persistence does not require the current enforcement intensity — it has been partially naturalized. This affects whether the constraint is a snare (pure structural extraction with external suppression) or a tangled rope (extraction that has become partially self-sustaining through narrative internalization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Degree to which suppression is structural vs. internalized through narrative adoption.').

omega_variable(
    kernel_reading_contest_field,
    'This constraint is one reading (covenant-continuity) of a contested kernel (territorial_sovereignty_legitimacy). The sibling readings (self_determination_reading, existential_matrix_reading) offer structurally distinct framings of legitimacy. Are these readings contesting alternative policies within a shared framework, or do they represent incompatible frameworks for how legitimacy itself is grounded?',
    'Examine whether parties holding different readings can recognize each other''s premises as coherent within their own logic, or whether each reading treats the others as incoherent (category mistakes, logical fallacies, false framings). If mutual recognition exists, readings coexist; if each dismisses the others as incoherent, they foreclose each other.',
    'This is the field condition for whether the three sibling readings are best modeled as coexisting (different legitimate framings of the same kernel) or foreclosing (one reading''s core premise eliminates the structural possibility of the others). The impact is on how the narrative_ontology system models the constraint family: as a constellation of live alternatives or as a zero-sum fight over framework authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_field, conceptual, 'Whether kernel readings are coexisting alternatives or mutually foreclosing frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t10, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(terr_tr_t10, observed).
narrative_ontology:measurement(terr_tr_t20, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(terr_tr_t20, observed).
narrative_ontology:measurement(terr_tr_t30, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(terr_tr_t30, observed).
narrative_ontology:measurement(terr_tr_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(terr_tr_t50, observed).
narrative_ontology:measurement(terr_tr_t75, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(terr_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t10, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(terr_be_t10, observed).
narrative_ontology:measurement(terr_be_t20, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(terr_be_t20, observed).
narrative_ontology:measurement(terr_be_t30, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(terr_be_t30, observed).
narrative_ontology:measurement(terr_be_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement_basis(terr_be_t50, observed).
narrative_ontology:measurement(terr_be_t75, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(terr_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t10, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(terr_su_t10, observed).
narrative_ontology:measurement(terr_su_t20, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(terr_su_t20, observed).
narrative_ontology:measurement(terr_su_t30, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement_basis(terr_su_t30, observed).
narrative_ontology:measurement(terr_su_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(terr_su_t50, observed).
narrative_ontology:measurement(terr_su_t75, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 75, 0.71).
narrative_ontology:measurement_basis(terr_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel territorial_sovereignty_legitimacy. The covenant-continuity reading treats legitimacy as grounded in ancient divine covenant, continuous (non-residential) historical presence, and modern international recognition (Balfour, Partition). Sibling readings are self_determination_reading (modern principle applied to Arab majority) and existential_matrix_reading (zero-sum survival framing). All three are separate constraint stories sharing the same kernel ID. The constraint family is linked via network.affects_constraints to show the kernel relationship. Each reading is ε-invariant, generates its own stakeholder surface, and instantiates its own cs_structure with reading_relations and axioms. The kernel contest itself — whether these readings coexist, influence, or foreclose each other — is modeled through cs_structure.reading_relations and routed to omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
