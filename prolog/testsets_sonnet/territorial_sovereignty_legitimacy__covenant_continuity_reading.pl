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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty Legitimacy — Covenant/Continuity Reading
 *   domain: political theory / international relations / territorial sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the covenant-continuity reading of the
 *   territorial sovereignty legitimacy kernel: the claim that legitimate
 *   sovereignty over the territory derives from an ancient covenant (divine
 *   promise), reinforced by continuous Jewish presence across the intervening
 *   centuries, and ratified rather than originated by modern international
 *   acts (the Balfour Declaration, the UN Partition Plan, and the 1948
 *   establishment of the state). Under this reading, the 1947-48 partition is
 *   understood as a political compromise of a pre-existing right, not the
 *   creation of a new one, and post-1948 settlement is framed as return
 *   rather than colonization. This is a distinct constraint from the
 *   self-determination reading (which locates legitimacy in 19th-20th century
 *   Arab demographic majority and residence) and the existential-matrix
 *   reading (which denies that legitimacy is primarily juridical at all).
 *   Each reading carries a different ε and a different victim/beneficiary
 *   structure; they are linked here only through the shared kernel, not
 *   merged into one constraint.
 *
 * KEY AGENTS:
 *   - israeli_state_institutions: agenda_setter/beneficiary (institutional/arbitrage) — administers and enforces the legitimacy chain
 *   - jewish_settler_communities: beneficiary (organized/constrained) — occupies land under the return framing
 *   - religious_zionist_movements: beneficiary/agenda_setter (organized/identity_locked) — supplies and defends the theological-historical scholarship
 *   - palestinian_arab_residents: payer (powerless/trapped) — bears the subordination of present residence to ancient claim
 *   - palestinian_refugees_1948: payer (powerless/trapped) — displaced at the founding moment this reading treats as vindication
 *   - international_recognition_bodies: observer/agenda_setter (institutional/analytical) — issued the acts this reading cites as ratifying evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.58).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.62).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy — Covenant/Continuity Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political theory / international relations / territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '8b8f5514-e74e-4891-b7b1-3d99ccfbf859').
narrative_ontology:cs_kernel_codification('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', distributed).
narrative_ontology:cs_authority_grounding('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', lineage).
narrative_ontology:cs_interpretation_layer_present('8b8f5514-e74e-4891-b7b1-3d99ccfbf859').
narrative_ontology:cs_reading_relation('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', territorial_sovereignty_legitimacy__self_determination_reading, influences).
narrative_ontology:cs_reading_relation('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', foundational, covenantal_right_survives_demographic_absence).
narrative_ontology:cs_axiom_status(covenantal_right_survives_demographic_absence, holdable).
narrative_ontology:cs_axiom_grounding('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', covenantal_right_survives_demographic_absence, theological).
narrative_ontology:cs_axiom('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', foundational, modern_recognition_ratifies_preexisting_right).
narrative_ontology:cs_axiom_status(modern_recognition_ratifies_preexisting_right, holdable).
narrative_ontology:cs_axiom_grounding('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', modern_recognition_ratifies_preexisting_right, conventional).
narrative_ontology:cs_axiom('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', secondary, settlement_constitutes_return_not_acquisition).
narrative_ontology:cs_axiom_status(settlement_constitutes_return_not_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', settlement_constitutes_return_not_acquisition, empirically_contingent).
narrative_ontology:cs_reference_frame('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', covenantal_promise_with_continuous_presence).
narrative_ontology:cs_drift_state('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', post_1993_oslo_and_international_legal_scrutiny_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8b8f5514-e74e-4891-b7b1-3d99ccfbf859', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_settler_communities).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_movements).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, west_bank_arab_landholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, covenantal_promise_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, continuous_presence_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_sequence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers land registry, citizenship law, and military governance over the contested territory, drawing legitimacy claims from a chain running through biblical covenant, continuous presence, and the Balfour/Partition/1948 sequence. Sets policy on settlement expansion, land allocation, and residency status by invoking this legitimacy chain; enforces it through military and civil administration.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions, beneficiary).

% Live on land whose acquisition and continued occupation is framed as return to ancestral covenant territory rather than new settlement. Receive state subsidy, security infrastructure, and legal backing premised on the covenant-continuity legitimacy claim; would lose land tenure and state backing if the claim were rejected.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_settler_communities, beneficiary,
    organized, generational, constrained, regional).

% Provide the theological and historical scholarship that grounds the covenant claim; lobby for policies (settlement expansion, annexation) that depend on the claim's continued authority. Their institutional identity and political program are constituted by the covenant-continuity reading — abandoning it dissolves their reason for existing as a distinct political-religious movement.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_movements, beneficiary,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_movements, agenda_setter).

% Live under a legal and administrative regime that treats their residence as subordinate to a legitimacy chain reaching back before their own presence is dated. Face land confiscation, movement restriction, and unequal legal status justified by appeal to the covenant/continuity/recognition sequence; exit means displacement, not negotiated relief.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_residents, payer,
    powerless, biographical, trapped, regional).

% Displaced in the founding events the covenant-continuity reading treats as vindication of pre-existing right rather than as the point at which a new right was created at their expense. Denied return under a legal architecture that treats 1948 as legitimate culmination rather than contested origin; scattered across host states and camps with no direct standing in the legitimacy debate.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, global).

% Hold land title systems predating the modern state whose validity is subordinated, in contested zones, to settlement expansion justified by the covenant-continuity claim of return. Face administrative and judicial proceedings in which their tenure competes against a legitimacy framework that treats their title as circumstantial rather than foundational.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, west_bank_arab_landholders, payer,
    powerless, biographical, trapped, regional).

% Issued the Balfour Declaration, the UN Partition Plan, and subsequent recognitions that this reading treats as ratifying a pre-existing covenantal right rather than creating a new one. Continue to adjudicate recognition, statehood questions, and boundary disputes; their historical acts are cited as load-bearing evidence within this reading but they do not themselves adjudicate between competing readings of the kernel.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies, agenda_setter).

% Draw identity, religious meaning, and political solidarity from the covenant-continuity narrative without bearing the territorial costs directly. Provide diplomatic, financial, and political support premised on the legitimacy chain; can disengage from the conflict's material consequences at any time.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, diaspora_jewish_communities, beneficiary,
    moderate, civilizational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying historical-legal-theological narrative that lets a dispersed and persecuted people coordinate return, statehood, and international recognition around a single coherent claim rather than a purely juridical or purely military one.
% TRANSFER_FUNCTION: Moves land, residency rights, and administrative authority from Arab residents (present-day and displaced) to the Israeli state and Jewish settler communities, justified by treating covenant and continuous presence as prior and controlling over intervening demographic change.
% ABSENT_VOICES: Palestinian Arab residents, 1948 refugees, and West Bank landholders would contest the framing of partition as compromise of a pre-existing right rather than creation of a new one at their expense; they are largely outside the institutional bodies that adjudicate recognition and are represented, if at all, through advocacy organizations and international forums with limited enforcement power.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity legitimacy claim were to disappear as an operative justification, Israeli state institutions would need to rebuild legitimacy purely on security, self-determination, or acquired-fact grounds; religious Zionist movements would lose their organizing rationale for settlement expansion. Whether the material facts on the ground (state existence, settlement populations) would actually change is disputed — proponents say sovereignty is already secured by other means; critics say the covenant claim is precisely what licenses continued expansion beyond the 1948/1967 lines.
% FOUNDING_PROBLEM: Providing a legitimacy basis for Jewish return and sovereignty that could survive nearly two millennia of demographic absence from the territory, combining religious-historical continuity with modern international legal recognition to answer the objection that a people without continuous physical presence and majority population has no valid claim.
% FOUNDING_PROBLEM_CORROBORATION: Israeli state institutions and religious Zionist scholarship attest the founding problem remains live — that covenant and continuity are necessary because purely modern self-determination arguments cannot ground return after millennia of absence. Independent historians, international law scholars, and Palestinian advocacy organizations attest the covenantal claim functions primarily to override the self-determination claims of the resident population; UN bodies and international legal scholarship outside both benefiting parties treat 1948 and subsequent territorial acquisition as contested rather than settled, corroborating that the founding problem's 'solution' remains an active point of international dispute rather than a closed historical question.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because the reading's practical operation increasingly licenses land transfer and administrative subordination beyond what recognition acts alone would require — the covenant claim is invoked to justify actions (settlement expansion, annexation debates) that a purely juridical recognition-based claim would not straightforwardly cover. Suppression is authored higher (0.62) and rising sharply after 1948 and 1967, reflecting the military and administrative enforcement apparatus required to maintain the legitimacy chain's practical consequences against contestation by resident and displaced populations. Theater ratio is low-moderate (0.28) — the coordination function (providing a durable identity-and-return narrative for a historically dispersed people) is genuinely operative, not primarily performative, though its performative share has grown as the claim increasingly does argumentative work beyond its original scope. Accessibility collapse is moderate (0.45): the claim has not fully foreclosed rival legitimacy arguments even within Israeli discourse (secular Zionist and security-based arguments coexist), so alternatives to the covenant framing persist even among beneficiaries. Resistance is high (0.78), reflecting sustained contestation by Palestinian communities, international legal bodies, and historians who reject the return framing.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Israeli state institutions) and the beneficiary seats (settler communities, religious Zionist movements), this reading functions as coordination: it stabilizes identity, licenses return, and organizes diplomatic and religious energy around a coherent claim. From the payer seats (Palestinian residents, refugees, landholders) the same structure operates as enforced subordination of their own presence and title to a claim whose temporal depth is precisely what makes it unfalsifiable and unanswerable within the same evidentiary terms. The engine should compute these as structurally different experiences of the identical constraint, not as disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions and religious Zionist movements sit near the full-beneficiary end: they administer the claim, collect political and territorial benefit from it, and face minimal exit cost since their power lets them shape the claim's application. Settler communities benefit materially (land, subsidy, security backing) but their exit is more constrained than the state's, since their tenure is entirely contingent on the claim holding. Palestinian residents, refugees, and landholders sit near the full-target end: they bear land loss, displacement, or subordinated title, and their exit options are trapped — displacement is not a chosen exit but an imposed one. International recognition bodies are structurally analytical/observer with institutional power, but their historical acts are cited as load-bearing evidence within the reading, which is why a secondary agenda_setter role is assigned even though they do not currently adjudicate between competing readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (grounding return and sovereignty legitimacy against nearly two millennia of demographic absence) is genuinely contested rather than simply dead or simply live — this prevents mislabeling the constraint as either pure obsolete extraction (ignoring that a live and sincerely held identity/coordination function persists in diaspora and religious communities) or pure legitimate coordination (ignoring that the same claim now licenses territorial actions well beyond what the historical recognition acts themselves specified). The tangled_rope classification reflects exactly this: a real coordination function (identity, return, diaspora solidarity) coexisting with asymmetric extraction (land and residency transfer from a population the claim treats as secondary to a prior right) requiring active enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_claim_survives_demographic_absence,
    'Can a legitimacy claim grounded in ancient covenant meaningfully survive nearly two millennia of demographic absence and intervening continuous residence by another population, or does extended absence itself dissolve the claim regardless of its historical validity?',
    'No empirical resolution mechanism exists — this is a conceptual/normative question about what conditions defeat or preserve a legitimacy claim across time, dependent on contested theories of historical entitlement versus occupancy-based right.',
    'If demographic absence is held to defeat or substantially weaken the claim, this reading''s legitimacy chain collapses to the recognition-acts component alone, converging structurally toward the self-determination reading''s evidentiary basis; if absence does not defeat the claim, the covenant-continuity reading retains independent force beyond what modern recognition alone would supply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_claim_survives_demographic_absence, conceptual, 'Whether temporal continuity of a covenantal claim can survive extended demographic absence.').

omega_variable(
    partition_as_compromise_versus_creation,
    'Does the 1947 UN Partition Plan and 1948 establishment ratify a pre-existing right (as this reading holds) or does it constitute the creation of a new right through an act of the international community, independent of any prior covenantal claim?',
    'Legal-historical analysis of the Partition Plan''s own drafting record and the legal theory underlying UN General Assembly resolutions on statehood — whether such resolutions are understood in international law as recognitive or constitutive acts.',
    'If constitutive (creation of a new right), the covenant-continuity reading''s claim that partition merely compromised a pre-existing right is undermined, and the reading''s legitimacy basis narrows to the political fact of successful state establishment; if recognitive, the reading''s core structural claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_as_compromise_versus_creation, conceptual, 'Whether the 1948 international acts recognized or created the sovereignty claim.').

omega_variable(
    settlement_return_versus_colonization_framing,
    'Is post-1948 (and especially post-1967) settlement activity in contested territories accurately characterized as return to ancestral covenant land, or does it structurally match colonization regardless of the historical-religious narrative attached to it?',
    'Comparative analysis against established international legal criteria for colonization/occupation (population transfer, administrative subordination of existing residents, land seizure mechanisms) applied independent of the narrative framing used by any party.',
    'If the settlement pattern matches colonization criteria structurally, the ''return'' framing functions as a legitimacy narrative overlaying an extractive practice, supporting the tangled_rope classification and the authored victim declarations; if it does not match, the extraction component of this constraint would need to be reassessed downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_return_versus_colonization_framing, empirical, 'Whether settlement activity structurally resembles return or colonization independent of framing.').

omega_variable(
    fsm_natural_versus_constructed_legitimacy,
    'Is the covenant-continuity legitimacy claim best understood as a sincerely held historical-religious truth claim that happens to have identifiable institutional beneficiaries, or is it substantially a constructed legitimating narrative whose primary function is to benefit those same institutions — i.e., is this constraint closer to a Mountain (irreducible historical-religious fact) misclassified as tangled_rope, or is the tangled_rope reading correct?',
    'This constraint is NOT authored as a mountain (claimed_type is tangled_rope), so FSM does not directly apply here; however, the underlying ambiguity about whether covenant claims constitute natural/irreducible fact versus constructed narrative is the same ambiguity that would trigger FSM if a sibling story claimed mountain status for the covenant premise alone. Resolution would require independent historiographic and theological analysis of covenant doctrine''s development and use across different periods of Jewish history.',
    'If a future story separately claims the covenant premise itself as a Mountain-type historical/theological fact, that story would need to declare beneficiaries and carry this same omega under FSM rules; this story''s tangled_rope classification already treats the operative claim as constructed-with-real-coordination-function rather than pure natural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fsm_natural_versus_constructed_legitimacy, conceptual, 'Whether covenant legitimacy is natural/irreducible historical fact or a constructed narrative with identifiable beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.24).
narrative_ontology:measurement(terr_tr_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.32).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.52).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(terr_be_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.48).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(terr_su_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_sovereignty_legitimacy kernel. covenant_continuity_reading (this story) grounds legitimacy in ancient covenant plus continuity plus modern recognition, extending temporal scope to the biblical period and framing partition as compromise of pre-existing right. self_determination_reading grounds legitimacy in 19th-20th century Arab demographic majority and residence — a structurally distinct and much narrower temporal claim with a different victim/beneficiary inversion. existential_matrix_reading denies juridical primacy altogether, framing the conflict as zero-sum existential competition independent of legal-historical argument. Each has its own ε, beneficiary/victim structure, and classification; they are linked here via affects_constraints rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
