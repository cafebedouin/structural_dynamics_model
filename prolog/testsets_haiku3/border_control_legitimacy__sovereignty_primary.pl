% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: State Territorial Sovereignty and Border Control Authority (Sovereignty-Primary Reading)
 *   domain: political_philosophy/international_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty-primary' reading of
 *   the border-control-legitimacy kernel: the claim that territorial state
 *   sovereignty logically entails absolute discretion to exclude
 *   non-citizens, and that border control is constitutive of statehood
 *   itself. The reading is authored here as a mountain—a natural and
 *   irreducible structural feature of political community—but the measurement
 *   data show rising extraction and suppression over the interval, and the
 *   authored beneficiaries (state apparatus, protected domestic workers)
 *   indicate the arrangement has beneficiaries. This is a FALSE-SUMMIT
 *   candidate: the reading claims naturalness (emerges_naturally: true) but
 *   declares identifiable beneficiaries who capture extraction from would-be
 *   migrants. The omega variables document the contestation: whether
 *   sovereignty-as-border-closure is genuinely natural or constructed
 *   institutional ideology; whether the international legal regime's human
 *   rights provisions are external constraints on plenary authority or
 *   constitutive of what legitimacy means; and whether the committer
 *   interests (state systems maintaining mutual non-interference norms)
 *   sustain the reading against alternatives. The constraint sits at the
 *   intersection of international law's Westphalian settlement and its
 *   human-rights overlay—the overlap is where this reading's natural-law
 *   claim is most vulnerable.
 *
 * KEY AGENTS:
 *   - nation_state_apparatus (institutional, agenda-setter): claims sovereignty entails unqualified border discretion; administers enforcement
 *   - would_be_migrants_excluded (powerless, identity-locked, payer): bear exclusion costs; lack voice in the reading's justification frame
 *   - domestic_labor_market_protected_workers (organized, beneficiary): benefit indirectly from labor-market protection justified via border control
 *   - humanitarian_advocates (moderate, excluded): argue human rights constrain border authority; systematically absent from this reading's policy frame
 *   - international_legal_regime (institutional, observer): formally recognizes sovereignty while encoding human rights; structural site of the reading contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.68).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.79).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, mountain).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "State Territorial Sovereignty and Border Control Authority (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law").

domain_priors:emerges_naturally(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, 'ee7c8dac-02d2-4d60-88e6-f65eae7bccfb').
narrative_ontology:cs_kernel_codification('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', fixed_text).
narrative_ontology:cs_authority_grounding('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', lineage).
narrative_ontology:cs_interpretation_layer_present('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb').
narrative_ontology:cs_reading_relation('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', foundational, absolute_border_closure_discretion).
narrative_ontology:cs_axiom_status(absolute_border_closure_discretion, holdable).
narrative_ontology:cs_axiom_grounding('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', absolute_border_closure_discretion, deontological).
narrative_ontology:cs_axiom('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', foundational, border_control_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(border_control_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', border_control_constitutive_of_statehood, deontological).
narrative_ontology:cs_reference_frame('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', westphalian_sovereign_state_system).
narrative_ontology:cs_drift_state('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', contemporary_globalized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee7c8dac-02d2-4d60-88e6-f65eae7bccfb', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, nation_state_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, domestic_labor_market_protected_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, would_be_migrants_excluded).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_state_system).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, absolute_territorial_sovereignty).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, border_closure_as_sovereign_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers border enforcement, sets inclusion/exclusion criteria, controls entry authorization. Justifies border control as constitutive of statehood—the boundary between members and non-members that makes a state 'a state' rather than a universal governance space. Claims absolute discretion in determining who enters territory and on what terms. Authority derives from the reading that territorial sovereignty entails unqualified border closure power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, nation_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Benefit from restrictions on non-citizen labor supply, which the state apparatus justifies as protecting labor conditions and wage levels. Organized worker groups advocate for strict immigration controls and enforcement; border closure protects their labor monopoly within the territory. Their benefit is indirect—flows through state policy rather than through direct collection—but sustained by the same enforcement machinery.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, domestic_labor_market_protected_workers, beneficiary,
    organized, biographical, constrained, national).

% Bear the cost of exclusion: forgo economic opportunity, family reunification, and access to territory they may depend on for survival or development. Excluded from the decision-making frame entirely; their consent is never sought. Cannot exit this constraint because the constraint defines their ineligibility by birth/status. The reading frames their exclusion as a legitimate exercise of sovereignty rather than as a deprivation requiring justification.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, would_be_migrants_excluded, payer,
    powerless, biographical, identity_locked, global).

% Argue that human rights (freedom of movement, asylum rights, family unity) constrain legitimate state authority; are largely absent from policy-making in states that adopt the sovereignty-primary reading. Their position is that border control requires justification beyond 'we are a state,' but that voice is systematized out of the decision frame.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, humanitarian_advocates, excluded,
    moderate, biographical, mobile, global).

% Formally recognizes state territorial sovereignty while also encoding human rights and asylum obligations (1951 Refugee Convention, ICCPR, etc.). This reading treats those obligations as external constraints on otherwise plenary authority, not as constitutive of legitimate sovereignty. The tension between the two aspects of international law is the structural location of the reading contest.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_legal_regime, observer,
    institutional, civilizational, analytical, global).

% Theoretically have standing to criticize another state's border closure as illegitimate, but are locked in mutual recognition of sovereign discretion. Each state benefits from the reading's legitimacy because each claims the same absolute discretion for itself. Their exclusion is mutual—they cannot coherently challenge the reading without undermining their own claimed authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, rival_state_systems, excluded,
    institutional, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, nation_state_apparatus).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes the membership boundary that distinguishes the state as a distinct political and legal community. Creates a defined population subject to shared governance, taxation, and law, enabling administration and collective action. Solves the problem of 'who counts as us' by treating territorial presence as the primary criterion.
% TRANSFER_FUNCTION: Moves economic opportunity (access to labor markets, public goods, legal protection) from non-citizens to citizens and to the state apparatus itself (through enforcement expenditure justified as defending sovereignty). Transfers authority to define membership unilaterally from a universal rights frame to a state-discretionary frame.
% ABSENT_VOICES: Would-be migrants and movement-rights advocates are structurally excluded. Their objection would be that the constraint takes 'the right to exclude' as foundational when it should be derivative from human rights or universal principles—a voice that the sovereignty-primary reading systematically silences by treating their exclusion as sovereignty's very content.
% DISAPPEARANCE_RATIONALE: If border control discretion vanished, the Westphalian state system as currently understood would require fundamental restructuring. The territorial boundary and its control are treated (by this reading) as constitutive of statehood itself; without border authority, the state would need to redefine its legitimacy around something else—jurisdiction over rights/law within territory, rather than absolute control of entry.
% FOUNDING_PROBLEM: The emergence of bounded territorial states from feudal, imperial, and religious authority: how to establish distinct political communities with recognized boundaries that other powers respect. Border control became the mechanism for marking state identity and enforcing mutual non-interference (Westphalian settlement, 1648).
% FOUNDING_PROBLEM_CORROBORATION: State systems theory and international relations scholarship recognize the historical founding: treaties, state formation, and mutual recognition all depend on border control authority. However, human rights scholarship and migration-rights advocates contest whether that historical founding problem still justifies absolute exclusion authority in a globalized world where movement constraints cause human harm. The state apparatus and nationalism-aligned scholars affirm the problem is live; rights-based and cosmopolitan frameworks attest the problem is outdated and the constraint persists as ideology justifying extraction, not as solution to a current need.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, ExtMetricName, E),
    domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(border_control_legitimacy__sovereignty_primary),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.68 at interval end) reflects that border control operates to exclude a class of people (would-be migrants) from resources and opportunity they would otherwise access, justified by a reading that treats their exclusion as constitutive of state identity rather than as deprivation requiring justification. The suppression measurement (0.79) is high because enforcing the exclusion requires active mechanisms (immigration enforcement, border patrol, biometric surveillance, legal exclusions) and because the constraint persists despite substantial resistance from movement-rights advocates and excluded migrants themselves. Theater ratio (0.42 at interval end) captures that enforcement rhetoric increasingly emphasizes security, sovereignty-defense, and state-identity narratives rather than addressing the actual coordination problem (establishing membership and governance scope). The rising extractiveness and suppression over the interval reflect intensification of border enforcement infrastructure and rhetoric—the constraint is being actively defended and elaborated, not passively maintained. The measurement grid is unified across all three metrics at every time point, allowing the engine to detect temporal relationships. The flatline at t=50 onwards suggests the constraint reached a stable enforcement level and has maintained it (not continuing to intensify).
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus seat, the constraint is sovereignty-defense—the condition of being a state at all. From the would-be-migrant seat (excluded), it is pure exclusion from livelihood. From the domestic-worker seat, it is protection of labor conditions (incidental to sovereignty but experienced as benefit). From the humanitarian-advocate seat, it is rights violation dressed in sovereignty language. The engine should compute different type classifications from each seat: the state apparatus may compute mountain-or-rope (depending on how much it attributes to coordination vs. extraction); the excluded migrant computes snare (pure extraction with suppression); the protected worker computes rope (coordination function with indirect benefit); the advocate computes snare-or-tangled-rope (extraction with false coordination claim). The perspectival divergence is structural, not evaluative—each seat sees the constraint from its position in the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The nation-state-apparatus is the agenda-setter and primary beneficiary (low directionality, d near 0.0)—it controls the constraint, collects the sovereignty 'rent' (the authority to exclude), and has high exit options (arbitrage: it can trade exclusivity against other state prerogatives). Domestic workers are incidental beneficiaries (d near 0.25-0.35): they benefit from labor protection but don't control the constraint and have constrained exit (exit from the labor market means economic hardship). Would-be migrants are the primary targets (d near 0.95-1.0): they bear pure exclusion costs, have no voice in the constraint's definition, and are identity-locked (cannot exit by becoming citizens of choice—the constraint defines them as ineligible). This directionality distribution explains why the constraint persists: the agenda-setter has the highest payoff and the lowest cost to exit (arbitrage), so it maintains enforcement; the targets are powerless and identity-locked, so they cannot exit; the incidental beneficiaries benefit but don't run the machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing distinct sovereign states with recognized territorial boundaries) was live in the Westphalian era and contributed to mutual recognition and the reduction of religious and imperial arbitrariness. By the contemporary era (this reading's contest) the problem is contested: some argue sovereignty-as-border-closure is still necessary (states claim security, tax collection, rule-of-law administration). Others argue the founding problem is dead—modern states have alternatives (identification/credentialing systems, targeted taxation, legal jurisdiction) that don't require absolute entry exclusion. The measurement trajectory (extractiveness rising from 0.45 to 0.68 while founding_problem_status is contested) suggests the constraint is persisting and intensifying not because the founding problem is live but because the state apparatus benefits from extracting via exclusion. The rising theater ratio (0.25 to 0.42) further suggests enforcement is increasingly performative—defending the claim to exclusion authority rather than solving a coordination problem. This is a mandatrophy case: the founding problem is disputed but the constraint persists and is elaborated via enforcement. The claim/metric gap is intentional here: claiming mountain (emerges_naturally) while authoring rising extractiveness and suppression creates a testable hypothesis for the engine—false-summit detection can evaluate whether the naturalness claim withstands the extraction data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_authority,
    'Is territorial sovereignty and border closure authority a natural or irreducible feature of political community (as the reading asserts), or is it a constructed institutional arrangement that could be otherwise?',
    'Historical and comparative study: do all human political forms require border closure, or do some stable political orders (city-states, empires, federal systems, open-membership communities) operate without absolute entry exclusion?',
    'If sovereignty-as-border-closure is constructed, the constraint is not a mountain but a snare or tangled rope using natural-law framing to justify extraction. The naturality claim collapses and remedial alternatives become available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, conceptual, 'Whether the constraint''s naturalness claim is defensible or constructed.').

omega_variable(
    reading_contest_boundary_location,
    'This reading claims the kernel (territorial sovereignty) entails absolute border closure discretion. The freedom-of-movement reading claims the same kernel entails a human right of movement. Where exactly does the contest lie—in the definition of sovereignty itself, or in what legitimacy requires beyond sovereignty?',
    'Genealogical analysis of the kernel''s historical codification (Westphalian treaties, state theory, international law evolution) to identify whether border closure was always treated as part of sovereignty or entered as a modern addition.',
    'If border closure was always part of the sovereignty concept, the readings truly foreclose each other. If border closure is a later historical addition, the readings coexist and the contest is about whether legitimacy requires more than sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_boundary_location, conceptual, 'Whether the competing readings foreclose or coexist within the kernel.').

omega_variable(
    extraction_beneficiary_ambiguity,
    'Who actually benefits from border control—the state apparatus (institutional survival), domestic workers (labor protection), or a broader public (security/community)? Are the identified beneficiaries capturing extraction or are they incidental recipients of coordination benefits?',
    'Cost-benefit analysis: if border closure were removed, what would actually change for each beneficiary group? Would domestic workers'' conditions deteriorate, or would labor markets adjust without systematic harm?',
    'If beneficiaries are incidental rather than capturing, the constraint may be less extractive than authored. If beneficiaries actively organize enforcement to maintain extraction, the constraint is more snare-like than mountain-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_ambiguity, empirical, 'Whether the constraint''s beneficiaries are active capturers or passive recipients.').

omega_variable(
    human_rights_as_external_vs_constitutive,
    'Does this reading treat human rights constraints (freedom of movement, asylum, family unity) as external limits on otherwise plenary state authority, or are human rights constitutive of what legitimate sovereignty means?',
    'Textual and jurisprudential analysis: do international instruments (ICCPR, Refugee Convention, regional human rights treaties) present rights as constraints on sovereignty or as definitions of legitimate sovereignty?',
    'If rights are constitutive, the reading''s claim to absolute discretion fails—the constraint is not a mountain but something constrained. If rights are genuinely external, the reading''s framing holds and the constraint may be justified as sovereignty-defense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_rights_as_external_vs_constitutive, conceptual, 'Whether human rights limit or constitute legitimate sovereignty in this reading.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.79) structural—legal barriers, enforcement infrastructure, surveillance systems—or internalized—migrants internalize the belief they have no right to move, excluders internalize the belief in absolute state authority?',
    'Post-enforcement trajectory analysis: where enforcement infrastructure is removed (open-border experiments, asylum policy changes), does suppression persist (internalized) or dissolve (structural)?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure—the target carries exclusion internalized into their self-concept after exit. If structural, removal of enforcement mechanisms would dissolve the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether border-exclusion suppression is structural or internalized.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the ''sovereignty-primary'' reading of the border-control-legitimacy kernel. What are the committer interests that sustain this particular reading over its siblings (freedom-of-movement-primary, jurisdictional-sovereignty)?',
    'Institutional genealogy: which institutional actors (state apparatuses, nationalist movements, labor unions, international state system) benefit from this reading being the default frame? Would they actively resist the alternative readings?',
    'If committer interests sustain the reading, it may be sustained not by truth but by power—the reading is ideological cover for institutional extraction. If multiple independent actors converge on the reading, it may have genuine structural merit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, preference, 'Committer structural interest in the sovereignty-primary reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t12, border_control_legitimacy__sovereignty_primary, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(bord_tr_t12, observed).
narrative_ontology:measurement(bord_tr_t25, border_control_legitimacy__sovereignty_primary, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t37, border_control_legitimacy__sovereignty_primary, theater_ratio, 37, 0.38).
narrative_ontology:measurement_basis(bord_tr_t37, observed).
narrative_ontology:measurement(bord_tr_t50, border_control_legitimacy__sovereignty_primary, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(bord_tr_t50, observed).
narrative_ontology:measurement(bord_tr_t62, border_control_legitimacy__sovereignty_primary, theater_ratio, 62, 0.42).
narrative_ontology:measurement_basis(bord_tr_t62, observed).
narrative_ontology:measurement(bord_tr_t75, border_control_legitimacy__sovereignty_primary, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(bord_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t12, border_control_legitimacy__sovereignty_primary, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(bord_be_t12, observed).
narrative_ontology:measurement(bord_be_t25, border_control_legitimacy__sovereignty_primary, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t37, border_control_legitimacy__sovereignty_primary, base_extractiveness, 37, 0.65).
narrative_ontology:measurement_basis(bord_be_t37, observed).
narrative_ontology:measurement(bord_be_t50, border_control_legitimacy__sovereignty_primary, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(bord_be_t50, observed).
narrative_ontology:measurement(bord_be_t62, border_control_legitimacy__sovereignty_primary, base_extractiveness, 62, 0.68).
narrative_ontology:measurement_basis(bord_be_t62, observed).
narrative_ontology:measurement(bord_be_t75, border_control_legitimacy__sovereignty_primary, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(bord_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t12, border_control_legitimacy__sovereignty_primary, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(bord_su_t12, observed).
narrative_ontology:measurement(bord_su_t25, border_control_legitimacy__sovereignty_primary, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t37, border_control_legitimacy__sovereignty_primary, suppression_requirement, 37, 0.75).
narrative_ontology:measurement_basis(bord_su_t37, observed).
narrative_ontology:measurement(bord_su_t50, border_control_legitimacy__sovereignty_primary, suppression_requirement, 50, 0.79).
narrative_ontology:measurement_basis(bord_su_t50, observed).
narrative_ontology:measurement(bord_su_t62, border_control_legitimacy__sovereignty_primary, suppression_requirement, 62, 0.79).
narrative_ontology:measurement_basis(bord_su_t62, observed).
narrative_ontology:measurement(bord_su_t75, border_control_legitimacy__sovereignty_primary, suppression_requirement, 75, 0.79).
narrative_ontology:measurement_basis(bord_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% Border control legitimacy decomposes into three structurally distinct constraint stories corresponding to three readings of the contested kernel 'border_control_legitimacy'. The sovereignty-primary reading (this story) treats border closure as constitutive of statehood and justifies exclusion via sovereignty defense. The freedom-of-movement reading treats freedom of movement as a fundamental right that sovereignty cannot override. The jurisdictional-sovereignty reading treats sovereignty as internal authority and leaves border closure discretion contestable. These three readings are not observational variations on one constraint—they have different ε values (this reading: 0.68; freedom-of-movement-primary: ~0.75-0.82 due to rights-violation framing; jurisdictional-sovereignty: ~0.55-0.65 due to balancing-against-rights framing), different victim sets (this reading excludes migrants; others include rights-violating states as targets), and different legitimacy structures. The three are linked via network.affects_constraints to enable the engine to trace how challenging one reading creates structural pressure on its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__sovereignty_primary, powerless, 0.96).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
