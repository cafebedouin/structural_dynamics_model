% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination Right to Statehood in Ancestral Homeland (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the liberal nationalist reading of the
 *   Jewish sovereignty over Palestine kernel. It frames Jewish collective
 *   self-determination as a universalizable national right, legitimately
 *   exercised through statehood in the ancestral homeland. Crucially, this
 *   reading recognizes Palestinian self-determination as co-equal in
 *   principle, requiring territorial compromise (partition or binational
 *   framework) — distinguishing it from religious Zionist and settler
 *   colonial readings. The beneficiary is the Jewish collective as
 *   rights-bearing nation; extraction is moderate because the reading's own
 *   logic demands territorial compromise, yet the historical operation has
 *   produced asymmetric outcomes (ongoing occupation, settlement expansion,
 *   refugee denial) that exceed the reading's stated terms. The constraint is
 *   a tangled rope: it performs genuine coordination (the two-state
 *   diplomatic framework, Oslo architecture, international consensus) while
 *   simultaneously enabling asymmetric extraction (land, sovereignty,
 *   resources from Palestinians to Jewish-Israeli collective).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.48).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.68).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Self-Determination Right to Statehood in Ancestral Homeland (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, 'c10f28ed-1407-43b2-8c32-4cac266383a3').
narrative_ontology:cs_kernel_codification('c10f28ed-1407-43b2-8c32-4cac266383a3', formalized).
narrative_ontology:cs_authority_grounding('c10f28ed-1407-43b2-8c32-4cac266383a3', lineage).
narrative_ontology:cs_interpretation_layer_present('c10f28ed-1407-43b2-8c32-4cac266383a3').
narrative_ontology:cs_reading_relation('c10f28ed-1407-43b2-8c32-4cac266383a3', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('c10f28ed-1407-43b2-8c32-4cac266383a3', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c10f28ed-1407-43b2-8c32-4cac266383a3', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('c10f28ed-1407-43b2-8c32-4cac266383a3', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('c10f28ed-1407-43b2-8c32-4cac266383a3', foundational, national_self_determination_universal).
narrative_ontology:cs_axiom_status(national_self_determination_universal, holdable).
narrative_ontology:cs_axiom_grounding('c10f28ed-1407-43b2-8c32-4cac266383a3', national_self_determination_universal, deontological).
narrative_ontology:cs_axiom('c10f28ed-1407-43b2-8c32-4cac266383a3', foundational, territorial_compromise_obligation).
narrative_ontology:cs_axiom_status(territorial_compromise_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c10f28ed-1407-43b2-8c32-4cac266383a3', territorial_compromise_obligation, conventional).
narrative_ontology:cs_axiom('c10f28ed-1407-43b2-8c32-4cac266383a3', secondary, two_states_partition).
narrative_ontology:cs_axiom_status(two_states_partition, holdable).
narrative_ontology:cs_axiom_grounding('c10f28ed-1407-43b2-8c32-4cac266383a3', two_states_partition, conventional).
narrative_ontology:cs_reference_frame('c10f28ed-1407-43b2-8c32-4cac266383a3', liberal_national_self_determination_framework).
narrative_ontology:cs_drift_state('c10f28ed-1407-43b2-8c32-4cac266383a3', post_oslo_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c10f28ed-1407-43b2-8c32-4cac266383a3', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_movement).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, national_self_determination_principle).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_nationalism_framework).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, two_state_solution_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutes as a rights-bearing nation claiming collective self-determination. The right to statehood in the ancestral homeland is experienced as existential — not merely political but identity-constitutive. Exit from this claim would mean dissolution of the collective national project. Benefits from international recognition of the right, but the claim's realization requires territorial compromise with Palestinian claimants.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective, beneficiary,
    organized, generational, identity_locked, global).

% Administers the sovereign framework that realizes Jewish self-determination. Sets borders, controls immigration, manages security apparatus, and negotiates (or refuses) territorial compromise. Collects legitimacy dividends from the liberal nationalist framing while bearing costs of occupation, international pressure, and demographic tension. Exit from the self-determination framework would mean state dissolution — constrained but not trapped.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state, beneficiary).

% Co-equal self-determination claimant whose realization is blocked by the same territorial framework that enables Jewish statehood. Experiences military occupation, settlement expansion, movement restrictions, and denial of sovereign agency. The liberal nationalist reading acknowledges their claim in principle but subordinates it in practice through partition terms that cement asymmetry. Exit from the constraint means either abandoning national claims or achieving sovereign realization — both structurally blocked.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people, payer,
    organized, generational, trapped, national).

% Displaced in 1948 and 1967, denied return under the liberal nationalist framework's demographic logic. Their claim enters as a 'refugee issue' to be solved by compensation and resettlement elsewhere, not by return. The constraint extracts their right of return as the price of Jewish demographic majority. No meaningful exit — stateless, dispersed, dependent on host states and UNRWA.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Interim self-governance body created by the Oslo framework — the liberal nationalist reading's operational translation. Administers civilian affairs in fragmented enclaves while security control remains with Israel. Collects limited autonomy benefits but pays with legitimacy erosion (seen as collaborator) and structural incapacity to deliver sovereignty. Constrained exit: cannot dissolve without chaos, cannot achieve statehood without Israeli agreement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority, beneficiary).

% Exploits the liberal nationalist reading's 'ancestral homeland' language to justify permanent settlement beyond partition lines. Benefits from state subsidies, military protection, and ideological validation. Their presence makes territorial compromise physically harder — they are both beneficiaries of the constraint's expansive reading and drivers of its extraction from Palestinians. Identity-locked: withdrawal experienced as existential betrayal.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_movement, beneficiary,
    organized, generational, identity_locked, local).

% Sustains the two-state paradigm through diplomatic recognition, aid conditioning, UN resolutions, and peace process management. The liberal nationalist reading is the dominant framework in Western foreign policy. Observes the constraint's operation but lacks enforcement will — treats the arrangement as a negotiation framework rather than a structure of extraction. Analytical exit: can shift frameworks but institutional inertia is massive.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_community, observer,
    institutional, generational, analytical, global).

% Palestinian citizens inside sovereign Israel — the liberal nationalist reading's 'demographic problem.' Granted individual civil rights but excluded from the collective self-determination the framework centers. Experience the Jewish nation-state law as constitutional subordination. Would object to the constraint's ethnic-national definition if their voice were structurally centered. Constrained exit: emigration possible but costly; integration without assimilation denied.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages competing national self-determination claims over the same territory by translating them into a partition framework (two states) or binational alternative — providing a recognized diplomatic language, legal architecture, and institutional pathway for resolving the conflict without total war.
% TRANSFER_FUNCTION: Transfers territorial sovereignty, demographic control, and resource access from Palestinian collective to Jewish collective — realized through 1948 borders, 1967 occupation, settlement expansion, and the refugee return denial. The Jewish collective receives the primary benefit (sovereign statehood with demographic majority); Palestinians bear the primary cost (dispossession, statelessness, fragmented autonomy).
% ABSENT_VOICES: Palestinian refugees in diaspora (structurally excluded from negotiations), Palestinian citizens of Israel (constitutionally subordinated), and the global Palestinian public opinion that consistently rejects partition-as-surrender — all are present as affected populations but absent as decision-makers in the liberal nationalist framework's diplomatic architecture.
% DISAPPEARANCE_RATIONALE: If the liberal nationalist constraint vanished overnight, the legal-diplomatic framework for two-state partition would collapse — but the competing claims (Jewish self-determination, Palestinian self-determination, settler colonial entitlement, religious territorial mandate) would remain. The territory would reorganize around either: (a) a single apartheid-style state with Jewish privilege, (b) a binational democratic state, (c) renewed warfare, or (d) external imposition. The constraint's disappearance does not resolve the underlying claims — it removes the mediated pathway.
% FOUNDING_PROBLEM: The post-WWI collapse of Ottoman sovereignty created a vacuum over Palestine where two national movements (Zionist and Palestinian Arab) made overlapping self-determination claims. The liberal nationalist reading emerged from the 1937 Peel Commission through 1947 UN Partition Resolution to the 1993 Oslo Accords as the international community's attempt to adjudicate these claims through territorial partition rather than winner-take-all conquest.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — adjudicating competing self-determination claims — is attested as still live by: (1) the international diplomatic consensus (UN, EU, Quartet) still formally endorses two states; (2) Palestinian leadership (PLO/PA) officially accepts two-state framework; (3) Israeli security establishment largely views separation as necessary for Jewish democracy. It is attested as dead or obsolete by: (1) the settler movement and religious nationalists who reject partition; (2) post-Zionist and one-state advocates who argue the framework entrenches inequality; (3) Hamas and rejectionist factions who deny Jewish national rights. No single external corroboration settles the dispute — the contestation IS the structural fact.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.48 reflects the tension between the reading's stated compromise logic and its historical outcomes: 1948 displacement, 1967 occupation, settlement project, refugee denial. Suppression at 0.68 captures the military enforcement, permit regimes, wall/barrier, and legal architecture maintaining the asymmetry. Theater ratio at 0.38 reflects the peace process as performance — negotiations that manage conflict without resolving it, creating a permission structure for continued extraction. Accessibility collapse at 0.55: alternatives exist (binational, one-state, confederation) but are structurally marginalized by the partition paradigm's institutional lock-in. Resistance at 0.78: sustained Palestinian national movement, intifadas, BDS, legal challenges, diplomatic campaigns — the constraint meets active, organized resistance across generations.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish collective/Israeli state seat, the constraint appears as a rope — genuine coordination solving the Jewish statelessness problem through internationally recognized self-determination. From the Palestinian people/refugee seats, it appears as a snare — the coordination story (partition) is cover for ongoing extraction (land, sovereignty, return). The Palestinian Authority seat experiences it as a degraded rope — coordination that delivers neither sovereignty nor security. The settler movement seat experiences it as a rope being betrayed — they read the 'ancestral homeland' language literally and see compromise as violation. The engine computes this divergence from the structural data; the claimed_type 'tangled_rope' captures the reading's own internal tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective and Israeli state are structural beneficiaries (d ~0.15-0.25): they receive the sovereign outcome, demographic majority, resource control, and international legitimacy. The Israeli state as agenda_setter has the lowest d (it writes the rules). Palestinian people and refugees are structural targets (d ~0.85-0.95): they bear the territorial, demographic, and sovereignty costs with trapped/constrained exit. Palestinian Authority sits at d ~0.65: constrained by Oslo architecture, dependent on Israeli cooperation, legitimacy-eroded. Settler movement is a beneficiary with identity_locked exit (d ~0.2) but drives extraction upward. International community is observer (d ~0.5): analytical distance but institutional complicity. Palestinian citizens of Israel are excluded (d ~0.7): inside the polity but outside the national self-definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The liberal nationalist reading's founding problem (adjudicating competing self-determination claims via partition) remains contested — not dead, not fully live. The constraint persists because: (1) no alternative framework has achieved consensus; (2) the Israeli state derives legitimacy and security from the framework's managed ambiguity; (3) the Palestinian Authority derives its institutional existence from it; (4) the international community has invested three decades in the Oslo architecture. Mandatrophy is not resolved — the constraint's coordination function (peace process) has atrophied into theater while its extraction function (settlements, occupation) has intensified. The reading's own axioms (universal self-determination, territorial compromise obligation) are violated by its own operational reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the liberal nationalist reading genuinely structure the constraint, or is it a legitimating veneer for the religious/settler colonial readings that drive actual policy?',
    'Track Israeli government composition, settlement growth rates, and official rhetoric across administrations: if policy consistently exceeds the reading''s territorial compromise logic, the reading is veneer not structure.',
    'If veneer, the constraint''s true type shifts toward snare (extraction masked by coordination talk); if structural, tangled_rope holds with the reading''s own axioms as the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the liberal nationalist framing is causally efficacious or epiphenomenal.').

omega_variable(
    palestinian_coequal_status,
    'Can Palestinian self-determination be co-equal in a framework that requires Jewish demographic majority in the sovereign space?',
    'Examine the demographic logic of partition proposals: do they grant Palestinians full sovereign equivalence (borders, resources, return, security control) or subordinated autonomy?',
    'If co-equality is structurally impossible under Jewish demographic majority, the reading''s coordination claim is internally contradictory — extraction is not accidental but constitutive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palestinian_coequal_status, conceptual, 'Structural tension between co-equal claimant recognition and demographic majoritarianism.').

omega_variable(
    partition_binational_ambiguity,
    'Does the reading''s ''partition or binational framework'' ambiguity conceal a refusal to specify which — allowing extraction to continue under cover of diplomatic process?',
    'Analyze official Israeli, PA, and US positions since 2000: has any actor committed to a specific, mapped, sovereign partition? Or has ''two states'' become a process substitute for outcome?',
    'If ambiguity is functional (enables delay/extraction), theater_ratio is understated and the constraint trends toward piton/snare; if ambiguity reflects genuine openness, the coordination function remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_binational_ambiguity, empirical, 'Whether partition/binational ambiguity is a genuine openness or a delay tactic.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military occupation, legal barriers, geographic fragmentation) or internalized (Palestinian leadership co-optation, international diplomatic framing that pathologizes resistance)?',
    'Post-exit suppression trajectory: if PA security coordination with Israel persists absent occupation, or if ''peace process'' discourse delegitimizes resistance internationally, internalized component is significant.',
    'If internalized, effective suppression exceeds structural measure — the constraint reproduces itself through the subjects'' own governance and diplomatic frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the Palestinian Authority / Oslo architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsp_lnr_tr_t1937, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(jsp_lnr_tr_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(jsp_lnr_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(jsp_lnr_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(jsp_lnr_tr_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(jsp_lnr_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(jsp_lnr_be_t1937, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(jsp_lnr_be_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1947, 0.35).
narrative_ontology:measurement(jsp_lnr_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.52).
narrative_ontology:measurement(jsp_lnr_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(jsp_lnr_be_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(jsp_lnr_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jsp_lnr_su_t1937, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(jsp_lnr_su_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1947, 0.55).
narrative_ontology:measurement(jsp_lnr_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(jsp_lnr_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(jsp_lnr_su_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(jsp_lnr_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.15).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, oslo_accords_framework).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, un_partition_resolution_181).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_settlement_enterprise).

% DUAL FORMULATION NOTE:
% This constraint family (jewish_sovereignty_palestine) decomposes the colloquial 'Zionism' or 'Israeli-Palestinian conflict' into five structurally distinct readings of the same kernel. The liberal nationalist reading claims universal self-determination logic with territorial compromise; the religious zionist reading claims divine mandate; the settler colonial reading identifies a displacement pattern; the cultural zionist reading rejects political sovereignty as necessary; the post-zionist reading diagnoses the framework itself as the obstacle. Their ε values differ widely: liberal nationalist (moderate, ~0.48), religious zionist (high, ~0.75), settler colonial (very high, ~0.85), cultural zionist (low, ~0.25), post-zionist (variable, diagnostic). They are linked by network.affects_constraints because each reading cites the others as evidence or foil.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__liberal_nationalist_reading, organized, 0.2).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__liberal_nationalist_reading, powerless, 0.95).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__liberal_nationalist_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
