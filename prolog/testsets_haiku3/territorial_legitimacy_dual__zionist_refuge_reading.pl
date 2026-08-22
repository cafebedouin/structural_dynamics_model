% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Israeli Territorial Legitimacy: Zionist Refuge Reading
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the Zionist refuge reading of Israel's
 *   territorial legitimacy. The reading grounds legitimacy in three elements:
 *   (1) the Jewish diaspora's historical persecution, culminating in the
 *   Holocaust, creating a categorical imperative for refuge; (2) divine
 *   promise as grounding claim for territorial claim; (3) UN Partition Plan
 *   acceptance as secular-legal warrant. Under this reading, the 1948
 *   establishment was legitimate response to persecution; 1967 territorial
 *   expansion is framed as consequence of Arab rejection of partition and
 *   subsequent security threat, not as independent territorial aggression.
 *   Palestinian displacement is narrated as outcome of Arab states' military
 *   choice to reject partition, not as Zionist policy objective. The
 *   constraint coordinates security for diaspora Jewish communities
 *   (extraction cost distributed across Palestinian population) while
 *   vindicating a doctrine of persecution-justified refuge and UN-mandated
 *   legitimacy. This reading coexists with Palestinian autochthony reading
 *   (which centers Palestinian continuous habitation and displacement trauma)
 *   and two-state coexistence reading (which grants dual legitimacy with 1967
 *   as compromise boundary). The kernel is the legitimacy claim itself; the
 *   readings differ in what grounds that legitimacy and who bears costs.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_communities: historical persecution experience, refugee seeking, security dependent
 *   - zionist_institutional_leadership: establishes and maintains the state, enforces territorial claims, sets legitimacy narrative
 *   - israeli_security_apparatus: maintains occupation, justifies suppression through security doctrine, administers enforcement
 *   - palestinian_arabs_1948_displaced: bear displacement cost, framed in this reading as consequence of Arab rejection rather than Zionist policy
 *   - palestinian_population_1967_occupied_territories: bear occupation cost, controlled by security apparatus, resistance meets suppression
 *   - arab_states_1948: rejects partition, triggers security narrative that justifies expansion
 *   - international_community_un: grants partition legitimacy (warrant for this reading), later contests occupation (pressure on this reading's boundaries)
 *   - analytical_observer: examines whether persecution justification and UN warrant hold under scrutiny, whether suppression is proportionate to security need
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.71).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Israeli Territorial Legitimacy: Zionist Refuge Reading").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, 'dbefa907-1587-447a-b8a3-764112bdb969').
narrative_ontology:cs_kernel_codification('dbefa907-1587-447a-b8a3-764112bdb969', formalized).
narrative_ontology:cs_authority_grounding('dbefa907-1587-447a-b8a3-764112bdb969', lineage).
narrative_ontology:cs_interpretation_layer_present('dbefa907-1587-447a-b8a3-764112bdb969').
narrative_ontology:cs_reading_relation('dbefa907-1587-447a-b8a3-764112bdb969', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbefa907-1587-447a-b8a3-764112bdb969', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('dbefa907-1587-447a-b8a3-764112bdb969', foundational, persecution_justifies_refuge_establishment).
narrative_ontology:cs_axiom_status(persecution_justifies_refuge_establishment, holdable).
narrative_ontology:cs_axiom_grounding('dbefa907-1587-447a-b8a3-764112bdb969', persecution_justifies_refuge_establishment, deontological).
narrative_ontology:cs_axiom('dbefa907-1587-447a-b8a3-764112bdb969', foundational, un_partition_acceptance_grants_legitimacy).
narrative_ontology:cs_axiom_status(un_partition_acceptance_grants_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('dbefa907-1587-447a-b8a3-764112bdb969', un_partition_acceptance_grants_legitimacy, conventional).
narrative_ontology:cs_axiom('dbefa907-1587-447a-b8a3-764112bdb969', secondary, divine_promise_territorial_grounding).
narrative_ontology:cs_axiom_status(divine_promise_territorial_grounding, overridden).
narrative_ontology:cs_axiom_grounding('dbefa907-1587-447a-b8a3-764112bdb969', divine_promise_territorial_grounding, theological).
narrative_ontology:cs_axiom('dbefa907-1587-447a-b8a3-764112bdb969', secondary, security_doctrine_justifies_occupation_expansion).
narrative_ontology:cs_axiom_status(security_doctrine_justifies_occupation_expansion, holdable).
narrative_ontology:cs_axiom_grounding('dbefa907-1587-447a-b8a3-764112bdb969', security_doctrine_justifies_occupation_expansion, empirically_contingent).
narrative_ontology:cs_reference_frame('dbefa907-1587-447a-b8a3-764112bdb969', diaspora_persecution_refuge_imperative).
narrative_ontology:cs_drift_state('dbefa907-1587-447a-b8a3-764112bdb969', contemporary_occupation_entrenchment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dbefa907-1587-447a-b8a3-764112bdb969', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_security_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, zionist_institutional_leadership).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_arabs_1948_displaced).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_population_1967_occupied_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, israeli_security_apparatus).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, persecution_justifies_refuge_establishment).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_legitimacy).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, security_doctrine_territorial_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain refuge state and security apparatus protecting Jewish life and continuity. Persecution history grounds their need for this arrangement; they experience it as life-preserving. They can exit by assimilating into host countries or emigrating to other refuge options (though few historical alternatives existed), but the state apparatus provides identity continuity and security that alternatives do not. Their experience is that without this constraint, diaspora vulnerability persists as permanent condition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_communities, beneficiary,
    organized, civilizational, arbitrage, global).

% Establishes, maintains, and narrates the state; sets territorial claims and legitimacy doctrine; enforces occupation and settlement. Benefits from territorial control, administrative authority, and institutional power. Sets the agenda for what the constraint means (refuge, security, divine promise, UN warrant). Could exit by negotiating territorial compromise, but that would reduce institutional scope and authority they administer. Their situation is that expansion and occupation consolidate their institutional power.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, zionist_institutional_leadership, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, zionist_institutional_leadership, beneficiary).

% Maintains occupation machinery, justifies suppression through security doctrine, administers enforcement of territorial control. Bears costs of militarization and constant vigilance (resource drain, operational risk, moral burden of enforcement). But institutional identity is fused with security mission—security apparatus cannot exit without dissolving its own reason for existing. The constraint's expansion justifies expanded security apparatus; the apparatus is locked into maintaining the expansion. Experiences constraint as requiring increasing suppression as Palestinian resistance grows.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_security_apparatus, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, israeli_security_apparatus, payer).

% Displaced during 1948 war; lost homes, property, livelihood. In this reading, their displacement is framed as consequence of Arab states' rejection of partition (not as Zionist policy objective). They cannot return (under Israeli law), cannot reclaim property (under Israeli law), cannot exit to viable alternative—they are trapped in refugee camps or dependent communities in neighboring states. Their situation is permanent dispossession; this reading narrates it as necessary consequence rather than primary extraction goal, but they bear the cost regardless.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_arabs_1948_displaced, payer,
    powerless, generational, trapped, regional).

% Lives under military occupation; movement controlled by checkpoints and permits; homes subject to settlement expansion and demolition; property subject to military seizure; political representation absent. Resistance meets suppression. Identity is fused with territory and Palestinian national claim, making exit (emigration, assimilation into Israel as citizens) structurally impossible without dissolving identity. Bears ongoing extraction through occupation control, settlement expansion, resource restriction. Experiences constraint as permanent suppression justified by security narrative they do not accept.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_population_1967_occupied_territories, payer,
    powerless, biographical, identity_locked, regional).

% Rejected UN partition plan in 1948; militarily opposed state establishment. In this reading, their rejection is the causal trigger for security narrative that justifies territorial expansion. They remain excluded from negotiation within the constraint (territorial legitimacy terms are set by Israel and international community, not negotiated with Arab states). Their rejection is blamed for the Palestinian displacement that followed, shifting causation from Israeli agency to Arab rejection. Trapped between acknowledging the state (which concedes legitimacy) and rejecting it (which perpetuates conflict).
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, arab_states_1948, excluded,
    organized, biographical, trapped, continental).

% Granted partition legitimacy (1947); observes state establishment and territorial expansion; contests occupation (post-1967 resolutions). Sits at analytical distance, granting initial warrant but increasingly questioning expansion. Can observe whether security doctrine justifies observed expansion patterns. Their role is witness to whether the constraint's operation matches its stated grounds.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_community_un, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, zionist_institutional_leadership).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes refuge state and security apparatus for diaspora Jewish communities; creates institutional continuity for Jewish identity and political self-determination; provides collective security against persecution threats that historically characterized diaspora existence.
% TRANSFER_FUNCTION: Transfers territorial control, political authority, and settlement rights from Palestinian inhabitants to Israeli state apparatus. Moves Palestinian labor and resources into Israeli economy. Moves Palestinian displacement and occupation costs onto Palestinian population. Moves security burden and militarization costs onto Israeli security apparatus and Jewish diaspora (constant vigilance, military service, diplomatic isolation costs).
% ABSENT_VOICES: Palestinian inhabitants and their historical claims are structurally excluded from the constraint's legitimacy grounding (they are narrated as consequences, not parties). Arab states' legitimacy claims are excluded from the negotiation. International human-rights observers questioning whether occupation suppression is proportionate to security need are present but not agenda-setting. Voices from within Israeli/Jewish tradition questioning whether occupation serves or contradicts original refuge mandate are internal dissenters, not excluded but marginalized.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared (Israeli territorial control dissolved), the refugee diaspora would lose institutional security apparatus and would face existential vulnerability or forced return-migration to countries with uncertain reception; Palestinian displaced population would attempt return; occupied territories would reorganize; regional security architecture would shift. Arrangements depend structurally on the constraint's persistence.
% FOUNDING_PROBLEM: Jewish diaspora faces categorical vulnerability to persecution (culminating in Holocaust) and requires institutional refuge—a state and security apparatus that guarantees collective safety and political self-determination. Without such refuge, diaspora Jewry lacks sovereign protection against antisemitic violence and exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust historical record and diaspora persecution history attest the founding problem was categorical and urgent in 1948. Israeli security establishment attests the founding problem remains live (persistent antisemitism, regional threats). Palestinian historians and human-rights organizations attest the founding problem was addressed by 1948 state establishment but that subsequent territorial expansion and occupation are independent policies not required by the refuge mandate. International observers (UN human-rights bodies, international law scholars) acknowledge the historical persecution while contesting that occupation policies are proportionate responses to security need. The status 'contested' reflects that adherents of this reading maintain the problem is live, while observers of sibling readings maintain the problem was solved in 1948 and subsequent territorial claims represent expansion beyond legitimate refuge, not continuation of refuge mandate.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins low (0.35 at t=0) because the 1948 state establishment is presented as legitimate response to categorical need (refuge from persecution), with broad international consensus. It rises to 0.68 by t=36 as territorial expansion beyond partition, settlement in occupied territories, and Palestinian displacement accumulate—extraction increases because the constraint's scope expands and the beneficiary/victim asymmetry deepens. Suppression rises from 0.42 to 0.71 as occupation machinery hardens: checkpoints, settlement enforcement, military administration require increasing active suppression (resistance grows, enforcement must intensify). Theater_ratio rises from 0.18 to 0.42, indicating growing gap between stated security rationale and actual territorial control enforcement—security justification carries diminishing explanatory power as settlement patterns and restrictions accumulate beyond defensible security perimeters. The measurements are authored on one shared time grid (t=0,12,24,36) so every metric is valued at every time point. The time period models 1948–1984 (36-year interval), capturing the shift from 1948 legitimacy consensus through 1967 expansion to early 1980s entrenchment.
 *
 * PERSPECTIVAL GAP:
 *   The Zionist institutional beneficiary seat reads the constraint as coordinated refuge grounded in legitimate need and international warrant, with Palestinian displacement as tragic but logically inevitable consequence of Arab rejection. The Palestinian victim seat reads the same structure as dispossession and occupation justified post-hoc by security narrative, with their displacement as the constitutive extraction mechanism. The analytical seat can observe that the claimed coordination function (diaspora refuge) is real and beneficent, but the extraction mechanism (Palestinian displacement and occupation suppression) appears structurally independent of the refuge coordination—one could establish diaspora refuge within 1947 partition boundaries without occupation expansion. The reading's beneficiaries experience it as necessary security doctrine; the victims experience it as permanent dispossession. The reading's own tradition (Israeli state and Zionist institutions) increasingly acknowledges 1967 as boundary question (moving toward two-state coexistence reading) but maintains that 1948 legitimacy is uncontestable and Palestinian displacement is regrettable but not reversible.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora communities occupy the beneficiary position (d near 0.0): they gain refuge, security, and state apparatus that protects their interests. Israeli security apparatus and Zionist institutional leadership are beneficiary-adjacent (d~0.1–0.25): they collect rents (territorial control, administrative power) but also bear costs (militarization, diplomatic isolation, constant enforcement burden). Palestinian populations displaced and occupied occupy victim position (d near 1.0): they bear displacement, dispossession, occupation, and suppression with constrained exits. The Arab states that rejected partition are framed in this reading as indirect victims (d~0.8): their rejection of partition is narrated as triggering the security dynamic that justifies expansion, yet they do not directly bear the enforcement cost—the Palestinian population does. International community sits at d~0.5–0.6: they granted legitimacy to partition but later contest occupation, creating diplomatic extraction for Israel but also diplomatic constraint on expansion.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading exhibits early mandatrophy signals: the founding mandate (1948 refuge for persecuted diaspora Jews) was live and pressing; the founding problem (Holocaust, diaspora vulnerability) was categorical. By t=36 the founding problem shows mixed status: the immediate persecution threat that catalyzed 1948 is historically resolved (though antisemitism persists), yet security discourse has become self-perpetuating (each security action triggers Palestinian resistance, which justifies further security action). The constraint persists not because the founding problem remains acute, but because territorial control and occupation have become institutional interests independent of the original refugee mandate. The reading's own tradition is partially acknowledging this (growing Israeli debate about occupation sustainability, two-state possibility), which indicates mandate erosion. The theater_ratio rise from 0.18 to 0.42 tracks this: security justification carries decreasing functional weight and increasing performative weight. Mandatrophy is not yet fully resolved (the founding problem retains shadow force in security consciousness), but trajectory is toward degraded piton state if occupation without clear security rationale persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_displacement_causation,
    'Are Palestinian displacement and dispossession framed as consequences of Arab rejection of partition, or as independent outcomes of Zionist settlement and military action? Does this reading''s framing of 1948 causation foreclose Palestinian autochthony reading''s causation account, or do both causation narratives coexist?',
    'Comparative historiography across reading traditions; examination of whether adherents of each reading can simultaneously hold their causation claim without logical contradiction when jointly situated in a single normative framework.',
    'If causation framings are mutually foreclosing (each rules out the other''s reading of agency and responsibility), the readings exhibit genuine forecloses relation; if both can be held as competing causal narratives within one framework, they coexist_with. This resolves the reading_relations classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_displacement_causation, conceptual, 'Whether this reading''s causation framing forecloses or coexists with Palestinian causation reading.').

omega_variable(
    divine_promise_grounding_status,
    'Is the divine promise axiom (theological grounding) holding steady in contemporary state practice and international legitimacy discourse, or has state legitimacy migrated to secular-legal grounds (UN partition, defensive war doctrine) with divine promise relegated to historical narrative only?',
    'Textual analysis of official Israeli state documents, international advocacy, and legal arguments (1948 Declaration of Independence vs. contemporary UN addresses vs. Supreme Court rulings); examine whether divine promise still functions as a warrant for territorial claims or is invoked primarily as historical/cultural context.',
    'If divine promise has been formally overridden or displaced by secular-legal grounding in contemporary state legitimacy claims, the axiom_status shifts from holdable to overridden (or the reading''s authority_grounding shifts from theological/distributed to legal/lineage); this affects whether the reading''s core normative framework remains intact or has undergone internal reformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_promise_grounding_status, empirical, 'Whether divine promise remains a live foundational axiom or has been superseded by secular legal grounds within this reading''s own tradition.').

omega_variable(
    suppression_internalization_interpersonal,
    'In contexts of Palestinian resistance and Israeli counter-resistance, what proportion of suppression is structural (legal barriers, settlement expansion, military occupation machinery) versus internalized (Palestinian population''s internalized acceptance of powerlessness, Israeli population''s security narrative justifying suppression)? Does the measured suppression dissipate if occupation mechanisms are removed, or does it persist as internalized narrative?',
    'Post-withdrawal empirical trajectories from comparable occupation scenarios; longitudinal tracking of resistance/compliance ratios after removal of structural barriers; examination of whether populations that exit the constraint carry suppressive narrative dependencies with them.',
    'If suppression is substantially internalized, the effective suppression load is higher than the structural measure suggests and reflects deeper ideological lock-in on both sides; if primarily structural, removing the constraint mechanisms would more readily dissolve the suppression. This informs whether the constraint is a Snare (identity_locked suppression) or Tangled Rope (enforced but not identity-fused).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_interpersonal, empirical, 'Structural vs. internalized suppression in occupation and resistance dynamics.').

omega_variable(
    partition_mandate_vs_territorial_expansion,
    'Does the UN partition vote (1947) ground legitimacy for 1948 borders only, or does this reading extend UN partition legitimacy to post-1967 territorial claims? If extended, on what grounds—security doctrine, demographic facts, or a reinterpretation of partition intent?',
    'Textual analysis of partition resolution language vs. contemporary Israeli territorial claims; examination of whether the reading''s adherents present 1967 territories as part of the original partition mandate or as justified expansion beyond it.',
    'If partition legitimacy is extended to post-1967 territories without explicit regrounding, the reading is internally inconsistent (partition means something specific, then means something larger); if territories are justified by a separate doctrine (security, demographic, biblical), the reading''s axiom structure becomes multi-grounded rather than partition-centered. This affects cs_structure.axioms specificity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_mandate_vs_territorial_expansion, conceptual, 'Whether UN partition legitimacy is meant to ground 1948 borders only or is extended to territorial claims beyond partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t12, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(terr_tr_t12, observed).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(terr_tr_t24, observed).
narrative_ontology:measurement(terr_tr_t36, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 36, 0.42).
narrative_ontology:measurement_basis(terr_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t12, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(terr_be_t12, observed).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(terr_be_t24, observed).
narrative_ontology:measurement(terr_be_t36, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement_basis(terr_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t12, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(terr_su_t12, observed).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement_basis(terr_su_t24, observed).
narrative_ontology:measurement(terr_su_t36, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 36, 0.71).
narrative_ontology:measurement_basis(terr_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__zionist_refuge_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy_dual kernel decomposes into three constraint stories, one per reading: (1) zionist_refuge_reading (this file)—legitimacy grounded in persecution, divine promise, UN partition; (2) palestinian_autochthony_reading—legitimacy grounded in continuous habitation and displacement trauma; (3) two_state_coexistence_reading—dual legitimacy with negotiated 1967 boundary compromise. Each reading has distinct beneficiary/victim structure and ε value. This reading's ε=0.68 reflects high extraction (occupation, displacement, suppression) despite coordination function (refuge). Palestinian reading's ε would reflect extraction from the Jewish perspective (displacement as dispossession cost). Two-state reading's ε would reflect lower extraction (negotiated compromise). The three stories are linked by network.affects_constraints so the corpus can track how readings influence each other (this reading's acceptance of 1948 legitimacy influences two-state reading; Palestinian reading forecloses or coexists depending on causation framing). They share the kernel (territorial legitimacy claim) but instantiate different ε values and victim/beneficiary structures because the readings constitute fundamentally different claims about what grounds legitimacy and who bears its cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
