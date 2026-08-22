% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Zionist Refuge Reading of Territorial Legitimacy (Persecution, Promise, Partition)
 *   domain: political_theory/international_relations
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested legitimacy kernel: the
 *   claim that Israel's territorial sovereignty is legitimately grounded in
 *   the historical persecution of the Jewish people, the covenant promise,
 *   and acceptance of the UN partition boundary. Under this reading the
 *   standing arrangement — the state as constituted, with its occupation
 *   administration and refugee-status architecture — is assessed as
 *   substantially legitimate: the founding transfer is externalized to the
 *   Arab rejection of partition, post-1967 control is framed as security
 *   necessity with negotiable boundaries, and the extraction the reading
 *   itself registers is the residue it cannot externalize (settler
 *   differential rights, permit-regime burdens, fifty-plus years of
 *   unconcluded negotiation). KEY AGENTS (by structural relationship): -
 *   israeli_jewish_citizens: Primary beneficiary (organized/identity_locked)
 *   — the sovereign public consuming the arrangement's protections -
 *   diaspora_jewish_communities: Secondary beneficiary (organized/mobile) —
 *   holds the refuge guarantee as insurance without daily exposure -
 *   west_bank_settler_movement: Concentrated beneficiary
 *   (powerful/identity_locked) — receives the material gains of the post-1967
 *   dimension - palestinian_refugee_diaspora: Primary target
 *   (organized/trapped) — bears the founding displacement's continuing costs
 *   - west_bank_palestinians: Primary target (moderate/trapped) — bears
 *   occupation administration directly - gaza_palestinians: Primary target
 *   (powerless/trapped) — bears the blockade regime - israeli_arab_citizens:
 *   Dual-positioned (moderate/constrained) — citizenship goods inbound,
 *   displacement-memory and equality-gap costs outbound -
 *   israeli_state_authorities: Agenda setter (institutional/constrained) —
 *   administers enforcement and produces the security assessments feeding the
 *   justification - international_legal_institutions: Analytical observer
 *   (analytical/analytical) — audits conformity without enforcement power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.44).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.73).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Territorial Legitimacy (Persecution, Promise, Partition)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '5763a543-4c49-45de-ae7c-1b02277748fd').
narrative_ontology:cs_kernel_codification('5763a543-4c49-45de-ae7c-1b02277748fd', distributed).
narrative_ontology:cs_authority_grounding('5763a543-4c49-45de-ae7c-1b02277748fd', lineage).
narrative_ontology:cs_interpretation_layer_present('5763a543-4c49-45de-ae7c-1b02277748fd').
narrative_ontology:cs_reading_relation('5763a543-4c49-45de-ae7c-1b02277748fd', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('5763a543-4c49-45de-ae7c-1b02277748fd', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('5763a543-4c49-45de-ae7c-1b02277748fd', foundational, persecution_history_grounds_sovereignty).
narrative_ontology:cs_axiom_status(persecution_history_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5763a543-4c49-45de-ae7c-1b02277748fd', persecution_history_grounds_sovereignty, deontological).
narrative_ontology:cs_axiom('5763a543-4c49-45de-ae7c-1b02277748fd', foundational, partition_acceptance_confers_legality).
narrative_ontology:cs_axiom_status(partition_acceptance_confers_legality, holdable).
narrative_ontology:cs_axiom_grounding('5763a543-4c49-45de-ae7c-1b02277748fd', partition_acceptance_confers_legality, conventional).
narrative_ontology:cs_axiom('5763a543-4c49-45de-ae7c-1b02277748fd', secondary, covenant_title_secures_land_claim).
narrative_ontology:cs_axiom_status(covenant_title_secures_land_claim, holdable).
narrative_ontology:cs_axiom_grounding('5763a543-4c49-45de-ae7c-1b02277748fd', covenant_title_secures_land_claim, theological).
narrative_ontology:cs_reference_frame('5763a543-4c49-45de-ae7c-1b02277748fd', partition_sanctioned_refuge_sovereignty).
narrative_ontology:cs_drift_state('5763a543-4c49-45de-ae7c-1b02277748fd', contemporary_post_1967, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5763a543-4c49-45de-ae7c-1b02277748fd', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jewish_citizens).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_settler_movement).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, gaza_palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_arab_citizens).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, israeli_arab_citizens).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_181_legality_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, persecution_refuge_entitlement_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold citizenship in the state this framework legitimizes: military defense, immigration priority under the Law of Return, and state institutions funded by the arrangement flow to them. Family narratives of persecution supply the legitimacy account's evidentiary core. Emigration is possible and common enough to have a name, but leaving reads socially as abandoning the refuge project, and citizenship elsewhere rarely substitutes for the collective guarantee.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jewish_citizens, beneficiary,
    organized, generational, identity_locked, national).

% Live outside the state but hold its refuge guarantee as insurance against persecution. They contribute political advocacy and philanthropy and experience the arrangement through identity affiliation, travel, and crisis moments rather than daily governance. Their exposure to the arrangement's costs is indirect and episodic, and full disaffiliation through assimilation remains available at personal cost.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Resides beyond the 1949 armistice lines under state subsidy, military protection, and a parallel legal infrastructure. Housing, roads, water allocation, and schooling are financed by the state. Removal would require the state to act against its own citizens, and ideological commitment binds many residents to place regardless of policy shifts.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_settler_movement, beneficiary,
    powerful, generational, identity_locked, regional).

% Descendants of those displaced in the 1948 war, registered as refugees across the Levant and beyond. Return to homes inside Israel is barred by the state the framework legitimizes; host-state integration is legally blocked in places such as Lebanon; camp residency passes across generations. The framework dates their displacement to the Arab rejection of partition and treats the resulting population movements as settled.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugee_diaspora, payer,
    organized, generational, trapped, continental).

% Live under military administration: permit regimes govern work, travel, and building; land declarations and settlement growth shrink the space available to them; fragmentation separates communities from each other and from Jerusalem. Citizenship in the legitimizing state is unavailable, and the alternative sovereignty promised in exchange has been under negotiation for decades without conclusion.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_palestinians, payer,
    moderate, generational, trapped, regional).

% Live under air, sea, and land restrictions tightened after 2007; movement outside the strip requires permits rarely granted; reconstruction materials are rationed through inspection regimes. The framework presents these controls as security measures responding to armed attack, and the governed population has no vote over the authorities imposing them.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, gaza_palestinians, payer,
    powerless, biographical, trapped, local).

% Hold citizenship, vote, and serve in courts and parliament, while carrying family histories of displacement from 1948 villages and facing land claims, planning disparities, and periodic loyalty scrutiny. The narrative that grounds the state's legitimacy marks their community as the internal remainder of the partition their leadership is recorded as having rejected.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_arab_citizens, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, israeli_arab_citizens, payer).

% Set and administer the arrangement: the government legislates citizenship and land law, the military runs the occupation's permit and closure systems, and the foreign ministry defends the framework diplomatically. Threat assessments produced inside this apparatus feed the security justification the framework rests on; budget, jurisdiction, and narrative authority all expand with the arrangement's persistence.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_authorities, agenda_setter,
    institutional, generational, constrained, national).

% UN bodies, the International Court of Justice, and treaty monitors assess the framework's conformity with international law, issue advisory opinions and resolutions, and document conditions on the ground. They hold no enforcement power over the arrangement and depend on member-state politics for any traction.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_legal_institutions, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jewish_citizens).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides collective refuge, immigration intake, and self-defense for a historically stateless and persecuted minority; coordinates membership (Law of Return), national institutions, and territorial administration for a dispersed population; anchors these functions in a legitimacy account — persecution history, covenant promise, and acceptance of the UN partition boundary — that stabilizes expectations among members, patrons, and adversaries.
% TRANSFER_FUNCTION: Moves territorial control, land, water, and movement freedom from the non-citizen Palestinian population toward the Jewish sovereign public and the settlement periphery; moves diaspora financial and political support inward; moves the costs of insecurity — closures, permits, rations — onto the governed population that cannot vote over them.
% ABSENT_VOICES: The refugee diaspora holds formal representation (PLO, UNRWA registration) but no seat in the legitimacy-granting conversation: the framework cites the powers that accepted partition, a table the displaced were never invited to. Host states sheltering refugees for generations bear costs without any role in final-status design. Palestinian citizens of Israel vote, but the founding narrative treats their consent as unnecessary to legitimacy.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, every arrangement resting on it would need immediate replacement: the citizenship regime, the settlement administration, the refugee-registration architecture, diaspora identity structures, and the security doctrine all cite it. Neighboring states' treaties, patron-state aid architecture, and the region's diplomatic map presuppose it; nothing in the current configuration survives its removal unchanged.
% FOUNDING_PROBLEM: European Jewry's statelessness culminating in the Holocaust: a population with no sovereign able or willing to protect it, no open immigration door during the extermination years, and no territorial base for self-defense. The UN partition plan of 1947 offered a legal mechanism — two states, economic union, an internationalized Jerusalem — and the Jewish leadership accepted it.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust-era archives, Nuremberg trial records, and contemporaneous refugee-board documentation corroborate the founding problem from outside any benefiting party. Contemporary antisemitism monitoring (community security incident reporting, EU fundamental-rights surveys) corroborates that diaspora vulnerability persists. Palestinian representative bodies and human-rights organizations attest the problem was real while disputing its present scope and the arrangement's exclusivity — corroboration of the problem, contestation of the solution.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).
:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.44 by this reading's own lights over the fixed referent (the standing arrangement): the reading externalizes the 1948 displacement to Arab rejection and frames territorial control as security necessity, so the ε it can honestly register is the unexternalizable residue — settlement differential rights, permit-regime burdens, and the widening gap between 'negotiable boundaries' rhetoric and permanent facts on the ground. Suppression is a raw structural property, unscaled by power or scope: the enforcement machinery (military government, permit regimes, closure systems, border enforcement against return) is extensive regardless of how the reading justifies it, hence 0.73. Theater rises monotonically from 0.12 to 0.40 as commemorative infrastructure, diplomatic performance, and peace-process ritual accumulate atop a delivery function that dominated the early decades (mass absorption of survivors and Mizrahi refugees was overwhelmingly functional). The suppression series tracks enforcement-capacity change, not mere extraction shift: martial law over Arab citizens (t0), occupation administration (t19), First Intifada suppression (t39), Oslo-era delegation to the PA producing a genuine dip (t45), Second Intifada re-hardening and barrier construction (t52), blockade consolidation (t57 onward). All three series run on one shared eight-point grid so every metric is authored at every examined time point; the t45 dip is a real delegation event, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute very differently from identical structural data. From the refugee and occupied-population seats — trapped exit, generational horizons, no vote over the enforcing authority — the arrangement presents as enforced extraction with a coordination veneer. From the citizen and diaspora seats the same structure presents as refuge delivered and security maintained. The settler seat adds a third register: concentrated material gain fused with ideological identity-lock. The engine computes these divergences from power, exit, and declared position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the citizen, diaspora, and settler seats toward the beneficiary pole; the diaspora's mobile exit places it nearest the subsidy end (it collects the insurance value while bearing almost none of the enforcement cost). Victim declarations plus trapped exit drive the three Palestinian seats toward the full-target pole. The dual-positioned israeli_arab_citizens seat derives near-symmetric, slightly net-target. One override is declared: israeli_state_authorities holds no array-listed position, so the canonical fallback would treat the administrator as neutral — but the arrangement demonstrably subsidizes the administrator (budget, jurisdiction, and narrative authority expand with its persistence), so d is overridden to 0.15 for the institutional power atom, which after making the observer seat analytical applies to no other agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — statelessness and persecution culminating in the Holocaust — remains live with extra-beneficiary corroboration, so the composite constraint is not mandatrophy-resolved and the (status=live x verdict=world_rearranges) cell is consistent: no zombie flag. But the composite contains a decaying sub-mandate: the post-1967 control component was framed as defensive buffer and bargaining collateral, and its 'negotiable' framing now performs a mandate that practice has abandoned — the theater series' rise is partly this decay made visible. The tangled_rope classification prevents two symmetrical mislabelings: reading the whole arrangement as pure extraction erases the live refuge-and-membership function that still delivers for its beneficiaries; reading it as pure coordination erases the payer seats whose costs ride the same enforcement machinery. The classification holds both truths in one structure, which is what the hybrid category exists to do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the zionist_refuge_reading of the territorial_legitimacy_dual kernel; what would instantiating the palestinian_autochthony_reading change structurally?',
    'Generate the sibling reading as its own constraint file over the same referent and compare computed classifications; the disagreement is located in the moral weight assigned to pre-state persecution versus continuous habitation, and in whether UN 181 confers legitimacy or merely procedural cover.',
    'Sibling instantiation would re-date the constraint''s extraction origin to 1948 rather than 1967, expand the victim set backward to the founding displacement, raise epsilon sharply, and shift the computed classification toward pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: this story is one reading of a contested legitimacy kernel, not the topic whole.').

omega_variable(
    partition_acceptance_scope,
    'Does acceptance of UN Resolution 181 confer durable legitimacy when the plan''s reciprocal terms — the second state, the repatriation clauses, the Jerusalem regime — were never implemented?',
    'Legal-historical comparison of legitimacy conferred by partially implemented international plans across comparable cases, together with the ICJ''s treatment of Resolution 181 in advisory proceedings.',
    'If partial acceptance voids the legitimacy transfer, the reading''s conventional pillar collapses, the legality claim rests solely on accomplished facts, and effective extraction rises materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_acceptance_scope, conceptual, 'Whether the partition-acceptance pillar of the legitimacy account survives its own unimplemented reciprocity.').

omega_variable(
    displacement_causation_weight,
    'What relative causal weight do Arab rejection of partition and organized expulsion carry in producing the 1948 refugee population?',
    'Historiographic synthesis of captured state archives, village-level case studies, and the revisionist literature against official-version records; the dispute is documentarily tractable even where it remains politically live.',
    'If expulsions were systematic policy rather than war''s incidental debris, the reading''s externalization of displacement fails, epsilon rises materially, and the victim set expands backward to the founding year.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_causation_weight, empirical, 'The causal-weight question underneath the reading''s displacement framing.').

omega_variable(
    security_necessity_vs_rent,
    'Is post-1967 territorial control driven by security necessity, or has it accumulated a rent component — settlement subsidies, land banking, water allocation — that persists independently of threat levels?',
    'Budgetary and counterfactual analysis: settlement-investment trajectories during active negotiation windows, comparative cost of the settlement enterprise against defense-equivalent alternatives, and behavior when credible third-party guarantees were offered.',
    'A persistent rent component converts the ''negotiable boundaries'' element from coordination cost into extraction, raising epsilon and pushing the composite toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_vs_rent, empirical, 'Whether the security justification still accounts for the territorial-control component.').

omega_variable(
    theological_premise_load,
    'How much load-bearing work does the divine-promise premise do in this reading relative to the persecution-history and partition-acceptance premises?',
    'Survey of the reading''s institutional articulations: whether secular-Zionist formulations sustain the full legitimacy claim without covenant language, and where religious-Zionist institutions treat the covenant premise as indispensable.',
    'If the reading survives on persecution plus partition alone, the theological axiom is decorative; if covenant language is load-bearing, removing it dissolves the reading into a purely conventional-legality position and changes its foreclosure relations with secular sibling framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_premise_load, conceptual, 'Internal weight distribution among the reading''s three legitimacy pillars.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zionist_refuge_reading_tr_t0, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(zionist_refuge_reading_tr_t19, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 19, 0.18).
narrative_ontology:measurement(zionist_refuge_reading_tr_t39, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 39, 0.24).
narrative_ontology:measurement(zionist_refuge_reading_tr_t45, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement(zionist_refuge_reading_tr_t52, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 52, 0.32).
narrative_ontology:measurement(zionist_refuge_reading_tr_t57, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 57, 0.34).
narrative_ontology:measurement(zionist_refuge_reading_tr_t66, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 66, 0.37).
narrative_ontology:measurement(zionist_refuge_reading_tr_t76, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 76, 0.4).

% Extraction over time
narrative_ontology:measurement(zionist_refuge_reading_be_t0, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(zionist_refuge_reading_be_t19, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 19, 0.28).
narrative_ontology:measurement(zionist_refuge_reading_be_t39, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 39, 0.33).
narrative_ontology:measurement(zionist_refuge_reading_be_t45, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 45, 0.3).
narrative_ontology:measurement(zionist_refuge_reading_be_t52, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 52, 0.36).
narrative_ontology:measurement(zionist_refuge_reading_be_t57, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 57, 0.38).
narrative_ontology:measurement(zionist_refuge_reading_be_t66, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 66, 0.41).
narrative_ontology:measurement(zionist_refuge_reading_be_t76, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 76, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(zionist_refuge_reading_su_t0, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(zionist_refuge_reading_su_t19, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 19, 0.58).
narrative_ontology:measurement(zionist_refuge_reading_su_t39, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 39, 0.66).
narrative_ontology:measurement(zionist_refuge_reading_su_t45, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement(zionist_refuge_reading_su_t52, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 52, 0.7).
narrative_ontology:measurement(zionist_refuge_reading_su_t57, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 57, 0.68).
narrative_ontology:measurement(zionist_refuge_reading_su_t66, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 66, 0.71).
narrative_ontology:measurement(zionist_refuge_reading_su_t76, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 76, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Israel's legitimacy' decomposes into three structurally distinct readings of one kernel, each with its own epsilon, beneficiary/victim structure, and classification. This file (zionist_refuge_reading) is the upstream member: its partition-acceptance premise supplies the legal basis the two_state_coexistence_reading builds on (relation: influences), and its institutionalization of refugee status created the apparatus that sustains the palestinian_autochthony_reading's claim (relation: coexists_with — the two narratives compete live without either logically eliminating the other in public discourse). Sibling files declare their own edges; epsilon differs across the family because each reading assesses the same standing arrangement by its own lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
