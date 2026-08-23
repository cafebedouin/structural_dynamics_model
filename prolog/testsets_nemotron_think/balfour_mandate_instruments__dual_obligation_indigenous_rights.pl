% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate Dual Obligation: Arab Rights Superior to National Home
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The Palestine Mandate (1922) embeds a structural contradiction: Article 2
 *   and the preamble commit the mandatory power to establishing a 'Jewish
 *   national home,' while Article 2 and Article 15 require safeguarding the
 *   civil and religious rights of existing non-Jewish communities (Arab
 *   majority ~90% in 1922). The dual obligation reading — advanced by the
 *   Permanent Mandates Commission, Arab leadership, and British officials
 *   like Hope Simpson and Passfield — holds that the mandate's general
 *   provisions (protection of existing rights, preparation for
 *   self-government) take precedence over the specific national home
 *   undertaking. This reading imposes land transfer restrictions (1930 White
 *   Paper, 1940 Land Transfer Regulations), immigration quotas (1939 White
 *   Paper), and a sovereignty path for the Arab majority. It functions as a
 *   tangled rope: genuine coordination (international legal framework for
 *   post-colonial transition, minority protection) combined with asymmetric
 *   extraction (Zionist organizations blocked from demographic parity,
 *   British administration constrained by contradictory mandate terms). The
 *   constraint's extraction rises over the mandate period as the national
 *   home project expands and Arab resistance intensifies, requiring
 *   escalating suppression (culminating in 1936-39 Arab Revolt repression).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.72).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate Dual Obligation: Arab Rights Superior to National Home").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'a40de32b-50d6-4d40-9181-2525a790e80a').
narrative_ontology:cs_kernel_codification('a40de32b-50d6-4d40-9181-2525a790e80a', formalized).
narrative_ontology:cs_authority_grounding('a40de32b-50d6-4d40-9181-2525a790e80a', lineage).
narrative_ontology:cs_interpretation_layer_present('a40de32b-50d6-4d40-9181-2525a790e80a').
narrative_ontology:cs_reading_relation('a40de32b-50d6-4d40-9181-2525a790e80a', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('a40de32b-50d6-4d40-9181-2525a790e80a', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('a40de32b-50d6-4d40-9181-2525a790e80a', foundational, indigenous_rights_superiority).
narrative_ontology:cs_axiom_status(indigenous_rights_superiority, holdable).
narrative_ontology:cs_axiom_grounding('a40de32b-50d6-4d40-9181-2525a790e80a', indigenous_rights_superiority, deontological).
narrative_ontology:cs_axiom('a40de32b-50d6-4d40-9181-2525a790e80a', foundational, self_determination_applies_to_arab_majority).
narrative_ontology:cs_axiom_status(self_determination_applies_to_arab_majority, holdable).
narrative_ontology:cs_axiom_grounding('a40de32b-50d6-4d40-9181-2525a790e80a', self_determination_applies_to_arab_majority, deontological).
narrative_ontology:cs_reference_frame('a40de32b-50d6-4d40-9181-2525a790e80a', mandate_dual_obligation_framework).
narrative_ontology:cs_drift_state('a40de32b-50d6-4d40-9181-2525a790e80a', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a40de32b-50d6-4d40-9181-2525a790e80a', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_settlers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, indigenous_rights_precedence_under_mandate_law).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, self_determination_applies_to_arab_majority).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_principles_constrain_demographic_engineering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Notable families, religious leadership, and merchant classes who leverage mandate protections for Arab land tenure and political representation. They petition the Permanent Mandates Commission, organize delegations to London, and use the mandate's dual obligation language to block land transfers and demand representative institutions. Their position depends on the mandate's legal framework remaining operative.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    organized, generational, constrained, regional).

% Peasant cultivators (fellahin), urban workers, and Bedouin tribes whose land tenure and civil rights the mandate instruments nominally protect. They benefit from land transfer restrictions (1930 White Paper, 1940 Land Transfer Regulations) and immigration quotas that slow demographic displacement. However, they bear the costs of mandatory administration (taxation, conscription, repression of revolt) and lack effective representation in the mandate's governance structures.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, payer).

% World Zionist Organization, Jewish Agency, and affiliated settlement bodies (JNF, Keren Hayesod) that seek land acquisition, unrestricted immigration, and institutional autonomy toward statehood. They experience the dual obligation reading as extraction: mandate restrictions on land purchase (Article 6 'close settlement' interpreted narrowly), immigration quotas (1939 White Paper cap), and political representation limits block their demographic and territorial goals. They maintain external fundraising, diplomatic channels, and paramilitary capacity (Haganah) as exit options.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    powerful, generational, mobile, global).

% High Commissioner, Colonial Office, and mandatory bureaucracy that administer the mandate. They are the enforcement mechanism for land transfer restrictions, immigration quotas, and public order. They bear administrative costs, military expenses (especially 1936-39 Arab Revolt), and diplomatic pressure from both sides. Their discretion is constrained by the mandate text, League oversight, and Whitehall policy shifts — they cannot fully satisfy Zionist demands without violating the mandate's dual obligation, nor fully satisfy Arab demands without violating the national home pledge.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, payer).

% Yishuv residents — agricultural kibbutzim, urban workers, revisionist groups — directly subject to immigration certificates, land purchase restrictions, and security risks. They experience the dual obligation as personal constraint: inability to bring relatives, purchase adjacent land, or expand settlements. Their exit is constrained by European persecution (1930s-40s), British immigration enforcement, and economic dependence on the Yishuv economy.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_settlers, payer,
    moderate, biographical, constrained, local).

% Permanent Mandates Commission in Geneva that receives annual mandatory reports, hears petitions, and issues observations. It interprets the mandate's dual obligation as requiring genuine protection of Arab rights (1920s-30s reports criticize land alienation, demand representative institutions). Its authority is advisory; it lacks enforcement power beyond moral pressure and annual report scrutiny. Dissolved 1946, functions transferred to UN Trusteeship Council (never activated for Palestine).
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations_pmc, observer,
    institutional, generational, analytical, global).

% Landless or small-holding cultivators who work land owned by urban notables or waqf. They are the primary subjects of land tenure protections but have no voice in mandate negotiations, Arab Executive delegations, or British policy. Debt, tenancy insecurity, and displacement from Jewish land purchases affect them most directly, yet they are represented only through elite intermediaries or revolt (1936-39).
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_peasantry_fellahin, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate system coordinates post-Ottoman territorial administration under international law, balancing the League's 'sacred trust of civilization' (Article 22) with the specific undertaking to facilitate a Jewish national home while protecting existing non-Jewish communities' civil and religious rights. The dual obligation reading coordinates by fixing the hierarchy: indigenous rights are the baseline; the national home is a conditional, subordinate project.
% TRANSFER_FUNCTION: Moves land alienation rights from Arab owners to Jewish purchasers — restricted by Land Transfer Regulations (1940). Moves demographic control from open immigration to quota-based certificates — capped by 1939 White Paper (75,000 over 5 years, then Arab acquiescence required). Moves political sovereignty from British administration toward Arab majority representative government — delayed by mandate terms. Moves administrative discretion from British officials to constrained mandate interpretation — supervised by PMC.
% ABSENT_VOICES: Palestinian peasantry (fellahin) — landless cultivators most affected by tenure protections and land sales, excluded from Arab Executive representation. Jewish refugees from Nazi Europe (1933-45) — denied entry by immigration quotas, no representation in mandate governance. Arab nationalist movements in neighboring states (Syria, Iraq, Egypt) — support Palestinian cause but operate outside mandate framework. British anti-appeasement voices (Churchill, Amery) — argue mandate requires Jewish statehood, excluded from Colonial Office policy after 1939.
% DISAPPEARANCE_RATIONALE: If the dual obligation constraint vanished overnight (1939 White Paper revoked, Land Transfer Regulations lifted, PMC oversight removed), Jewish land acquisition would accelerate rapidly (JNF had capital, demand existed), immigration would surge (European refugees, post-war displaced persons), and the demographic balance would shift decisively toward Jewish majority within a decade. Arab political claims to representative government based on majority status would collapse. The 1947 UN partition would likely produce a larger Jewish state with fewer Arab citizens. The entire trajectory of 1948 war, refugee creation, and subsequent conflict would restructure.
% FOUNDING_PROBLEM: Post-WWI settlement of Ottoman Arab territories: how to administer former Ottoman provinces under international law while managing competing nationalist claims (Arab independence promised by McMahon-Hussein correspondence, Jewish national home pledged by Balfour Declaration, French/British imperial interests in Sykes-Picot). The mandate system was the coordination mechanism — 'tutelage' toward self-determination — but the Palestine mandate uniquely embedded a contradictory obligation (national home) within the general mandate framework (protecting existing populations).
% FOUNDING_PROBLEM_CORROBORATION: League of Nations Permanent Mandates Commission annual reports (1921-1939) consistently interpret mandate as requiring Arab self-government progression and land protection — independent verification. Shaw Commission (1929) and Hope Simpson Report (1930) — British-appointed inquiries concluding land alienation and immigration threaten Arab rights, corroborating dual obligation reading. Palestinian Arab delegations to London conferences (1939, 1947) — attest problem remains live. Zionist leadership (Ben-Gurion, Weizmann) — attest founding problem solved by 1948 state creation; dual obligation was temporary scaffold. Historians (Khalidi, Morris, Segev, Pappé) — debate whether mandate structure made conflict inevitable or whether British policy choices determined outcome.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint actively transfers land access, demographic control, and political sovereignty from Zionist organizations to Arab communities — a zero-sum redistribution enforced by mandatory law. Suppression (0.68) reflects the coercive apparatus needed: land transfer enforcement, immigration interception (naval patrols, detention camps like Atlit), military repression of Arab revolt (1936-39), and restriction of Jewish paramilitary activity. Theater ratio (0.28) is low-moderate: the mandate's coordination function (international legal oversight, minority protection, administrative modernization) is real and not merely performative, but the dual obligation's enforcement increasingly serves extraction (protecting Arab majority status against demographic change) rather than pure coordination. Accessibility collapse (0.55) is moderate: alternatives existed (binational state proposals, partition, cantonal autonomy) but collapsed under pressure from both national movements and British policy exhaustion. Resistance (0.78) is high: Zionist organizations pursued diplomatic, legal, and paramilitary resistance to restrictions; Arab Revolt (1936-39) was direct armed resistance to mandate implementation; British administration resisted both sides' maximal demands.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (Arab elites, communities) experience the constraint as genuine coordination — international law protecting indigenous rights against colonial settlement. The payer seats (Zionist organizations, Jewish settlers) experience it as enforced extraction — mandate law weaponized to block their national project. The agenda-setter seat (British administration) experiences it as impossible administration — contradictory legal obligations requiring constant policy improvisation. The engine computes this divergence from the structural data: beneficiaries have identity-locked exit and organized power; payers have mobile/constrained exit and powerful/moderate power; the same legal text produces opposite lived realities.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab elites and communities are structural beneficiaries (d near 0.0-0.2): the constraint legally protects their land tenure, caps demographic displacement, and grounds their sovereignty claim. Their exit is constrained/identity-locked — they cannot leave the mandate territory, their identity is fused to the land. Zionist organizations are structural targets (d near 0.8-0.9): they bear the extraction (blocked land purchases, capped immigration, denied institutional supremacy). Their exit is mobile — global fundraising, diplomatic channels, paramilitary capacity, and eventual state creation outside mandate framework. British administration sits near symmetric (d ~0.5): they administer the constraint (agenda_setter) but bear costs (military, administrative, diplomatic) and are constrained by contradictory mandate terms. Jewish settlers are payers (d ~0.7): directly subject to immigration/land restrictions with constrained exit (European persecution, British enforcement). PMC is analytical observer (d=0.5): no extraction, no benefit, pure oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The dual obligation reading prevents mislabeling: it is not pure coordination (rope) because extraction is asymmetric and enforced; not pure extraction (snare) because the coordination function (minority protection, international legal oversight, administrative modernization) is genuine and benefits identifiable parties; not a mountain because it is a constructed legal arrangement requiring active enforcement; not a scaffold because it lacks a sunset clause and its justification is not transitional; not a piton because it was actively maintained and contested until termination, not maintained theatrically after functional atrophy. The founding problem (post-Ottoman settlement with competing claims) remains contested — Arab side says live, Zionist side says resolved by 1948, historians debate — so mandatrophy is unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_obligation_genuine_vs_cover,
    'Was the dual obligation (Arab rights superior to national home) a genuine coordination commitment by the League and British, or a diplomatic cover for imperial management of conflicting promises?',
    'Archival analysis of League Council deliberations (1920-22), Colonial Office internal memos, and Balfour/Weizmann correspondence vs. McMahon-Hussein correspondence. Compare PMC independent observations with British mandatory reports.',
    'If genuine coordination, the tangled_rope classification holds — real coordination function with asymmetric extraction. If diplomatic cover, the constraint is a snare: the coordination story masks extraction (British imperial control via divide-and-rule, using Arab rights as lever against Zionist demands, using national home as lever against Arab independence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_obligation_genuine_vs_cover, conceptual, 'Whether the dual obligation''s coordination function is authentic or instrumental.').

omega_variable(
    british_subversion_vs_implementation,
    'Did the British mandatory administration genuinely attempt to implement the dual obligation, or did it systematically subvert Arab protections while facilitating the national home?',
    'Quantitative analysis: land transfer statistics (Jewish vs. Arab purchases 1920-48), immigration certificates issued vs. quotas, Arab vs. Jewish representation in advisory councils, military expenditure on Arab Revolt vs. Zionist paramilitary tolerance. Qualitative: High Commissioner correspondence, Colonial Office minutes, PMC critiques.',
    'If systematic subversion, the constraint''s effective extraction on Arab communities is higher than the dual obligation reading suggests — the mandate operated as a snare on Arabs (rights proclaimed but violated) and a rope on Zionists (facilitation disguised as restriction). If genuine implementation attempt, the tangled_rope classification holds with British as constrained agenda_setter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(british_subversion_vs_implementation, empirical, 'Whether British administration was a faithful enforcer or a strategic subverter of the dual obligation.').

omega_variable(
    land_tenure_protection_effectiveness,
    'How effective were the Land Transfer Regulations (1940) and earlier restrictions in actually protecting Arab peasant tenure versus merely slowing elite land sales?',
    'Land registry data (1940-48): volume of Arab-to-Jewish transfers in each zone (Zone A prohibited, Zone B restricted, Zone C free). Peasant dispossession rates via debt/foreclosure vs. voluntary sale. Comparison with Transjordan (no Jewish land purchase) and Syria/Lebanon (French mandate, different land law).',
    'If regulations primarily blocked elite sales while peasants lost land to debt/taxation, the dual obligation''s coordination function for the majority Arab population is weaker — extraction on peasants continues via economic mechanisms not addressed by mandate. If regulations broadly protected tenure, coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(land_tenure_protection_effectiveness, empirical, 'Whether land protections reached the most vulnerable Arab cultivators or only elite owners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1922, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_dual_obligation_tr_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1922, 0.15).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1925, 0.18).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1929, 0.22).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1930, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1930, 0.25).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1936, 0.28).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1939, 0.28).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1942, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1942, 0.28).
narrative_ontology:measurement(balfour_dual_obligation_tr_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1948, 0.28).

% Extraction over time
narrative_ontology:measurement(balfour_dual_obligation_be_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1922, 0.45).
narrative_ontology:measurement(balfour_dual_obligation_be_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1925, 0.48).
narrative_ontology:measurement(balfour_dual_obligation_be_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1929, 0.55).
narrative_ontology:measurement(balfour_dual_obligation_be_t1930, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(balfour_dual_obligation_be_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1936, 0.65).
narrative_ontology:measurement(balfour_dual_obligation_be_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1939, 0.7).
narrative_ontology:measurement(balfour_dual_obligation_be_t1942, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1942, 0.71).
narrative_ontology:measurement(balfour_dual_obligation_be_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1948, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(balfour_dual_obligation_su_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1922, 0.4).
narrative_ontology:measurement(balfour_dual_obligation_su_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1925, 0.45).
narrative_ontology:measurement(balfour_dual_obligation_su_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1929, 0.55).
narrative_ontology:measurement(balfour_dual_obligation_su_t1930, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(balfour_dual_obligation_su_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1936, 0.75).
narrative_ontology:measurement(balfour_dual_obligation_su_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1939, 0.7).
narrative_ontology:measurement(balfour_dual_obligation_su_t1942, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1942, 0.68).
narrative_ontology:measurement(balfour_dual_obligation_su_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1948, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, un_partition_resolution_181).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, israeli_land_law_1960).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_refugee_status_unrwa).

% DUAL FORMULATION NOTE:
% This constraint is one member of the balfour_mandate_instruments constraint family (kernel_id: balfour_mandate_instruments). The family has three readings: dual_obligation_indigenous_rights (this story, ε=0.72 tangled_rope), jewish_national_home_primacy (ε=0.68 snare/tangled_rope), mandatory_interpretive_discretion (ε=0.55 piton/rope). The dual obligation reading's ε is higher because its enforcement machinery actively restricts Zionist demographic goals while the primacy reading's extraction falls on Arab communities. The readings are linked by shared mandate text but diverge on which obligation is primary — the ε-invariance principle requires separate stories because the referent (the mandate's operational effect) differs by reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
