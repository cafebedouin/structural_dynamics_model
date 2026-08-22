% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Zionist Settlement as Settler-Colonial Displacement Regime
 *   domain: political_philosophy/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint instantiates the settler-colonial reading of Zionism:
 *   that Jewish immigration to Palestine, regardless of the refugees'
 *   legitimate flight from European antisemitism, institutionalizes a
 *   structural pattern of land dispossession, demographic replacement, and
 *   indigenous displacement identical to European settler colonialism in
 *   Australia, North America, and southern Africa. The reading holds that the
 *   arrangement benefits primarily imperial powers (Britain, U.S.) while
 *   extracting the full cost of displacement from Palestinians. Jewish
 *   immigrants are positioned as both refugees (fleeing persecution) and
 *   settlers (implementing dispossession) — a dual position that
 *   identity-locks them into the structure regardless of individual intent.
 *   Palestinians are positioned as powerless targets of a zero-sum
 *   territorial logic. The constraint persists through active enforcement:
 *   military occupation, settlement expansion, refugee exclusion, and
 *   international political support from Western powers. The reading does NOT
 *   deny Jewish vulnerability to antisemitism; it holds that vulnerability
 *   was addressed through a colonial pattern rather than through coexistence
 *   or power-sharing arrangements.
 *
 * KEY AGENTS:
 *   - jewish_immigrants_and_settlers: displaced persons and refugees implementing the settlement project; identity-locked into the role of settlers by their own safety needs and nationalist framing
 *   - palestinian_arabs: indigenous inhabitants experiencing progressive land loss, exclusion, and displacement; trapped by military occupation and international powerlessness
 *   - british_imperial_interests: initial beneficiary; uses Jewish settlement to strengthen strategic position and imperial control
 *   - us_imperial_interests: later beneficiary; backs Israeli military capacity to anchor regional dominance
 *   - zionist_movement_leadership: agenda-setters; frame settlement as fulfilling self-determination while structurally implementing dispossession
 *   - palestinian_refugees: ultimate victims; displaced, denied return, held in permanent limbo
 *   - liberal_nationalist_counterreading: competing interpretive position; absent voice in the architectural process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.82).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.79).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Zionist Settlement as Settler-Colonial Displacement Regime").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political_philosophy/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, 'b3d5903a-8198-4a82-aff2-97a9d4283e21').
narrative_ontology:cs_kernel_codification('b3d5903a-8198-4a82-aff2-97a9d4283e21', formalized).
narrative_ontology:cs_authority_grounding('b3d5903a-8198-4a82-aff2-97a9d4283e21', extraction).
narrative_ontology:cs_interpretation_layer_present('b3d5903a-8198-4a82-aff2-97a9d4283e21').
narrative_ontology:cs_reading_relation('b3d5903a-8198-4a82-aff2-97a9d4283e21', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('b3d5903a-8198-4a82-aff2-97a9d4283e21', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('b3d5903a-8198-4a82-aff2-97a9d4283e21', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_reading_relation('b3d5903a-8198-4a82-aff2-97a9d4283e21', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('b3d5903a-8198-4a82-aff2-97a9d4283e21', foundational, settler_colonialism_structural_inevitability).
narrative_ontology:cs_axiom_status(settler_colonialism_structural_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('b3d5903a-8198-4a82-aff2-97a9d4283e21', settler_colonialism_structural_inevitability, empirically_contingent).
narrative_ontology:cs_axiom('b3d5903a-8198-4a82-aff2-97a9d4283e21', foundational, displacement_unjustifiable_by_refugee_status).
narrative_ontology:cs_axiom_status(displacement_unjustifiable_by_refugee_status, holdable).
narrative_ontology:cs_axiom_grounding('b3d5903a-8198-4a82-aff2-97a9d4283e21', displacement_unjustifiable_by_refugee_status, deontological).
narrative_ontology:cs_reference_frame('b3d5903a-8198-4a82-aff2-97a9d4283e21', indigenous_palestinian_territorial_continuity_and_equal_rights).
narrative_ontology:cs_drift_state('b3d5903a-8198-4a82-aff2-97a9d4283e21', contemporary_post_2000_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b3d5903a-8198-4a82-aff2-97a9d4283e21', '2026-06-11T14:32:18Z').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_interests).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_and_settlers).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish immigrants, many fleeing European persecution, establish communities and institutions in Palestine under British Mandate. They are simultaneously the implementing agents of the settlement project (conducting land acquisition, building institutions, establishing armed forces) and trapped within a structural logic they may not fully articulate: their escape from European antisemitism becomes the mechanism of Palestinian displacement. Their identity as refugees and as Jewish nationals becomes inseparable from their role as settlers. They cannot exit without abandoning the territorial claim that their refuge depends on.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_and_settlers, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_and_settlers, payer).

% The indigenous Arab population of Palestine, in place for centuries, faces progressive land acquisition, exclusion from Jewish-majority communities and institutions, and eventually forced displacement. Their structural position is that of subjects to be removed: they are not invited into the settler project, not offered integration, not recognized as having prior claim. Their territory becomes the object of zero-sum competition. They bear the full cost of displacement while possessing minimal institutional power to resist or negotiate terms.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, regional).

% Britain, holding the League of Nations Mandate for Palestine after WWI, initially frames itself as a neutral administrator but benefits from settler expansion: Jewish settlement creates a friendly allied population, strengthens British strategic position in the region, creates economic activity and tax revenue, and aligns with Britain's broader imperial interests in controlling Middle Eastern resources and geopolitics. Britain does not pay the cost of displacement; Palestinians do. The arrangement subsidizes British power.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests, agenda_setter).

% The United States, especially after WWII, inherits Britain's strategic role and benefits from a Jewish-majority state as a regional ally, a counterweight to Soviet influence, and an anchor for Western interest in Middle Eastern oil and geopolitics. U.S. military and economic aid becomes the primary life-support for the Israeli state's military capability. The U.S. gains strategic positioning and regional leverage; Palestinians pay in displacement, refugee camps, and blocked return.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_interests, beneficiary,
    institutional, generational, arbitrage, global).

% Palestinians expelled or fleeing during 1948 and subsequent conflicts live in refugee camps in neighboring countries or in fragmented territorial enclaves (West Bank, Gaza), denied right of return by Israeli law. They are the ultimate victims of the displacement logic: forced out, held in limbo, treated as permanent outsiders in their own region. Their condition is structurally maintained: return is forbidden, resettlement in host countries is blocked, and they carry statelessness as a permanent condition.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% The organizational leadership of the Zionist movement (Jewish Agency, later Israeli state structures) sets the terms of settlement: determines land acquisition strategy, establishes Jewish-only communities, creates military and security institutions, defines boundaries of Jewish identity and citizenship. They frame the settlement project as fulfilling Jewish national self-determination and building a safe haven; the reading under examination holds this framing masks a structural displacement logic that operates regardless of intent.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, zionist_movement_leadership, agenda_setter,
    organized, generational, identity_locked, regional).

% UN bodies, international law mechanisms, and postcolonial nations repeatedly object to settlement expansion, issue resolutions (often vetoed by U.S. in Security Council), recognize Palestinian statehood claims, and document displacement as a rights violation. They are excluded from enforcing remedies by the military and economic power backing Israeli statehood and the U.S./Western commitment to Israel's existence and security.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_community, excluded,
    institutional, biographical, analytical, global).

% The competing reading within the kernel that frames Jewish statehood as a legitimate exercise of collective self-determination in an ancestral homeland. This reading does not occupy a seat in the world but rather a contending interpretive position: it asserts that the arrangement is coordination (enabling Jewish safety and self-governance) rather than extraction (displacement of Palestinians). This observer position is noted to clarify that the settler-colonial reading is one of several coherent framings of the same kernel.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, liberal_nationalist_counterreading, observer,
    analytical, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(jewish_sovereignty_palestine__settler_colonial_reading, liberal_nationalist_counterreading).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solves the problem of Jewish political safety and sovereignty through territorial concentration and majority status in a defined homeland — creating a Jewish-majority state where Jewish security is embedded in institutional control.
% TRANSFER_FUNCTION: Transfers Palestinian land, political rights, and territorial access from Palestinian inhabitants to Jewish settlers and the Israeli state. Transfers strategic regional position from local Palestinian actors to Western imperial powers (Britain, U.S.). Transfers the cost of displacement entirely to Palestinians while distributing benefits to Jewish settlers, imperial powers, and Western strategic interests.
% ABSENT_VOICES: Palestinian intellectual and political leadership were structurally excluded from the settlement process: they were not invited to negotiations over the Mandate, not represented in Jewish-majority institutions, not asked to consent to demographic transformation of their homeland. Indigenous Palestinian voices calling for equal political participation and territorial continuity remain unheard in the institutions that shape the arrangement. International postcolonial voices and Global South constituencies oppose settlement expansion but are structurally excluded from enforcement by Western military and economic power.
% DISAPPEARANCE_RATIONALE: If the Israeli state and the settlement structure vanished overnight, Palestinian territorial continuity would be restored, refugee return would become possible, and Palestinian political self-determination would no longer be structurally blocked. The arrangement is not a natural-law feature — it is an active institutional construction that requires continuous enforcement (military occupation, settlement expansion, refugee exclusion, restricted movement). Its disappearance would reshape Middle Eastern geopolitics, remove a strategic anchor from Western regional interests, and restore Palestinians to the position of primary actors in their own homeland rather than permanent subjects of displacement.
% FOUNDING_PROBLEM: The founding problem stated by Zionist framing: European antisemitism and Jewish vulnerability created an urgent need for a Jewish national homeland where Jews could achieve political self-determination and escape persecution. The founding problem stated by settler-colonial analysis: the same legitimate desire for safety and self-determination was institutionalized through a colonial pattern that displaced an indigenous population rather than seeking coexistence or power-sharing.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians and movement leaders attest that antisemitism and the Holocaust created the founding necessity. International courts and postcolonial scholars attest that the founding problem was real but the solution enacted follows the structural logic of settler colonialism: land acquisition from indigenous populations, demographic replacement, cultural erasure, and justification through narratives of civilization/development. Palestinian historians and human rights organizations attest that the founding problem of Jewish safety was pursued at the cost of Palestinian displacement. Holocaust survivor testimonies document the urgency of Jewish refuge; Palestinian refugee testimonies document the consequence of the refuge-seeking solution. The founding problem exists in both tellings but is resolved differently depending on which reading governs.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the arrangement is zero-sum: Palestinian land, rights, and territorial access flow entirely to settlers and the Israeli state. There is no mutual gain or coordinate benefit — Palestinians lose what Jews gain. Suppression is high (0.79) because persistence depends on active military enforcement: occupation, settlement expansion blockade on Palestinian development, refugee law, movement restrictions, and the continuous threat of force. Theater (0.41) is moderate: some genuine security infrastructure exists (Israel does face external threats), but an increasing share of enforcement activity maintains demographic dominance and settlement expansion rather than responding to external security threats. Accessibility_collapse is high (0.76 at interval end): for Palestinians, alternatives to accepting Israeli state authority have collapsed — they cannot return to their homeland, cannot establish independent territorial state free of occupation, cannot migrate freely. The measurement series traces the escalation from 1900 (early settlement) through 1948 (establishment of Israeli state, major Palestinian displacement) to 2024 (occupation, settlement expansion, refugee exclusion institutionalized). The trajectory shows monotonic increase in extractiveness and suppression with theater holding relatively stable after 1967 (indicating that the settlement structure shifted from expansion-focused to maintenance-focused, but did not reduce enforcement). The leveled coercion grid shows that suppression and stakes differ substantially across levels: individual Palestinians face the highest concrete suppression (checkpoints, restrictions, displacement risk); organizational Palestinian actors face coordination barriers and military threat; class-level Palestinian resistance exists but lacks institutional power to block settlement or enforce rights; structural-level international opposition exists but is blocked by Western veto power. This leveling shows the suppression is not uniform — it concentrates on the most powerless agents.
 *
 * PERSPECTIVAL GAP:
 *   The settler-colonial reading projects dramatically different classifications for different stakeholders. From the Jewish immigrant/Israeli seat: the arrangement is a hard-won achievement of self-determination and security, justified by refugee necessity and existential threat — they would classify it as rope (genuine coordination toward shared safety) or tangled_rope at worst. From the Palestinian seat: the same arrangement is pure extraction and dispossession — they classify it as snare. From the imperial-power seat (Britain, U.S.): the arrangement is beneficial — extraction of strategic advantage, regional position, and leverage at no cost to imperial interests — also a form of snare but one from which they profit. The engine computes these divergences from the structural data: Jewish immigrants hold institutional power and territorial control (d high, beneficiary axis); Palestinians are powerless and dispossessed (d very high, target axis); imperial powers gain without losing (d near 0, subsidy axis). The claim here is SNARE because the arrangement is analyzed as pure extraction with cover story (security, self-determination) masking structural dispossession.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish immigrants/Israeli state: beneficiary on one axis (territorial control, state sovereignty, refugee safety), target on another (trapped within a structure that requires perpetual military enforcement and demographic dominance to maintain). The reading here treats them primarily as agenda-setters implementing dispossession, with directionality d near 0.65 (moderate-to-high target position, pulled down by their institutional power and territorial control). Palestinians: maximum-target directionality (d near 0.95) — powerless, dispossessed, trapped, bearing the full cost. British/U.S. imperial interests: maximum-beneficiary directionality (d near 0.05) — gain strategic advantage at no cost, can arbitrage in and out of the region. Zionist movement leadership: agenda-setter directionality (d near 0.30) — they set the terms but are themselves trapped within an institutional logic that identity-locks them into perpetual enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading DISPUTES the mandate history of Zionism. The liberal-nationalist reading states: founding mandate is still live (Jewish safety and self-determination remain necessary). The settler-colonial reading states: the founding mandate (refugee escape) was legitimate but the solution enacted (territorial displacement) has outlived any justified necessity — it is now maintained as a regime of occupation and settlement expansion that extracts from Palestinians indefinitely. The founding_problem_status of 'contested' reflects that both readings observe Jewish safety needs and Palestinian dispossession but disagree on whether the current structure is a proportionate response (contested_live) or an unjustified perpetuation (contested_dead). The measurement trajectory (extractiveness rising from 1948 to 2024, suppression rising, theater stabilizing) supports the reading that the arrangement shifted from justifiable emergency response to institutionalized extraction — the extraction continued and deepened even as the founding emergency (European antisemitism, immediate refugee crisis) receded. This is mandatrophy in slow motion: the founding problem solved itself (European Jews safe, Jewish state established, Holocaust survivors resettled) but the extraction logic persisted and hardened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_vs_refugee_legitimacy,
    'Can an arrangement be simultaneously a legitimate refugee response to antisemitism AND a structurally extractive settler-colonial displacement? Or are these mutually exclusive descriptions?',
    'Genealogical analysis: if Jewish safety could have been achieved through power-sharing, integration, or territorial sharing arrangements with Palestinian consent, then the settler-colonial pattern was a choice, not a necessity. If no such alternatives were available or viable in the political conditions of the 1920s–1940s, then refuge and displacement are structurally linked. Historical counterfactual analysis examining rejected integration proposals and power-sharing frameworks.',
    'If displacement was avoidable: the arrangement is pure snare masked by refugee narrative. If displacement was the only viable refuge solution available: the arrangement is tragic tangled_rope (genuine coordination need + asymmetric extraction). This does not justify displacement but contextualizes it as path-dependent rather than chosen malice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_vs_refugee_legitimacy, empirical, 'Whether settler-colonialism and refugee legitimacy are logically compatible or mutually exclusive').

omega_variable(
    identity_lock_mechanism_jewish_settlers,
    'Are Jewish settlers trapped in the displacement structure by identity-fusion (their sense of self as Jewish national/refugee inseparably bound to statehood), by institutional lock-in (they cannot exit without abandoning territory/resources), or by genuine security threat (Palestinians and neighbors pose credible existential threat justifying perpetual enforcement)?',
    'Survey research on identity-fusion and exit preferences among Israeli Jews; comparative analysis of security threat levels over time vs. settlement expansion rates; study of populations that exited (Israeli diaspora, Israeli Arabs, settlers who abandoned outposts) and their motivation structure.',
    'If primarily identity-locked: the Jewish-settler position is itself extractive — their identity depends on dispossession and cannot be reformed without identity dissolution, making exit structurally impossible and perpetual enforcement necessary. If primarily institutional: reform is possible through property law and constitutional change. If primarily security-driven: the threat level is the variable — as threat decreases, enforcement should decrease; if enforcement increases despite decreased threat, identity-lock and institutional lock-in are operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_jewish_settlers, empirical, 'Whether Jewish-settler exit-lock is psychological (identity fusion), institutional (property/territory control), or security-driven').

omega_variable(
    structural_vs_constructed_settler_colonialism,
    'Does Zionism instantiate settler-colonialism as a structural inevitability of the political conditions (European nation-state pattern, refugee crisis, weak Palestinian institutional power, imperial dynamics), or as a deliberate ideological choice by the Zionist movement to pursue territorial exclusivity and demographic dominance?',
    'Historiography of Zionist movement internal debates (rejected integration proposals, rejected binational-state frameworks, rejected power-sharing models); comparison to other refugee-receiving contexts (Jewish refugees to U.S., Caribbean, Madagascar proposals); analysis of leadership statements prioritizing Jewish majority and territorial control vs. prioritizing Jewish safety.',
    'If structural inevitability: settler-colonialism is a tragic outcome of conflicting rights claims and power asymmetries, not a malicious choice. If deliberate choice: the settler-colonial reading indicts Zionist leadership for prioritizing ethnic dominance over refugee safety. This affects culpability framing but not the classification itself — the snare persists under either condition, but the causal story differs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_vs_constructed_settler_colonialism, empirical, 'Whether Zionist settler-colonialism was structurally inevitable or ideologically chosen').

omega_variable(
    kernel_reading_alternative_framings,
    'The kernel jewish_sovereignty_palestine can be read through five distinct frameworks. Which reading captures the true structural logic: liberal-nationalist (self-determination right), settler-colonial (displacement regime), post-zionist (mandate completed and outdated), religious-zionist (theological claim), or cultural-zionist (cultural center without political dominance)?',
    'No empirical resolution possible — different readings rest on incommensurable axioms about legitimacy, history, and justice. The readings coexist as live interpretive positions held by different parties. The engine processes this as an axis of irreducible interpretive contention, not a factual dispute resolvable by measurement.',
    'If settler-colonial reading is correct: Zionism is a snare to be dismantled or reformed. If liberal-nationalist reading is correct: Zionism is a rope or tangled_rope (legitimate coordination with tragic costs). If post-zionist reading is correct: Zionism''s mandate is fulfilled; current arrangements persist by inertia (piton). This omega documents that the classification itself is reading-dependent and no single empirical process can adjudicate between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'The kernel-reading indeterminacy: settler-colonial vs. liberal-nationalist vs. other readings are incommensurable interpretive frameworks, not empirically resolvable disputes').

omega_variable(
    imperial_benefit_vs_jewish_security_entanglement,
    'Is U.S./British imperial benefit from Israeli statehood incidental to Jewish security (an external party gaining advantage from solving a Jewish problem), or is imperial benefit CONSTITUTIVE of the arrangement (the arrangement could not exist without imperial backing and would not persist without imperial interest)?',
    'Counterfactual: could Israel have been established and persisted without British Mandate facilitation and subsequent U.S. military/economic support? Cost analysis: what proportion of Israeli military capacity derives from U.S. aid? Behavioral analysis: do U.S. policy positions prioritize Israeli security or U.S. regional interests when they diverge (settlements, Palestinian statehood, refugees)?',
    'If incidental: imperial powers are free-riding beneficiaries of a primarily Jewish-Palestinian arrangement. If constitutive: the arrangement is structurally dependent on imperial interests and cannot be reformed without imperial buy-in. This affects whether the arrangement is a two-party snare (Jews and Palestinians) or a three-party snare (Jews, Palestinians, and imperial powers where powers are the primary extractors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_benefit_vs_jewish_security_entanglement, empirical, 'Whether imperial benefit is incidental or constitutive to Israeli statehood').

omega_variable(
    reading_relation_liberal_nationalist,
    'This settler-colonial reading vs. the liberal-nationalist reading: do they FORECLOSE each other (one''s core premise logically rules out the other''s in any single framework), COEXIST (both remain live as different parties'' positions), or do they have an INFLUENCES relationship (one reading creates structural pressure on the other)?',
    'Axiom analysis: settler-colonial reading rests on ''ethnic-nationalist displacement is inherently unjust structural pattern''; liberal-nationalist rests on ''collective self-determination right justifies territorial sovereignty''. These axioms directly contradict each other in any single framework claiming to adjudicate legitimacy — if self-determination right is valid, ethnic dominance is not unjust; if ethnic dominance is structurally unjust, self-determination does not override indigenous rights. However, both readings are held simultaneously by different parties (Israeli Jews, Palestinian Arabs, international observers) without one side abandoning its axiom. So the logical relation is FORECLOSURE (core premises contradict) but the social relation is COEXISTENCE (both held live).',
    'This omega documents that two readings that logically foreclose each other persist as coexisting social positions, which is itself the marker of an unresolved kernel contention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relation_liberal_nationalist, conceptual, 'Settler-colonial and liberal-nationalist readings: logical foreclosure vs. social coexistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(measurement_theater_t1900, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(measurement_theater_t1920, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(measurement_theater_t1945, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(measurement_theater_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.31).
narrative_ontology:measurement(measurement_theater_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.38).
narrative_ontology:measurement(measurement_theater_t2000, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(measurement_theater_t2024, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(measurement_extractiveness_t1900, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(measurement_extractiveness_t1920, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1920, 0.28).
narrative_ontology:measurement(measurement_extractiveness_t1945, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1945, 0.56).
narrative_ontology:measurement(measurement_extractiveness_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement(measurement_extractiveness_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement(measurement_extractiveness_t2000, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2000, 0.81).
narrative_ontology:measurement(measurement_extractiveness_t2024, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(measurement_suppression_t1900, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement(measurement_suppression_t1920, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1920, 0.25).
narrative_ontology:measurement(measurement_suppression_t1945, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement(measurement_suppression_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(measurement_suppression_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.74).
narrative_ontology:measurement(measurement_suppression_t2000, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2000, 0.77).
narrative_ontology:measurement(measurement_suppression_t2024, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2024, 0.79).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1900, tn=2024
narrative_ontology:measurement(jewi_grid_01, jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse(class), 1900, 0.2).
narrative_ontology:measurement(jewi_grid_02, jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse(class), 2024, 0.81).
narrative_ontology:measurement(jewi_grid_03, jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse(individual), 1900, 0.18).
narrative_ontology:measurement(jewi_grid_04, jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse(individual), 2024, 0.82).
narrative_ontology:measurement(jewi_grid_05, jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse(organizational), 1900, 0.22).
narrative_ontology:measurement(jewi_grid_06, jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse(organizational), 2024, 0.78).
narrative_ontology:measurement(jewi_grid_07, jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse(structural), 1900, 0.16).
narrative_ontology:measurement(jewi_grid_08, jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse(structural), 2024, 0.76).
narrative_ontology:measurement(jewi_grid_09, jewish_sovereignty_palestine__settler_colonial_reading, resistance(class), 1900, 0.52).
narrative_ontology:measurement(jewi_grid_10, jewish_sovereignty_palestine__settler_colonial_reading, resistance(class), 2024, 0.75).
narrative_ontology:measurement(jewi_grid_11, jewish_sovereignty_palestine__settler_colonial_reading, resistance(individual), 1900, 0.55).
narrative_ontology:measurement(jewi_grid_12, jewish_sovereignty_palestine__settler_colonial_reading, resistance(individual), 2024, 0.68).
narrative_ontology:measurement(jewi_grid_13, jewish_sovereignty_palestine__settler_colonial_reading, resistance(organizational), 1900, 0.48).
narrative_ontology:measurement(jewi_grid_14, jewish_sovereignty_palestine__settler_colonial_reading, resistance(organizational), 2024, 0.72).
narrative_ontology:measurement(jewi_grid_15, jewish_sovereignty_palestine__settler_colonial_reading, resistance(structural), 1900, 0.4).
narrative_ontology:measurement(jewi_grid_16, jewish_sovereignty_palestine__settler_colonial_reading, resistance(structural), 2024, 0.65).
narrative_ontology:measurement(jewi_grid_17, jewish_sovereignty_palestine__settler_colonial_reading, stakes_inflation(class), 1900, 0.14).
narrative_ontology:measurement(jewi_grid_18, jewish_sovereignty_palestine__settler_colonial_reading, stakes_inflation(class), 2024, 0.8).
narrative_ontology:measurement(jewi_grid_19, jewish_sovereignty_palestine__settler_colonial_reading, stakes_inflation(individual), 1900, 0.12).
narrative_ontology:measurement(jewi_grid_20, jewish_sovereignty_palestine__settler_colonial_reading, stakes_inflation(individual), 2024, 0.79).
narrative_ontology:measurement(jewi_grid_21, jewish_sovereignty_palestine__settler_colonial_reading, stakes_inflation(organizational), 1900, 0.15).
narrative_ontology:measurement(jewi_grid_22, jewish_sovereignty_palestine__settler_colonial_reading, stakes_inflation(organizational), 2024, 0.81).
narrative_ontology:measurement(jewi_grid_23, jewish_sovereignty_palestine__settler_colonial_reading, stakes_inflation(structural), 1900, 0.1).
narrative_ontology:measurement(jewi_grid_24, jewish_sovereignty_palestine__settler_colonial_reading, stakes_inflation(structural), 2024, 0.75).
narrative_ontology:measurement(jewi_grid_25, jewish_sovereignty_palestine__settler_colonial_reading, suppression(class), 1900, 0.12).
narrative_ontology:measurement(jewi_grid_26, jewish_sovereignty_palestine__settler_colonial_reading, suppression(class), 2024, 0.8).
narrative_ontology:measurement(jewi_grid_27, jewish_sovereignty_palestine__settler_colonial_reading, suppression(individual), 1900, 0.1).
narrative_ontology:measurement(jewi_grid_28, jewish_sovereignty_palestine__settler_colonial_reading, suppression(individual), 2024, 0.81).
narrative_ontology:measurement(jewi_grid_29, jewish_sovereignty_palestine__settler_colonial_reading, suppression(organizational), 1900, 0.14).
narrative_ontology:measurement(jewi_grid_30, jewish_sovereignty_palestine__settler_colonial_reading, suppression(organizational), 2024, 0.79).
narrative_ontology:measurement(jewi_grid_31, jewish_sovereignty_palestine__settler_colonial_reading, suppression(structural), 1900, 0.08).
narrative_ontology:measurement(jewi_grid_32, jewish_sovereignty_palestine__settler_colonial_reading, suppression(structural), 2024, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__settler_colonial_reading, 0.18).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_territorial_self_determination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, middle_east_imperial_competition).

% DUAL FORMULATION NOTE:
% This settler-colonial reading of jewish_sovereignty_palestine is one of five readings of a contested kernel. The kernel is the legitimacy claim that Jewish sovereignty in Palestine/Israel is a valid exercise of collective self-determination and refugee rights. This reading analyzes the kernel through postcolonial theory, holding that the instantiated structure follows the logic of European settler-colonialism: land acquisition from indigenous populations, demographic replacement, cultural erasure, and justification through civilizational narratives. Sibling readings (liberal-nationalist, religious-zionist, post-zionist, cultural-zionist) analyze the same kernel through different axioms and produce different ε values and beneficiary structures. Each reading is a separate constraint story with its own classification. The readings are linked through network.affects_constraints because they compete for legitimacy over the same institutional kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__settler_colonial_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
