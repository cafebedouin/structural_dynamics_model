% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Ethnic-National State Framework (Post-Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This is the post-Zionist reading of the jewish_sovereignty_palestine
 *   kernel. The post-Zionist interpretation holds that the Zionist project
 *   successfully achieved its founding aim — Jewish statehood and security —
 *   but that the ethnic-national framework and legitimacy narratives that
 *   achieved statehood now obstruct the transition to civic equality and
 *   regional integration. The reading accepts Jewish statehood as historical
 *   fact and (in many formulations) as legitimate exercise of
 *   self-determination, but argues the state's ongoing ethnic-national
 *   character, Law of Return asymmetry, and territorial expansion constitute
 *   structural extraction from Palestinian populations. This reading coexists
 *   with liberal-nationalist readings (which defend ethnic statehood as
 *   permanently necessary for security) and settler-colonial readings (which
 *   dispute the legitimacy of the entire state project). It differs from
 *   both: unlike liberals, it identifies ethnic-national privilege as the
 *   problem rather than as permanent security necessity; unlike
 *   settler-colonial critics, it does not necessarily call for elimination of
 *   a Jewish state, but for de-ethnicization of state institutions.
 *
 * KEY AGENTS:
 *   - Jewish citizens of Israel (beneficiary, agenda-setter; institutional power; hold state apparatus; benefit from Law of Return and land access asymmetries)
 *   - Israeli Palestinians (payer; moderate power; constrained exit; excluded from civic equality despite formal rights)
 *   - Occupied West Bank Palestinians (payer; powerless; trapped; bear enforcement costs of military occupation and settlement)
 *   - Gaza Palestinians (payer; powerless; trapped; excluded entirely; bear costs of blockade and exclusion)
 *   - Liberal Zionist institutional voices (beneficiary; powerful; mobile; defend self-determination framing)
 *   - Palestinian resistance movements (excluded; organized; trapped; structurally barred from negotiating framework)
 *   - International human rights observers (observer; moderate power; analytical standing only; document discrimination patterns)
 *   - Diaspora Jewish communities (beneficiary; organized; mobile; benefit symbolically and materially from ethnic-state framework)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.68).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.71).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Ethnic-National State Framework (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '0a2ed5cd-a337-4631-aa02-d5604e5eb6f6').
narrative_ontology:cs_kernel_codification('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', formalized).
narrative_ontology:cs_authority_grounding('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', extraction).
narrative_ontology:cs_interpretation_layer_present('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6').
narrative_ontology:cs_reading_relation('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_axiom('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', foundational, jewish_statehood_historical_achievement).
narrative_ontology:cs_axiom_status(jewish_statehood_historical_achievement, holdable).
narrative_ontology:cs_axiom_grounding('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', jewish_statehood_historical_achievement, empirically_contingent).
narrative_ontology:cs_axiom('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', foundational, ethnic_national_framework_now_obstructive).
narrative_ontology:cs_axiom_status(ethnic_national_framework_now_obstructive, holdable).
narrative_ontology:cs_axiom_grounding('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', ethnic_national_framework_now_obstructive, deontological).
narrative_ontology:cs_axiom('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', secondary, civic_equality_incompatible_with_ethnic_privilege).
narrative_ontology:cs_axiom_status(civic_equality_incompatible_with_ethnic_privilege, holdable).
narrative_ontology:cs_axiom_grounding('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', civic_equality_incompatible_with_ethnic_privilege, deontological).
narrative_ontology:cs_reference_frame('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', zionist_ethnic_national_state_legitimacy).
narrative_ontology:cs_drift_state('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0a2ed5cd-a337-4631-aa02-d5604e5eb6f6', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, liberal_zionist_institutional_voices).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, jewish_people_self_determination_principle).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, state_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish citizens benefit from Law of Return (automatic immigration right), preferential land access through quasi-state agencies, political representation weighted toward Jewish majority, and state institutions justified through Jewish ethnic-national narrative. The state apparatus was founded to serve Jewish national aspirations; they are simultaneously beneficiaries of the constraint and (through institutional dominance) agenda-setters who maintain it. Exit would mean accepting civic equality that does not privilege Jewish collective claims.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel, agenda_setter).

% Palestinian citizens of Israel hold formal equal rights but operate within a state apparatus whose foundational legitimacy rests on Jewish ethnic-national self-determination. They face systematic underinvestment in Arab municipalities, exclusion from land-access mechanisms tied to Jewish immigration, and institutional structures that treat them as exceptional to the state's core purpose. Exit means emigration or resignation; political voice within the framework is constrained by the framework's asymmetric foundations.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinians, payer,
    moderate, biographical, constrained, national).

% Live under military administration and partial Palestinian self-governance in an indefinite occupation. Their legal status, resource access, and territorial control are governed by Israeli security doctrine that extends the state's ethnic-national framework into occupied territory. They bear the constraint's enforcement costs directly: restriction of movement, settlement expansion, administrative detention, and denial of right of return. Exit is physically and legally barred.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).

% Subject to blockade and periodic military incursion by Israel, justified partly through the security imperatives of maintaining Jewish-majority statehood. They are excluded from Israeli civic participation entirely. The constraint's enforcement in Gaza operates through exclusion: they bear the cost of resource restriction and militarized boundaries without access to the state institutions or civic forums that might alter the arrangement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinians, payer,
    powerless, immediate, trapped, regional).

% International and domestic liberal voices who defend Jewish statehood as a legitimate exercise of self-determination right, arguing the state can evolve toward civic equality without abandoning its Jewish-national character. They benefit rhetorically (the state framework vindicates their self-determination principle) and institutionally (they occupy university, legal, and policy positions where self-determination doctrine is canonical). They have the exit option of repositioning their defense or emigrating.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, liberal_zionist_institutional_voices, beneficiary,
    powerful, generational, mobile, global).

% Palestinian political movements (Fatah, Hamas, other factions) are structurally excluded from negotiating the framework's foundational terms; negotiations are bounded by the constraint that Jewish statehood and demographic security are non-negotiable Israeli positions. They resist the constraint from outside institutional forums, with limited access to mechanisms that could alter state legitimacy narratives.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_resistance_movements, excluded,
    organized, generational, trapped, regional).

% UN bodies, NGOs, and other accountability mechanisms investigate whether the state's ethnic-national framework and its territorial application constitute discrimination or apartheid. They have analytical standing but limited enforcement power. Their role is to document structure and offer remedial frameworks that would require dismantling or fundamentally reconstructing the ethnic-national constraint.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_observers, observer,
    moderate, biographical, analytical, global).

% Diaspora Jewish institutions and communities benefit symbolically (the state embodies Jewish collective aspiration and provides immigration safety valve) and materially (funding channels, educational institutions, cultural legitimacy). They have the option of reorienting their identity and resources toward non-ethnic frameworks or emigrating; many actively support the state precisely because its ethnic-national foundation aligns with their own diaspora self-conception.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national self-determination and collective security: founding Israel as a Jewish-majority state solved historical problem of Jewish statelessness and vulnerability to persecution, providing framework for Jewish immigration, cultural institutions, and political sovereignty.
% TRANSFER_FUNCTION: Transfers civic rights, land access, and institutional legitimacy from Palestinians (Israeli citizens, occupied populations, refugees) to Jewish citizens and Jewish collective identity. Law of Return privileges Jewish immigration; land mechanisms channel territory toward Jewish settlement; institutions justify policy through Jewish demographic security.
% ABSENT_VOICES: Palestinian refugees displaced in 1948 and their descendants are structurally excluded — they would argue for right of return, civic equality independent of ethnic identity, and territorial restitution, but these are treated as existential threats rather than legitimate framework-negotiation positions.
% DISAPPEARANCE_RATIONALE: If ethnic-national framework disappeared, the state would transform to civic-democratic structure with equal political standing for Palestinians and Jews, migration rights independent of ethnicity, and institutional legitimacy based on democratic choice rather than ethnic narrative. Occupation, settlement, and differential citizenship would require fundamental restructuring.
% FOUNDING_PROBLEM: Jewish historical persecution, statelessness, and vulnerability to pogroms and genocide — need for politically sovereign refuge where Jewish collective self-determination could be exercised without dependence on majority populations elsewhere.
% FOUNDING_PROBLEM_CORROBORATION: Historical persecution and Holocaust are corroborated; persistence of founding problem as justification for CURRENT ethnic-national framework is contested. Post-Zionist and Palestinian sources argue the problem is adequately addressed through democratic statehood and international law without ethnic-national privilege. Liberal Zionists argue antisemitism persists. Religious Zionists argue security requires ethnic dominance. No source outside benefiting Jewish institutional structure attests the founding problem justifies current constraint form.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68) because the constraint transfers civic standing and resource access from Palestinians to Jews on the basis of ethnic-national identity rather than residence or democratic choice. The transfer is substantial but not total: Israeli Palestinians retain formal citizenship (though subordinated); occupied Palestinians lack citizenship entirely. Suppression is high (0.71) because maintaining ethnic-national privilege against democratic and human-rights challenges requires active institutional defense: military enforcement of occupation, settlement expansion, selective application of citizenship law, and rhetorical framing of equality demands as threats to Jewish security. Theater is moderate (0.44) because institutional activity genuinely coordinates Jewish security and self-determination (not purely performative) but an increasing share of enforcement effort serves ethnic privilege maintenance rather than security per se — settlement expansion, for instance, is justified through security but operates as territorial extraction. The measurement series shows accumulating extractiveness and suppression over 78 years: the founding constraint was more narrowly targeted (1948 value 0.42) but has broadened to encompass territorial occupation and institutionalized Palestinian subordination. Theater ratio rises as the functional coordination (security, immigration) becomes less necessary and more activity becomes defensive preservation of ethnic privilege.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Jewish institutional leadership), the constraint solves self-determination and security — it is experienced as coordination protecting vulnerability. From the payer seat (Israeli Palestinians and occupied populations), the same structure operates as enforced ethnic hierarchy — they experience the constraint as extraction backed by state violence. From the excluded seat (Palestinian resistance), the framework is illegitimate ex-ante — it embodies displacement rather than self-determination. The engine should compute these divergences from the structural data (beneficiary vs. payer vs. excluded positioning; institutional power asymmetry; trapped vs. mobile exit).
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish citizens sit at d near 0.0 (beneficiary, institutional power, arbitrage exit via global Jewish networks). Israeli Palestinians sit at d near 0.75 (payer, moderate power, constrained by state institutional dependence). West Bank and Gaza Palestinians sit at d near 1.0 (payer, powerless, identity-locked by Palestinian national identity and territorial confinement; trapped exit). Liberal Zionist voices sit at d near 0.15 (beneficiary, powerful, mobile exit — they can reposition or emigrate). The structural data (beneficiary/victim declarations + power + exit options) should drive these directions; no override needed if the declarations are precise.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT mandatrophy (the founding mandate — Jewish statehood and security — remains live and is still pursued). The post-Zionist reading does not claim the mandate is obsolete, but argues the mandate has been operationalized in a form that now extracts from Palestinians rather than merely achieving security. The coordination component (Jewish self-determination, security against persecution) is genuine; the extraction component (ethnic-national privilege, territorial expansion, subordination of Palestinians) has grown over time. Tangled rope classification fits: there is coordination (real security problem solved, real self-determination achieved) and real extraction (ethnic privilege enforced, Palestinian rights subordinated). The ticket to rope would require demonstrating beneficiary and payer status could be decoupled from ethnic identity — i.e., that Jewish security could be achieved through civic equality rather than ethnic institutional privilege. The post-Zionist reading does not establish this decoupling is possible; it argues it is necessary but does not demonstrate sufficiency. Thus tangled rope is appropriate: mixed coordination and extraction with no clear path to disaggregation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Does Jewish historical persecution and vulnerability remain sufficiently acute to justify ethnic-national state privilege in the 21st century, or has international law, diaspora security, and Jewish institutional power adequately addressed the founding problem?',
    'Comparative analysis of Jewish safety and security across diaspora and state contexts; assessment of whether antisemitism and genocide risk remain at levels requiring ethnic-state institutional privilege; examination of whether civic-democratic statehood (without ethnic-national framework) would provide equivalent security.',
    'If the founding problem is substantially resolved, the constraint becomes pure extraction with attenuated coordination justification (road to snare classification). If the problem remains acute, the coordination component justifies tangled-rope framing. This determines whether the state''s ethnic-national character is permanent necessity or historical artifact now obstructing equality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, preference, 'Whether the founding problem (Jewish vulnerability to persecution) persists as justification for current ethnic-national framework.').

omega_variable(
    ethnic_national_vs_civic_security_decoupling,
    'Is Jewish security operationally dependent on ethnic-national state privilege, or could equivalent security be achieved through civic-democratic statehood with robust international law protections and Jewish institutional autonomy?',
    'Comparative historical analysis of security outcomes in ethnic vs. civic states; assessment of whether occupation and settlement expansion genuinely enhance security or create long-term instability; modeling of security outcomes under alternative constitutional frameworks.',
    'If security is decoupled from ethnic privilege, the constraint is reclassified as snare (extraction with attenuated coordination story). If security depends on ethnic institutional dominance, the tangled-rope classification holds. This omega determines whether the path to de-Zionization is security-compatible or security-sacrificing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethnic_national_vs_civic_security_decoupling, empirical, 'Whether Jewish state security operationally requires ethnic-national institutional privilege or whether civic-democratic alternatives could provide equivalent protection.').

omega_variable(
    israeli_palestinian_assimilation_vs_subordination,
    'Are Israeli Palestinians structurally subordinated by ethnic-national framework (extraction victim), or are they integrated citizens whose formal equality is gradually expanding despite institutional heritage from founding period?',
    'Tracking of Palestinian citizen political representation, resource allocation to Arab municipalities, judicial equity in civil rights cases, institutional access, and self-reported perceptions of civic standing over time; comparison to Jewish citizen access and resource distribution.',
    'If integration is increasing and subordination is diminishing, Israeli Palestinians may transition from payer to beneficiary seat and the constraint classification may shift toward rope. If subordination is stable or worsening, the extraction reading holds and the constraint remains tangled rope or becomes snare. This determines the internal Israeli trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(israeli_palestinian_assimilation_vs_subordination, empirical, 'Whether Israeli Palestinians'' position within the ethnic-national framework is improving toward civic equality or remaining structurally subordinated.').

omega_variable(
    occupation_necessity_vs_territorial_expansion,
    'Is the occupation of West Bank and Gaza territory operationally necessary for Jewish state security, or does it constitute territorial expansion driven by ethnic-national settlement ideology independent of security rationale?',
    'Military-strategic analysis of occupation necessity; examination of settlement expansion patterns against security perimeter requirements; comparative analysis of Israeli security metrics before and after 1967 and 1982 territorial expansion.',
    'If occupation is security-necessary, it may be justified within tangled-rope coordination frame (security problem solved). If expansion is driven by ethnic-national ideology independent of security, occupation becomes pure extraction (snare). This determines whether occupied Palestinians are collateral cost of security (tangled rope) or intentional victims of territorial extraction (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_necessity_vs_territorial_expansion, empirical, 'Whether West Bank and Gaza occupation is operationally necessary for Israeli security or driven by ethnic-national settlement ideology.').

omega_variable(
    reading_identity_fusion_lock,
    'For Jewish citizens committed to the post-Zionist reading, what prevents exit from defending ethnic-national statehood? Is the constraint maintained by institutional dependence (jobs, services, institutions depend on state apparatus) or by identity fusion (Jewish identity is fused with state legitimacy)?',
    'Analysis of post-Zionist advocates'' own stated positions on Jewish institutional identity; observation of whether institutional change or emigration becomes more plausible once identity-state fusion relaxes; comparative study of Jewish communities in other civic democracies.',
    'If identity fusion is primary, even post-Zionist advocates may resist de-Zionization despite intellectual critique, and the constraint''s persistence is explained by internalized identity lock rather than purely institutional enforcement. If institutional dependence is primary, constraint removal becomes possible through structural reform without requiring identity dissolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_fusion_lock, empirical, 'For advocates of post-Zionist readings within Israel, whether suppression is structural (institutional dependence) or internalized (identity fusion with ethnic-state).').

omega_variable(
    reading_kernel_contest_status,
    'Does the post-Zionist reading represent a coherent contender in Israeli and global political discourse, or is it a minority intellectual position with limited institutional standing?',
    'Tracking of post-Zionist representation in Israeli media, academia, political parties, and institutional policy; comparison to liberal-nationalist and settler-colonial reading representation; longitudinal analysis of discourse share over time.',
    'If post-Zionist reading gains institutional power, it may influence or foreclose other readings through institutional pressure. If it remains marginal, other readings (liberal nationalist, religious) continue to dominate state policy and constraints classification reflects institutional majority rather than intellectual contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_status, empirical, 'Whether the post-Zionist reading has achieved sufficient institutional standing to influence constraint evolution, or remains a marginal intellectual position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement_basis(jewi_tr_t1948, projected).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1987, 0.28).
narrative_ontology:measurement_basis(jewi_tr_t1987, observed).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t2005, observed).
narrative_ontology:measurement(jewi_tr_t2015, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t2015, observed).
narrative_ontology:measurement(jewi_tr_t2026, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2026, 0.44).
narrative_ontology:measurement_basis(jewi_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.42).
narrative_ontology:measurement_basis(jewi_be_t1948, projected).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.51).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1987, 0.58).
narrative_ontology:measurement_basis(jewi_be_t1987, observed).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement_basis(jewi_be_t2005, observed).
narrative_ontology:measurement(jewi_be_t2015, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement_basis(jewi_be_t2015, observed).
narrative_ontology:measurement(jewi_be_t2026, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(jewi_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.35).
narrative_ontology:measurement_basis(jewi_su_t1948, projected).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.48).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1987, 0.58).
narrative_ontology:measurement_basis(jewi_su_t1987, observed).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(jewi_su_t2005, observed).
narrative_ontology:measurement(jewi_su_t2015, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement_basis(jewi_su_t2015, observed).
narrative_ontology:measurement(jewi_su_t2026, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(jewi_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__post_zionist_reading, 0.18).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, palestinian_right_of_return_constraint).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_civic_equality_constraint).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, occupation_of_west_bank_and_gaza).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, law_of_return_immigration_asymmetry).

% DUAL FORMULATION NOTE:
% The jewish_sovereignty_palestine kernel yields five distinct readings, each with different victim/beneficiary structures and ε values. The post-Zionist reading (this file) treats the ethnic-national framework as the source of extraction; the liberal-nationalist reading defends it as necessary security mechanism; the settler-colonial reading contests the entire state legitimacy; the religious-Zionist reading grounds it in divine promise; the cultural-Zionist reading emphasizes cultural rather than political centrality. Each reading is a separate constraint story with its own ε, stakeholder structure, and type classification. They are linked as a kernel family via network edges and committer-frame omegas. The structural delta is substantial: liberal-nationalist assigns low victimhood to Palestinians; settler-colonial assigns total delegitimacy to Jewish state; religious-Zionist and cultural-Zionist occupy different framing spaces. The post-Zionist reading sits between liberal and settler readings, accepting statehood legitimacy but identifying ethnic framework as the obstruction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
