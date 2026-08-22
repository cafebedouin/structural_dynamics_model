% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionism as European Settler-Colonial Movement (Settler-Colonial Reading)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This story instantiates the settler-colonial reading of the contested
 *   Zionist-legitimacy kernel: the founding and maintenance of Israel as a
 *   Jewish-majority ethno-state is understood as structurally continuous with
 *   other European settler-colonial projects — a metropole-linked settler
 *   population establishing sovereignty over an already-inhabited territory
 *   through the displacement, subordination, or exclusion of the indigenous
 *   population, with that displacement treated as constitutive of the
 *   arrangement's function rather than as regrettable collateral damage. This
 *   is ONE of three readings of the same kernel (zionist_legitimacy_basis);
 *   the national_liberation_reading and religious_restoration_reading are
 *   separate constraint stories with their own ε, beneficiary/victim
 *   structures, and classifications. This story does not describe or average
 *   across those readings — see commentary.kernel_context and the omega
 *   variables for how the readings relate.
 *
 * KEY AGENTS:
 *   - jewish_israeli_settler_population: Primary beneficiary of territorial consolidation and demographic majority (organized/constrained) — benefits from land allocation and security infrastructure built on prior displacement
 *   - israeli_state_apparatus: Agenda-setter administering land, citizenship, and military-governance regimes (institutional/arbitrage)
 *   - palestinian_refugees_1948: Primary historical victims of the founding displacement (powerless/trapped) — denied return under Israeli law despite UNGA 194
 *   - palestinian_residents_present_day: Ongoing targets of differentiated legal regimes read as continuation of the same colonial logic (powerless/trapped)
 *   - settler_colonial_studies_scholars: Analytical observers supplying the comparative typology this reading is built from (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.78).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.72).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionism as European Settler-Colonial Movement (Settler-Colonial Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7').
narrative_ontology:cs_kernel_codification('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', distributed).
narrative_ontology:cs_authority_grounding('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', distributed).
narrative_ontology:cs_reading_relation('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', foundational, colonial_structure_determines_legitimacy).
narrative_ontology:cs_axiom_status(colonial_structure_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', colonial_structure_determines_legitimacy, conventional).
narrative_ontology:cs_axiom('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', foundational, displacement_is_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(displacement_is_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', displacement_is_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_reference_frame('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', pre_1917_ottoman_demographic_baseline).
narrative_ontology:cs_drift_state('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', post_oslo_collapse_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2a9d96a-14cc-4505-9a2d-ab1cc4c719b7', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_settler_population).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, land_development_institutions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_residents_present_day).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, bedouin_communities_negev).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, international_diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, national_self_determination_via_territorial_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Settled and built permanent civic, agricultural, and urban infrastructure on land from which the prior population was displaced or excluded. Benefits from state-backed land allocation, immigration privileges (Law of Return), and security infrastructure. Many arrived as refugees themselves from Europe and Arab states, and experience the arrangement as safety and self-determination rather than extraction — the settler-colonial reading treats this lived experience as real but not as overriding the structural fact of displacement it depends on.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_settler_population, beneficiary,
    organized, generational, constrained, national).

% Administers land registries, military governance over occupied territory, citizenship and residency law, and the demographic-engineering apparatus (settlement expansion, permit regimes, home demolitions) that maintains a Jewish demographic and territorial majority. Sets and enforces the rules; can revise them but derives continued legitimacy and territorial control from not doing so.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Quasi-governmental bodies (national land funds, settlement authorities) that administer land acquired through pre-1948 purchase, 1948 depopulation, and post-1967 occupation. Convert absentee and confiscated land into exclusively or preferentially available land for Jewish settlement, converting historical displacement into an ongoing administrative default.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, land_development_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, land_development_institutions, agenda_setter).

% Displaced during and after 1948 (the Nakba) from villages and towns now inside Israel; denied the right of return by Israeli law despite UN General Assembly Resolution 194. Live in refugee camps and diaspora across the region and world, holding property deeds and keys with no functioning legal path to return or restitution. This displacement is, under this reading, not incidental collateral damage but the constitutive founding act that made the ethno-state's demographic majority possible.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees_1948, payer,
    powerless, civilizational, trapped, regional).

% Live under differentiated legal regimes — citizenship with structural inequality inside Israel, military occupation in the West Bank, blockade in Gaza — that this reading treats as the continuation of a single settler-colonial logic of territorial control and demographic management rather than separate, unrelated policy problems. Exit is foreclosed by permit regimes, checkpoints, and lack of sovereign alternative.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_residents_present_day, payer,
    powerless, generational, trapped, national).

% Indigenous Arab Bedouin communities subject to village demolition, land unrecognition, and forced urbanization/relocation policies inside sovereign Israeli territory, illustrating that displacement dynamics under this reading are not confined to 1948 or the occupied territories but continue as an internal administrative practice.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, bedouin_communities_negev, payer,
    powerless, generational, trapped, regional).

% Benefit from the Law of Return as a standing option and from Israel's existence as a claimed place of ultimate refuge, without directly bearing the costs of displacement or occupation. Largely absent from the internal debate over the structural character of the founding, their support functions as external legitimation for the arrangement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, international_diaspora_jewish_communities, excluded).

% Host large, often unintegrated Palestinian refugee populations for over seven decades, absorbing the humanitarian and political costs of unresolved displacement. Historically instrumentalized refugee status for their own political purposes; largely excluded from any negotiated settlement framework beyond return-of-territory or normalization tracks that bypass the underlying displacement question.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, arab_host_states, excluded,
    organized, generational, constrained, regional).

% Comparative historians and political theorists who situate the founding of Israel within a typology of settler-colonial projects (alongside South Africa, Algeria, Australia, the Americas) based on shared structural features: metropole-linked founding population, replacement or subordination of an indigenous population, and land-centered rather than purely labor-extractive economic logic. They supply the analytical framework this reading is built from.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, settler_colonial_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, diffuse).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the physical safety, immigration absorption, and collective self-governance of a persecuted diaspora population by consolidating them under sovereign control of a bounded territory with dedicated security and legal infrastructure — solving, for the coordinated population, the genuine problem of statelessness and repeated historical persecution.
% TRANSFER_FUNCTION: Moves land, residency rights, water and resource access, and political sovereignty from the indigenous Arab population present in Mandate Palestine to the incoming and established Jewish settler population and the state apparatus built to secure their majority status, principally via the 1948 depopulation, subsequent land confiscation statutes, and post-1967 occupation and settlement expansion.
% ABSENT_VOICES: Palestinian refugees and their descendants are structurally absent from Israeli domestic political processes that determine return, restitution, and citizenship; West Bank Palestinians live under military law with no vote in the legislature that governs them; Gaza residents are excluded from any negotiating table with sovereign standing. Their objection — that the founding required and requires their displacement — is the premise this reading treats as central rather than peripheral.
% DISAPPEARANCE_RATIONALE: If the state and its administrative apparatus disappeared overnight, the settler-colonial reading holds that displaced Palestinian communities would press claims to return and restitution, land and citizenship arrangements would be renegotiated from a substantially different baseline, and the current demographic and territorial status quo would not persist unaltered. The national-liberation and religious-restoration readings would characterize disappearance very differently (as loss of the only place of Jewish national safety or the reversal of a divinely warranted process) — this contest over what 'disappearance' even means is itself part of what indexes the kernel dispute, which is why this field is authored as contested rather than resolved within this single reading.
% FOUNDING_PROBLEM: European and Russian antisemitism, culminating in pogroms and the Holocaust, created an urgent stateless-persecution problem for Jewish populations with no reliable sovereign guarantor of physical safety anywhere in the world.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust historians and refugee-law scholars outside the Zionist movement corroborate that the underlying persecution problem was real and severe. Independent of that corroboration, Palestinian historians, UN human rights bodies (e.g. UN Special Rapporteurs on the OPT), and comparative settler-colonial scholars attest — from outside the beneficiary population — that the chosen solution's mechanism (establishing an ethno-national majority in an already-inhabited territory) necessarily produced a second, ongoing displacement problem that the founding-problem narrative does not resolve or excuse; this reading treats that second problem as constitutive of the arrangement's actual function, not as an unfortunate side effect of solving the first.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, contested).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 by 2024) because the reading holds that the arrangement's core function — sustaining a Jewish demographic and territorial majority — structurally requires continued land and rights transfer away from the indigenous population, not a one-time historical event that has since been absorbed. Suppression is authored high (0.72) reflecting active legal, administrative, and military enforcement (permit regimes, land confiscation statutes, military governance, demolition orders) required to maintain the arrangement against resistance and international objection. Accessibility_collapse is authored moderate (0.5) rather than near-total because alternative arrangements (binational state, confederation, full return-and-restitution models) remain articulated and actively argued for by displaced populations and international legal bodies — the alternatives have not collapsed from the discourse, only from practical political availability. Resistance is authored high (0.82) reflecting sustained Palestinian political mobilization, international legal challenge (ICJ proceedings, UN resolutions), and BDS-type civil society pressure. The 1993 dip in extractiveness/suppression reflects the Oslo-era negotiation period when displacement dynamics were partially bracketed by a two-state process; the subsequent rise reflects settlement expansion and blockade intensification after that process stalled.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler population and state apparatus sit near the beneficiary end: they hold decision-making power over land and citizenship rules and their material situation is enhanced by the arrangement's continuation. Palestinian refugees, present-day residents, and Bedouin communities sit near the full-target end: trapped exit options (no legal path to return, no sovereign alternative, administrative unrecognition), powerless power atom, and directly borne costs (land loss, residency restriction, demolition). International diaspora Jewish communities are authored as beneficiaries with mobile exit — they gain a standing refuge option without bearing displacement costs, which differentiates them from the settled population despite shared beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (antisemitic persecution requiring a sovereign safety guarantor) is authored as status: contested rather than dead, because the reading does not claim the original persecution problem was ever illegitimate or resolved — it claims the chosen solution generated a second, ongoing problem (Palestinian displacement) that the founding narrative is used to obscure rather than address. This prevents the classification from either (a) treating the entire arrangement as pure cover-story extraction with no genuine coordination function (it does solve a real problem for the coordinated population) or (b) treating the founding problem's genuine severity as license for indefinite, unexamined continuation of the displacement mechanism. The tangled_rope classification — genuine coordination for one population, asymmetric extraction from another, both riding the same structural mechanism — is the intended reading; a pure snare classification would deny the coordination function is real, and a pure rope classification would deny the extraction is structural rather than incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_vs_returning_population_framing,
    'Is the Jewish population that settled Mandate Palestine and post-1948 Israel structurally a ''settler'' population analogous to European colonists in Algeria/South Africa, or a persecuted population exercising a return claim to an ancestral homeland with continuous historical presence — and does the answer depend on which population source (European Ashkenazi immigration vs. expelled Mizrahi/Sephardi populations from Arab states) is examined?',
    'This is precisely the axis on which the three sibling readings (settler_colonial, national_liberation, religious_restoration) diverge and cannot be resolved by additional data alone — it is a framing dispute over which historical facts are constitutive of legitimacy versus incidental to it. Comparative settler-colonial typology (metropole linkage, replacement logic, land-centered economy) supports this reading''s framing for the Ashkenazi-led founding institutions; the framing is more contested for Mizrahi/Sephardi populations who were themselves expelled from Arab states without European metropole backing.',
    'If the settler-colonial framing does not extend cleanly to the Mizrahi/Sephardi population (roughly half of Israeli Jews), the beneficiary/victim structure authored here may over-generalize a framework built primarily on the Ashkenazi-led founding institutions (Jewish Agency, JNF, Yishuv leadership) to the entire present-day Jewish-Israeli population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_vs_returning_population_framing, conceptual, 'Whether the settler-colonial typology applies uniformly across the internally diverse Jewish-Israeli population.').

omega_variable(
    displacement_constitutive_vs_incidental,
    'Was the 1948 displacement of the Palestinian population a constitutive design feature of the Zionist state-building project (as this reading holds) or an incidental, war-contingent outcome that the movement''s founding documents and leadership did not require as a matter of design?',
    'Archival historiography (the ''New Historians'' debate — Morris, Pappe, Shlaim, Karsh) over pre-1948 Zionist leadership planning documents (e.g., Plan Dalet), and comparison of stated founding aims against documented population-transfer discussions within Zionist institutions in the 1930s-40s.',
    'If displacement is established as substantially planned/anticipated by pre-state institutions, this reading''s core premise (displacement as constitutive) is strengthened toward a settled empirical matter; if displacement is established as predominantly a contingent outcome of the 1948 war rather than institutional design, the settler-colonial reading''s foreclosure of the national-liberation reading''s ''incidental tragedy'' framing weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_constitutive_vs_incidental, empirical, 'Whether 1948 displacement was designed or contingent — the central historiographical dispute underlying this reading''s core axiom.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly do the three sibling readings of zionist_legitimacy_basis locate their disagreement — is it a factual dispute (what happened), a framework dispute (which comparative typology applies — colonial, national-liberation, or theological), or a values dispute (which harms and which goods count as legitimacy-determining)?',
    'None fully available — this is a genealogical and conceptual dispute rather than a purely empirical one, since all three readings can accept much of the same documentary record (population movements, land transfers, legal statutes) while disagreeing about which facts are constitutive of the arrangement''s legitimacy and which are incidental.',
    'Structurally, this determines the reading_relations in cs_structure: if the dispute is primarily factual, one reading could eventually foreclose others via historiographical consensus; if it is primarily a framework/values dispute, the readings are likely to coexist indefinitely as competing normative lenses on a shared, contested factual record — which is the relation authored here (coexists_with for both siblings, not forecloses).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating the kernel disagreement among fact, framework, and values axes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1917, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(zion_tr_t2005, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(zion_be_t1917, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.68).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(zion_be_t2005, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1917, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(zion_su_t2005, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the single natural-language label 'Zionism'/'Zionist legitimacy.' Per the ε-invariance principle, the three readings are authored as separate constraint stories because they produce substantially different ε values and different beneficiary/victim structures depending on which structural claim is evaluated: the settler_colonial_reading (this story) authors high extractiveness (0.78) grounded in a displacement-as-constitutive premise; the national_liberation_reading is expected to author substantially lower extractiveness grounded in a return-and-self-determination premise treating displacement as incidental to a legitimate liberation claim; the religious_restoration_reading is expected to author yet another profile grounded in a covenant-fulfillment premise largely orthogonal to the colonial/liberation axis. All three link to each other via affects_constraints; none is the 'correct' single-story account of Zionism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
