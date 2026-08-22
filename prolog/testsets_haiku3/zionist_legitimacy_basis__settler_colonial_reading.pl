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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Settler-Colonial Legitimacy Structure
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint story presents Zionism as a European settler-colonial
 *   movement that legitimates the establishment of an ethno-nationalist state
 *   through the systematic displacement of an indigenous Palestinian
 *   population. The reading frames the constraint's structure as colonial
 *   settlement backed by law, military force, and institutional exclusion —
 *   not as a legitimate national liberation movement or religious
 *   restoration. The constraint operates at the level of legitimacy claims:
 *   how the displacement and institutional exclusion are justified,
 *   naturalized, and enforced. The settler-colonial reading explicitly
 *   rejects the framing of Zionism as return to an ancestral homeland and
 *   instead analyzes the mechanisms by which European Jewish settlement was
 *   imposed on territory inhabited by Palestinians, and the apparatus that
 *   sustains Palestinian dispossession and exclusion. This is ONE reading of
 *   the contested kernel 'zionist_legitimacy_basis'; sibling readings
 *   (national_liberation_reading, religious_restoration_reading) instantiate
 *   different constraints from the same kernel with different ε values,
 *   beneficiary/victim structures, and types.
 *
 * KEY AGENTS:
 *   - European Jewish settlers and institutions: primary beneficiaries; set policy, allocate resources, define membership and legitimacy
 *   - Palestinian indigenous population: primary victims; dispossessed of land, confined to enclaves, barred from return, denied citizenship and political rights
 *   - Zionist political institutions (state and quasi-state): agenda-setter; enforce settlement expansion, land expropriation, Palestinian exclusion through law and military
 *   - Western governments (US, UK, etc.): secondary beneficiaries; provide military aid, diplomatic cover, political legitimation
 *   - International Zionist movement: secondary beneficiaries; mobilize resources, immigration, capital for the settler project
 *   - Palestinian resistance movements: structurally dominated; operate from asymmetric position against enforcement machinery
 *   - International human rights observers: excluded from authority framework; document violations but lack enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.82).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.78).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Settler-Colonial Legitimacy Structure").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '65ff8a77-6b04-4ab3-910f-713ea10367a7').
narrative_ontology:cs_kernel_codification('65ff8a77-6b04-4ab3-910f-713ea10367a7', fixed_text).
narrative_ontology:cs_authority_grounding('65ff8a77-6b04-4ab3-910f-713ea10367a7', extraction).
narrative_ontology:cs_interpretation_layer_present('65ff8a77-6b04-4ab3-910f-713ea10367a7').
narrative_ontology:cs_reading_relation('65ff8a77-6b04-4ab3-910f-713ea10367a7', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('65ff8a77-6b04-4ab3-910f-713ea10367a7', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('65ff8a77-6b04-4ab3-910f-713ea10367a7', foundational, colonial_structure_determines_legitimacy).
narrative_ontology:cs_axiom_status(colonial_structure_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('65ff8a77-6b04-4ab3-910f-713ea10367a7', colonial_structure_determines_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('65ff8a77-6b04-4ab3-910f-713ea10367a7', foundational, palestinian_displacement_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(palestinian_displacement_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('65ff8a77-6b04-4ab3-910f-713ea10367a7', palestinian_displacement_constitutive_not_incidental, deontological).
narrative_ontology:cs_reference_frame('65ff8a77-6b04-4ab3-910f-713ea10367a7', european_settler_colonial_project).
narrative_ontology:cs_drift_state('65ff8a77-6b04-4ab3-910f-713ea10367a7', contemporary_international_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('65ff8a77-6b04-4ab3-910f-713ea10367a7', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, european_jewish_settler_population).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, zionist_political_institutions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_land_owners).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, international_zionist_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, western_governments).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_resistance_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European Jewish migrants and their descendants acquire land, establish settlements, and consolidate political control over territory previously inhabited by Palestinians. They benefit from state resources, privileged legal status, and exclusive access to land and citizenship. The constraint legitimates their occupation as a return to ancestral homeland rather than as colonial settlement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, european_jewish_settler_population, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, european_jewish_settler_population, agenda_setter).

% State and quasi-state institutions (Jewish Agency, World Zionist Organization, Israeli government, military) that set policy, allocate resources, enforce settlement expansion, and define who belongs in the territory. They frame displacement and exclusion as necessary for Jewish majority and security, not as colonial extraction.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_political_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Palestinians who inhabited the territory for centuries are systematically dispossessed of land, political rights, and citizenship. They are confined to fragmented enclaves (West Bank, Gaza) with restricted movement, limited resources, and no recourse to legal remedies controlled by the settler institutions. Their displacement is rationalized as making room for Jewish return; their resistance is framed as terrorism.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, national).

% Specific Palestinian individuals and families whose land is expropriated through purchase, legal maneuvering (absentee property laws, tenant evictions), or military conquest. They have no meaningful legal recourse; compensation is minimal or nonexistent. Their land becomes settler property or state land.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_land_owners, payer,
    powerless, biographical, trapped, national).

% Palestinians expelled or who fled during 1948 and subsequent conflicts, now living in refugee camps in neighboring countries or stateless diaspora. They are barred from return by law and military enforcement. Their dispossession is the structural foundation of the constraint.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees, payer,
    powerless, generational, trapped, global).

% Global Zionist organizations, diaspora Jewish communities, and international networks that mobilize resources, immigration, capital, and political support for the settler project. They benefit from the constraint's operation and from the state apparatus it sustains.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_zionist_movement, beneficiary,
    organized, generational, mobile, global).

% US, UK, and other Western powers provide military aid, diplomatic recognition, and political cover. They benefit from a strategically positioned ally in the region and from the framing of the constraint as a legitimate Jewish return rather than colonialism.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, western_governments, beneficiary,
    institutional, generational, mobile, global).

% Palestinian political and armed groups that resist displacement but operate from a structurally asymmetric position: the constraint's enforcement machinery (military, law, settlement expansion) vastly outmatches their capacity. Their resistance is necessary to the constraint's operation (justifies suppression) yet remains subordinate.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_resistance_movements, payer,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, palestinian_resistance_movements, observer).

% UN bodies, international human rights organizations, academic researchers who document displacement, settlement expansion, and human rights violations. They are excluded from enforcement and their findings are contested or dismissed by beneficiary institutions.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_human_rights_observers, observer,
    analytical, biographical, analytical, global).

% Palestinian civil society organizations, intellectual communities, and cultural institutions are restricted in their operation, subject to surveillance and control, and unable to exercise meaningful influence over the constraint's structure. Their voices are structurally excluded from the authority framework.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_civil_society, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, european_jewish_settler_population).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading rejects the coordination frame entirely: the constraint does not solve a coordination problem between equals. It is a colonial apparatus imposing ethnic homogeneity through force and law.
% TRANSFER_FUNCTION: Transfers land, property, citizenship, and political authority from Palestinian population to European Jewish settlers and their institutional structures. Backed by military enforcement, expropriation law, and exclusionary citizenship.
% ABSENT_VOICES: Palestinian populations dispossessed or confined to enclaves are structurally excluded from the authority framework. Palestinian refugees barred from return have no voice in decisions affecting their dispossession. Palestinian civil society is restricted and unable to exercise meaningful influence. These excluded voices would directly contest the naturalization of displacement and demand recognition as indigenous inhabitants with prior rights.
% DISAPPEARANCE_RATIONALE: If the constraint's enforcement apparatus (military, law, institutions) were dissolved, the territory would reorganize around Palestinian demographic majority, property rights would revert to displaced owners or their heirs, and refugee return would become possible. The settler-colonial structure is not natural; it is actively maintained and would collapse without enforcement.
% FOUNDING_PROBLEM: European antisemitism created political persecution of Jewish populations, generating a movement (Zionism) seeking territorial refuge and sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The reality of European antisemitism is historically corroborated. However, by this reading the founding problem is substantially solved within Western contexts (legal equality, protected rights). The constraint persists not to solve persecution but to maintain territorial ethno-state and prevent demographic dilution. Historians of settler-colonialism, human rights bodies, and international observers attest that the founding problem does not justify or necessitate Palestinian displacement. The constraint's persistence depends on enforcing Palestinian exclusion, not on providing asylum for the persecuted—indicating mandatrophy.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high and rising (0.45 → 0.82 over the interval) because the constraint systematically transfers land, property, and political authority from Palestinians to settlers, and this transfer accelerates as settlements expand and Palestinian enclaves contract. The transfer is not negotiated or consensual; it is imposed. Suppression rises throughout (0.52 → 0.78) because maintaining Palestinian exclusion and preventing their return requires escalating military enforcement, legal restriction, and control of movement and resources. Theater ratio rises to a plateau (0.15 → 0.42) reflecting how the constraint's justification increasingly depends on framing displacement as security necessity or historical justice (narratives that obscure the colonial structure) rather than on legitimate governance functions. Accessibility collapse is high (0.71) because Palestinians within the territory have almost no meaningful exit (trapped), while the settler population has high mobility and external support. Resistance remains substantial (0.68) because Palestinian resistance movements, though structurally dominated, persist and delegitimize the constraint in international discourse—the constraint's persistence depends on suppressing this resistance, not on winning Palestinians' consent. The claim/metric independence rule is honored: the constraint is CLAIMED as snare (pure extraction with no coordination function) and the metrics describe that structure honestly.
 *
 * PERSPECTIVAL GAP:
 *   Settler-institutional actors experience the constraint through a lens of legitimate return and necessary security; they see Palestinian resistance as unjustified violence against a Jewish state exercising self-determination. Palestinian actors experience the same constraint as colonial violence and dispossession; they see resistance as necessary self-defense against ongoing occupation. The constraint's legitimacy narrative is controlled by the beneficiary seat—the settler institutions shape how the constraint is presented internationally, in law, in education, and in political discourse. This informational and discursive asymmetry is part of the constraint's operation: the narrative apparatus that frames displacement as 'return' is enforced as vigorously as the military apparatus that prevents Palestinian return. The per-seat type computation should reveal this: beneficiary seats may compute as rope-users or mountain-dwellers (the constraint is natural, legitimate); payer seats compute as snare-victims (the constraint is extractive, maintained by force).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. European Jewish settlers are beneficiaries with mobile exit (arbitrage d around 0.15): they could leave the territory and pursue settlement elsewhere or assimilate into other societies, but ideological commitment and institutional investment keep them in place. Zionist institutions are beneficiary-agenda-setters with analytical exit (they could dissolve, but institutional survival is the function d around 0.0). Palestinians are victims with trapped exit (d near 1.0): they cannot leave without abandoning homes, property, and identity; within the territory they are confined by law and military force; internationally they are often barred from entry by host countries. Refugees are victims with trapped exit (d near 1.0): they are legally barred from return and live in perpetual displacement. The asymmetry is stark: the constraint benefits those with exit options and traps those without. No directionality overrides are needed; the structural data yields the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (European antisemitism and persecution of Jews) is real and historically attested. The constraint was built to solve it: create a territory where Jews could have political majority and self-determination. HOWEVER: the founding problem does not explain or justify the specific solution chosen (settlement in Palestine) nor the mechanisms of that solution (displacement of Palestinians, denial of Palestinian return, ethnic exclusivity). The mandatrophy test asks: does the founding problem persist, and does the constraint still serve it? By this reading, the founding problem (persecution) is substantially solved within Western contexts (legal equality, protected rights in democratic societies). The constraint persists not to solve the original founding problem but to maintain a territorial ethno-state and prevent demographic dilution through Palestinian return. This is mandatrophy: the constraint outlives its founding problem and is sustained by the extraction it enables (control of territory, resources, political power) rather than by solving the persecuted-people-need-sanctuary problem. The constraint's persistence depends on active enforcement of Palestinian exclusion and settlement expansion—mechanisms that would be unnecessary if the constraint were solving its founding problem. The theater ratio rising (0.15 → 0.42) reflects this: an increasing share of enforcement activity is devoted to maintaining the ethno-state structure and preventing return, not to providing asylum or security for persecuted peoples.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonialism_framework_applicability,
    'Does the settler-colonial analytical framework from the American West, Australia, and South Africa accurately apply to the Zionist project in Palestine, or do the historical and political circumstances demand a distinct analytical category?',
    'Comparative historical analysis: systematically compare mechanisms of land dispossession, indigenous population exclusion, institutional legitimacy structures, and demographic transformation across settler-colonial cases and the Israeli case. Identify structural similarities and differences.',
    'If the framework applies strongly, the constraint''s classification as snare with high extraction is robust across comparative analysis. If the framework is substantially inapplicable, the ε value and victim/beneficiary structure may require recalibration and alternative analytical categories become necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settler_colonialism_framework_applicability, conceptual, 'Whether settler-colonial analysis is the appropriate analytical frame for this constraint.').

omega_variable(
    indigenous_status_of_palestinian_population,
    'Is the Palestinian population accurately described as ''indigenous'' to the territory in this reading''s framework, and does that classification change if the referent population and time period change?',
    'Establish demographic presence and continuous residence: Palestinian presence in the territory is attested from at least medieval Islamic period onward (12th century+), with unbroken residence and cultural continuity. Jewish presence in the territory was also historical (pre-70 CE) but was interrupted (Diaspora beginning post-70 CE) and Jewish settlement did not resume until 19th-century Zionist immigration (post-1880s). By historical continuity, Palestinians are indigenous; European Jewish settlers are not indigenous despite religious historical connections.',
    'If this is accepted, the constraint''s classification as snare with Palestinian victims and settler beneficiaries is confirmed. If Palestinian indigeneity is denied, the constraint''s victim/beneficiary structure and ε value shift substantially, making it rope or contested-type. This is the crux of the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_status_of_palestinian_population, empirical, 'Historical continuity and demographic presence defining indigenous status.').

omega_variable(
    necessity_of_palestine_specific_settlement,
    'Did the founding problem (European antisemitism and Jewish persecution) necessitate settlement specifically in Palestine, or were other territorial solutions available and politically rejected?',
    'Historical analysis of alternative settlement proposals: the Uganda Plan (British offer of territory in East Africa, 1903), Argentina settlement proposals, Cyprus, and other locations were discussed within Zionist movement; some were rejected on the basis that Palestine was religiously/historically significant. This is a choice, not a necessity.',
    'If other solutions were available and rejected, the founding problem does not justify the displacement of Palestinians as necessary. The constraint''s framing as solving the founding problem becomes mandatrophic (constraint outlives its problem). If settlement in Palestine was the only viable option, the necessity question shifts to whether displacement was the only means of settlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_of_palestine_specific_settlement, empirical, 'Whether Palestinian settlement was the only solution to the founding problem or a choice among alternatives.').

omega_variable(
    extraction_vs_security_asymmetry,
    'To what extent does the suppression_requirement (0.78) reflect genuine security threats that require enforcement, versus institutional expansion and extraction maintenance disguised as security?',
    'Comparative analysis of settlements in areas with low Palestinian resistance versus areas of high resistance; examine whether suppression and military spending correlate with objective security threats or with settlement expansion rate and Palestinian population density. Analyze enforcement mechanisms targeting settlement expansion versus enforcement mechanisms responding to violent attacks.',
    'If suppression correlates strongly with settlement expansion regardless of security threat level, the theater_ratio and suppression metric are driven by extraction maintenance rather than security necessity, confirming the snare classification. If suppression correlates with actual security threats, part of the measured extraction may be security cost rather than pure institutional rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_security_asymmetry, empirical, 'Whether suppression is structurally driven by security necessity or by extraction/expansion maintenance.').

omega_variable(
    reading_coexistence_logical_structure,
    'Can the settler-colonial reading and the national-liberation reading of the Zionist legitimacy kernel coexist within a single coherent framework, or does one reading''s core premise logically foreclose the other?',
    'Examine the foundational axioms of each reading. Settler-colonial reading: colonial structure determines legitimacy; Palestinian displacement is constitutive. National-liberation reading: Zionism is indigenous return; Jewish majority-state is legitimate. These axioms directly contradict: either Palestinians are indigenous inhabitants whose displacement is colonial, or they are not indigenous and Jewish settlement is indigenous return. No single framework can hold both as true.',
    'If axioms foreclose (one reading logically eliminates the other), the reading_relations should be ''forecloses'' rather than ''coexists_with.'' If they can coexist in different parties'' frameworks without logical contradiction (different parties simply believe different things), coexists_with is correct. This affects how the engine models the kernel''s stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_logical_structure, conceptual, 'Logical structure of axiom relations across readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(zion_tr_t0, observed).
narrative_ontology:measurement(zion_tr_t20, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(zion_tr_t20, observed).
narrative_ontology:measurement(zion_tr_t40, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(zion_tr_t40, observed).
narrative_ontology:measurement(zion_tr_t60, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(zion_tr_t60, observed).
narrative_ontology:measurement(zion_tr_t80, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(zion_tr_t80, observed).
narrative_ontology:measurement(zion_tr_t100, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(zion_tr_t100, observed).
narrative_ontology:measurement(zion_tr_t120, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 120, 0.42).
narrative_ontology:measurement_basis(zion_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(zion_be_t0, observed).
narrative_ontology:measurement(zion_be_t20, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(zion_be_t20, observed).
narrative_ontology:measurement(zion_be_t40, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(zion_be_t40, observed).
narrative_ontology:measurement(zion_be_t60, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement_basis(zion_be_t60, observed).
narrative_ontology:measurement(zion_be_t80, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement_basis(zion_be_t80, observed).
narrative_ontology:measurement(zion_be_t100, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 100, 0.82).
narrative_ontology:measurement_basis(zion_be_t100, observed).
narrative_ontology:measurement(zion_be_t120, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 120, 0.82).
narrative_ontology:measurement_basis(zion_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(zion_su_t0, observed).
narrative_ontology:measurement(zion_su_t20, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(zion_su_t20, observed).
narrative_ontology:measurement(zion_su_t40, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(zion_su_t40, observed).
narrative_ontology:measurement(zion_su_t60, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement_basis(zion_su_t60, observed).
narrative_ontology:measurement(zion_su_t80, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 80, 0.77).
narrative_ontology:measurement_basis(zion_su_t80, observed).
narrative_ontology:measurement(zion_su_t100, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 100, 0.78).
narrative_ontology:measurement_basis(zion_su_t100, observed).
narrative_ontology:measurement(zion_su_t120, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 120, 0.78).
narrative_ontology:measurement_basis(zion_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__settler_colonial_reading, 0.25).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, israeli_settlement_expansion_logic).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugee_right_of_return).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, israel_palestine_one_state_two_state_solution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'zionist_legitimacy_basis.' The kernel contest has three readings: settler_colonial_reading (this one, snare type, high extraction), national_liberation_reading (rope or mountain type, coordination framing), and religious_restoration_reading (scaffold or tangled_rope, religious/messianic legitimation). Each reading instantiates a distinct constraint with its own ε, beneficiary/victim structure, and stakeholder situation. The readings are linked via network.affects_constraints and each carries omega variables documenting the kernel contest and reading-specific uncertainties. The settler-colonial reading rejects the legitimacy of the constraint itself and frames it as extraction requiring decolonization; the sibling readings accept or reframe the constraint as legitimate. All three are live positions held by different parties in the global discourse; none has been foreclosed except by logical incompatibility of axioms in the national_liberation case.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
