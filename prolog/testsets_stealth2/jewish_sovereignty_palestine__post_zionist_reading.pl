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
 *   human_readable: Ethnic-National Framework of the Israeli State (Post-Zionist Reading)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   The standing arrangement under assessment is the Israeli ethnic-national
 *   framework as it actually operates: the Law of Return and its mirror
 *   exclusions, the constitutional definition of the state's national
 *   character, the national-institution land channels, the parallel legal
 *   regimes of the occupation and blockade, and the enforcement machinery
 *   that maintains them. This story instantiates the post-Zionist reading,
 *   whose structural claim is that the framework's coordination function is
 *   real — defense, absorption, services, refuge — and that its ethnic
 *   character now obstructs civic equality inside the state and regional
 *   integration around it, with de-Zionization of state institutions as the
 *   remedy. The epsilon authored here is this reading's assessment of the
 *   standing arrangement itself, never of any alternative arrangement; the
 *   claim and the metrics are independent authored facts.
 *
 * KEY AGENTS:
 *   - israeli_state_institutions: Agenda-setter (institutional/identity_locked) — administers the citizenship hierarchy, the land channels, and the occupation's legal architecture; could re-found the framework on civic terms but is constituted around it
 *   - jewish_citizens_israel: Primary beneficiary (organized/mobile) — holds Law-of-Return citizenship, land and budget priority, constitutional standing; also carries conscription and conflict costs (dual-positioned payer)
 *   - law_of_return_diaspora_jews: Secondary beneficiary (organized/arbitrage) — holds a guaranteed entry-and-citizenship right no other population has
 *   - palestinian_citizens_israel: Primary target among citizens (moderate/constrained) — votes and pays taxes outside the Law of Return, under admissions committees, land asymmetry, and budget gaps
 *   - east_jerusalem_palestinians: Target seat (powerless/trapped) — revocable residency, demolition and expropriation exposure, no secure status
 *   - west_bank_palestinians: Primary target under occupation (powerless/trapped) — military law, permits, settlement expansion, no vote over the governing state
 *   - gaza_palestinians: Target seat under blockade (powerless/trapped) — borders, airspace, and access controlled externally; mass displacement in 2023-24
 *   - palestinian_refugees_diaspora: Excluded claimant (powerless/trapped) — denied return while the mirror right admits any Jewish applicant
 *   - regional_arab_states: Excluded integration party (institutional/mobile) — holds the regional-integration offer the ethnic framework keeps frozen
 *   - israeli_post_zionist_scholars: Analytical observer (moderate/analytical) — documents the asymmetries against the archival record
 *   - international_community: Analytical observer (institutional/analytical) — adjudicates legality from outside; alters costs, holds no seat in maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.74).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.78).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Ethnic-National Framework of the Israeli State (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '92b35922-bfe0-4b5e-a83f-699d237d3f3f').
narrative_ontology:cs_kernel_codification('92b35922-bfe0-4b5e-a83f-699d237d3f3f', formalized).
narrative_ontology:cs_authority_grounding('92b35922-bfe0-4b5e-a83f-699d237d3f3f', extraction).
narrative_ontology:cs_interpretation_layer_present('92b35922-bfe0-4b5e-a83f-699d237d3f3f').
narrative_ontology:cs_reading_relation('92b35922-bfe0-4b5e-a83f-699d237d3f3f', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('92b35922-bfe0-4b5e-a83f-699d237d3f3f', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('92b35922-bfe0-4b5e-a83f-699d237d3f3f', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('92b35922-bfe0-4b5e-a83f-699d237d3f3f', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('92b35922-bfe0-4b5e-a83f-699d237d3f3f', foundational, ethnic_framework_obstructs_civic_equality).
narrative_ontology:cs_axiom_status(ethnic_framework_obstructs_civic_equality, holdable).
narrative_ontology:cs_axiom_grounding('92b35922-bfe0-4b5e-a83f-699d237d3f3f', ethnic_framework_obstructs_civic_equality, empirically_contingent).
narrative_ontology:cs_axiom('92b35922-bfe0-4b5e-a83f-699d237d3f3f', foundational, civic_equality_supersedes_ethnic_privilege).
narrative_ontology:cs_axiom_status(civic_equality_supersedes_ethnic_privilege, holdable).
narrative_ontology:cs_axiom_grounding('92b35922-bfe0-4b5e-a83f-699d237d3f3f', civic_equality_supersedes_ethnic_privilege, deontological).
narrative_ontology:cs_reference_frame('92b35922-bfe0-4b5e-a83f-699d237d3f3f', state_of_all_its_citizens).
narrative_ontology:cs_drift_state('92b35922-bfe0-4b5e-a83f-699d237d3f3f', post_nation_state_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('92b35922-bfe0-4b5e-a83f-699d237d3f3f', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, law_of_return_diaspora_jews).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, east_jerusalem_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, jewish_demographic_security_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Knesset, government ministries, the IDF, the Jewish National Fund and World Zionist Organization channels, and the land authorities administer the citizenship regime: the Law of Return, the land-transfer and admissions-committee system, the separation of citizenship from national status, and the legal architecture of the occupation and blockade. These bodies could re-found citizenship and land law on civic terms, but their mandates, budgets, and personnel pipelines are constituted around the ethnic-national framework, and the judiciary polices the 'Jewish and democratic' balance without revising it. Exiting the framework would mean dissolving the mandates these institutions are made of.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive automatic citizenship and immediate full rights through the Law of Return, preferential access to state land through national-institution channels, national-service and budget priorities, and a state constitutionally defined as theirs. They also carry conscription obligations, war costs, and the security burdens of the conflict the framework perpetuates, and many hold foreign passports that make emigration a real option. Their collective standing in the state depends on the framework's continuation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel, payer).

% Any Jewish person worldwide holds an immediate, non-revocable right to immigrate, receive citizenship, and access land and institution channels closed to non-Jewish applicants — a standing option no other population holds. They need not exercise it to benefit: the guarantee shapes their security calculus and their claim on the state. The right is the asset; exit is irrelevant to a seat whose privilege is entry itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, law_of_return_diaspora_jews, beneficiary,
    organized, generational, arbitrage, global).

% About a fifth of the state's citizens: they vote, sit in the Knesset, litigate, and use state services, but sit outside the Law of Return, face admissions committees and land-allocation channels reserved for Jewish nationals, receive smaller municipal and school budgets, and are formally defined out of the state's self-description. Emigration is possible but means forfeiting home, family land, and the only citizenship that recognizes their residence; staying means permanent minority status inside a state defined against them.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_israel, beneficiary).

% Hold revocable permanent residency rather than citizenship in the state that annexed their city; they may apply for citizenship under conditions most do not meet and many refuse on principle. Residency is lost by prolonged absence; homes are exposed to demolition orders and expropriation under zoning and absentee-property law; they pay taxes and municipal fees under a status that can be stripped.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, east_jerusalem_palestinians, payer,
    powerless, biographical, trapped, regional).

% Live under military law in Areas B and C while settlers around them hold civilian Israeli law; movement between towns runs through checkpoints and permit gates; land in Area C is effectively closed to Palestinian building while settlement expands; they have no vote in the state that governs their water, land, and movement. Leaving means refugee flight; staying means administering life under a regime they cannot elect or remove.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).

% Live behind a blockade controlled by the state and Egypt, with borders, airspace, and maritime access shut; exit runs through narrow permit and crossing regimes, and the 2023-24 war displaced most of the strip's residents internally. They hold no standing in the state that controls their access to food, fuel, water, and movement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinians, payer,
    powerless, biographical, trapped, regional).

% Families displaced in 1948 and 1967 and their descendants, in camps and exile across the region and beyond, hold a documented claim to return that the framework answers with its mirror image: a Jew from anywhere may enter, a Palestinian from inside may not. They are not seated in any negotiation that maintains the arrangement, and their return claim is held as a final-status question indefinitely deferred.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_diaspora, excluded,
    powerless, generational, trapped, global).

% Hold the regional-integration offer — recognition, normalization, economic corridors — that the framework's ethnic character keeps conditional and partial; normalization proceeds state-by-state on security terms while the civic-equality question that would unlock full integration stays frozen. They are parties to every war and every settlement of this conflict yet outside the conversation that maintains the internal framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, regional_arab_states, excluded,
    institutional, generational, mobile, continental).

% Historians, sociologists, and legal scholars inside the academy who document the founding narrative against the archival record and map the institutional asymmetries; they supply the evidence base for the de-Zionization argument and pay for it in career costs, funding pressure, and political attack. They analyze the structure without administering it or collecting from it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_post_zionist_scholars, observer,
    moderate, biographical, analytical, national).

% UN bodies, the ICJ and ICC, foreign ministries, and human-rights organizations adjudicate the framework's legality from outside, impose reporting and sanction pressure, and document the occupation; their leverage alters the framework's costs but they hold no seat in its maintenance.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-statelessness coordination problem for the Jewish people: one sovereign apparatus provides collective defense, immigration absorption, land development, national institutions, and shared civic infrastructure — courts, services, refuge — for the population under its control, and gives a persecuted diaspora a guaranteed refuge.
% TRANSFER_FUNCTION: Moves land access, immigration and citizenship rights, development budgets, national-institution resources, and constitutional standing to Jewish citizens and Law-of-Return eligibles; moves displacement, expropriation, permit-bound movement, subordinate legal status, and denial of return onto Palestinian citizens, occupied residents, blockaded residents, and the refugee diaspora.
% ABSENT_VOICES: The refugee diaspora denied return, the occupied and blockaded residents who hold no vote over the state governing them, and regional Arab publics are outside the conversation that maintains the framework. Inside the state, Palestinian citizens vote and litigate, but their equality demands are structurally outvoted and their account of 1948 is excluded from official memory and school curricula.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished overnight, the Law of Return and its mirror exclusions would lapse, the national-institution land channels would convert to state-civic ownership, the Nation-State Law's hierarchy would dissolve, the settlement and occupation architecture would lose its legal re-founding, and regional normalization would shift from security-bloc bargains toward civic integration — citizenship law, land regime, budget allocation, and the state's self-description would all have to be rebuilt on new terms. Nearly every arrangement named in this story is constituted by the framework; the world does not stay the same.
% FOUNDING_PROBLEM: Securing collective physical safety and self-determination for the Jewish people after centuries of statelessness and persecution, culminating in the Holocaust — answered by sovereign statehood in Palestine with a Jewish demographic and institutional core.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's reality is corroborated from outside the benefiting parties: Holocaust historiography, the archival record of European statelessness, and diaspora Jewish communities attest it. Its status is disputed along the same external lines: post-Zionist and New Historian scholarship (inside the discourse, outside the beneficiary set) attests that statehood achieved the safety function and the framework now persists past it; Palestinian and Arab historiography attests the problem never warranted the displacement remedy. No corroborating source outside the beneficiary set attests that the ethnic framework remains necessary to the safety function — that attestation comes only from the framework's maintainers.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored 0.74 (moderate-high, per this reading's own lights): privilege is decoupled from contribution — a Jew from anywhere enters with full standing while a Palestinian born inside cannot return; land flows through national-institution channels reserved by nationality; a fifth of the citizenry sits outside the state's self-description; occupation and blockade run parallel legal regimes. Suppression 0.78 is structural, not internalized: citizenship hierarchy, military law, checkpoints, demolition orders, blockade — the mechanism is law and force, so no internalization omega is carried (contrast interpersonal stories). Theater 0.38 is non-monotonic across the interval and the shape is the point: it climbs through the Oslo years (0.47 at 1999) as performative diplomacy — process without transfer, talks while settlements grow, an intermittent-reinforcement dynamic in which the peace process itself functioned partly as cover — then falls after the Nation-State Law (0.38 at 2024) as the facade erodes and the ethnic hierarchy is stated outright rather than papered over. The theater peak is documented, not noise. Enforcement machinery ratchets across the interval (military rule over Arab citizens 1948-66, occupation from 1967, settlement enforcement from 1977, mass suppression of the First Intifada 1987, barrier and administrative detention after 2000, war displacement 2023-24), which is why suppression_requirement is tracked on the shared grid. Accessibility_collapse 0.55: alternatives (civic statehood, binational arrangements, two states, return) remain articulable and argued — this is a contested human arrangement, not a natural law — but each is closed to the seats without power to enact it. Resistance 0.68: two intifadas, BDS, Joint List litigation, refugee activism, regional refusal and conditional normalization — the constraint meets organized, sustained resistance and holds by enforcement, not acquiescence; the victims' coalition capacity (Joint List, transnational boycott campaigns) is real but has not converted into structural change. Claim and metrics are independent: claimed_type tangled_rope is this reading's structural judgment (real coordination + asymmetric extraction + active enforcement); the metrics are this reading's descriptive assessment of the standing arrangement; the engine computes per-seat types from the structural data, and any divergence from the claim is the signal this corpus exists to take. Boltzmann note: identity_coordination is declared because the framework's primary coordination function is membership and boundary maintenance — but the coupling concentrates extraction on powerless seats at national-to-regional scope, which is the signature of extractive coupling that the identity offset does not excuse.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the state-institutions seat the framework is administration: the 'Jewish and democratic' balance is a live interpretive task and de-Zionization reads as self-dissolution — hence identity-locked exit. From the Jewish-citizen seat it is home, security, and standing, with the conflict's costs (conscription, war, isolation) experienced as the price of the coordination rather than of the hierarchy. From the Palestinian citizen seat it is enforceable second-class membership; from the occupied seats it is a regime they cannot elect or remove; from the refugee seat it is a locked door answering a mirror key. Same power atom, different constraint: palestinian_citizens_israel (moderate/constrained) and israeli_post_zionist_scholars (moderate/analytical) share the moderate atom but diverge by exit — the scholars can exit into critique and do; the citizens' critique is bound to the state that defines them. Inter-institutionally, regional Arab states and international bodies meet the framework from outside with mobile and analytical exits, so they experience it as a diplomatic object whose costs they can raise, not a life structure they inhabit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put jewish_citizens_israel and law_of_return_diaspora_jews near the beneficiary end; victim declarations put the five Palestinian seats near the target end, with trapping (east Jerusalem, West Bank, Gaza, refugees all trapped; citizens constrained) pushing the occupied seats nearer the full-target end than the citizen seat, whose citizenship carries real services and standing that damp its d. The agenda-setter administers and collects through its mandates, sitting near the beneficiary end. The observers and the excluded regional seat carry no beneficiary/victim declaration and are left to the engine's role handling. No directionality overrides are authored: overrides key to power atoms, and this story's atoms each span structurally distinct seats (the institutional atom holds the agenda-setter, an excluded regional party, and an observer; the moderate atom holds a constrained payer and an analytical observer), so atom-level overrides would blur the distinctions the role and exit declarations already draw. Suppression is authored as a raw structural property (0.78) and is not scaled by anything in this story; the engine alone scales extraction by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real and externally corroborated; statehood achieved it; the framework now persists in a privilege-maintenance function this reading judges counterproductive to both civic equality and regional integration. The classification discipline matters in both directions: the genuine coordination function (defense, absorption, services, refuge) blocks a pure-extraction reading — the arrangement is not a cover story in the way the settler-colonial sibling alleges — while the asymmetric extraction (Law-of-Return asymmetry, land channels, occupation architecture) blocks a pure-coordination reading. The honest structure is hybrid, and the founding problem's status is authored contested rather than dead because the parties dispute obsolescence even though no external corroborator defends the framework's continued necessity. If the identity frame broke — if the state's institutions re-constituted their mandates on civic-equality terms — the constraint would convert toward transitional support with a real sunset, and the reading's de-Zionization program is exactly that conversion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_dispute_location,
    'This constraint is one reading of kernel jewish_sovereignty_palestine; at which structural element do the five readings'' disagreements actually bind — the legitimacy of ethnic self-determination, the truth of the founding narrative, or the framework''s present function?',
    'Comparative classification across the five sibling stories: if the liberal-nationalist sibling computes as rope while this reading computes as tangled_rope, the binding dispute is the necessity of ethnic privilege to the coordination function, not the statehood fact itself.',
    'Locating the dispute determines which sibling premise this reading''s axioms foreclose versus merely contest, and which de-Zionization demands are shared across readings versus unique to this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_dispute_location, conceptual, 'Where the kernel contest actually binds across the sibling readings.').

omega_variable(
    de_zionization_remedy_scope,
    'What does this reading''s remedy actually require — narrative revision, conversion of the national institutions (JNF/WZO channels) to state-civic ownership, constitutional re-founding on civic equality, or full binational transformation — and are these separable stages?',
    'Programmatic literature of the post-Zionist current plus institutional feasibility analysis: which remedies appear in court petitions, Knesset bills, and civil-society programs, and which require constituent-level change.',
    'If the remedy is institutional-conversion-grade, fixing stays costly but the constraint is a correctable hybrid; if full-transformation-grade, the reading converges toward the settler-colonial sibling''s remedy set and the type assessment hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_zionization_remedy_scope, preference, 'Scope ambiguity in the reading''s own remedy program.').

omega_variable(
    founding_narrative_falsity_vs_completion,
    'Does the obstruction claim rest on the founding narrative being false (the historiographic charge) or on it being fulfilled and obsolete (the mandatrophy charge)?',
    'The New Historians'' archival record adjudicates the falsity prong; outcome data on the safety function (refuge operations, diaspora security) adjudicates the completion prong.',
    'A falsity finding pushes the constraint toward snare structure (the coordination story as cover); a completion finding keeps it in hybrid territory where the framework persists past its function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_narrative_falsity_vs_completion, empirical, 'Whether the reading''s charge against the founding narrative is historiographic or functional.').

omega_variable(
    civic_channel_sufficiency,
    'Can the framework''s internal civic channels — Supreme Court equality jurisprudence, Arab parliamentary representation, civil-society litigation — deliver civic equality without de-Zionization?',
    'Forward outcome tracking: land and budget litigation results, representation effects, and whether Nation-State-Law-era rulings expand or contract the equality channel.',
    'If sufficient, the constraint is a correctable tangled_rope and the reading''s de-Zionization call overstates; if insufficient, extraction hardens toward snare structure and the reading''s remedy is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civic_channel_sufficiency, empirical, 'Whether internal correction channels can substitute for de-Zionization.').

omega_variable(
    occupation_temporality,
    'Is the occupation a temporary emergency layer over the framework or a permanent structural component of it — and does the epsilon authored here integrate the occupied seats as standing parties or as contingent ones?',
    'Settlement-population growth curves, annexation legislation, and the absence of any terminal-status instrument across the interval adjudicate permanence.',
    'If permanent, the occupied seats are standing victims and extraction stays high with hardening type pressure; if genuinely temporary, part of the measured extraction belongs to a separable emergency constraint and this story''s epsilon should be decomposed into a second linked story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(occupation_temporality, empirical, 'Whether the occupation layer is structural or contingent within the referent arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.22).
narrative_ontology:measurement(jewi_tr_t1958, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1958, 0.25).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.31).
narrative_ontology:measurement(jewi_tr_t1977, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1977, 0.34).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1987, 0.39).
narrative_ontology:measurement(jewi_tr_t1999, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1999, 0.47).
narrative_ontology:measurement(jewi_tr_t2011, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2011, 0.43).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(jewi_be_t1958, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1958, 0.57).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.64).
narrative_ontology:measurement(jewi_be_t1977, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1977, 0.67).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1987, 0.7).
narrative_ontology:measurement(jewi_be_t1999, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1999, 0.66).
narrative_ontology:measurement(jewi_be_t2011, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2011, 0.71).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2024, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.58).
narrative_ontology:measurement(jewi_su_t1958, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1958, 0.61).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.66).
narrative_ontology:measurement(jewi_su_t1977, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1977, 0.7).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1987, 0.75).
narrative_ontology:measurement(jewi_su_t1999, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1999, 0.64).
narrative_ontology:measurement(jewi_su_t2011, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2011, 0.72).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition note: the natural-language label 'Zionism / the Jewish sovereign project' covers five structurally distinct commitments (this file plus four sibling stories). All five share the standing Israeli ethnic-national arrangement as the epsilon referent and author different epsilon values, victim sets, and claimed types; this story instantiates the post-Zionist reading (statehood achieved, framework now obstructive, de-Zionization remedy). Sibling edges are typed in cs_structure.reading_relations; the network edges mirror them for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
