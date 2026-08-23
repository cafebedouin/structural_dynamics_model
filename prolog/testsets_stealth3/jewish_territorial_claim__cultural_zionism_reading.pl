% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Hebrew Cultural Center in Palestine Without Sovereignty (Cultural Zionist Reading)
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   This story authors the cultural-Zionist arrangement on its own terms: a
 *   Hebrew-language spiritual and cultural center built in Palestine under
 *   Ottoman and then British imperial rule, financed by diaspora
 *   philanthropy, populated by a deliberate minority of committed settlers
 *   chosen for quality over number, and — constitutively for this reading —
 *   requiring neither political sovereignty nor a Jewish demographic
 *   majority. The coordination core is real and verifiable from any seat:
 *   Hebrew revives as a spoken vernacular, a university, press, and school
 *   system come into being, and diaspora cultural life acquires a productive
 *   anchor. The cost structure is equally real: the same land-purchase
 *   machinery that houses the center alienates tenanted land from Palestinian
 *   farming families, a cost this reading's own founder flagged in 1891 and
 *   never denied. This file is one member of a decomposed constraint family
 *   (see network.dual_formulation_note and commentary.kernel_context);
 *   sibling readings are separate constraints with their own epsilon values,
 *   and nothing about their mutual contest is adjudicated inside this file.
 *
 * KEY AGENTS:
 *   - - palestinian_tenant_farmers: Primary target (powerless/trapped) — bears the land-alienation costs of the purchase economy
 *   - - diaspora_jewish_communities: Primary beneficiary (moderate/mobile) — funds the center, consumes its cultural output
 *   - - hebrew_cultural_leadership: Agenda-setting coordinator-beneficiary (organized/identity_locked) — builds and administers the institutional fabric
 *   - - hebrew_cultural_settlers: Resident beneficiaries (moderate/constrained) — staff and embody the center day to day
 *   - - palestinian_effendi_landsellers: Transactional beneficiaries (powerful/arbitrage) — monetize and exit, externalizing tenancy displacement
 *   - - ottoman_imperial_administration: Governing authority to 1917 (institutional/arbitrage) — restrictive legal frame
 *   - - british_mandate_administration: Governing authority 1917-1948 (institutional/arbitrage) — facilitating legal frame
 *   - - palestinian_arab_nationalists: Excluded opposition voice (organized/trapped) — never consulted, resists after the fact
 *   - - bundist_diaspora_autonomists: Excluded intra-Jewish alternative (organized/mobile) — territorial-free cultural program
 *   - - league_mandates_commission: Analytical observer (institutional/analytical) — sees the full structure, holds no lever
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.52).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.62).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Hebrew Cultural Center in Palestine Without Sovereignty (Cultural Zionist Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, '40e897db-a0de-49bc-8cb7-695e51a8a184').
narrative_ontology:cs_kernel_codification('40e897db-a0de-49bc-8cb7-695e51a8a184', distributed).
narrative_ontology:cs_authority_grounding('40e897db-a0de-49bc-8cb7-695e51a8a184', lineage).
narrative_ontology:cs_interpretation_layer_present('40e897db-a0de-49bc-8cb7-695e51a8a184').
narrative_ontology:cs_reading_relation('40e897db-a0de-49bc-8cb7-695e51a8a184', jewish_territorial_claim__political_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('40e897db-a0de-49bc-8cb7-695e51a8a184', jewish_territorial_claim__revisionist_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('40e897db-a0de-49bc-8cb7-695e51a8a184', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_axiom('40e897db-a0de-49bc-8cb7-695e51a8a184', foundational, sovereignty_not_required_for_center).
narrative_ontology:cs_axiom_status(sovereignty_not_required_for_center, holdable).
narrative_ontology:cs_axiom_grounding('40e897db-a0de-49bc-8cb7-695e51a8a184', sovereignty_not_required_for_center, instrumental).
narrative_ontology:cs_axiom('40e897db-a0de-49bc-8cb7-695e51a8a184', foundational, arab_presence_not_inherently_threatening).
narrative_ontology:cs_axiom_status(arab_presence_not_inherently_threatening, holdable).
narrative_ontology:cs_axiom_grounding('40e897db-a0de-49bc-8cb7-695e51a8a184', arab_presence_not_inherently_threatening, empirically_contingent).
narrative_ontology:cs_axiom('40e897db-a0de-49bc-8cb7-695e51a8a184', secondary, quality_over_quantity_settlement).
narrative_ontology:cs_axiom_status(quality_over_quantity_settlement, holdable).
narrative_ontology:cs_axiom_grounding('40e897db-a0de-49bc-8cb7-695e51a8a184', quality_over_quantity_settlement, instrumental).
narrative_ontology:cs_reference_frame('40e897db-a0de-49bc-8cb7-695e51a8a184', spiritual_center_without_sovereignty).
narrative_ontology:cs_drift_state('40e897db-a0de-49bc-8cb7-695e51a8a184', mandate_end_sovereign_realization, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('40e897db-a0de-49bc-8cb7-695e51a8a184', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_intelligentsia).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, palestinian_effendi_landsellers).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_tenant_farmers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writers, editors, and institution-builders in the Hovevei Zion orbit and later the Hebrew University boards: they select sites, recruit teachers, set the quality bar for who settles, and raise the funds. Their language, careers, and life-work exist only inside Hebrew cultural production anchored in Palestine; leaving the project would mean abandoning the vocation that constitutes them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_leadership, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_leadership, beneficiary).

% Fund the schools, presses, and the university through donations and national-fund collections, and consume what the center returns: textbooks, literature, a prestige standard for Hebrew education, an answer to assimilation anxiety. They can redirect giving or disengage town by town without dismantling their own communal life.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities, beneficiary,
    moderate, generational, mobile, global).

% Teachers, students, printers, and artisans who move to Jerusalem, Jaffa, and Haifa to live the cultural program daily — running the gymnasium network, the publishing houses, the university. They accept hard material conditions, depend on the institutions for livelihood and community, and returning to diaspora professions means losing the mission that brought them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_settlers, beneficiary,
    moderate, biographical, constrained, national).

% Fellaheen families working land owned by absentee proprietors in the valleys and coastal plain. When owners sell to the Jewish funds, tenancy agreements terminate and villages face eviction with little compensation and nowhere comparable to go. Some absorb into wage labor in the growing towns; most sink into landlessness.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_tenant_farmers, payer,
    powerless, biographical, trapped, regional).

% Urban notable families — Beirut trading houses, coastal and valley landlords — who sell large tracts at sharply rising prices, convert immovable patrimony into portable capital, and relocate to Beirut, Paris, or Cairo. They bank the proceeds and leave the displaced-tenancy problem behind them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_effendi_landsellers, beneficiary,
    powerful, biographical, arbitrage, regional).

% Governs Palestine through 1917: registers land title, permits and restricts immigration by decree, taxes transactions, and treats the influx with suspicion as a possible separatist wedge. Sets the legal conditions the center must operate inside; exits the stage entirely with the war.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, ottoman_imperial_administration, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Inherits the registries in 1917 and, under the Mandate charter, facilitates Jewish immigration and land purchase while administering the single legal order both peoples live under. Balances contradictory wartime promises, issues and tightens quotas as violence grows, and departs in 1948 leaving the arrangement to its fate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, british_mandate_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Notables, journalists, and congress organizers in the Muslim-Christian Associations and the Arab Executive who were never consulted when the Balfour Declaration and San Remo assigned their country's future. They respond with delegations, petitions, boycotts of land sales, general strikes, and finally the 1936-39 revolt; the repression that answers the revolt falls hardest on their villages.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_nationalists, excluded,
    organized, generational, trapped, national).

% Yiddishist socialists in Eastern Europe who hold that Jewish cultural life belongs where Jews live and needs no Palestinian anchor. They fight the territorial idea inside Zionist congresses and out, build their own secular school networks, and after losing the argument inside Jewish politics continue it from their own institutions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, bundist_diaspora_autonomists, excluded,
    organized, generational, mobile, continental).

% Reviews annual Mandate reports and hears the petitions — including Arab grievances that never reached the decision rooms — and records the widening gap between Mandate rhetoric and practice, with scrutiny but no power to alter the arrangement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, league_mandates_commission, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates Hebrew-language cultural production in one place: a scattered minority language surviving liturgically but not commercially needs a territorial base where it functions as a daily vernacular, sustaining writers, teachers, publishers, and eventually a university whose output regenerates diaspora cultural life. The center solves for the diaspora what no dispersed community could solve alone.
% TRANSFER_FUNCTION: Moves diaspora wealth (donations, national-fund collections) into Palestinian land and institutions; moves land title from absentee Arab proprietors and their tenant farmers toward Jewish national ownership; moves people — selected for cultural commitment rather than mass rescue — from diaspora cities to Jerusalem, Jaffa, and Haifa; and moves cultural authority within Jewry from traditional rabbinic and Yiddishist elites toward the new Hebrew center.
% ABSENT_VOICES: Palestinian Arabs were structurally absent from every decision that mattered: the Balfour Declaration and the San Remo assignment proceeded without their consent, and land changed hands in transactions among effendi sellers, Zionist purchasers, and imperial registries in which the working tenants figured only as encumbrances. Their voices surfaced afterward — as petitions to the Mandates Commission and as the Arab Executive's delegations and strikes. Inside Jewry, the Bundist autonomists who argued diaspora cultural life needs no territorial anchor were repeatedly outvoted in Zionist forums and carried their case on from outside.
% DISAPPEARANCE_RATIONALE: Overnight disappearance strands the Hebrew revival mid-flight: the university, gymnasium network, presses, and teacher corps lose their base; diaspora Hebrew education loses its prestige anchor and funding circuit; land-title transitions already executed remain executed while the institutional fabric absorbing displaced tenants into wage labor never forms; and the British lose the proxy structure their Mandate policy leaned on. Arrangements on three continents visibly depend on it.
% FOUNDING_PROBLEM: Modern emancipation and secularization were dissolving the traditional religious framework of Jewish life faster than any substitute culture was forming: Jews were acculturating into host nations, Hebrew was narrowing to a liturgical relic, and Ahad Ha'am diagnosed the process as slow national death. The arrangement was built to supply a living territorial center where Hebrew functions as a vernacular, from which regenerated Jewish culture radiates outward.
% FOUNDING_PROBLEM_CORROBORATION: No seat disputes that the founding deficit was real; the dispute concerned the remedy. That the specific problem no longer obtains is attested from outside the benefiting parties by a plain observable — Hebrew cultural institutions now reproduce themselves without diaspora philanthropy — and by contemporaneous outside analysts (Najib Nassar's al-Karmil, Ottoman interior-ministry assessments) and later non-Zionist historians of nationalism, who documented the movement's aims and their supersession. No corroborating source claims the non-sovereign form remains necessary.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the arrangement couples a functioning coordination core (language revival, university, press — outputs independently verifiable) to a real transfer channel (purchase converting tenanted Arab estates into Jewish national property). Neither dominates: the cultural output is genuine and the displacement is bounded but not trivial, placing epsilon mid-range rather than rope-clean or snare-heavy. Suppression 0.62 reflects enforcement that matured across the interval rather than inhering in the cultural program itself: Ottoman title registries and immigration controls gave way to Mandate courts and police that actively serviced land transfer, and by the 1930s holding the settlement fabric required armed guards and emergency regulation — hence the suppression_requirement series is authored (rising enforcement-infrastructure trajectory), not left to the static scalar. Theater 0.30: output-dominated throughout, with a late rise as the non-sovereign ideal turned partly rhetorical once sovereignty existed on the ground. Accessibility collapse 0.42: Yiddishist autonomism, America-centered diasporism, and religious anti-activism remained live alternatives the center never eliminated. Resistance 0.58: opposition on three flanks — intra-Jewish (Bundists, orthodox), imperial (Ottoman suspicion), and Palestinian (sale boycotts, strikes, and finally the 1936-39 revolt, the tenants' coalition attempt at scale, broken at heavy cost). All three series share one seven-point grid (1891-1948) so no metric row is backfilled or scalar-substituted; the trajectory is a ratchet with wartime perturbation, not a cycle, so no cyclical reinforcement claim is authored. Receipt surface: gains were checked seat by seat and none captures them individually — the leadership recycles them into institutions, settlers hold land and posts collectively, the diaspora consumes the output, and the effendis took one-time sale proceeds and exited — so 'diffuse' is an affirmative finding, not a default. Fixing cost is prohibitive for any seat positioned to act: sunk institutional investment, identity fusion, and imperial entanglement priced reversal beyond willingness, as the 1939 partition-and-limits episode's cost curve demonstrated.
 *
 * PERSPECTIVAL GAP:
 *   The same arrangement computes differently by seat, and the divergence is structural rather than rhetorical. From the palestinian_tenant_farmers seat — trapped, powerless, near-full-target directionality — the operation presents at its harshest: a transfer of subsistence executed among sellers, buyers, and registries in which they appear only as an obstruction to be cleared. From the diaspora_jewish_communities seat — mobile exit, subsidy-side directionality — it presents as voluntary giving that returns language, prestige, and meaning. The hebrew_cultural_leadership seat experiences the constraint as something it built and administers, with identity lock making the question of exit unintelligible rather than merely costly. The imperial seats meet it as administrative routine (British) or a suspect anomaly to be policed (Ottoman). The engine derives these divergences from roles, exits, and scopes; the claimed type adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (diaspora communities, intelligentsia, settlers, effendi sellers) derive low d; the trapped victim declaration (tenant farmers) derives d near 1.0. Mobile exit pushes the diaspora communities to the extreme beneficiary end; identity lock holds the leadership low-d but immobile; constrained exit keeps the settlers beneficiary-side though materially exposed. The effendi sellers derive low d despite bearing long-run losses because they captured large immediate proceeds and exited with portable capital — the derivation reads their realized position correctly. No directionality_overrides are authored: the derivation chain produces accurate values for every array-declared agent, and adding overrides would duplicate it. One granularity limit is recorded instead: the Ottoman and British administrations share the institutional power atom while relating oppositely to the arrangement (restrictive versus facilitating), and the override surface keys on power atoms, so it cannot express that split without mispricing one of the two; the differentiation is left qualitative here and to per-seat engine computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no living territorial base for Hebrew culture — was solved by the arrangement's own success, and the non-sovereign form was then overtaken by the sovereignty the reading never required: mandatrophy_resolved is authored true at interval end, and the measurement series records the drift. Classifying as tangled_rope rather than rope keeps the tenant-farming cost inside the account alongside the genuine cultural achievement, correcting the reading's own recurring temptation to book displacement as regrettable friction; classifying it snare-heavier would erase the verifiable coordination output and the sustained binational effort the reading mounted in earnest. The receipt surface (diffuse gains, prohibitive fix) is piton-sided diagnostically, but the theater ratio (output-dominated) and the live coordination function disqualify the piton reading within this interval; the genuinely theatrical remnant — rhetorical binationalism after sovereignty made the form obsolete — is a later, separate constraint outside this file's scope. The mismatch consumer will read founding_problem_status=dead together with disappearance_verdict=world_rearranges as a capture/zombie flag; for the non-sovereign FORM that reading is correct, and it is exactly the obsolescence the series documents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file instantiates only the cultural_zionism_reading of the jewish_territorial_claim kernel; what structurally changes under each sibling reading?',
    'Compare against the sibling story files'' victim sets, enforcement requirements, and epsilon values; the disagreement is located in the sovereignty-necessity premise this reading denies and its siblings variously require.',
    'The political and revisionist siblings authorize sovereignty-plus-majority as constitutive, implying heavier suppression and enlarged victim sets; the labor sibling relocates the coordination function onto economic transformation. Resolving which reading governs the terrain changes the computed classification of the same geography.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a four-reading kernel; siblings are separate constraints, not parameters of this one.').

omega_variable(
    epsilon_referent_discipline,
    'Is epsilon measured for the non-sovereign cultural-center arrangement as this reading assesses it, rather than drifting onto the realized sovereign arrangement that later occupied the same ground?',
    'Audit that the measurement series samples the cultural arrangement''s own operations (philanthropic funding circuits, institution-building, purchase under cultural-aegis programs) and not the successor state''s post-1948 operations.',
    'A referent slip would silently substitute the political reading''s epsilon for this one and corrupt cross-reading comparison; holding the referent keeps the family''s epsilon differences interpretable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_discipline, conceptual, 'Referent discipline for a kernel-reading story: the standing arrangement under contest is the center-without-sovereignty, never the reading''s endorsed or successor alternative.').

omega_variable(
    land_alienation_attribution,
    'How much tenant displacement runs through the cultural program''s purchasing specifically, as against labor-settlement agriculture and speculative purchase by non-cultural actors?',
    'Parcel-level land-registry attribution by purchaser identity and subsequent use: urban institutional parcels (university, gymnasium quarters, press housing) versus plantation and agricultural colonies.',
    'A minimal cultural-attributable share pulls epsilon downward toward rope and softens the tangled_rope claim; a substantial share pushes toward snare-adjacent territory and strengthens the payer seat''s reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_alienation_attribution, empirical, 'Whether the displacement cost belongs to this reading''s arrangement or to its siblings'' settlement modes on shared terrain.').

omega_variable(
    binational_path_feasibility,
    'Was a binational, non-majoritarian framework structurally available during the Mandate window, or was it foreclosed by demography, economics, and regional political reaction regardless of which reading governed?',
    'Counterfactual analysis of the Brit Shalom and Ihud moments: demographic ratios over time, Arab mobilization capacity, imperial incentives, and the price trajectory of the missed windows.',
    'Availability supports this reading''s coordination component as real and its loss as contingent; unavailability marks the reading as structurally naive and implies its epsilon understates what its program actually cost the populations it touched.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_path_feasibility, conceptual, 'Feasibility of the reading''s own endorsed framework — the question on which the reading''s moral standing most depends.').

omega_variable(
    suppression_attribution,
    'How much of the measured suppression is enforcement the arrangement itself required (title litigation, contract enforcement, guard deployment) versus ambient imperial policing and communal violence it rode upon?',
    'Separate security and legal expenditure traceable to cultural-institution initiatives from general Mandate and Ottoman policing budgets; compare enforcement incidents initiated by institutional actors against baseline unrest.',
    'A high ambient share lowers the arrangement''s intrinsic suppression and softens the tangled_rope reading toward rope; a high own-initiated share strengthens the enforcement-dependence the tangled_rope gate requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_attribution, empirical, 'Attribution of the suppression series between the arrangement''s own machinery and its environment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1891, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(czr_jtc_tr_t1891, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1891, 0.12).
narrative_ontology:measurement(czr_jtc_tr_t1901, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1901, 0.14).
narrative_ontology:measurement(czr_jtc_tr_t1911, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1911, 0.17).
narrative_ontology:measurement(czr_jtc_tr_t1921, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1921, 0.2).
narrative_ontology:measurement(czr_jtc_tr_t1931, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1931, 0.23).
narrative_ontology:measurement(czr_jtc_tr_t1941, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1941, 0.27).
narrative_ontology:measurement(czr_jtc_tr_t1948, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1948, 0.3).

% Extraction over time
narrative_ontology:measurement(czr_jtc_be_t1891, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1891, 0.34).
narrative_ontology:measurement(czr_jtc_be_t1901, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1901, 0.38).
narrative_ontology:measurement(czr_jtc_be_t1911, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1911, 0.43).
narrative_ontology:measurement(czr_jtc_be_t1921, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1921, 0.46).
narrative_ontology:measurement(czr_jtc_be_t1931, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1931, 0.55).
narrative_ontology:measurement(czr_jtc_be_t1941, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1941, 0.57).
narrative_ontology:measurement(czr_jtc_be_t1948, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1948, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(czr_jtc_su_t1891, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1891, 0.28).
narrative_ontology:measurement(czr_jtc_su_t1901, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1901, 0.31).
narrative_ontology:measurement(czr_jtc_su_t1911, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1911, 0.35).
narrative_ontology:measurement(czr_jtc_su_t1921, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1921, 0.44).
narrative_ontology:measurement(czr_jtc_su_t1931, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1931, 0.53).
narrative_ontology:measurement(czr_jtc_su_t1941, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1941, 0.58).
narrative_ontology:measurement(czr_jtc_su_t1948, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1948, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Jewish territorial claim' covers four structurally distinct arrangements that differ on the sovereignty-necessity premise, on enforcement burden, and on victim sets; per the epsilon-invariance principle they are authored as four linked stories rather than one observable-parameterized constraint. This file carries the lightest enforcement profile and the smallest (though real) victim set of the family. Edge direction: this reading is upstream of the others in cultural infrastructure — the Hebrew institutions, press, and teacher corps it built were inherited by labor settlement practice and by the political reading's state-building — while the political reading's 1948 realization is downstream of and terminal for this arrangement's non-sovereign form. Sibling epsilons differ accordingly: this file's 0.52 reflects coordination-plus-bounded-displacement; the political and revisionist files should author materially higher enforcement-dependent values; the labor file intermediate with its own victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
