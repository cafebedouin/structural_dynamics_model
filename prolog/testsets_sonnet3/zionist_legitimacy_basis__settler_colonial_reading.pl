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
 *   human_readable: Zionist State Formation Read as European Settler-Colonial Displacement Structure
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This story authors the settler-colonial reading of the Zionist legitimacy
 *   kernel — one of three declared readings of a single contested commitment
 *   (the legitimacy basis of the Zionist project and the state it
 *   established). Under this reading, the founding of Israel is structurally
 *   continuous with European settler-colonial projects: an organized movement
 *   of a demographically distinct population, backed initially by an imperial
 *   mandate power (Britain) and later by independent state capacity,
 *   established sovereignty over an already-inhabited territory by
 *   displacing, subordinating, or excluding the indigenous population, with
 *   displacement functioning as constitutive of the project's success rather
 *   than as an unfortunate byproduct of war. This is emphatically not a story
 *   about which reading is correct — it is the ε-invariant instantiation of
 *   ONE reading, authored from that reading's own internal logic and
 *   evidentiary commitments. The sibling readings (national liberation,
 *   religious restoration) are separate constraint files with their own ε,
 *   beneficiary/victim structures, and classifications; they are not blended
 *   in here.
 *
 * KEY AGENTS:
 *   - post_1948_jewish_immigrant_settlers: beneficiary population receiving land and citizenship rights denied to the displaced
 *   - israeli_state_apparatus: institutional agenda-setter administering the land, citizenship, and military-governance architecture
 *   - palestinian_1948_refugees: primary payer class, denied return under domestic law despite international resolution language
 *   - palestinian_present_absentee_landowners: internally displaced citizens whose confiscation despite continued residency is read as evidence the mechanism is structural, not war-contingent
 *   - israeli_settler_movement: the most self-conscious, organized agent of ongoing territorial extension under this reading
 *   - international_human_rights_bodies: analytical/observer seat corroborating the displacement record from outside the directly contesting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.82).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.78).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist State Formation Read as European Settler-Colonial Displacement Structure").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '5b547e4a-f23c-4f9a-ade8-e48cf16272bc').
narrative_ontology:cs_kernel_codification('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', distributed).
narrative_ontology:cs_authority_grounding('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', distributed).
narrative_ontology:cs_reading_relation('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', foundational, displacement_is_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(displacement_is_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', displacement_is_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', foundational, colonial_structural_position_determines_legitimacy_regardless_of_founding_population_persecution_history).
narrative_ontology:cs_axiom_status(colonial_structural_position_determines_legitimacy_regardless_of_founding_population_persecution_history, holdable).
narrative_ontology:cs_axiom_grounding('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', colonial_structural_position_determines_legitimacy_regardless_of_founding_population_persecution_history, conventional).
narrative_ontology:cs_reference_frame('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', pre_partition_demographic_status_quo_1917).
narrative_ontology:cs_drift_state('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', contemporary_post_2000_settlement_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b547e4a-f23c-4f9a-ade8-e48cf16272bc', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_landholders).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, post_1948_jewish_immigrant_settlers).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_1948_refugees).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_present_absentee_landowners).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_west_bank_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_gaza_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, bedouin_naqab_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_settler_movement).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, national_self_determination_via_territorial_sovereignty).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, demographic_majority_as_legitimating_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Arrived under state-sponsored immigration and absorption programs onto land vacated by expelled or fled Palestinian residents, often via the Absentee Property Law transferring title to the state and then to settlement. Many are themselves refugees of European persecution or expulsion from Arab states; this reading treats their beneficiary position as structural regardless of individual motive or history, which is itself a point of dispute within the reading's own tradition.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, post_1948_jewish_immigrant_settlers, beneficiary,
    organized, generational, constrained, national).

% Administers land law, citizenship law (Law of Return vs. absence of Palestinian right of return), military governance in the West Bank, and the settlement enterprise. Sets and enforces the demographic and territorial architecture that this reading identifies as the mechanism of displacement. Has the institutional capacity to alter policy but treats the current arrangement as constitutive of state legitimacy rather than as a contingent policy choice.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Hold title, use rights, or residency on land and in housing stock whose provenance traces to depopulated Palestinian villages or confiscated property. Benefit from state-subsidized housing, agricultural land allocation via the Jewish National Fund, and legal frameworks (Absentee Property Law, Israel Lands Administration) that this reading treats as the transfer mechanism proper to settler colonialism.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_landholders, beneficiary,
    moderate, biographical, mobile, national).

% Expelled or fled during the 1947-49 war and its aftermath; denied return under Israeli law despite UN General Assembly Resolution 194 language on refugee return. Live in refugee camps or diaspora across the region and world, holding UNRWA-registered status but no path to the land or property lost. This reading treats their exclusion as the structure's constitutive act, not a side effect of war.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_1948_refugees, payer,
    powerless, generational, trapped, regional).

% Internally displaced Palestinian citizens of Israel whose land was confiscated under the Absentee Property Law despite continued residence within the state's borders — a legal category this reading reads as demonstrating that displacement, not war exigency, is the operative mechanism. Hold Israeli citizenship but cannot recover or access ancestral land now held by the state or Jewish agricultural collectives.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_present_absentee_landowners, payer,
    powerless, generational, trapped, national).

% Live under military occupation and administrative law since 1967, subject to settlement expansion, land expropriation for settler infrastructure, checkpoints, and permit regimes governing movement, building, and water access. This reading identifies ongoing settlement construction as the same colonial logic operating in real time rather than a separate post-1967 phenomenon.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_west_bank_residents, payer,
    powerless, biographical, trapped, regional).

% Subject to blockade, movement restriction, and repeated military operations; this reading treats Gaza's status as a further expression of the containment logic applied to a displaced and refugee-majority population unable to return to land of origin inside present-day Israel.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_gaza_residents, payer,
    powerless, biographical, trapped, regional).

% Face ongoing village demolition, land dispossession, and forced relocation into planned townships in the Negev/Naqab under Israeli land and planning law. This reading treats their situation as an internal, less-visible instance of the same settlement-and-displacement structure applied to non-Jewish indigenous populations.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, bedouin_naqab_communities, payer,
    powerless, generational, trapped, regional).

% UN bodies, human rights organizations, and international courts document settlement expansion, demolitions, and refugee status, producing reports (UNRWA, B'Tselem, Amnesty International, ICJ advisory proceedings) that this reading draws on as corroborating evidence external to the parties in direct contest.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Organized settler organizations (e.g. Amana, Yesha Council) actively drive West Bank settlement construction, lobby for land allocation, and receive state subsidy and military protection for expansion. This reading identifies this movement as the sharpest, most self-conscious agent of the displacement structure, distinct from the diffuse 1948-era state apparatus.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_settler_movement, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, israeli_settler_movement, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading there is a real coordination function for the in-group: establishing a sovereign, demographically secured homeland for a historically persecuted diaspora population, consolidating land, security, immigration absorption, and legal protection into a single state apparatus that no longer depends on host-country tolerance.
% TRANSFER_FUNCTION: Moves land title, residency rights, water and agricultural resources, and political sovereignty from the pre-1948 Arab-Palestinian population and their descendants to Jewish-Israeli settlers and the state, principally via expulsion, the Absentee Property Law, military administration, and settlement construction — with denial of the right of return functioning as the mechanism that locks the transfer in place across generations.
% ABSENT_VOICES: Palestinian refugees and their descendants have no direct voice in Israeli policymaking over the land and property this reading identifies as taken from them; UNRWA registration and diaspora advocacy substitute for direct political standing. Their objection — that the transfer was not incidental to statehood but constitutive of it — is precisely the premise excluded from the national liberation and religious restoration readings' own accounts.
% DISAPPEARANCE_RATIONALE: If the demographic and legal architecture this reading identifies (Absentee Property Law, Law of Return asymmetry, military administration of the West Bank, refusal of Palestinian return) were dismantled overnight, land tenure, citizenship composition, and territorial control would be renegotiated from the ground up — the state's current demographic character and territorial boundaries depend on these arrangements remaining in force.
% FOUNDING_PROBLEM: The founding problem as this reading frames it: European Jewish populations, facing pogroms, exclusion, and ultimately genocide, sought a sovereign territory secured by demographic majority and legal control, and selected Ottoman/British Mandate Palestine — already inhabited by an Arab-Palestinian population — as the site, requiring that population's displacement or subordination to achieve the demographic majority the project required.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the 'New Historians' school (Ilan Pappé, Benny Morris's documentary findings on 1948 expulsions, though Morris himself does not adopt the settler-colonial frame normatively) and Palestinian historiography corroborate the displacement record from outside directly-interested advocacy; Israeli state historiography and religious-restoration readings dispute the settler-colonial characterization of the founding problem while not disputing the underlying displacement events themselves.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.82, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.82 at 2024) because the reading identifies land, water, and sovereignty as flowing systematically from a displaced population to a settling one via legally codified mechanisms (Absentee Property Law, differential citizenship pathways, military land administration) rather than incidental war outcomes. Suppression is high (0.78) because return is affirmatively barred by statute and enforced by border and military control, and resistance is high (0.88 at the class level in the grid) because the payer population has never accepted the arrangement as legitimate and has organized continuously against it (diplomatic, legal, and armed resistance across generations). Accessibility collapse is authored moderate rather than near-total (0.45) because, unlike a genuine mountain, the reading holds that alternative arrangements (binational state, right of return, restitution) remain conceptually and legally live, not physically or logically foreclosed — the collapse is political and military, not structural-necessity collapse. Theater ratio is moderate (0.32) reflecting that alongside genuinely functioning security and settlement administration, a portion of the justificatory apparatus (e.g. characterizing 1948 expulsions as voluntary flight, or Absentee Property administration as neutral land management) performs a cover function this reading identifies as theater over the extractive core.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute sharply different seat classifications for the settler and settled populations from identical structural data: beneficiary seats (institutional/organized power, arbitrage/mobile exit) should compute toward tangled_rope or rope-like readings of their own experience (real coordination benefit: safety, sovereignty, belonging), while payer seats (powerless, trapped) should compute toward snare. This divergence is the whole point of the classification — a story whose claimed_type (snare) matches only the payer-seat computed type while beneficiary seats compute differently is not an inconsistency; it is what per-seat computation is for.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary status (post-1948 settlers, landholders, the state apparatus, the organized settler movement) derives low-d treatment: they collect land, security, and demographic majority through the same mechanisms that dispossess the payer classes. Palestinian refugees, present absentees, West Bank and Gaza residents, and Bedouin Naqab communities are all authored as high-d targets: trapped exit options, powerless power atom, and structural or legal exclusion from the resource flows the constraint governs. The state apparatus and settler movement both hold agenda_setter roles because both actively administer and extend the arrangement, though at different institutional scales (state law vs. organized settlement construction) — this differentiates two same-nominal-level institutional actors by their distinct operational leverage over the mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status/corroboration fields are the load-bearing check here: this reading holds the founding problem (securing a persecuted diaspora population against genocide and exclusion via sovereign territorial control) achieved its core aim by 1948 and hardened afterward into an ongoing structural project (continued settlement, denial of return) whose justificatory logic increasingly outruns the original emergency. Corroboration is drawn from historiography outside the immediately benefiting parties (the New Historians' archival findings on 1948, though not all adopt this reading's normative frame) to avoid the mandatrophy trap of a genealogy attested only by beneficiaries. The disappearance_verdict of world_rearranges reflects that under this reading the current demographic and territorial configuration is entirely contingent on the ongoing legal architecture, not natural or inevitable — removing the architecture would materially reorganize land tenure and political power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_framework_applicability,
    'Does the settler-colonial theoretical framework (developed primarily from Anglophone settler states like the US, Canada, Australia, and South Africa) apply structurally to a movement whose founding population was itself a persecuted, often stateless diaspora rather than a metropole''s colonizing citizenry?',
    'Comparative historical-sociological analysis of settler-colonial theory''s defining criteria (metropole sponsorship, logic of elimination, replacement demographic project) against the specific historical record of the Zionist movement''s relationship to British Mandate authority, European antisemitism, and post-1948 state formation.',
    'If the framework does not structurally fit, this reading''s core categorization (settler-colonial) is contestable at the conceptual level even while the empirical displacement record it relies on remains uncontested; if it fits, the reading''s classification is strengthened independent of the sibling readings'' normative claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_framework_applicability, conceptual, 'Whether settler-colonial theory''s defining criteria structurally match the Zionist case despite the founding population''s persecution history.').

omega_variable(
    constitutive_vs_incidental_displacement,
    'Is Palestinian displacement structurally constitutive of Zionist state formation (this reading''s premise) or an incidental, war-caused, and in-principle-reversible byproduct (the national liberation reading''s premise)?',
    'Archival and legal analysis of pre-1948 Zionist institutional planning documents (Jewish Agency, JNF land purchase and settlement patterns), the sequencing and command structure of 1948 expulsions (per New Historians'' archival work), and continuity analysis of post-1948, post-1967, and present-day settlement and land policy to assess whether displacement functioned as a designed mechanism or a contingent wartime outcome.',
    'This is the central variable distinguishing this reading from the national_liberation_reading sibling; if resolved toward ''incidental,'' the settler-colonial reading''s ε would not survive as authored (this is exactly the case for two structurally distinct constraints under the ε-invariance principle — the readings are not reconciled, they are separately authored).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_incidental_displacement, empirical, 'Whether the displacement mechanism is constitutive (this reading) or incidental (sibling reading) to state formation — the load-bearing disagreement between readings.').

omega_variable(
    beneficiary_population_heterogeneity,
    'The beneficiary group post_1948_jewish_immigrant_settlers includes populations who were themselves refugees of genocide, pogrom, or expulsion from Arab states — does treating them uniformly as structural beneficiaries under a colonial-settler frame erase relevant distinctions this reading''s own tradition sometimes contests internally?',
    'Disaggregated analysis distinguishing settler population by immigration wave, prior refugee status, and degree of institutional participation in land-transfer mechanisms, rather than treating the beneficiary class as homogeneous.',
    'A finding of significant heterogeneity would not change the structural classification (beneficiary status is defined by position in the transfer mechanism, not by individual moral desert) but would refine the directionality assignment for specific sub-populations and is relevant to internal debates within settler-colonial scholarship itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_population_heterogeneity, conceptual, 'Whether uniform beneficiary treatment of a heterogeneous immigrant population obscures relevant internal distinctions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1917, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(zion_tr_t2005, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(zion_be_t1917, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.72).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(zion_be_t2005, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2005, 0.74).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1917, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(zion_su_t2005, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.78).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1917, tn=2024
narrative_ontology:measurement(zion_grid_01, zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse(class), 1917, 0.2).
narrative_ontology:measurement(zion_grid_02, zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse(class), 2024, 0.7).
narrative_ontology:measurement(zion_grid_03, zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse(individual), 1917, 0.2).
narrative_ontology:measurement(zion_grid_04, zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse(individual), 2024, 0.75).
narrative_ontology:measurement(zion_grid_05, zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse(organizational), 1917, 0.15).
narrative_ontology:measurement(zion_grid_06, zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse(organizational), 2024, 0.55).
narrative_ontology:measurement(zion_grid_07, zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse(structural), 1917, 0.25).
narrative_ontology:measurement(zion_grid_08, zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse(structural), 2024, 0.45).
narrative_ontology:measurement(zion_grid_09, zionist_legitimacy_basis__settler_colonial_reading, resistance(class), 1917, 0.3).
narrative_ontology:measurement(zion_grid_10, zionist_legitimacy_basis__settler_colonial_reading, resistance(class), 2024, 0.88).
narrative_ontology:measurement(zion_grid_11, zionist_legitimacy_basis__settler_colonial_reading, resistance(individual), 1917, 0.1).
narrative_ontology:measurement(zion_grid_12, zionist_legitimacy_basis__settler_colonial_reading, resistance(individual), 2024, 0.5).
narrative_ontology:measurement(zion_grid_13, zionist_legitimacy_basis__settler_colonial_reading, resistance(organizational), 1917, 0.2).
narrative_ontology:measurement(zion_grid_14, zionist_legitimacy_basis__settler_colonial_reading, resistance(organizational), 2024, 0.65).
narrative_ontology:measurement(zion_grid_15, zionist_legitimacy_basis__settler_colonial_reading, resistance(structural), 1917, 0.2).
narrative_ontology:measurement(zion_grid_16, zionist_legitimacy_basis__settler_colonial_reading, resistance(structural), 2024, 0.6).
narrative_ontology:measurement(zion_grid_17, zionist_legitimacy_basis__settler_colonial_reading, stakes_inflation(class), 1917, 0.2).
narrative_ontology:measurement(zion_grid_18, zionist_legitimacy_basis__settler_colonial_reading, stakes_inflation(class), 2024, 0.8).
narrative_ontology:measurement(zion_grid_19, zionist_legitimacy_basis__settler_colonial_reading, stakes_inflation(individual), 1917, 0.2).
narrative_ontology:measurement(zion_grid_20, zionist_legitimacy_basis__settler_colonial_reading, stakes_inflation(individual), 2024, 0.85).
narrative_ontology:measurement(zion_grid_21, zionist_legitimacy_basis__settler_colonial_reading, stakes_inflation(organizational), 1917, 0.15).
narrative_ontology:measurement(zion_grid_22, zionist_legitimacy_basis__settler_colonial_reading, stakes_inflation(organizational), 2024, 0.6).
narrative_ontology:measurement(zion_grid_23, zionist_legitimacy_basis__settler_colonial_reading, stakes_inflation(structural), 1917, 0.3).
narrative_ontology:measurement(zion_grid_24, zionist_legitimacy_basis__settler_colonial_reading, stakes_inflation(structural), 2024, 0.65).
narrative_ontology:measurement(zion_grid_25, zionist_legitimacy_basis__settler_colonial_reading, suppression(class), 1917, 0.25).
narrative_ontology:measurement(zion_grid_26, zionist_legitimacy_basis__settler_colonial_reading, suppression(class), 2024, 0.85).
narrative_ontology:measurement(zion_grid_27, zionist_legitimacy_basis__settler_colonial_reading, suppression(individual), 1917, 0.15).
narrative_ontology:measurement(zion_grid_28, zionist_legitimacy_basis__settler_colonial_reading, suppression(individual), 2024, 0.75).
narrative_ontology:measurement(zion_grid_29, zionist_legitimacy_basis__settler_colonial_reading, suppression(organizational), 1917, 0.2).
narrative_ontology:measurement(zion_grid_30, zionist_legitimacy_basis__settler_colonial_reading, suppression(organizational), 2024, 0.7).
narrative_ontology:measurement(zion_grid_31, zionist_legitimacy_basis__settler_colonial_reading, suppression(structural), 1917, 0.3).
narrative_ontology:measurement(zion_grid_32, zionist_legitimacy_basis__settler_colonial_reading, suppression(structural), 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, religious_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the zionist_legitimacy_basis kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: settler_colonial_reading (this file, ε=0.82, snare), national_liberation_reading (separate file, expected lower ε under its own account of displacement as incidental war outcome rather than constitutive mechanism), and religious_restoration_reading (separate file, ε authored from a theological-legitimacy frame where territorial claim derives from covenant rather than either colonial mechanism or liberation narrative). All three readings share the same underlying historical events (1917 Balfour Declaration through 2024) but assign different causal and normative structures to those events, producing different beneficiary/victim mappings and different classifications. They are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
