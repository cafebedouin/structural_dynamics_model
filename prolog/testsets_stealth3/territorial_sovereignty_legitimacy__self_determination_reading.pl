% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy — Modern Self-Determination Reading (Continuous Arab Demographic Majority Title)
 *   domain: political theory/international relations/territorial sovereignty
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested kernel 'territorial
 *   sovereignty legitimacy': the modern self-determination reading, under
 *   which rightful sovereignty follows from the self-determination of the
 *   population holding continuous demographic majority residence across the
 *   modern period (19th–20th centuries). On this reading the standing
 *   arrangement under contest — the sovereignty established in 1948 over a
 *   then-minority population's objection, the post-1967 occupations, and the
 *   barred return of the displaced — is an arrangement whose coordination
 *   presentation (unified governance, security, development) rides atop a
 *   sustained transfer of land, water, jurisdiction and residency rights away
 *   from the continuous-residence majority, imposed originally through
 *   external imperial and great-power instruments (Balfour, the Mandate,
 *   partition) rather than through the governed population's consent. KEY
 *   AGENTS (by structural relationship): - palestinian_arab_residents:
 *   Primary target (moderate/trapped) — continuous-residence population
 *   bearing loss of land, planning authority and franchise over the sovereign
 *   governing them. - palestinian_refugee_diaspora: Primary target
 *   (moderate/trapped) — displaced population for whom the arrangement's
 *   central enforcement act is the permanent closing of return. -
 *   israeli_state_institutions: Agenda-setter and receipt-holder
 *   (institutional/identity_locked) — administers the arrangement and
 *   receives its gains. - israeli_jewish_citizenry: Beneficiary
 *   (organized/mobile) — enfranchised population receiving the arrangement's
 *   allocations and protections. - external_great_power_patrons: Secondary
 *   beneficiary (institutional/arbitrage) — converts maintenance of the
 *   arrangement into regional leverage. - un_system_and_international_courts:
 *   Analytical observer — produces the record all seats argue from. -
 *   neighboring_arab_states: Excluded voices — absorb consequences without a
 *   seat in the conversation. Sibling readings are separate constraint files
 *   linked through network.affects_constraints; their differing epsilon
 *   assessments are documented in the network note and the kernel omega
 *   below, not averaged here.
 *
 * KEY AGENTS:
 *   - - palestinian_arab_residents: Primary target (moderate/trapped) — continuous-residence majority population; loses land access, planning authority, water shares and franchise over the governing sovereign.
 *   - - palestinian_refugee_diaspora: Primary target (moderate/trapped) — displaced 1948/1967 population; the arrangement's defining enforcement act is barring restoration of their prior residence.
 *   - - israeli_state_institutions: Agenda-setter (institutional/identity_locked) — administers the arrangement, enforces the return ban, and is the seat the arrangement's gains accrue to.
 *   - - israeli_jewish_citizenry: Primary beneficiary (organized/mobile) — enfranchised recipient of allocations, protections and electoral primacy under the arrangement.
 *   - - external_great_power_patrons: Secondary beneficiary (institutional/arbitrage) — trades military and diplomatic maintenance for regional leverage.
 *   - - un_system_and_international_courts: Analytical observer (institutional/analytical) — custodian of the documentary and juridical record.
 *   - - neighboring_arab_states: Excluded (organized/constrained) — border and refugee consequences without a seat in the substantive conversation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.84).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.8).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy — Modern Self-Determination Reading (Continuous Arab Demographic Majority Title)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political theory/international relations/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, 'b703aa4d-1478-4b98-86df-c47f3ef0b341').
narrative_ontology:cs_kernel_codification('b703aa4d-1478-4b98-86df-c47f3ef0b341', formalized).
narrative_ontology:cs_authority_grounding('b703aa4d-1478-4b98-86df-c47f3ef0b341', lineage).
narrative_ontology:cs_interpretation_layer_present('b703aa4d-1478-4b98-86df-c47f3ef0b341').
narrative_ontology:cs_reading_relation('b703aa4d-1478-4b98-86df-c47f3ef0b341', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('b703aa4d-1478-4b98-86df-c47f3ef0b341', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('b703aa4d-1478-4b98-86df-c47f3ef0b341', foundational, demographic_continuity_confers_exclusive_title).
narrative_ontology:cs_axiom_status(demographic_continuity_confers_exclusive_title, holdable).
narrative_ontology:cs_axiom_grounding('b703aa4d-1478-4b98-86df-c47f3ef0b341', demographic_continuity_confers_exclusive_title, conventional).
narrative_ontology:cs_axiom('b703aa4d-1478-4b98-86df-c47f3ef0b341', foundational, externally_imposed_allocation_void_of_consent).
narrative_ontology:cs_axiom_status(externally_imposed_allocation_void_of_consent, holdable).
narrative_ontology:cs_axiom_grounding('b703aa4d-1478-4b98-86df-c47f3ef0b341', externally_imposed_allocation_void_of_consent, deontological).
narrative_ontology:cs_reference_frame('b703aa4d-1478-4b98-86df-c47f3ef0b341', demographic_continuity_self_determination).
narrative_ontology:cs_drift_state('b703aa4d-1478-4b98-86df-c47f3ef0b341', contemporary_standing_arrangement, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b703aa4d-1478-4b98-86df-c47f3ef0b341', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_citizenry).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugee_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, external_great_power_patrons).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, modern_self_determination_principle).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, demographic_majority_title_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, status_quo_ante_restitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected government, courts, military and land administration of the state established in 1948. They define who may enter, build, vote and own across the territory, administer the regimes in the areas captured in 1967, and enforce the bar on large-scale refugee return. Revenues, conscription base and coalition politics depend on continuing to administer the territory as constituted; abandoning that role would require re-founding the state's own premise, which no governing coalition has ever attempted.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, identity_locked, regional).

% The population enfranchised by the state receives housing, infrastructure, water allocations and security organized under its jurisdiction, including in localities built on land depopulated in 1948 or occupied after 1967. Individual members can emigrate under reciprocal arrangements and many hold second citizenships, though the large majority remain. They bear taxation and conscription costs of the security apparatus while receiving the arrangement's protections, land allocations and electoral primacy.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_citizenry, beneficiary,
    organized, biographical, mobile, national).

% The Arabic-speaking population descended from continuous residence across the 19th and 20th centuries. Those inside Israel proper hold citizenship but face documented gaps in land access, planning rights and budget allocation; those in the West Bank and Gaza live under military or blockade administration without a vote in the state that governs their borders, water and construction. Movement, permits and family unification are administered by authorities they cannot elect. Permanent departure is physically possible but forfeits any claim to return.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_residents, payer,
    moderate, generational, trapped, national).

% Families displaced in 1948 and 1967, registered with UNRWA across Jordan, Lebanon, Syria and beyond, numbering in the millions by descent. They hold no operative remedy to reclaim property or residence: return is barred at the border by the standing state, compensation schemes have never functioned, and host-country integration ranges from partial citizenship to legal exclusion. The claim is carried in registration cards and house keys rather than in any accessible exit.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugee_diaspora, payer,
    moderate, generational, trapped, continental).

% Foreign states, principally the United States, that supply the standing state with military aid, diplomatic shielding at the Security Council and preferential trade treatment, receiving in return basing cooperation, intelligence ties and regional leverage. They also fund relief for the displaced population through UNRWA while sustaining the arrangement that produced the displacement. Commitments are revisable on election cycles; several patrons have shifted posture before.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, external_great_power_patrons, beneficiary,
    institutional, generational, arbitrage, global).

% United Nations organs, the International Court of Justice and treaty bodies that issued the partition resolution, maintain the refugee registry, and periodically rule on the legality of settlements, the separation barrier and the occupation. They produce the authoritative documentary record that all parties argue from, but command no independent enforcement.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, un_system_and_international_courts, observer,
    institutional, civilizational, analytical, global).

% States hosting large refugee populations and sharing borders — Jordan, Lebanon, Syria, Egypt — that fought the arrangement's founding wars, later signed treaties with its state, and now stand largely outside the substantive conversation about the territory's status despite absorbing its displaced people and bearing its border and water consequences. Normalization channels proceed around them on the core status questions.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, neighboring_arab_states, excluded,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__self_determination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single operative jurisdiction over the territory: unified administration, currency, utilities, courts and security for those living under it, resolving competing communal and individual title claims into one enforceable registry instead of overlapping systems.
% TRANSFER_FUNCTION: Moves land title, water shares, planning authority, residency rights and electoral jurisdiction from the Arab resident and refugee population to the state's institutions and enfranchised citizenry; moves taxation and conscription burdens onto the citizenry; moves patron-state aid flows into maintenance of the security apparatus.
% ABSENT_VOICES: The refugee diaspora — millions whose legal status the arrangement defines — sits outside every negotiation forum; West Bank and Gaza residents vote in no election of the state administering them; descendant communities of depopulated villages hold no seat. They are present in UNRWA registries, exile political bodies and court dockets, not in the rooms where the arrangement is maintained or renegotiated.
% DISAPPEARANCE_RATIONALE: Every neighboring state's border regime, the region's alliance architecture, the status of several million registered refugees, and the domestic coalitions of the standing state are organized around the arrangement's continuation. Overnight removal would reopen title, residency and property questions across the whole territory simultaneously — forcing immediate, likely violent, renegotiation of jurisdiction and belonging.
% FOUNDING_PROBLEM: Late-Ottoman collapse left the territory's status unresolved while persecution in Europe drove a nationalist movement seeking statehood there and imperial Britain sought a manageable client position in the region. The arrangement was built to solve the European Jewish displacement crisis and British administrative needs by allocating the territory — over the recorded objection of its then-demographic majority.
% FOUNDING_PROBLEM_CORROBORATION: Mandate-era censuses, the Peel Commission hearings and League of Nations documents — archives held by no current party — corroborate both the demographic composition of the period and the majority's recorded objection; UNRWA registration records corroborate the scale and persistence of displacement. Advocates of the arrangement attest the original refuge problem as persistently live, citing ongoing antisemitism and threatened wars of elimination; the reading's own tradition attests it as solved and superseded by the cost imposed on the resident majority. No attester spans both verdicts, hence contested.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.84 at interval end) because the reading locates the arrangement's core operation in a one-directional transfer — land title, water, planning jurisdiction and the right of residence moved from the continuous-residence majority to the state and its enfranchised citizenry, with the displaced population's restoration affirmatively prevented rather than merely unfunded. Suppression is high (0.80) and is authored as a raw structural property, unscaled by power or scope: the arrangement persists through border closure against return, permit and movement regimes, and military administration — mechanisms that close exits rather than persuade. Theater ratio rises sharply at the 1993 observation point: the negotiated-process architecture performed resolution while facts on the ground advanced, which is precisely the proxy-substitution signature the temporal track exists to catch. Accessibility collapse is moderate (0.60): exits available to the targeted population are largely closed (permanent departure forfeits return; internal movement is administered), but the normative and international arenas retain live alternatives — adjudication, sanctions advocacy, restitution claims — so alternatives are narrowed, not annihilated. Resistance is high (0.72): sustained uprisings, civil mobilization and standing international litigation meet the arrangement continuously. All three tracked series run on one shared eight-point grid (1917–2023) so no metric's row is silently filled by another's end-state. Claim and metrics are independent authored facts: the reading claims the arrangement is a snare (its coordination presentation is, on this reading, cover for maintained dispossession with identifiable victims and forcibly closed exits); the metrics are authored to describe what is observably true, and the engine computes per-seat classifications from the structural data regardless of this claim.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute sharply different types. From the israeli_state_institutions seat, the arrangement presents as governance it built and administers — coordination with enforcement costs — and the identity-lock of the administering institutions fuses the state's self-conception with holding the territory, so exit from the arrangement reads as self-dissolution. From the palestinian_arab_residents and palestinian_refugee_diaspora seats, the identical structure operates as enforced transfer with no accessible remedy: the same registry that secures one seat's property is the instrument that extinguishes the other's. The enfranchised citizenry seat experiences mostly benefit with taxation and conscription overhead; the patron seat experiences pure positional gain at zero territorial exposure. The observer seat computes a juridical object whose classification is itself the dispute. The engine derives these divergences from power, exit and directional data; nothing in this commentary adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation. israeli_state_institutions appears in beneficiaries and is the named receipt seat, placing it near the beneficiary end — corrected modestly upward from the floor because it alone funds and staffs the enforcement apparatus. israeli_jewish_citizenry sits near the beneficiary end with dampened effective extraction, softened further by real (if unused) individual mobility. palestinian_arab_residents and palestinian_refugee_diaspora are declared victims with trapped exit: trapped or identity-bound targets sit nearest the full-target end, and the diaspora's multi-generation entrapment amplifies rather than decays the reading's assessment. external_great_power_patrons are indirect beneficiaries — positional gains without exposure to the arrangement's costs — sitting near the beneficiary end via arbitrage-grade exit. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms reproduce the intended structure without correction, and adding overrides here would assert distinctions the structural data already carries.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two opposite mislabels. First, labeling the arrangement a rope on the strength of its genuine governance residue — courts, utilities, a functioning land registry — would credit coordination that, on this reading, operates inseparably from the transfer it enforces; the victims declaration plus the suppressed-return structure block that reading, and the theater series shows the negotiated-process layer peaking (0.40–0.45) exactly while measured extraction resumed climbing, which is the metric-substitution signature rather than functional recovery. Second, labeling it a piton would require diffuse receipts and no concentrated beneficiary; here the receipt surface names a concrete capturing seat and fixing is prohibitive, which is the snare cell, not the inertial one. The R5 interview feeds the obsolescence check: the founding problem (refuge for persecuted European Jewry, resolved by external allocation) is contested rather than dead — proponents attest persistent danger, the reading attests accomplishment-at-others'-expense — so the dead-status-plus-world-rearranges zombie flag does not fire spuriously, while the contested status keeps the genealogy open to corroboration from Mandate-era archives that belong to no current party. Mandatrophy resolution here is thus a boundary-keeping result: the structure is neither a sunsetted transitional device nor an empty shell, but an actively maintained arrangement whose maintenance burden is the tell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the kernel territorial_sovereignty_legitimacy (reading: self_determination_reading). What changes structurally if a sibling reading is adopted instead?',
    'Comparative classification across the sibling constraint files: covenant_continuity_reading would raise the standing arrangement''s perceived legitimacy and cut epsilon sharply (recognition instruments count as title-conferring); existential_matrix_reading suspends the juridical ledger altogether and scores survival-capacity asymmetries instead of demographic title.',
    'The victim set, the epsilon value, and the computed type are all reading-indexed. Under the covenant sibling the same referent computes far closer to a defended-but-legitimate structure; under the existential sibling the categories ''victim'' and ''beneficiary'' partially lose grip. Nothing about this file''s numbers transfers across readings without re-derivation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Reading-indexing of epsilon and victim structure within the territorial sovereignty kernel.').

omega_variable(
    temporal_scope_of_title,
    'Does restricting the title window to continuous residence during the modern period (19th–20th centuries) decide the title question, or would wider temporal scopes (pre-modern residence layers, older population strata) dilute or redistribute the exclusive demographic-majority claim?',
    'Historical-demographic reconstruction of residence layers across successive periods, and analysis of which scope choices each legal tradition treats as dispositive.',
    'If the modern window is arbitrary rather than principled, the reading''s exclusive-title foundation weakens and partial or layered title accounts compete; if the modern window tracks the birth of the self-determination norm itself, the restriction is internally justified and the claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_scope_of_title, empirical, 'Whether the modern-period scoping of demographic title is principled or selective.').

omega_variable(
    restoration_executability_after_displacement,
    'Does the right of return, framed as restoration of the status quo ante, remain materially executable after generations of displacement, population growth on both sides, and urbanization of the former sites?',
    'Demographic and land-use modeling of return scenarios at varying scales, combined with comparative study of implemented historical restorations.',
    'Full executability supports the reading''s restoration framing as a live remedy; partial executability converts the claim into a bargaining chip whose valuation others set; non-executability would push the reading''s remedy toward symbolic or compensatory forms, changing the constraint''s practical stakes though not its legitimacy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_executability_after_displacement, preference, 'Material executability of status quo ante restoration versus its normative force.').

omega_variable(
    governance_coordination_vs_cover_boundary,
    'Are the arrangement''s governance functions — courts, utilities, land registration, public health — a genuine coordination contribution separable from the dispossession they are embedded in, or structurally inseparable instruments of it?',
    'Counterfactual analysis of parallel jurisdictions providing equivalent services without the transfer component (comparable administrations in the region), plus audit of which services fail when severed from the transfer machinery.',
    'If separable, part of the measured burden reflects ordinary statehood cost rather than the arrangement''s specific operation, and the reading''s net-assessment shifts accordingly; if inseparable, the coordination presentation is fully absorbed into the transfer account and the snare characterization strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_coordination_vs_cover_boundary, conceptual, 'Separability of service-provision coordination from the transfer it administers.').

omega_variable(
    counterfactual_partition_alternatives,
    'Was any allocation formula available to the external decision-makers in 1947 capable of satisfying the self-determination of both populations, or did the demographic geography make every partition an imposition on someone?',
    'Reconstruction of the partition proposals considered (including federal and cantonal variants), their rejection histories, and demographic-geographic feasibility analysis of each.',
    'If a consent-compatible formula existed and was rejected, the imposition charge sharpens against the rejecting parties specifically; if none existed, ''partition as unjust imposition'' generalizes into a critique of the partition method itself, altering which actors the reading indicts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_partition_alternatives, conceptual, 'Existence of consent-compatible allocation alternatives at the founding juncture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1917, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_sdr_tr_t1917, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(tsl_sdr_tr_t1929, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1929, 0.12).
narrative_ontology:measurement(tsl_sdr_tr_t1947, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1947, 0.2).
narrative_ontology:measurement(tsl_sdr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(tsl_sdr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(tsl_sdr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(tsl_sdr_tr_t2005, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(tsl_sdr_tr_t2023, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2023, 0.42).

% Extraction over time
narrative_ontology:measurement(tsl_sdr_be_t1917, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1917, 0.3).
narrative_ontology:measurement(tsl_sdr_be_t1929, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1929, 0.38).
narrative_ontology:measurement(tsl_sdr_be_t1947, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1947, 0.52).
narrative_ontology:measurement(tsl_sdr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.74).
narrative_ontology:measurement(tsl_sdr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(tsl_sdr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.76).
narrative_ontology:measurement(tsl_sdr_be_t2005, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2005, 0.79).
narrative_ontology:measurement(tsl_sdr_be_t2023, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2023, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(tsl_sdr_su_t1917, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1917, 0.2).
narrative_ontology:measurement(tsl_sdr_su_t1929, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1929, 0.35).
narrative_ontology:measurement(tsl_sdr_su_t1947, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1947, 0.45).
narrative_ontology:measurement(tsl_sdr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(tsl_sdr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(tsl_sdr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.62).
narrative_ontology:measurement(tsl_sdr_su_t2005, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(tsl_sdr_su_t2023, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, resource_allocation).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, existential_matrix_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who is sovereign legitimacy grounded in for this territory' decomposes into three structurally distinct constraints (readings of one kernel), written as separate files with separate epsilon values: this file (self_determination_reading) scores the standing arrangement as the continuous-residence Arab majority's title-bearer sees it — high extraction, suppressed returns; covenant_continuity_reading scores the same arrangement through divine-grant-plus-recognition lenses — substantially lower epsilon; existential_matrix_reading declines the juridical accounting entirely and scores survival-capacity instead. The readings disagree about the same referent, not about different topics; each file keeps one stable epsilon over that shared referent per the epsilon-invariance rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
