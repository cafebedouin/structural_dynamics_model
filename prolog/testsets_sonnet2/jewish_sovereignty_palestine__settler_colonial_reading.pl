% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Zionist Settlement as Settler-Colonial Displacement Regime
 *   domain: political/postcolonial
 *
 * SUMMARY:
 *   This constraint instantiates the settler-colonial reading of the
 *   jewish_sovereignty_palestine kernel: it treats Jewish immigration to and
 *   statehood in Palestine/Israel as structurally continuous with European
 *   settler-colonial patterns elsewhere (Algeria, South Africa, Rhodesia,
 *   North America), in which an external metropole licenses or enables
 *   population transfer into an already-inhabited territory, producing
 *   displacement of the indigenous population regardless of the settlers' own
 *   motivations, including flight from persecution. The reading's ε is
 *   authored for the standing displacement arrangement it describes — legal,
 *   territorial, and demographic — as this reading's own lights assess it,
 *   not for any resolution (binational state, right of return, restitution)
 *   the reading might endorse. That endorsed alternative is not the referent
 *   and would not make ε near zero; the referent is the arrangement under
 *   contest.
 *
 * KEY AGENTS:
 *   - palestinian_indigenous_population: primary structural victim (powerless/trapped) — bears displacement
 *   - jewish_settler_population: structural beneficiary and, in earlier periods, refugee-victim of a separate persecution regime — dual position this reading holds without resolving the tension
 *   - british_mandate_authority: initiating agenda-setter — licensed the demographic transfer for imperial strategic ends
 *   - us_imperial_strategic_interests: successor beneficiary — sustains the arrangement post-1948 for its own strategic returns
 *   - international_human_rights_bodies: analytical observer — documents without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.82).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.78).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Zionist Settlement as Settler-Colonial Displacement Regime").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, 'ac8bb8d6-92d3-428c-b6bb-eb7a9351e047').
narrative_ontology:cs_kernel_codification('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', distributed).
narrative_ontology:cs_authority_grounding('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', extraction).
narrative_ontology:cs_interpretation_layer_present('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047').
narrative_ontology:cs_reading_relation('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', foundational, settler_intent_structurally_irrelevant_to_displacement_classification).
narrative_ontology:cs_axiom_status(settler_intent_structurally_irrelevant_to_displacement_classification, holdable).
narrative_ontology:cs_axiom_grounding('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', settler_intent_structurally_irrelevant_to_displacement_classification, conventional).
narrative_ontology:cs_axiom('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', foundational, metropole_sponsorship_constitutes_colonial_relation_regardless_of_sponsor_identity).
narrative_ontology:cs_axiom_status(metropole_sponsorship_constitutes_colonial_relation_regardless_of_sponsor_identity, holdable).
narrative_ontology:cs_axiom_grounding('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', metropole_sponsorship_constitutes_colonial_relation_regardless_of_sponsor_identity, empirically_contingent).
narrative_ontology:cs_reference_frame('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', pre_mandate_indigenous_demographic_and_land_tenure_baseline).
narrative_ontology:cs_drift_state('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', post_oslo_negotiation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ac8bb8d6-92d3-428c-b6bb-eb7a9351e047', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_authority).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_strategic_interests).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees_1967).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, settler_colonial_theory_applicability_to_zionism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Resident agrarian and urban population of Mandate Palestine before and during Jewish immigration waves. Loses land through purchase-and-transfer mechanisms, later through military conquest and legal exclusion from return. Has no sovereign instrument to prevent displacement and is subject to expulsion, permit regimes, and demographic engineering aimed at reducing their presence within the territory claimed for Jewish statehood.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, regional).

% Population expelled or fled during the 1948 war and denied return under Israeli law (Absentees' Property Law) while displaced Jewish immigrants were simultaneously granted automatic citizenship and settlement rights on the vacated land. Their claims are structurally unenforceable absent a change in the sovereign's own founding legal architecture.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Population displaced from the West Bank, Gaza, and Golan following the 1967 war, subject to ongoing settlement expansion into occupied territory and to permit, checkpoint, and land-appropriation regimes that continue the demographic transfer logic identified in this reading as constitutive of the founding pattern.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees_1967, payer,
    powerless, generational, trapped, regional).

% Immigrants and their descendants who receive land, citizenship, and state protection through the Zionist project; in this reading, their individual motivations (flight from European antisemitism, religious conviction, socialist utopianism) do not alter the structural fact that settlement proceeds through displacement of the indigenous population. Some bear real historical trauma as refugees themselves, which this reading treats as separate from, not a justification for, the structural position they occupy relative to Palestinians.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population, payer).

% Issued the Balfour Declaration and administered the Mandate, licensing Jewish immigration and land purchase as an instrument of imperial strategy (securing the Suez corridor and regional influence) while formally holding trusteeship obligations toward the existing population. Withdrew in 1948 having set the demographic and legal conditions the successor state inherited.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_authority, agenda_setter,
    institutional, biographical, arbitrage, regional).

% Post-1948 patron whose military aid, diplomatic cover at the UN, and strategic partnership sustain the settlement and occupation infrastructure in exchange for a reliable regional military and intelligence asset. Bears no direct cost of displacement and can adjust the relationship without existential risk to itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, us_imperial_strategic_interests, beneficiary,
    institutional, civilizational, arbitrage, global).

% UN agencies, human rights organizations, and international courts that document displacement, issue advisory opinions, and produce reports characterizing settlement expansion and refugee non-return as violations of international law, without enforcement power to compel remedy.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% European Jewish communities facing pogroms, legal exclusion, and eventually genocide who are not party to the colonial-metropole decision-making that licensed the Mandate but whose desperate need for refuge is instrumentalized by this reading as a structural input rather than treated as the framework's central moral fact — a tension this reading names but does not resolve.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, diaspora_jewish_communities_pre_state, excluded,
    powerless, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__settler_colonial_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transfer of a defined territory from indigenous control to an incoming settler population under an external sponsoring power, using immigration licensing, land purchase law, and eventually military force as the coordinating instruments.
% TRANSFER_FUNCTION: Moves land, water rights, political sovereignty, and physical presence from the indigenous Palestinian population to Jewish settlers and the state they establish; moves strategic regional influence from the mandate/patron power's rivals to itself.
% ABSENT_VOICES: Palestinian refugees denied return have no seat in Israeli legal or electoral processes that determine land and citizenship law; their claims are adjudicated, if at all, in international forums with no enforcement power over the sovereign that holds the land.
% DISAPPEARANCE_RATIONALE: If the displacement regime were reversed or dissolved — full right of return, reparative land restitution, dismantling of demographic-preference legal architecture — the territorial, demographic, and political structure of the entire region would be fundamentally reorganized: current land tenure, citizenship law, and the two-tier legal regime in occupied territory would all have to be rebuilt from a different baseline.
% FOUNDING_PROBLEM: From this reading's perspective, the arrangement was built to resolve the colonial metropole's strategic interest in a reliable regional foothold and the Zionist movement's demographic and territorial project, using Jewish immigration (including genuine refugees) as the settlement vehicle — not to resolve Palestinian displacement, which is the arrangement's product, not its problem.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian testimony, UN Relief and Works Agency historical records, and Israeli 'new historian' scholarship (Benny Morris's documentation of 1948 expulsions, Ilan Pappé's demographic analysis) attest to the displacement mechanism from outside the beneficiary population; British Mandate cabinet correspondence corroborates the strategic-interest framing of the original license. Jewish settler and Zionist institutional sources dispute this framing entirely and offer the sibling readings instead — this reading's corroboration is contested at its root, which the omega variables below document rather than paper over.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.82, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.82 by 2024) because the reading holds that land, sovereignty, and physical presence transferred from an indigenous population to an incoming settler population under conditions the indigenous population never consented to and cannot reverse through ordinary legal process — this is the zero-sum territorial logic named in the structural delta. Suppression is authored high (0.78) because maintaining the arrangement requires an ongoing apparatus (permit regimes, land law, military administration in occupied territory, non-return statutes) actively preventing the displaced population from reversing the transfer; this is not incidental friction but constitutive enforcement. Accessibility collapse is authored moderate (0.4), not near-mountain levels, because this reading holds the arrangement is a contingent historical-political construction, not a natural or inevitable one — alternatives (binational state, federated sovereignty, restitution) remain conceptually and often politically live, which is precisely why resistance is authored very high (0.88): the arrangement is persistently and visibly contested by its victims, by international bodies, and by a scholarly literature explicitly organized around contesting it. Theater ratio rises after 1993 (Oslo era) reflecting this reading's assessment that negotiation processes increasingly performed resolution without altering underlying land and demographic trajectories, then moderates as that performative frame itself lost credibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian populations across all three stakeholder entries are authored as high-d targets: trapped exit, powerless power atom, generational time horizon under conditions they did not choose and cannot exit. The British Mandate authority and later U.S. strategic interests are authored as low-d beneficiaries with arbitrage-grade exit — they can and did adjust their level of involvement without bearing the structural costs they helped set in motion. Jewish settler population is authored with a genuine dual position (beneficiary + payer secondary role) because this reading holds that individual settlers, especially early refugees from European antisemitism and genocide, occupy real victim status in a SEPARATE persecution structure while simultaneously occupying structural-beneficiary position relative to Palestinians in THIS constraint — the reading names this tension explicitly rather than collapsing it in either direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored 'live' rather than 'dead' because this reading holds the displacement mechanism (demographic preference in citizenship and land law, non-return statutes, settlement expansion) continues to operate in materially the same form as at founding, not merely as inertial residue — this is the key move that keeps the classification from softening toward piton. The disappearance_verdict of world_rearranges reflects that stakeholders' concrete legal and territorial positions depend on the arrangement's continuation, distinguishing this from a constraint whose function has evaporated leaving only theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    refugee_settler_dual_status_resolution,
    'Does the settler-colonial framework adequately account for the moral distinctiveness of settlers who were themselves refugees fleeing genocide, or does treating flight-from-persecution as structurally equivalent to metropole-sponsored settlement erase a morally relevant asymmetry between, e.g., 19th-century Algerian French colonists and 1930s-40s Jewish refugees from Nazi Europe?',
    'Comparative historical analysis of settler-colonial cases with and without a genocide-flight component in the settling population, examining whether the analytic category holds constant explanatory power across both, or whether refugee status introduces a structural variable existing settler-colonial theory does not yet formalize.',
    'If the distinction is structurally load-bearing, this reading''s flat ''regardless of intent'' framing overstates uniformity and a modified reading (perhaps closer to post_zionist or a hybrid) would need to differentiate extraction contribution by settler cohort and period; if the distinction is not load-bearing at the structural level (as this reading holds), the current unified treatment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_settler_dual_status_resolution, conceptual, 'Whether refugee-status settlers are structurally distinguishable from metropole-sponsored settlers within settler-colonial theory.').

omega_variable(
    metropole_beneficiary_continuity,
    'Is the causal chain from British Mandate strategic interest to U.S. strategic patronage genuinely continuous (one displacement regime with a changed sponsor) or are these two structurally distinct arrangements that happen to share a territory and victim population?',
    'Archival and diplomatic-history tracing of the transition period (1948-1967) to establish whether U.S. involvement reproduces the same legal/demographic mechanisms Britain licensed, or introduces materially different mechanisms that would warrant treating post-1967 occupation as a related but separate constraint.',
    'If continuous, this single-story treatment (1917-2024, one ε trajectory) is structurally sound; if discontinuous, the ε-invariance principle would require decomposing this story into a Mandate-era constraint and a post-1967 occupation constraint, each with its own ε and network link.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metropole_beneficiary_continuity, empirical, 'Whether treating 1917-2024 as one continuous constraint versus two linked constraints changes the classification.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_narrative,
    'Should this reading''s cs_structure treat the kernel as the territorial/demographic claim itself (as authored below), or as the layered legitimacy narrative (international law framework, UN partition resolution, subsequent state recognition) that adjudicates whose territorial claim counts as authoritative?',
    'Compare classification outcomes under both framings: the territorial-claim framing centers Balfour/Mandate licensing as the kernel; the legitimacy-narrative framing would center UNGA 181 and subsequent international recognition as the kernel, with authority_grounding shifting from extraction toward distributed (competing international bodies).',
    'Under the authored territorial-claim framing, authority_grounding=extraction fits (Mandate authority extracted strategic benefit from licensing settlement). Under the legitimacy-narrative framing, authority_grounding would trend toward distributed (no single adjudicating body), which would remove interpretation_layer_present eligibility. This story adopts the territorial-claim framing because the settler-colonial reading''s own analytic focus is the displacement mechanism, not the legitimacy discourse layered above it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_narrative, conceptual, 'Alternative CS framings (territorial claim vs. legitimacy narrative) that would change authority_grounding classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t1917, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement_basis(jewi_tr_t1993, observed).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement_basis(jewi_tr_t2005, observed).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2024, 0.3).
narrative_ontology:measurement_basis(jewi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1917, 0.45).
narrative_ontology:measurement_basis(jewi_be_t1917, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1993, 0.78).
narrative_ontology:measurement_basis(jewi_be_t1993, observed).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement_basis(jewi_be_t2005, observed).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2024, 0.82).
narrative_ontology:measurement_basis(jewi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1917, 0.35).
narrative_ontology:measurement_basis(jewi_su_t1917, observed).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.78).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1993, 0.72).
narrative_ontology:measurement_basis(jewi_su_t1993, observed).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2005, 0.76).
narrative_ontology:measurement_basis(jewi_su_t2005, observed).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2024, 0.78).
narrative_ontology:measurement_basis(jewi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__settler_colonial_reading, 0.1).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the jewish_sovereignty_palestine kernel, each authored as a separate ε-invariant constraint per the decomposition principle: liberal_nationalist_reading (Jewish self-determination right legitimately exercised; low-to-moderate ε), religious_zionist_reading (theological fulfillment; ε authored from the theological claim's own internal logic), cultural_zionist_reading (spiritual/cultural project without political domination requirement; low ε), post_zionist_reading (statehood achieved but founding ethnic-national framework now obstructs civic equality; moderate ε concentrated on internal Israeli civic structure rather than the founding displacement). This settler_colonial_reading authors the highest ε among the five because it treats the displacement mechanism itself, rather than any of the movement's self-justifying narratives, as the object of measurement. Each sibling constraint documents this same kernel-decomposition relationship in its own commentary.kernel_context field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
