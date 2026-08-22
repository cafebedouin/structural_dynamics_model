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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Zionist Legitimacy via Settler-Colonial Displacement
 *   domain: political/historical/postcolonial
 *
 * SUMMARY:
 *   Zionism, in the settler-colonial reading, is understood as a European
 *   political movement that used nationalist and religious framing to
 *   legitimize the establishment of a Jewish-majority state in Palestine
 *   through the systematic dispossession of the Palestinian indigenous
 *   population. Beginning in the 1880s with migration and land purchase,
 *   intensifying under the British Mandate (1920–1948), and consolidating
 *   through military and legal enforcement after 1948, the constraint
 *   operates by: (1) defining Jewish settlement as restoration/return rather
 *   than colonization; (2) treating Palestinian presence and rights as
 *   obstacles to be removed rather than indigenous claims to be negotiated;
 *   (3) using state institutions to enforce demographic and territorial
 *   dominance; (4) suppressing Palestinian political voice and right of
 *   return through legal, military, and diplomatic means. This reading places
 *   the settler-colonial structure at the center of what other readings frame
 *   as national liberation or religious restoration. The constraint's
 *   persistence depends on active enforcement (military occupation, legal
 *   discrimination, land-acquisition mechanisms) and on narrative
 *   suppression—the delegitimization of Palestinian historical presence and
 *   rights claims in Western institutional discourse.
 *
 * KEY AGENTS:
 *   - Jewish European settler population: migrated to Palestine, built institutional networks, established political/military authority, define state legitimacy and Palestinian status
 *   - Palestinian indigenous population: majority population with established property rights and social structures, systematically dispossessed, transformed into refugees or permanent minority under occupation
 *   - Zionist institutional hierarchy: manages settlement, land law, military enforcement, narrative production, controls state apparatus and exit options for Jewish participants
 *   - Arab dispossessed refugees: live in camps and diaspora, barred from return, economically and legally subordinated, bear the costs of statelessness
 *   - Western imperial powers: facilitated the Mandate, provide strategic and military support, benefit from regional dominance, maintain gatekeeping over legitimacy narratives
 *   - Postcolonial scholarship: analyzes the structure through indigenous-rights and decolonial frameworks, documents displacement and comparative colonialism, largely excluded from institutional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.82).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.79).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy via Settler-Colonial Displacement").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political/historical/postcolonial").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '9e3de589-3984-4398-82de-fde4d3db1a1e').
narrative_ontology:cs_kernel_codification('9e3de589-3984-4398-82de-fde4d3db1a1e', distributed).
narrative_ontology:cs_authority_grounding('9e3de589-3984-4398-82de-fde4d3db1a1e', extraction).
narrative_ontology:cs_interpretation_layer_present('9e3de589-3984-4398-82de-fde4d3db1a1e').
narrative_ontology:cs_reading_relation('9e3de589-3984-4398-82de-fde4d3db1a1e', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('9e3de589-3984-4398-82de-fde4d3db1a1e', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('9e3de589-3984-4398-82de-fde4d3db1a1e', foundational, palestinians_are_indigenous_population).
narrative_ontology:cs_axiom_status(palestinians_are_indigenous_population, holdable).
narrative_ontology:cs_axiom_grounding('9e3de589-3984-4398-82de-fde4d3db1a1e', palestinians_are_indigenous_population, empirically_contingent).
narrative_ontology:cs_axiom('9e3de589-3984-4398-82de-fde4d3db1a1e', foundational, settler_colonial_displacement_is_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(settler_colonial_displacement_is_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('9e3de589-3984-4398-82de-fde4d3db1a1e', settler_colonial_displacement_is_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_reference_frame('9e3de589-3984-4398-82de-fde4d3db1a1e', pre_zionist_palestinian_settlement_baseline).
narrative_ontology:cs_drift_state('9e3de589-3984-4398-82de-fde4d3db1a1e', contemporary_2024, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9e3de589-3984-4398-82de-fde4d3db1a1e', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_european_settler_population).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, zionist_institutional_hierarchy).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, arab_dispossessed_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, western_imperial_powers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_diaspora_communities).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, european_colonial_superiority).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, indigenous_claim_irrelevance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European Jewish migrants established institutions, land purchase networks, and political apparatus to create a majority-Jewish state in Palestine. They frame migration as return and restoration; they set state policy, control military and administrative enforcement, and define who belongs. Exit from the project would mean abandoning the constructed identity and institutional apparatus built over generations.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_european_settler_population, agenda_setter,
    institutional, generational, identity_locked, national).

% Palestinians were the majority population with established communities, property, social structures, and centuries of settlement. They were systematically dispossessed through legal frameworks, land laws, military force, and institutional exclusion. Exit means permanent displacement (refugee camps, diaspora) or accepting permanent minority/subjugated status without political voice. Physical departure was coerced; return is legally barred.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, national).

% The Zionist movement, Jewish Agency, and state institutions manage the constraint's operation: land acquisition, population transfer, legal codification of Jewish-majority rule, military enforcement of borders and dispossession, and narrative production legitimating the arrangement. They collect political sovereignty, territorial control, and institutional authority; they can exit through territorial compromise or constitutional reform but have chosen escalating entrenchment.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_institutional_hierarchy, beneficiary,
    institutional, civilizational, arbitrage, global).

% Palestinians displaced in 1948 and subsequent wars live in refugee camps in neighboring states, diaspora communities globally, or occupied territories. They bear costs of statelessness, economic marginalization, legal subordination in host states, and permanent separation from property and origin communities. Return is legally impossible; resettlement elsewhere is politically blocked; remaining in occupied territory means subordination under military law.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, arab_dispossessed_refugees, payer,
    powerless, generational, trapped, global).

% Britain facilitated the Balfour Declaration and Mandate, France supported Israeli development, the United States provides military and diplomatic support. These powers benefit from a strategic regional ally and from the deflection of European antisemitism onto a colonial project outside Europe; they can exit through recognition of Palestinian sovereignty but maintain support for Israeli institutional authority.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, western_imperial_powers, beneficiary,
    institutional, biographical, arbitrage, global).

% Palestinian solidarity movements, indigenous-rights advocates, and postcolonial scholars offer alternative framings (decolonization, right of return, indigenous sovereignty) but are largely excluded from institutional decision-making, UN resolution enforcement, and Western media narrative-setting. They would reframe the constraint entirely and reject its legitimacy; their exclusion is maintained through institutional gatekeeping.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_solidarity_networks, excluded,
    moderate, biographical, constrained, global).

% Diaspora Jewish communities are framed as beneficiaries of a Jewish state as refuge and homeland symbol, providing political and financial support. Many hold genuine affective investment in the state's existence; some dissent from the settler-colonial framing and support Palestinian rights. Their ability to exit support or shift position is constrained by community pressure, identity fusion, and institutional expectations.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, jewish_diaspora_communities, observer).

% Scholars analyzing the constraint from decolonial, indigenous-rights, and comparative settler-colonialism frameworks see the structural displacement as constitutive, not incidental. They document the constraint's operation through legal analysis, oral history, demographic data, and comparative method; they can influence interpretation but not institutional enforcement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, postcolonial_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, zionist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Zionism as interpreted in this reading solves no genuine coordination problem shared by Jewish Europeans and Palestinians. Instead, it coordinates Jewish European settlement and institutional power consolidation by legally and militarily removing the indigenous population — it coordinates settler interests, not authentic mutual benefit.
% TRANSFER_FUNCTION: Transfers Palestinian land, property, political rights, and national sovereignty to a Jewish-majority state and its institutions. Palestinian labor was appropriated during early settlement; Palestinian dispossession funded Jewish settlement infrastructure. Ongoing transfer: Palestinian tax revenue under occupation, restricted access to water and resources, territorial control by the settler state.
% ABSENT_VOICES: Palestinian political leadership (especially 1948 onwards) was excluded from decisions about their own dispossession and had no seat at negotiating tables. Indigenous-rights advocates, Arab governments, and international solidarity movements opposing the settler project were excluded from institutional decision-making. The constraint persists through the systematic exclusion of those it most harms from the rooms where legitimacy is negotiated.
% DISAPPEARANCE_RATIONALE: If the settler-colonial constraint and its enforcement vanished — if Palestinian right of return were recognized, if equal citizenship replaced ethno-state hierarchy, if land restitution occurred — the entire political structure would reorganize: a binational or Palestinian-majority state would emerge, Jewish-Israeli institutional dominance would dissolve, regional power dynamics would shift fundamentally. The constraint is not a natural fact; it is an institutional arrangement whose removal would produce massive structural change.
% FOUNDING_PROBLEM: The founding problem the settler-colonial reading identifies is NOT a genuine collective-action problem but rather a European antisemitism crisis and European colonial ambitions in the Middle East. European Jewish migration to Palestine was framed as solving persecution in Europe by establishing a majority-Jewish state elsewhere — exporting the problem rather than solving it, and at the cost of displacing the indigenous population.
% FOUNDING_PROBLEM_CORROBORATION: The settler-colonial reading corroborates its diagnosis through: (1) historical documentation of European colonial attitudes embedded in early Zionist thought (Max Nordau, Theodor Herzl writings); (2) comparative analysis by postcolonial scholars showing structural parallels to South African apartheid, US settler colonialism, and Australian Aboriginal dispossession; (3) Palestinian testimonies and historical records documenting continuous settlement, dispossession, and institutional displacement; (4) international legal analysis finding violations of UN resolutions on decolonization and indigenous rights. The Zionist institutional reading contests this diagnosis, attributing settlement to indigenous return rather than European colonialism. The factual dispute is the kernel contest itself.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   The settler-colonial reading authorizes high extractiveness (0.82) because the constraint transfers Palestinian land, rights, and sovereignty to Israeli institutional control with no reciprocal benefit — it is not a coordination solving a shared problem but rather a unilateral taking defended by force and law. Suppression (0.79) is also high because the constraint's persistence requires: (1) military occupation and border control preventing Palestinian return; (2) legal frameworks (Law of Return for Jews, marriage law restricting Palestinian family reunification, land laws preventing Palestinian purchase) that encode settler preference; (3) institutional gatekeeping (Palestinian political voice is minimized in negotiation processes, international law is not enforced); (4) narrative suppression in Western media and academic institutions, where Palestinian historical presence and indigenous rights are systematically delegitimized. Theater (0.48) is moderate-to-high because significant enforcement activity is dedicated to maintaining the narrative frame (describing dispossession as 'security,' settlement as 'development,' occupation as 'temporary military rule') rather than the functional object (ensuring Jewish safety, which could be achieved through many arrangements short of ethno-state dominance). The measurement series shows extractiveness rising from early settlement (0.55 in 1882, when Palestinian displacement was incomplete) to peak at 1948 (0.81, after the majority of Palestinians were expelled) and stabilizing in the post-1967 occupation (0.82–0.84). Suppression rises similarly, peaking at 1967 (0.82) and holding through the occupation period (0.79–0.82 in the contemporary era). Theater rises through the Mandate period and stabilizes post-1967 around 0.48–0.51, indicating that enforcement remains split between functional occupation and narrative maintenance. The temporal pattern shows that this is not a natural constraint that has always existed—it is a constructed arrangement built incrementally through institutional action, with extractiveness rising as displacement became complete.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Palestinian indigenous population, Arab refugees) should compute this as a snare with maximal extraction and suppression: they were dispossessed through institutional force, barred from return through legal prohibition, and excluded from negotiation processes. The agenda-setter seat (Jewish European settlers, Zionist institutions) computes this as legitimate national/religious restoration or justified security, depending on the reading chosen—but even within the settler-colonial frame, that reading itself becomes an omega variable (ambiguity about the frame itself). The beneficiary seat (Western imperial powers) computes this as advantageous regional dominance achieved through proxies. The observer seats (postcolonial scholarship, international solidarity networks) compute this as structural settler colonialism comparable to historical precedents (South Africa, Algeria, Australian Aboriginal dispossession). The engine should compute radically different types from different seats: snare from the payer perspective (high d toward victim), rope or tangled-rope from the agenda-setter perspective (beneficiary framing), snare from the observer perspective (external analysis of the structure). This divergence is the point—the constraint is only intelligible when we see how different structural positions produce different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality toward the victim/payer end (d → 1.0): Palestinians bear the extraction (land loss, refugee status, legal subordination, political exclusion, ongoing occupation) and have no exit options except permanent displacement or acceptance of minority status. They are trapped: return is legally barred, remaining means subordination, leaving means permanent exile. The d value for this seat is near 1.0 (full target). Directionality toward the beneficiary end (d → 0.0): Zionist institutions and Jewish settlers benefit from the constraint (territorial control, political sovereignty, demographic majority, institutional authority); they are identity-locked into the project (institutional position, national identity, settlement commitment), which reduces d slightly from zero (not pure beneficiary—some cost to maintaining the arrangement through ongoing enforcement, but the benefit far exceeds the cost). The d value for this seat is near 0.1–0.2 (heavily beneficiary). Western imperial powers: d near 0.15 (beneficiary through strategic alignment, arbitrage exit if the arrangement became inconvenient). This directionality distribution—extreme divergence between targets and beneficiaries, with power concentrated on the beneficiary side—is structurally characteristic of snares and defines how the engine computes effective extraction (χ) amplified for the powerless targets, damped for the institutional beneficiaries. The overrides are not needed here; the structural data produces the divergence without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (loss of original mandate) is the central diagnostic for this constraint. The settler-colonial reading identifies mandatrophy as follows: (1) FOUNDING MANDATE (1880s–1920s): Zionism was framed as solving the 'Jewish problem'—European antisemitism and persecution. Migration to Palestine was presented as the solution. The mandate was genuine for Jewish Europeans: a refuge from persecution. (2) PROBLEM STATUS (contemporary): European antisemitism has been substantially contained in Western societies (though not eliminated); Jewish safety is achievable through many institutional means (legal protection, political participation, integration) and is not dependent on Zionist state control. The founding problem is dead or substantially diminished—persecution exists but is not the primary driver of Jewish life in most Western democracies. (3) PERSISTENCE DESPITE MANDATE LOSS: The constraint persists not because the founding problem persists but because: (a) institutional interests (the Zionist apparatus, Israeli state, settler population) have invested in the arrangement and now benefit from it; (b) identity fusion—generations of Jewish settlers now identify as Israeli, and exit from the state project would mean identity dissolution; (c) narrative inertia—the founding legitimacy claim is repeated even though the founding problem it addressed is gone. The constraint has become a pure extraction mechanism (snare) wearing the clothes of the national-liberation narrative that once justified it. The mandatrophy signal should fire: the founding mandate (refuge from persecution) is dead; the constraint persists (extraction from Palestinians continues); the mechanism of persistence is not solving the founding problem but rather benefiting institutional actors who have substituted a new problem definition (security, demography, religious claim) to justify the old extraction. This is how a coordinate structure (solving a real problem) metamorphoses into an extractive one (solving no shared problem, benefiting concentrated actors, suppressing alternatives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_classification_ambiguity,
    'Is Zionism structurally a settler-colonial movement (dispossession of indigenous population through institutional force and settlement), or is it a national liberation movement (persecuted people returning to ancestral homeland)?',
    'Comparative historical and structural analysis: (1) demographic timeline—did settler population arrive as returning indigenous or as migrants from Europe? (2) institutional mechanisms—were political/military institutions built to remove indigenous population or to defend returning indigenous population? (3) beneficiary structure—does the arrangement benefit the settler/newcomer population or solve a shared collective-action problem? (4) exit options—could the founding problem (persecution) be solved through means other than territorial displacement of the indigenous population?',
    'If settler-colonial: constraint is a snare with Palestinian victims and Israeli beneficiaries; mandatrophy analysis applies (founding mandate—European persecution—is gone; constraint persists through institutional interest). If national-liberation: constraint is a rope with contested borders and tragic displacement; the founding mandate persists (indigenous return); displacement is side-effect rather than structure. The readings produce incompatible ε values and type classifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_colonial_classification_ambiguity, conceptual, 'Whether Zionism is fundamentally settler-colonial or national-liberation—the kernel contest itself. The question is not empirically resolvable because it requires a frame choice about what ''indigenous'' means and whose historical presence defines the baseline.').

omega_variable(
    european_jewish_indigeneity_claim,
    'What is the referent for ''return'' in Zionist framing? Are European Jews the indigenous population of Palestine (based on ancient Jewish kingdoms), or are Palestinians the indigenous population (based on continuous settlement for centuries)?',
    'Historiography and archaeology: (1) how long ago was the ancient Jewish presence? (2) what was the duration of continuous Jewish settlement between diaspora and modern immigration? (3) who lived continuously on the land for the centuries between ancient kingdoms and modern settlement? (4) does ''indigenous'' require unbroken settlement or does historical presence suffice?',
    'If Palestinians are indigenous (continuous settlement, no gap): Zionist immigration is settlement of someone else''s land, not return. The constraint is settler-colonial. If Jews are indigenous (based on ancient presence): Zionist immigration is return, and the constraint is national-liberation. The frame choice determines the entire type classification and ethics of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(european_jewish_indigeneity_claim, conceptual, 'The definition of indigenous status—does it rest on ancient presence or continuous settlement? Different definitions produce incompatible readings of Zionism.').

omega_variable(
    displacement_necessity_ambiguity,
    'Was the displacement of Palestinians a necessary feature of establishing a Jewish state, or was it a contingent choice made because alternatives were rejected?',
    'Counterfactual historical analysis: (1) did binational state proposals exist and were they rejected? By whom? (2) did equal-citizenship proposals exist? (3) what would have been the institutional requirements of a Jewish-majority state that did not displace Palestinians? (4) did the settler leadership choose displacement or was it forced by Palestinian resistance or international constraint?',
    'If displacement was necessary: the constraint is structurally snare-inevitable—no other way to establish the state. If displacement was contingent: the constraint is snare-chosen—the beneficiaries selected dispossession as the institutional path when alternatives existed. Contingent choice strengthens the mandatrophy diagnosis (the arrangement was chosen for benefit, not necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_necessity_ambiguity, empirical, 'Whether Palestinian displacement was structurally inevitable or contingently chosen.').

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (European persecution of Jews, framed as generating the need for a refuge state) still live, dead, or contested?',
    'Empirical evidence: (1) contemporary rates of antisemitism and persecution in the West; (2) how much of the Zionist institutional investment is driven by contemporary persecution vs. institutional self-perpetuation? (3) could Jewish safety be achieved through alternative means (legal protection, integration, political participation) without territorial displacement of Palestinians? (4) internal Zionist debates: do institutional actors justify the arrangement primarily as response to persecution or as settler interest?',
    'If the founding problem is dead: mandatrophy diagnosis is confirmed—the constraint persists despite the founding problem being gone, which means it is pure extraction maintained by institutional actors who benefit. If the founding problem is live: mandatrophy does not apply; the constraint might be re-legitimized as ongoing refuge. The status of the founding problem is the hinge for mandatrophy classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding mandate (refuge from persecution) still justifies the constraint''s operation.').

omega_variable(
    suppression_mechanism_internalization,
    'Is Palestinian suppression primarily structural (external barriers—military occupation, legal prohibition, economic subordination) or internalized (Palestinians have accepted the arrangement as inevitable, internalized inferiority, etc.)?',
    'Post-suppression trajectories: (1) if external barriers were removed (right of return granted, legal discrimination eliminated), would Palestinian resistance persist or would suppression disappear? (2) Palestinian political movements—do they continue to demand return and full equality, or have generations of oppression internalized acceptance? (3) cross-border Palestinian communities—do diaspora populations maintain return claims or have they integrated into host societies?',
    'If structural: suppression requires active enforcement; it would disappear if enforcement was removed. If internalized: suppression persists through psychological and cultural mechanisms; even with barriers removed, the target population may not exit. The distinction matters for remedies: structural suppression requires dismantling enforcement machinery; internalized suppression requires cultural and educational work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether Palestinian suppression is maintained by external force or internalized through generational trauma and social isolation.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the settler-colonial reading FORECLOSE the national-liberation reading (they cannot both be true in any single framework), or do they merely COEXIST as competing framings held by different parties?',
    'Logical analysis: (1) can the same events be described as both ''settler colonialism'' and ''indigenous return'' depending on how we define ''indigenous''? (2) is the contradiction in the facts or in the frames? (3) do the two readings share any common premises that would force a choice, or do they rest on incompatible foundational axioms that both sides reject when the other is adopted?',
    'If foreclosure: one reading is logically true and the other is false (under any consistent framework). If coexistence: both readings are defensible within their own premises, and the contest is about which premises to adopt. Foreclosure strengthens the claim that the settler-colonial reading is *correct*; coexistence frames it as a *reading* that is more or less compelling depending on your axioms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether the settler-colonial and national-liberation readings of Zionism are logically incompatible (foreclosing each other) or can coexist as different framings of the same events.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1882, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1882, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1882, 0.25).
narrative_ontology:measurement_basis(zion_tr_t1882, observed).
narrative_ontology:measurement(zion_tr_t1920, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1920, 0.32).
narrative_ontology:measurement_basis(zion_tr_t1920, observed).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.42).
narrative_ontology:measurement_basis(zion_tr_t1948, observed).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.51).
narrative_ontology:measurement_basis(zion_tr_t1967, observed).
narrative_ontology:measurement(zion_tr_t1995, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1995, 0.49).
narrative_ontology:measurement_basis(zion_tr_t1995, observed).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(zion_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(zion_be_t1882, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1882, 0.55).
narrative_ontology:measurement_basis(zion_be_t1882, observed).
narrative_ontology:measurement(zion_be_t1920, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1920, 0.68).
narrative_ontology:measurement_basis(zion_be_t1920, observed).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.81).
narrative_ontology:measurement_basis(zion_be_t1948, observed).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.84).
narrative_ontology:measurement_basis(zion_be_t1967, observed).
narrative_ontology:measurement(zion_be_t1995, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1995, 0.83).
narrative_ontology:measurement_basis(zion_be_t1995, observed).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.82).
narrative_ontology:measurement_basis(zion_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1882, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1882, 0.45).
narrative_ontology:measurement_basis(zion_su_t1882, observed).
narrative_ontology:measurement(zion_su_t1920, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1920, 0.62).
narrative_ontology:measurement_basis(zion_su_t1920, observed).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.78).
narrative_ontology:measurement_basis(zion_su_t1948, observed).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.82).
narrative_ontology:measurement_basis(zion_su_t1967, observed).
narrative_ontology:measurement(zion_su_t1995, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1995, 0.79).
narrative_ontology:measurement_basis(zion_su_t1995, observed).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.79).
narrative_ontology:measurement_basis(zion_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__settler_colonial_reading, 0.18).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, israeli_security_doctrine).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, western_imperial_support_systems).

% DUAL FORMULATION NOTE:
% The zionist_legitimacy_basis kernel has three constraint readings: settler_colonial_reading (this file), national_liberation_reading, and religious_restoration_reading. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and type classifications. The three stories are linked through affects_constraints; the kernel contest is the structure of their divergence. This reading treats the constraint as a snare with high extractiveness (0.82) and identifies Palestinian dispossession as constitutive; the national_liberation reading treats it as a rope with tragic displacement; the religious_restoration reading treats it as a snare with over 0.85 extractiveness and Palestinians as obstacles to divine mandate. The readings are not observable-dependent variants of one constraint—they instantiate genuinely different constraints grounded in different axioms about what 'indigenous' means, what counts as 'return,' and what the founding problem actually was.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__settler_colonial_reading, powerless, 0.98).
constraint_indexing:directionality_override(zionist_legitimacy_basis__settler_colonial_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
