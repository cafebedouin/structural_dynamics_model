% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__cultural_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__cultural_zionist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: jewish_sovereignty_palestine__cultural_zionist_reading
 *   human_readable: Cultural Zionist Spiritual-Center Program (Ahad Ha'am Reading)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the cultural Zionist reading of the contested
 *   kernel 'jewish_sovereignty_palestine': a Jewish cultural renaissance and
 *   spiritual center in Palestine that explicitly does not require political
 *   sovereignty or a demographic majority, with the land's Arab inhabitants
 *   seated as co-inhabitants of a shared cultural space. The kernel
 *   decomposes into five readings — this one plus the liberal-nationalist,
 *   religious-Zionist, settler-colonial, and post-Zionist readings — each a
 *   separate constraint story with its own epsilon, beneficiary structure,
 *   and classification, linked through network.affects_constraints. This
 *   reading's distinguishing structure: the beneficiary is Jewish cultural
 *   vitality itself, served by a voluntary, philanthropy-funded
 *   institution-building program that renounces the sovereignty and
 *   majority-seeking that make the sibling arrangements zero-sum. The epsilon
 *   referent is the standing arrangement under contest — the cultural-center
 *   program as this reading holds it — assessed by the reading's own lights,
 *   which include its founder's 1891 internal critique of settlement
 *   practice: extraction is real (land purchases, unconsented demographic and
 *   linguistic pressure) but modest and design-limited. The claimed type and
 *   the metrics are authored independently; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - cultural_zionist_movement_leadership: agenda-setter (organized/mobile) — writes the spiritual-center program, funds and directs cultural institution-building, polices the renunciation of sovereignty-seeking; its founder's 1891 critique is the arrangement's internal extraction record
 *   - world_jewry_diaspora: primary beneficiary (organized/mobile) — consumes the center's cultural vitality, funds it voluntarily, bears none of its local costs
 *   - hebrew_cultural_pioneers: beneficiary and payer (organized/identity_locked) — builds the center on the ground, bears the labor, hardship, and renounced sovereignty ambition; exit priced as self-betrayal
 *   - palestinian_communities: excluded co-inhabitants (moderate/constrained) — bear diffuse unconsented costs of land purchase and demographic change; no seat in the program's design
 *   - political_zionist_movement: capturing beneficiary (organized/mobile) — outcompeted the cultural program, inherited its mature outputs as statehood's cultural foundation
 *   - ottoman_mandate_administrations: external administrator (institutional/arbitrage) — capped or opened the program's scale by imperial interest
 *   - cultural_historians: analytical observer (analytical/analytical) — reconstructs the design-practice gap from the movement's own records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__cultural_zionist_reading, 0.35).
domain_priors:suppression_score(jewish_sovereignty_palestine__cultural_zionist_reading, 0.2).
domain_priors:theater_ratio(jewish_sovereignty_palestine__cultural_zionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Cultural Zionist Spiritual-Center Program (Ahad Ha'am Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political philosophy/nationalism studies/postcolonial theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, '28a600b1-fc55-4971-9716-4b133984ff8d').
narrative_ontology:cs_kernel_codification('28a600b1-fc55-4971-9716-4b133984ff8d', distributed).
narrative_ontology:cs_authority_grounding('28a600b1-fc55-4971-9716-4b133984ff8d', distributed).
narrative_ontology:cs_reading_relation('28a600b1-fc55-4971-9716-4b133984ff8d', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('28a600b1-fc55-4971-9716-4b133984ff8d', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('28a600b1-fc55-4971-9716-4b133984ff8d', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('28a600b1-fc55-4971-9716-4b133984ff8d', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('28a600b1-fc55-4971-9716-4b133984ff8d', foundational, cultural_renaissance_as_zionisms_purpose).
narrative_ontology:cs_axiom_status(cultural_renaissance_as_zionisms_purpose, holdable).
narrative_ontology:cs_axiom_grounding('28a600b1-fc55-4971-9716-4b133984ff8d', cultural_renaissance_as_zionisms_purpose, instrumental).
narrative_ontology:cs_axiom('28a600b1-fc55-4971-9716-4b133984ff8d', foundational, cohabitation_without_domination).
narrative_ontology:cs_axiom_status(cohabitation_without_domination, holdable).
narrative_ontology:cs_axiom_grounding('28a600b1-fc55-4971-9716-4b133984ff8d', cohabitation_without_domination, deontological).
narrative_ontology:cs_reference_frame('28a600b1-fc55-4971-9716-4b133984ff8d', spiritual_center_without_sovereignty).
narrative_ontology:cs_drift_state('28a600b1-fc55-4971-9716-4b133984ff8d', post_statehood_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('28a600b1-fc55-4971-9716-4b133984ff8d', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, world_jewry_diaspora).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_pioneers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, political_zionist_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_pioneers).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the program: a Jewish spiritual center in Palestine that renews Hebrew and Jewish cultural creativity while explicitly declining to seek political sovereignty or a demographic majority. Raises funds through diaspora committees, directs school, press, and publication projects, and polices the program's self-limitation — its most prominent figure publicly condemned settlement practices that mistreated Arab tenants, arguing that injustice would defeat the center's purpose. Operates from diaspora cities and can scale involvement up or down without personal exposure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, cultural_zionist_movement_leadership, agenda_setter,
    organized, generational, mobile, global).

% Reads the Hebrew literature, funds the schools and presses, and draws identity resources from the center's outputs. Participation is voluntary and reversible: a community can redirect its philanthropy to Bundist, territorialist, or assimilationist projects at any time. Bears no direct cost of the center's land purchases or demographic changes; those fall on people living in Palestine.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, world_jewry_diaspora, beneficiary,
    organized, generational, mobile, global).

% Immigrated to build the center: taught in Hebrew schools, ran presses and theaters, and made Hebrew a spoken domestic language for the first time in centuries. Gave up careers and security in the diaspora, accepted hardship, and accepted the program's renunciation of sovereignty-ambition as the price of the work. Leaving would mean abandoning the life's project their identity is built around; most who came stayed.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_pioneers, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_pioneers, payer).

% The Arab inhabitants of the towns and villages where the cultural settlement grew. Some sold land to Jewish purchasers, some found work in the growing towns, some lost tenancy when land changed hands. The program that reshaped their economic and linguistic environment was designed in Odessa, Jaffa, and London without their participation; no Palestinian delegate sat in the cultural congresses that set its terms.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_communities, excluded,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_communities, payer).

% Competed with the cultural program for direction of the national movement, arguing that a people without sovereignty cannot secure its culture. Won the congresses, secured the Balfour Declaration and the Mandate, and built the state. Inherited the cultural arrangement's mature outputs — a revived Hebrew-speaking public, schools, a university, a native literature — as the cultural foundation of statehood, without having borne the decades of small-scale cultural construction that produced them.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, political_zionist_movement, beneficiary,
    organized, generational, mobile, global).

% Administered the territory the program operated in: Ottoman restrictions on immigration and land purchase capped the cultural settlement's scale for decades; the Mandate framework that replaced them opened both. Adjusted the program's operating conditions according to imperial interest rather than participation in it, and could permit, restrict, or redirect it at will.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, ottoman_mandate_administrations, agenda_setter,
    institutional, generational, arbitrage, national).

% Reconstruct the movement's history from its correspondence, congress records, and press: the design-practice gap its own founder documented in 1891, the terms of its rivalry with political Zionism, and the fate of its institutions after statehood. Hold no stake in the arrangement's continuation; their testimony is the main check on the program's self-description.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__cultural_zionist_reading, political_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__cultural_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the revival of Hebrew as a living language and the concentration of Jewish cultural institution-building in Palestine, solving the diaspora's cultural-attrition problem — assimilation, language death, loss of a creative center — through a voluntary, philanthropy-funded program serving world Jewry.
% TRANSFER_FUNCTION: Moves diaspora money and immigrant labor into Palestine's cultural infrastructure; moves cultural vitality, linguistic resources, and prestige back to diaspora communities; and — the flow the reading's own lights flag — moves land and tenancy security within Palestine from Arab sellers and tenants to the Jewish cultural settlement.
% ABSENT_VOICES: Palestinian communities — the arrangement's co-inhabitants — had no seat in the cultural congresses, purchase negotiations, or school committees that designed the program; Arab notables who engaged early Zionist figures did so bilaterally, not as participants in its design. They would have objected to the scale of land purchase and immigration even under a program that renounced sovereignty. Also absent: the Arab tenant families displaced when purchased land changed hands, whose objection is recorded mainly in the movement's own internal critique.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — the revived Hebrew public sphere, the cultural institutions, the center-periphery ideal — Jewish cultural life rearranges: Hebrew reverts toward a liturgical language, the cultural institutions lose their base, and diaspora identity resources thin out. The region's political arrangements would not rearrange (those belong to the sibling readings' constraints), but the cultural facts this arrangement built are load-bearing for a living language community.
% FOUNDING_PROBLEM: Jewish cultural atrophy under diaspora conditions: assimilation eroding collective life, Hebrew reduced to liturgy, and — in Ahad Ha'am's diagnosis — a people losing the creative center that could renew its spirit. The founding answer was a spiritual center in Palestine radiating cultural vitality back to the diaspora.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by diaspora demography (survey research on Jewish identity and continuity documenting persistent assimilation pressure) and by historians of Hebrew culture writing outside the movement's institutions; within the era, the diagnosis was disputed by Bundists and assimilationists, whose rival programs attest the problem's centrality while contesting its solution. No Palestinian source attests the founding problem — it was defined entirely within the Jewish conversation.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__cultural_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.35 — low relative to the sovereignty-centered sibling arrangements, but not zero: even by this reading's own lights, land purchase concentrated land, immigration changed the demographic fabric without consent, and Hebrew revival pressed on Arabic's public space; 'Truth from Eretz Yisrael' (1891) is the reading's own registration of that extraction. Suppression is 0.2 — participation was voluntary, funded by philanthropy, and the alternatives (Bundism, territorialism, assimilationism) stayed live; the renunciation of sovereignty was self-imposed and unenforced, which is a structural fact, not a virtue: nothing held it against the rival program. Theater is 0.42 at interval end — the Hebrew revival's institutional achievements were functionally real, but as political Zionism captured the movement, spiritual-center rhetoric increasingly outran the shrunken program's function. Accessibility collapse is 0.35 — understanding the program collapses no alternatives; it was itself one alternative among several. Resistance is 0.45 — political Zionists fought it inside the movement, Ottoman administrators capped it from outside, and Palestinian objections met its settlement practice. The temporal series run on one shared grid (1889, 1901, 1913, 1925, 1936, 1948) with both tracked metrics authored at every point: base_extractiveness rises then plateaus as the reading loses control of the movement and its own arrangement stops scaling — the plateau is marginalization, not virtue — while theater accelerates as rhetoric outruns function.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the pioneer seat the arrangement is a vocation: identity fused with the Hebrew revival, exit priced as self-betrayal, the renunciation of sovereignty experienced as discipline rather than loss. From the diaspora seat it is consumption: cultural vitality arrives as literature and prestige; the costs land elsewhere. From the Palestinian seat it is unconsented change: economic and linguistic pressure from a program designed in other cities, with no seat at its congresses. From the political-Zionist seat it is infrastructure: a revived language and an educated public inherited as the foundation of statehood. Same arrangement, four different experiences, computed per seat from power, exit, and position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (world Jewry, the pioneers, and the political movement that inherited the outputs) derive low directionality — the arrangement subsidizes them. The pioneers' secondary payer position (decades of cultural labor, the renounced sovereignty ambition) keeps them near-symmetric rather than pure beneficiaries, but their net position by their own lights is beneficiary. Palestinian communities are deliberately NOT declared victims: this reading's frame seats them as co-inhabitants, and a victim declaration would import the settler-colonial reading's structure into this one, violating the one-reading-one-constraint rule. Because no structural declaration covers them, the derivation would fall back to the canonical moderate-power default; the directionality override (moderate → 0.6) encodes their actual position: above symmetric, bearing diffuse unconsented costs — land-market displacement, demographic pressure, linguistic marginalization — but far from full-target, because the arrangement's design renounces domination and its machinery never aimed at them. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — diaspora cultural atrophy — is live: Hebrew revival succeeded and the center-periphery problem persists in contemporary diaspora demography, so the mandate has not outlived its function and no mandatrophy resolution is declared. The classification discipline cuts both ways: reading the arrangement as a rope does not license ignoring its registered extraction (the 1891 internal critique is part of the record, and the design-practice omega tests whether the no-victim structure holds in practice), and the rising theater ratio does not license a degraded-institution verdict — the function (Hebrew as a living language, working institutions) is load-bearing, not performed. The genuinely mandatrophy-adjacent fact is capture: the arrangement's gains accrued to a rival program, which is recorded on the receipt surface (gain_flow) rather than smuggled into the type claim. The founding_problem_status (live) x disappearance_verdict (world_rearranges) pairing shows no mismatch flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    design_practice_extraction_gap,
    'Does the cultural-center arrangement in practice remain within its low-extraction design — co-inhabitation without displacement — or does its land-purchase and settlement practice generate a victim class the reading''s co-inhabitant frame does not register?',
    'Land-registry and tenancy records for parcels acquired by cultural-Zionist institutions (Hovevei Zion and Odessa committee funds), eviction and tenancy-dispute counts, and the Arabic-language press of the settlement era, cross-read against the movement''s internal critiques.',
    'If practice systematically displaced tenants, the reading''s epsilon is understated, its no-victim structure fails, and the constraint reclassifies toward tangled_rope with Palestinian communities as declared victims; if practice held within the design, the rope classification stands with the measured extraction treated as coordination-adjacent cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_practice_extraction_gap, empirical, 'Whether the reading''s no-victim co-inhabitant structure survives contact with its own settlement practice.').

omega_variable(
    self_limitation_enforceability,
    'What could hold the reading''s constitutive renunciation — no sovereignty-seeking, no majority-building — against a rival program that outcompetes it, and does the attenuated post-1948 cultural form instantiate the constraint or only commemorate it?',
    'Movement-historical analysis of the congress votes and institutional capture points where the renunciation failed, plus assessment of whether contemporary cultural-Zionist forms (diaspora-Israel cultural institutions, Hebrew cultural networks) carry operative self-limitation or merely the ideal.',
    'If the renunciation is structurally unenforceable, the arrangement''s low-extraction profile is contingent on this reading holding power — its stability is political, not structural, and the classification holds only inside that contingency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_limitation_enforceability, empirical, 'Enforceability of the reading''s constitutive self-limitation against outcompetition.').

omega_variable(
    cohabitation_consent_deficit,
    'The reading seats Palestinians as co-inhabitants of the shared cultural space, but the program was designed without their participation — can a shared-space arrangement be what the reading claims when its co-inhabitants never consented to it?',
    'Exhaust the historical record for Palestinian participation or consultation in the program''s design (cultural congresses, Hovevei Zion deliberations, purchase negotiations); assess what the program''s structure would require with Palestinian seats present.',
    'If consent is constitutive of the co-inhabitation premise, the reading''s arrangement existed mostly as intention — its low epsilon describes a design never fully instantiated, and the operative arrangement''s extraction lands on unconsented parties, raising effective extraction above the authored value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohabitation_consent_deficit, conceptual, 'Whether unconsented cohabitation satisfies the reading''s own constitutive premise.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel jewish_sovereignty_palestine — what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Compare the sibling constraint files: the liberal-nationalist reading makes statehood constitutive (raising epsilon via sovereignty contest and adding the state''s enforcement machinery); the religious-Zionist reading makes the territorial claim inalienable (removing the renunciation premise entirely); the settler-colonial reading makes displacement constitutive regardless of intent (declaring victims and raising suppression); the post-Zionist reading locates the obstruction in the ethnic-national framework itself (shifting the referent to the standing state arrangement).',
    'The family''s classifications diverge on one structural element — whether sovereignty and demographic majority are constitutive, rejectable, or displaced-by-displacement — so misreading this reading''s renunciation premise as shared across the family would collapse five distinct epsilon values into one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of five readings of the sovereignty kernel; disagreement located at the sovereignty-constitutivity premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 1889, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1889, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1889, 0.12).
narrative_ontology:measurement(jewi_tr_t1901, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1901, 0.15).
narrative_ontology:measurement(jewi_tr_t1913, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1913, 0.2).
narrative_ontology:measurement(jewi_tr_t1925, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1925, 0.28).
narrative_ontology:measurement(jewi_tr_t1936, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1936, 0.35).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1948, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1889, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1889, 0.15).
narrative_ontology:measurement(jewi_be_t1901, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1901, 0.2).
narrative_ontology:measurement(jewi_be_t1913, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1913, 0.26).
narrative_ontology:measurement(jewi_be_t1925, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1925, 0.3).
narrative_ontology:measurement(jewi_be_t1936, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1936, 0.33).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1948, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_sovereignty_palestine__cultural_zionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Zionism' conflates five structurally distinct claims about one kernel; per the epsilon-invariance principle this family decomposes into five constraint stories, one per reading. This story is upstream in one specific sense — its cultural infrastructure (the revived Hebrew-speaking public) is cited as evidence and resource by the liberal-nationalist and post-Zionist readings — while the settler-colonial reading contests its no-displacement premise directly. Each member carries its own epsilon, beneficiaries, and victim structure; these edges record influence and contest, not shared classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__cultural_zionist_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
