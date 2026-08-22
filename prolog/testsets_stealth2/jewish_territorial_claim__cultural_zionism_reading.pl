% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Cultural Zionist Spiritual-Center Claim (Palestine, 1889-1948)
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   Between 1889 and 1948, one strand of the Jewish national movement
 *   undertook to build in Palestine a spiritual-cultural center for world
 *   Jewry — schools, presses, a university, experimental settlements — while
 *   explicitly declining to demand political sovereignty or a Jewish
 *   demographic majority, limiting immigration by quality rather than
 *   quantity, and treating the Arab presence as compatible with the
 *   enterprise rather than as an obstacle. The arrangement was funded by
 *   diaspora philanthropy, staffed by ideological pioneers, and administered
 *   by a literary-intellectual leadership whose authority rested on argument
 *   rather than command. Its costs fell unevenly: tenant cultivators
 *   displaced when land changed hands, and a resident society transformed
 *   without consultation. This file instantiates the cultural reading of the
 *   territorial-claim kernel as a clean, epsilon-invariant constraint; the
 *   political, labor, and revisionist readings are separate stories linked
 *   through the network block, each with its own beneficiary/victim structure
 *   and extraction profile. The claimed type and the metrics below are
 *   authored independently: the claim states the structure I judge true of
 *   this arrangement; the metrics state what I judge descriptively accurate
 *   of its operation.
 *
 * KEY AGENTS:
 *   - cultural_zionist_leadership: Agenda-setting seat (moderate/identity_locked) — directs institution-building, allocates funds, defines faithful settlement
 *   - hebrew_cultural_pioneers: Beneficiary-executant seat (moderate/identity_locked) — builds and staffs the center, bears its hardships
 *   - diaspora_jewish_communities: Principal beneficiary-funder seat (organized/mobile) — consumes the cultural anchor, free to disengage
 *   - philanthropic_funders: Patron seat (powerful/arbitrage) — supplies capital, redirects it freely
 *   - palestinian_arab_residents: Primary cost-bearing seat (organized/constrained) — absorbs land-market and cultural change without consent
 *   - sovereignty_seeking_zionists: Constrained rival constituency (organized/mobile) — bears foregone-program cost under this arrangement
 *   - mandate_era_governing_authorities: Regulatory seat (institutional/analytical) — permits, taxes, registers; sets the legal terrain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.34).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.26).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Spiritual-Center Claim (Palestine, 1889-1948)").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, '240ffbca-1f92-49b7-bff9-92df2d5ff7b5').
narrative_ontology:cs_kernel_codification('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', distributed).
narrative_ontology:cs_authority_grounding('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', expertise).
narrative_ontology:cs_interpretation_layer_present('240ffbca-1f92-49b7-bff9-92df2d5ff7b5').
narrative_ontology:cs_reading_relation('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', foundational, spiritual_center_sufficiency).
narrative_ontology:cs_axiom_status(spiritual_center_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', spiritual_center_sufficiency, instrumental).
narrative_ontology:cs_axiom('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', foundational, demographic_majority_not_required).
narrative_ontology:cs_axiom_status(demographic_majority_not_required, holdable).
narrative_ontology:cs_axiom_grounding('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', demographic_majority_not_required, deontological).
narrative_ontology:cs_axiom('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', secondary, arab_coexistence_compatible).
narrative_ontology:cs_axiom_status(arab_coexistence_compatible, holdable).
narrative_ontology:cs_axiom_grounding('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', arab_coexistence_compatible, empirically_contingent).
narrative_ontology:cs_reference_frame('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', spiritual_center_without_sovereignty).
narrative_ontology:cs_drift_state('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', post_1948_sovereignty_realization, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('240ffbca-1f92-49b7-bff9-92df2d5ff7b5', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_pioneers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, philanthropic_funders).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_pioneers).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, sovereignty_seeking_zionists).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, hebrew_revival_viability).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, cultural_nationalism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Essayists, committee officers, and editors who define what counts as faithful settlement: they set expectations for the caliber of immigrants, allocate raised funds among schools, presses, and agricultural colonies, and articulate the movement's self-limiting rules — no demand for state power, no push for numerical dominance. Their standing rests on published argument and institutional stewardship rather than command; abandoning the mission would forfeit the vocation their lives and reputations are built around.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, cultural_zionist_leadership, agenda_setter,
    moderate, generational, identity_locked, global).

% Teachers, writers, agronomists, and settlers who staff the schools, journals, and experimental farms. They receive livelihood, a revived language, and purpose from the institutions they build, while bearing the costs: precarious salaries, frontier hardship, and dependence on distant patrons. Returning to diaspora life would mean dismantling the collective project their biographies are organized around.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_pioneers, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_pioneers, payer).

% Scattered communities that contribute dues and enroll their children and receive in return a living Hebrew culture, a prestige address, and an answer to assimilation anxiety. Participation is voluntary and reversible; a community that disengages loses access to the center's outputs but suffers no coercion and keeps its local institutions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Wealthy patrons and subscription networks financing land purchase and institution construction. They gain honorific standing and a place in the national narrative from patronage; their capital is mobile, and they repeatedly demonstrated willingness to redirect it when particular programs disappointed them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, philanthropic_funders, beneficiary,
    powerful, generational, arbitrage, global).

% The country's existing inhabitants — villagers, tenant cultivators, townspeople — who did not participate in creating the arrangement and were not asked. They experience its operation as land changing hands through purchases from absentee owners, with tenant cultivators bearing eviction when sales closed; as a new language and school system expanding alongside their own; and as a growing neighboring population. Staying means absorbing these changes; leaving means losing home and livelihood.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_residents, payer,
    organized, generational, constrained, national).

% Constituencies within the Jewish national movement who want the enterprise to culminate in state power and a Jewish majority. Under this arrangement their program is ruled out of order as premature and spiritually corrosive; they bear the cost of foregone statehood while remaining fully free to organize, publish, and outvote the cultural program in movement bodies — which they ultimately did.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, sovereignty_seeking_zionists, payer,
    organized, biographical, mobile, global).

% Ottoman and later British Mandatory administrations that permit, tax, register, and occasionally restrict the settlement activity. They neither fund nor inhabit the center; their seat is regulatory — setting the legal terrain of immigration quotas and land registries within which every other seat operates.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, mandate_era_governing_authorities, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__cultural_zionism_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__cultural_zionism_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed territorial site where Hebrew language, literature, and educational institutions could be built and maintained at a scale and continuity unavailable to dispersed communities acting separately; aggregates scattered cultural effort, philanthropy, and talent into a shared anchor that regenerates a national culture.
% TRANSFER_FUNCTION: Moves philanthropic wealth and immigrant talent from diaspora communities into Palestinian cultural institutions and land holdings; moves cultural prestige, a standardized language, and educated personnel back outward to the diaspora; transfers land title from existing holders — often absentee owners — and their tenant cultivators to Jewish institutions.
% ABSENT_VOICES: Palestinian Arab residents — the population whose land market, villages, and cultural environment the arrangement reshaped — were never seated in any of its deliberative bodies, and their consent was not sought by this reading any more than by its rivals. Also absent: the anti-nationalist Jewish majority of the era, enrolled by default in a project it never voted on. Both stand outside the congresses, boards, and subscription networks where the arrangement was decided.
% DISAPPEARANCE_RATIONALE: Overnight disappearance unravels the Hebrew-language infrastructure: teacher seminaries, publishing houses, the university project, and the funding channels feeding them; diaspora communities would reorganize cultural policy around local autonomist institutions; the pioneer settlements would lose their economic and symbolic anchor. The world rearranges because real institutions and livelihoods depend on the arrangement — though the cost-bearing resident seat would be relieved of further unconsented change.
% FOUNDING_PROBLEM: The nineteenth-century crisis of Jewish cultural continuity: emancipation was dissolving traditional religious life faster than any national culture replaced it, producing, in the founding diagnosis, a Jewry reduced to hollow assimilation or brittle orthodoxy. The arrangement was built to solve this by rooting a regenerating Hebrew culture in a territorial center that scattered communities could draw on.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the benefiting parties: contemporaneous diaspora-autonomist critics, who rejected the territorial remedy, independently attested the cultural crisis the arrangement addressed; academic historiography of the Haskalah and of the Hebrew revival, written outside the movement, documents both the crisis and the center's partial delivery; Palestinian Arab memoir and scholarship attest the costs from outside the beneficiary set. Whether the founding problem remains live is disputed between those who read assimilation pressure as answered by the center and those who read it as continuing in secular form.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).
:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.34: real but bounded costs — land-market displacement hitting tenant cultivators hardest, linguistic-institutional encroachment — set against a coordination function that demonstrably delivered (a revived vernacular Hebrew, durable institutions). Suppression 0.26, authored as a raw structural property and left unscaled: the arrangement polices itself by polemic and funding discipline, not coercion; rival programs organized, published, and ultimately prevailed freely. Theater ratio 0.24: the institutions genuinely functioned; performativity creeps upward late in the interval as 'spiritual center' rhetoric persists while resources and attention drain to the state-building program that defeated this one. Accessibility collapse 0.40: alternatives — diaspora autonomism, rival national programs, simple non-participation — remained live throughout, so understanding the arrangement does not foreclose exiting it. Resistance 0.55: sustained intra-movement opposition from sovereignty-seeking constituencies, traditionalist rejection of the whole enterprise, and mounting Arab suspicion of any settlement program. The three measurement series share one time grid (1889, 1901, 1913, 1925, 1936, 1948) with every metric authored at every point; all points are observed from the historical record.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the leadership and pioneer seats the arrangement presents as the coordination they built and staffed; from the Arab-resident seat the same operations arrive as unconsented transformation of land, tenure, and language; from the sovereignty-seeking seat the arrangement is experienced as a normative ceiling — suppression of the national program they wanted; from the funder seat it is voluntary patronage with full exit. Same structure, four different experiences of it; the engine computes this divergence from power, exit options, and directional position rather than from the authored claim, and the divergence is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (diaspora communities, pioneers, funders) place those seats near the subsidized end of the directionality range, and their exits (mobile, arbitrage) dampen effective extraction further — a funder who can move capital cannot be held near the target end. The Arab-resident seat is declared cost-bearing with constrained exit, so the derivation places it near the target end; this is correct, because directionality encodes structural relationship rather than magnitude — the modest magnitude of what this reading imposes shows up in epsilon, not in d. The sovereignty-seeking seat pays programmatically but exits freely, moderating its derived position. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the arrangement as pure coordination ignores the real, unconsented costs borne by the country's inhabitants through the land market — costs the reading's own founder documented and condemned at the movement's outset. Reading it as extraction wearing a coordination cover overstates the case: the arrangement carried no coercive enforcement machinery, its exits stayed open throughout, and its defining commitments — no sovereignty demand, no majority drive, quality over quantity — are precisely the subtraction of the extraction mechanisms its sibling readings installed. The mandate question resolves as displacement rather than atrophy: the arrangement was not maintained theatrically after its function died; it was outcompeted and superseded while its function (Hebrew cultural regeneration) was substantially delivered. Theater stays low because the institutions were real; the ceremonial residue that accumulated belongs to the winning siblings' story, not this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This story instantiates one reading (cultural_zionism_reading) of the contested kernel jewish_territorial_claim; how would the sibling readings restructure the arrangement''s beneficiary and victim sets?',
    'Compare the four sibling stories'' structural declarations: each names its own beneficiaries and cost-bearers over the same territory and period. The disagreement is located in whether national regeneration requires sovereignty, a demographic majority, facts-on-ground settlement mass, or none of these.',
    'Adopting a sibling reading changes the victim set (adding displaced and subordinated populations under sovereignty, settlement-mass, or maximalist programs), raises epsilon sharply, and switches enforcement requirements on. This reading''s low-extraction profile is reading-specific, not a property of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: this constraint is one of four readings of the territorial-claim kernel; sibling readings are separate files.').

omega_variable(
    territorial_anchor_necessity,
    'Is a territorial anchor in Palestine necessary for Jewish cultural continuity, or can diaspora cultural life sustain itself without one (the diaspora-autonomist counterclaim)?',
    'Comparative study of diaspora communities with and without engagement in the center''s institutions across the interval: language retention, institutional vitality, and cultural-production rates against degree of center affiliation.',
    'If the anchor is unnecessary, the arrangement''s coordination function shrinks toward optional patronage and its residual unconsented costs weigh heavier in classification; if necessary, the coordination justification strengthens and the low-extraction profile is more fully excused.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_anchor_necessity, empirical, 'Whether the coordination function is real necessity or constructed preference.').

omega_variable(
    land_purchase_extraction_boundary,
    'Does the land-acquisition vector — purchases from absentee owners that evict tenant cultivators — belong to this arrangement''s structure, or is it incidental spillover that the reading''s own authorities condemned?',
    'Track eviction incidence on acquisitions made under cultural-movement auspices versus comparable purchases by other buyers; test whether the movement''s self-limiting rules measurably reduced displacement where they bound.',
    'If the displacement is structural, the arrangement is a hybrid — coordination and displacement running through the same purchase mechanism — and its computed type should shift accordingly; if incidental, the low-extraction profile stands and the victims entry records spillover rather than design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_purchase_extraction_boundary, empirical, 'Whether the cost-bearing seat''s burden is constitutive or incidental to the arrangement.').

omega_variable(
    binational_counterfactual_viability,
    'Could the coexistence premise — Arab presence compatible with a growing Jewish cultural center — have held under a binational framework, given the region''s demographic and political trajectory?',
    'Counterfactual analysis anchored on the binational proposals'' actual reception: Arab responses to the cultural movement''s outreach in the 1920s-30s, and comparison with binational or consociational arrangements attempted elsewhere.',
    'If viable, the reading''s failure was contingent — outcompeted rather than refuted — and its low-cost profile was a real possibility forgone; if not viable, the coexistence axiom was empirically void from the start and the arrangement''s benign profile depended on a premise that could not scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_counterfactual_viability, conceptual, 'Viability of the reading''s coexistence premise under its own favored institutional form.').

omega_variable(
    mandate_status_after_sovereignty,
    'After sovereignty was achieved by other means, is the cultural-center mandate dead (its problem solved, the arrangement superseded) or live (the cultural-continuity crisis persisting in new form)?',
    'Assess post-sovereignty diaspora cultural indicators: assimilation rates, Hebrew literacy outside the center, institutional dependence on it, and whether the founding diagnosis still describes Jewish cultural life.',
    'Dead implies the arrangement persisted past its function and flags obsolescence; live implies the founding problem survives and the reading retains a residual coordination role that its displacement did not extinguish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_status_after_sovereignty, conceptual, 'Genealogy status of the founding problem once the sibling program realized the kernel differently.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1889, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jtc_cultural_zionism_tr_t1889, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1889, 0.1).
narrative_ontology:measurement_basis(jtc_cultural_zionism_tr_t1889, observed).
narrative_ontology:measurement(jtc_cultural_zionism_tr_t1901, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1901, 0.13).
narrative_ontology:measurement_basis(jtc_cultural_zionism_tr_t1901, observed).
narrative_ontology:measurement(jtc_cultural_zionism_tr_t1913, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1913, 0.17).
narrative_ontology:measurement_basis(jtc_cultural_zionism_tr_t1913, observed).
narrative_ontology:measurement(jtc_cultural_zionism_tr_t1925, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1925, 0.21).
narrative_ontology:measurement_basis(jtc_cultural_zionism_tr_t1925, observed).
narrative_ontology:measurement(jtc_cultural_zionism_tr_t1936, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1936, 0.23).
narrative_ontology:measurement_basis(jtc_cultural_zionism_tr_t1936, observed).
narrative_ontology:measurement(jtc_cultural_zionism_tr_t1948, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1948, 0.24).
narrative_ontology:measurement_basis(jtc_cultural_zionism_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(jtc_cultural_zionism_be_t1889, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1889, 0.2).
narrative_ontology:measurement_basis(jtc_cultural_zionism_be_t1889, observed).
narrative_ontology:measurement(jtc_cultural_zionism_be_t1901, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1901, 0.23).
narrative_ontology:measurement_basis(jtc_cultural_zionism_be_t1901, observed).
narrative_ontology:measurement(jtc_cultural_zionism_be_t1913, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1913, 0.27).
narrative_ontology:measurement_basis(jtc_cultural_zionism_be_t1913, observed).
narrative_ontology:measurement(jtc_cultural_zionism_be_t1925, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1925, 0.31).
narrative_ontology:measurement_basis(jtc_cultural_zionism_be_t1925, observed).
narrative_ontology:measurement(jtc_cultural_zionism_be_t1936, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1936, 0.33).
narrative_ontology:measurement_basis(jtc_cultural_zionism_be_t1936, observed).
narrative_ontology:measurement(jtc_cultural_zionism_be_t1948, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1948, 0.34).
narrative_ontology:measurement_basis(jtc_cultural_zionism_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(jtc_cultural_zionism_su_t1889, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1889, 0.16).
narrative_ontology:measurement_basis(jtc_cultural_zionism_su_t1889, observed).
narrative_ontology:measurement(jtc_cultural_zionism_su_t1901, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1901, 0.19).
narrative_ontology:measurement_basis(jtc_cultural_zionism_su_t1901, observed).
narrative_ontology:measurement(jtc_cultural_zionism_su_t1913, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1913, 0.22).
narrative_ontology:measurement_basis(jtc_cultural_zionism_su_t1913, observed).
narrative_ontology:measurement(jtc_cultural_zionism_su_t1925, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1925, 0.25).
narrative_ontology:measurement_basis(jtc_cultural_zionism_su_t1925, observed).
narrative_ontology:measurement(jtc_cultural_zionism_su_t1936, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1936, 0.26).
narrative_ontology:measurement_basis(jtc_cultural_zionism_su_t1936, observed).
narrative_ontology:measurement(jtc_cultural_zionism_su_t1948, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1948, 0.26).
narrative_ontology:measurement_basis(jtc_cultural_zionism_su_t1948, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the jewish_territorial_claim kernel: one colloquial label covers four structurally distinct arrangements. Epsilon differs across members because each reading installs different machinery — this member declines sovereignty and majority instruments, yielding the family's lowest extraction profile, while its siblings add state-power, settlement-mass, or military-deterrent mechanisms with correspondingly larger victim sets. Direction of influence: this reading predates the others and supplied cultural justification that the political program absorbed (the university, the 'spiritual center' vocabulary), while the revisionist program defined itself explicitly against this one's self-limitation. All four files carry reciprocal links through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
