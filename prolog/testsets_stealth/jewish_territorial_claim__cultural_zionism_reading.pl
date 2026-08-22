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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Cultural Zionist Spiritual-Center Claim (Ahad Ha'am Reading)
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel (see
 *   kernel_context): the cultural-Zionist claim, articulated by Ahad Ha'am
 *   from 1889 onward, that the Jewish national project in Palestine is the
 *   building of a spiritual and cultural center — revived Hebrew language,
 *   literature, schools, and eventually a university — and that this center
 *   neither requires nor seeks political sovereignty or a Jewish demographic
 *   majority. The arrangement under examination is the standing
 *   cultural-center program as this reading assesses it: diaspora-funded land
 *   purchase and institution-building governed by a self-limiting charter.
 *   Its coordination achievement is real — Hebrew was revived as a spoken
 *   national language and the Hebrew University opened in 1925 — and its
 *   restraint relative to the sibling readings is real: binational frameworks
 *   were proposed in earnest (Brit Shalom, 1925; Magnes's Ihud, 1942). Its
 *   costs are also real: every major purchase wave displaced Arab tenant
 *   farmers whose removal was written into sale contracts negotiated with
 *   absentee owners the tenants never met, and communal resources committed
 *   to the center were unavailable to Jews facing annihilation, whom the
 *   program addressed as donors rather than clients. Across the interval the
 *   reading's institutional carriers drifted: the anti-sovereign restraint
 *   eroded as the political and labor mainstreams absorbed the machinery, so
 *   that by the partition era the spiritual-center label persisted over an
 *   arrangement increasingly indistinguishable from ordinary state-building
 *   settlement. All three tracked metrics run on one shared eight-point grid
 *   (1889-1947). The claim/metric gap is deliberate: the reading CLAIMS a
 *   restrained cultural program; the authored metrics describe moderate and
 *   rising extraction with growing performative maintenance — the engine
 *   measures that divergence.
 *
 * KEY AGENTS:
 *   - - hebrew_cultural_elite: Primary beneficiary (moderate/identity_locked) — collects patronage, publication, and vocation from the center
 *   - - jnf_and_hebrew_institution_leadership: Agenda-setter (institutional/constrained) — administers purchases, Hebrew-language policy, and fund allocation
 *   - - diaspora_jewish_communities: Funder-beneficiary (organized/mobile) — pays in money and migrants, receives cultural renewal
 *   - - arab_tenant_farmers: Primary target (powerless/trapped) — bears displacement written into purchase contracts they never signed
 *   - - diaspora_jews_facing_persecution: Unprotected cost-bearer (powerless/trapped) — bears the security cost of the self-limiting charter
 *   - - palestinian_arab_notables: Excluded principal (organized/constrained) — consulted as sellers, never as principals
 *   - - british_mandate_administration: Regulatory observer (institutional/arbitrage) — sets the legal outer limits, declines to adjudicate the rival visions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.52).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.45).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Spiritual-Center Claim (Ahad Ha'am Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, 'c9725ef6-76e7-4466-9655-c70c01a29e29').
narrative_ontology:cs_kernel_codification('c9725ef6-76e7-4466-9655-c70c01a29e29', distributed).
narrative_ontology:cs_authority_grounding('c9725ef6-76e7-4466-9655-c70c01a29e29', lineage).
narrative_ontology:cs_interpretation_layer_present('c9725ef6-76e7-4466-9655-c70c01a29e29').
narrative_ontology:cs_reading_relation('c9725ef6-76e7-4466-9655-c70c01a29e29', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9725ef6-76e7-4466-9655-c70c01a29e29', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9725ef6-76e7-4466-9655-c70c01a29e29', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('c9725ef6-76e7-4466-9655-c70c01a29e29', foundational, spiritual_center_sufficiency).
narrative_ontology:cs_axiom_status(spiritual_center_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('c9725ef6-76e7-4466-9655-c70c01a29e29', spiritual_center_sufficiency, instrumental).
narrative_ontology:cs_axiom('c9725ef6-76e7-4466-9655-c70c01a29e29', foundational, arab_coexistence_compatibility).
narrative_ontology:cs_axiom_status(arab_coexistence_compatibility, holdable).
narrative_ontology:cs_axiom_grounding('c9725ef6-76e7-4466-9655-c70c01a29e29', arab_coexistence_compatibility, empirically_contingent).
narrative_ontology:cs_reference_frame('c9725ef6-76e7-4466-9655-c70c01a29e29', spiritual_center_without_sovereignty).
narrative_ontology:cs_drift_state('c9725ef6-76e7-4466-9655-c70c01a29e29', late_mandate_partition_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c9725ef6-76e7-4466-9655-c70c01a29e29', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_elite).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jnf_and_hebrew_institution_leadership).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, arab_tenant_farmers).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, diaspora_jews_facing_persecution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writers, teachers, translators, and scholars who built their lives and careers around the Hebrew revival: editing journals, teaching in Hebrew schools, staffing the new university and the language committee. Patronage, publication, and professional standing flow to them through the cultural institutions funded by diaspora donations. Leaving the project would mean abandoning the language of their life's work and the community that reads them; the binational wing among them (Buber, Magnes and their circle) tied their moral standing to the charter's restraint.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_elite, beneficiary,
    moderate, generational, identity_locked, regional).

% Directors of the Jewish National Fund, the Hebrew University board, the Hebrew Language Committee, and the cultural departments of the Zionist executive. They negotiate land purchases, set settlement standards, enforce Hebrew as the language of instruction and public life, and allocate the funds the diaspora raises. Their decisions fix the pace of purchase and the terms offered to sitting tenants; stepping outside the coordinated institutions would cost them the machinery their work depends on.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jnf_and_hebrew_institution_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% Fund land purchase, schools, publishing, and the university through the national funds and countless small donations, and look to Palestine as the wellspring of a renewed Hebrew culture that answers assimilation. They also carry the cost: money raised for the center is unavailable for local welfare, and families weighing emigration receive cultural encouragement rather than practical rescue. They can redirect giving to Bundist, religious, or assimilationist causes at any time, and some do.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities, payer).

% Work land owned by absentee landlords; when owners sell to the national funds, purchase contracts typically require the tenants' removal regardless of the buyers' stated intentions. Compensation is rare and meager. Their options are wage labor in the towns, casual work in the settlements where admitted, or leaving the countryside altogether. They were never party to the negotiations that disposed of their tenancies, and the buyers' professed goodwill toward Arab society did not restore their plots.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, arab_tenant_farmers, payer,
    powerless, biographical, trapped, local).

% Jews in Romania, Russia, Poland, and later Germany whose urgent need is physical safety. The communal resources they might appeal to are committed to building the cultural center, and the program offers them cultural belonging rather than haven; entry to Palestine is capped by immigration schedules others set. Their peril is immediate and their leverage over allocation is nil; the charter's self-limitation is a decision made about them, not by them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, diaspora_jews_facing_persecution, payer,
    powerless, immediate, trapped, global).

% Urban landowning and professional families who sell land, staff the Ottoman and later Mandate administrations, and speak for Arab society in newspapers and municipal councils. They were consulted as sellers and correspondents but never as principals deciding whether, how fast, and on what terms the Jewish center would grow; the binational overtures of the 1920s and 1940s reached them after the facts on the ground had already shifted the bargaining position.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_notables, excluded,
    organized, generational, constrained, regional).

% Administers the legal frame — land registries, immigration schedules, public-order powers — under which purchases and institutions proceed. It weighs petitions from both communities, commissions inquiries after each round of unrest, and can tighten or loosen the land-transfer and immigration rules that set the program's outer limits, while declining to adjudicate between the rival visions of what the Jewish presence is for.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, british_mandate_administration, observer,
    institutional, generational, arbitrage, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__cultural_zionism_reading, jnf_and_hebrew_institution_leadership).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__cultural_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the regeneration problem of a dispersed nation: concentrate a critical mass of Hebrew writers, teachers, and scholars in the historic homeland so that language, literature, and institutions can renew at the source and radiate outward to diaspora communities — sparing the movement the overhead and dangers of state-building.
% TRANSFER_FUNCTION: Moves money and migrants from diaspora communities into land purchase, Hebrew schooling, publishing, and the university in Palestine; moves cultural authority and prestige back out to the diaspora; and moves land title from absentee Arab owners to inalienable Jewish national holdings, with the displacement costs of each sale falling on the tenants who worked the land.
% ABSENT_VOICES: The tenants displaced by each purchase had no seat at the table, which was set with absentee owners; Palestinian Arab society had no vote on the pace or character of settlement; the diaspora poor whose small gifts funded the center had no say in allocation; and endangered Jewish communities were addressed as donors, not as clients with claims. Each absence is documented in the excluded and payer seats above.
% DISAPPEARANCE_RATIONALE: Without the cultural-center program, Hebrew revival loses its geographic anchor — the schools, language committee, press, and university that made Hebrew a spoken national tongue; diaspora philanthropy redirects to local welfare or rescue; the binational current loses its carrier; and both the subsequent character of the Jewish community in Palestine and the terms on which Arab society encountered Jewish settlement take materially different shapes.
% FOUNDING_PROBLEM: Assimilation and persecution were hollowing out Jewish national culture in the diaspora faster than philanthropy or diplomatic charters could counteract. Ahad Ha'am argued that a people losing its inner content could not be saved by territory or votes alone, and that a spiritual center in the historic homeland could regenerate Hebrew culture and give diaspora Judaism a living core.
% FOUNDING_PROBLEM_CORROBORATION: Hebrew cultural historians and diaspora-studies scholarship outside the movement corroborate the reality of the cultural-dissolution problem and document the dispute over whether the center answered it; Arab historians of the Mandate period, and the binationalists' own Arab correspondents, attest that coexistence was sincerely pursued but never tested at scale; no source outside the benefiting parties attests that the center alone sufficed for Jewish security — the political and revisionist wings denied it from within the movement, and the record of the 1930s and 1940s stands against it from without.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extractiveness ends at 0.52: the land-transfer mechanism was market-mediated but externalized its displacement onto sitting tenants, and the funding mechanism drew on a captive donor pool addressed as constituents rather than customers; the reading's own lights discount both costs (binational goodwill, long-run strengthening of Jewry), so the reading-indexed epsilon sits well below what a Palestinian-arab or rescue-priority reading would author over the same referent. Suppression ends at 0.45: enforcement ran through institutional discipline — Hebrew-monolingual norms in schools and public life, labor-exclusivity compliance inside cultural institutions, fund-allocation conditionality — amplified late by the Mandate's permit politics, but the reading never possessed a state terror apparatus of its own. Theater_ratio crosses 0.5 by interval end: as the anti-sovereign substance eroded, invocations of the spiritual-center charter increasingly performed a distinctiveness that practice no longer exhibited, while the underlying institutions (university, language committee, press) kept producing real output — hence the omega on late-interval diagnosis. Accessibility_collapse 0.40: alternatives stayed live throughout — diaspora cultural autonomy, Bundism, American and German Hebrew centers — so the center never monopolized Jewish cultural life. Resistance 0.60: the reading lost every decisive intra-Zionist vote from the First Congress onward, faced Arab resistance to each purchase wave, and persisted institutionally anyway. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation. The dynamics are monotonic drift, not cyclical: the mid-1920s binational plateau is visible as a flattening in the theater series, not a reversal.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute differently. From the arab_tenant_farmers seat the arrangement operates as dispossession conducted in the language of culture: contracts signed in distant capitals decided their tenancies. From the diaspora_jews_facing_persecution seat it operates as a library endowed while the house burned. From the leadership and elite seats the same structure is the only durable form of national survival ever attempted without a army or a state. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it. On coalition: the tenants' coalition potential was structurally blocked — a geographically dispersed rural class, mediated by landlord intermediaries who were themselves the sellers, and denied standing in every forum where purchase terms were set.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map to directionality as follows. jnf_and_hebrew_institution_leadership sits near the beneficiary end but not at it: it administers the machinery and the assets accrue under its control, yet it bears the enforcement burden and its exit is constrained by the institutions it runs. hebrew_cultural_elite sits near the beneficiary end with identity-fused exit: the center is constituted by their life's work. diaspora_jewish_communities sit near symmetric — genuine cultural benefit received, real resources paid, mobile exit pulling them toward the beneficiary side. arab_tenant_farmers sit at the full-target end: victim-declared, trapped exit, zero procedural standing. diaspora_jews_facing_persecution are targets through an opportunity-cost channel — the charter's self-limitation is what denies them the rescue-oriented allocation they never voted on; the reading denies intent, but the structural relationship is what feeds the computation. palestinian_arab_notables contribute as excluded rather than directional; the mandate administration contributes as observer. No directionality overrides are authored: the derivation from declarations plus exit options captures these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — a center INSTEAD of sovereignty — was resolved by obsolescence: sovereignty arrived via the rival readings, making the charter's defining clause moot while the cultural institutions it built continued inside the state its authors never required. mandatrophy_resolved is declared true on that basis, and the R5 interview records the mismatch (status contested x verdict world_rearranges) for the consumer's zombie/capture cross-check against the computed theater path. The tangled_rope classification prevents symmetric mislabelings: reading the arrangement as pure coordination ignores that each purchase wave's costs landed on tenants with no seat; reading it as pure extraction ignores the genuine cultural production, the sincere binational current, and the restraint this reading actually exercised relative to its siblings on the same terrain. Both halves are load-bearing, which is precisely the tangled-rope structure: coordination and extraction flowing through the same purchase contracts and the same fundraising appeals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (cultural_zionism_reading) of the kernel jewish_territorial_claim; which structural element of the kernel carries the contest — territorial presence as such, or the sovereignty-and-demographic-majority commitments the sibling readings add?',
    'Comparative classification across the four sibling reading files: if the political, labor, and revisionist readings compute as substantially higher-extraction structures while this reading computes moderate, the contest is located in the sovereignty and majority premises rather than in Jewish territorial presence.',
    'Locating the contest in the sovereignty premises supports treating cultural-presence arrangements as separable from state-building extraction; locating it in presence itself would collapse the four readings into variants of a single structure and merge their victim sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Where the kernel''s contestedness structurally resides across its four readings.').

omega_variable(
    binational_framework_feasibility,
    'Was a binational framework — parity institutions, shared citizenship, no majority requirement — achievable given the demographic and economic trajectories of the period, or incompatible with them from the start?',
    'Counterfactual demographic and economic modeling of the parity proposals (Brit Shalom 1925, Ihud 1942) against migration pressure and land-market dynamics, with comparative evidence from other binational arrangements.',
    'If feasible, the reading''s low-coercion profile reflects genuine design and the rising displacement series measures the abandonment of a workable path; if infeasible, the restraint was aspiration unable to bind practice, and the costs borne by Arab society were trajectory-bound regardless of the charter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binational_framework_feasibility, empirical, 'Whether the reading''s binational potential was structurally real or aspirational.').

omega_variable(
    displacement_attribution_across_readings,
    'How much tenant displacement is attributable to this reading''s own institutional carriers versus machinery shared with, and driven by, the labor and political readings?',
    'Archival separation of purchase decisions by sponsoring body and charter rationale: acquisitions executed under cultural-institution auspices versus settlement-driven acquisitions routed through the same funds.',
    'Determines whether the rising extractiveness series measures drift within this reading or capture by sibling programs operating the same machinery — which changes the correct remedial target and the reading''s own responsibility share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_attribution_across_readings, empirical, 'Attribution of shared-machinery harms between this reading and its siblings.').

omega_variable(
    cultural_sufficiency_for_security,
    'Can cultural regeneration substitute for sovereignty in protecting Jewish physical security, or does protection categorically require state power?',
    'Conceptual analysis joined to historical test cases: outcomes for communities holding strong cultural infrastructure but no sovereignty under conditions of acute persecution.',
    'If substitution fails categorically, the self-limiting charter imposes unavoidable costs on endangered Jews and the refuge-cost victim declaration is permanent; if substitutable in principle, that cost is contingent on the era''s specific dangers rather than on the charter itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_sufficiency_for_security, conceptual, 'Whether the center-versus-sovereignty tradeoff is categorical or contingent.').

omega_variable(
    late_interval_theater_diagnosis,
    'Does the late-interval rise in performative maintenance measure a degraded mandate kept alive rhetorically, or continuing genuine function (university, language academy, Hebrew press) that the ratio misreads?',
    'Institutional output audit for the 1931-1947 window: volume and uptake of Hebrew cultural production versus ceremonial invocations of the spiritual-center charter in movement discourse.',
    'High theater confirms obsolescence drift within the reading and dates the mandate''s effective death earlier; low theater supports a reading that retained function throughout and was overridden politically rather than decayed internally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_interval_theater_diagnosis, empirical, 'Whether late-interval persistence is performance or function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1889, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jtc_cultural_tr_t1889, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1889, 0.1).
narrative_ontology:measurement(jtc_cultural_tr_t1905, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1905, 0.14).
narrative_ontology:measurement(jtc_cultural_tr_t1914, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1914, 0.18).
narrative_ontology:measurement(jtc_cultural_tr_t1921, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1921, 0.26).
narrative_ontology:measurement(jtc_cultural_tr_t1925, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1925, 0.3).
narrative_ontology:measurement(jtc_cultural_tr_t1931, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1931, 0.38).
narrative_ontology:measurement(jtc_cultural_tr_t1936, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1936, 0.46).
narrative_ontology:measurement(jtc_cultural_tr_t1947, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1947, 0.54).

% Extraction over time
narrative_ontology:measurement(jtc_cultural_be_t1889, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1889, 0.24).
narrative_ontology:measurement(jtc_cultural_be_t1905, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1905, 0.3).
narrative_ontology:measurement(jtc_cultural_be_t1914, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1914, 0.34).
narrative_ontology:measurement(jtc_cultural_be_t1921, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1921, 0.38).
narrative_ontology:measurement(jtc_cultural_be_t1925, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1925, 0.44).
narrative_ontology:measurement(jtc_cultural_be_t1931, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1931, 0.47).
narrative_ontology:measurement(jtc_cultural_be_t1936, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1936, 0.5).
narrative_ontology:measurement(jtc_cultural_be_t1947, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1947, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(jtc_cultural_su_t1889, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1889, 0.14).
narrative_ontology:measurement(jtc_cultural_su_t1905, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1905, 0.17).
narrative_ontology:measurement(jtc_cultural_su_t1914, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1914, 0.21).
narrative_ontology:measurement(jtc_cultural_su_t1921, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1921, 0.27).
narrative_ontology:measurement(jtc_cultural_su_t1925, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1925, 0.31).
narrative_ontology:measurement(jtc_cultural_su_t1931, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1931, 0.37).
narrative_ontology:measurement(jtc_cultural_su_t1936, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1936, 0.41).
narrative_ontology:measurement(jtc_cultural_su_t1947, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1947, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Zionism' (and the kernel 'Jewish territorial claim') covers four structurally distinct claims with different epsilon values, different victim sets, and different enforcement profiles. This file is the upstream-most member on the restraint axis — it defines the minimal-sovereignty baseline against which the siblings' added commitments (majority requirement, labor exclusivity, maximal territory) register as incremental extraction. Edges run from this reading to all three siblings because its institutions and its moral critique shaped the environment in which they operated; the siblings' files carry their own reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
