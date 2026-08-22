% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__cultural_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: jewish_sovereignty_palestine__cultural_zionist_reading
 *   human_readable: Cultural Zionist Spiritual-Center Program in Palestine
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   From the 1880s onward, a stream of Jewish thinkers proposed renewing
 *   Jewish national culture by building a spiritual center in Palestine —
 *   Hebrew-language schools, presses, a university — deliberately decoupled
 *   from demands for political sovereignty or a Jewish demographic majority,
 *   with Palestinians figured as co-inhabitants of a shared cultural space.
 *   This story instantiates ONE reading of the contested kernel
 *   jewish_sovereignty_palestine (the cultural_zionist_reading); the sibling
 *   readings are separate constraint files, not folded in here. The epsilon
 *   referent is the standing arrangement under contest — the cultural-center
 *   program as it actually operated — assessed by this reading's own lights,
 *   which yields low but nonzero extraction: the reading itself (from Ahad
 *   Ha'am's 1891 report onward) registered the tenant displacements
 *   accompanying land purchase as a moral stain to be corrected, not as
 *   constitutive of the project. The claim and the metrics are independent
 *   authored facts: the reading is CLAIMED as rope (genuine coordination, net
 *   beneficiaries, open alternatives) while the metrics describe modest
 *   extraction that rises over the interval as the program scales and
 *   entangles with the surrounding political conflict.
 *
 * KEY AGENTS:
 *   - cultural_zionist_intelligentsia: agenda-setter (organized/mobile) — articulates the ideal, builds the institutions, raises the funds
 *   - jewish_diaspora_communities: primary beneficiary (organized/mobile) — supplies money, people, and attention; consumes the cultural output
 *   - hebrew_cultural_institutions: institutional beneficiary (constrained exit) — the rooted infrastructure the flows land in
 *   - palestinian_arab_residents: co-inhabitant cost-bearer and exchange partner (moderate/constrained) — dual-positioned payer and beneficiary
 *   - palestinian_tenant_farmers: concentrated cost-bearer (powerless/trapped) — bear the tenancy terminations when estates change hands
 *   - bundist_diaspora_autonomists: excluded voice (organized/mobile) — would redirect the effort to diaspora cultural autonomy
 *   - mandate_period_administrations: regulatory observer (institutional/arbitrage) — license, tax, and intermittently restrict
 *   - comparative_nationalism_analysts: analytical observer — sees the whole structure without stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__cultural_zionist_reading, 0.28).
domain_priors:suppression_score(jewish_sovereignty_palestine__cultural_zionist_reading, 0.12).
domain_priors:theater_ratio(jewish_sovereignty_palestine__cultural_zionist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Cultural Zionist Spiritual-Center Program in Palestine").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political philosophy/nationalism studies/postcolonial theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, 'a2cdd9e4-a4e1-482d-ab2b-819a60fc453e').
narrative_ontology:cs_kernel_codification('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', distributed).
narrative_ontology:cs_authority_grounding('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', expertise).
narrative_ontology:cs_interpretation_layer_present('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e').
narrative_ontology:cs_reading_relation('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', foundational, spiritual_center_sufficiency_without_sovereignty).
narrative_ontology:cs_axiom_status(spiritual_center_sufficiency_without_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', spiritual_center_sufficiency_without_sovereignty, deontological).
narrative_ontology:cs_axiom('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', foundational, binational_coinhabitation_of_shared_homeland).
narrative_ontology:cs_axiom_status(binational_coinhabitation_of_shared_homeland, holdable).
narrative_ontology:cs_axiom_grounding('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', binational_coinhabitation_of_shared_homeland, deontological).
narrative_ontology:cs_reference_frame('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', cultural_center_without_sovereignty_frame).
narrative_ontology:cs_drift_state('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', post_statehood_sovereign_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a2cdd9e4-a4e1-482d-ab2b-819a60fc453e', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_institutions).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_tenant_farmers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writers, essayists, and educators, centered on figures in the Ahad Ha'am circle, who articulate the ideal of renewing Jewish national culture in Palestine and organize the schools, presses, and eventually the university that embody it. They raise funds, recruit teachers and students, and publish the case for a spiritual center decoupled from demands for statehood. Most live in Europe and travel or relocate selectively; if the project disappoints them they can return to European literary life.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, cultural_zionist_intelligentsia, agenda_setter,
    organized, civilizational, mobile, global).

% Communities across Eastern Europe and later elsewhere that send money, teachers, students, and immigrants, and consume the books, newspapers, and schooling the Palestinian center produces. Hebrew school networks and cultural societies tie them to Palestine. They can deepen or loosen the tie at will; their communal life continues either way, which is precisely what the center is supposed to guarantee.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, mobile, global).

% Schools, publishing houses, the Hebrew University, theaters, and the Hebrew Language Committee rooted in Jerusalem and other cities. They receive philanthropic transfers and immigrant talent, hold purchased land in trust for national-cultural purposes, and set the standards of revived Hebrew. Relocating would dissolve them; their existence in place is the project.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_institutions, beneficiary,
    institutional, generational, constrained, national).

% Arabic-speaking townspeople and villagers who encounter the arriving community as neighbors, customers, employers, and purchasers of adjacent land. Some sell land, some find urban work in the growing mixed economy, some send children to new schools; many watch the language mix and land prices of their districts shift year over year. Leaving would mean abandoning home, kin networks, and livelihood, so nearly all stay and adapt.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_residents, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_residents, beneficiary).

% Families who farm plots owned by absentee landlords. When estates are sold to Jewish purchasing bodies, tenancy agreements terminate and the families must vacate land they never owned, with compensation that is inconsistent and often delayed. Their farming skill, credit, and social world are tied to the specific plots; there is no realistic alternative to the land they worked.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_tenant_farmers, payer,
    powerless, biographical, trapped, local).

% Jewish socialist organizers in Eastern Europe who argue for cultural autonomy where Jews actually live and regard Palestine-centered projects as a detour from the struggles of the diaspora masses. They publish critiques and build rival Yiddish school networks but take no part in the Palestinian institutions and are not seated in the bodies that allocate the movement's funds.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, bundist_diaspora_autonomists, excluded,
    organized, biographical, mobile, continental).

% First Ottoman, then British, authorities who license immigration and land transactions, tax the new institutions, and periodically restrict purchase or entry. They observe and regulate the conditions under which the center grows but do not run it; their posture shifts with metropolitan politics rather than with the cultural program itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, mandate_period_administrations, observer,
    institutional, generational, arbitrage, continental).

% Scholars of nationalism and later of Israel/Palestine studies who compare this cultural-center program with other diaspora-homeland projects and trace how its fortunes tracked the surrounding political conflict. They hold no stake in the flows and can see the whole structure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, comparative_nationalism_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_institutions).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__cultural_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-emancipation problem of sustaining a modern, territorially anchored Jewish culture: it concentrates language revival, scholarship, publishing, and teacher training in one living center that dispersed communities can draw on, replacing duplicated diaspora-by-diaspora effort with a shared cultural infrastructure — deliberately without requiring state machinery or a demographic majority.
% TRANSFER_FUNCTION: Moves philanthropic funds, immigrant labor, and cultural talent from diaspora communities to Palestinian-based institutions; moves renewed language, books, and cultural authority back to the diaspora; and moves land, largely from absentee owners, into Jewish national-institutional hands — with the attendant termination of tenancies borne locally.
% ABSENT_VOICES: Bundist autonomists and assimilationist reformers would object from inside Jewish life that the center drains energy and funds from diaspora struggles; organized Palestinian national representatives, once they form, would object to any large-scale immigration regardless of its cultural framing. Both sit outside the congresses, philanthropic boards, and purchase committees where the program's decisions are made.
% DISAPPEARANCE_RATIONALE: If the program vanished overnight, the Hebrew revival loses its anchoring institutions and proceeds fragmentarily or not at all; diaspora cultural trajectories diverge toward assimilation or autonomism; the land-purchase economy of the purchase associations never forms, and the tenant displacements tied to it do not occur. The arrangement's beneficiaries and cost-bearers alike are organized around it.
% FOUNDING_PROBLEM: The post-emancipation Jewish cultural crisis: legal integration into European states was dissolving distinctive Jewish culture, reducing Judaism to a thin confessional remnant without living national content. The program was built to regenerate that content by making Hebrew a spoken vernacular and Palestine its living center.
% FOUNDING_PROBLEM_CORROBORATION: Historians of emancipation-era Jewry, writing outside the movement, document the assimilation crisis the program answered, and contemporaneous Arabic-language papers (al-Karmil, Filastin) attest the local-cost side from outside the beneficiary set. The movement's own congress proceedings attest the problem's liveness from inside. No source outside the beneficiary set attests that the non-sovereign cultural-center form specifically remains the live answer — that claim rests with the movement's heirs, which is why the status is recorded as contested rather than live.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__cultural_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.28 at interval end) because the arrangement's transfers run mainly through voluntary channels — philanthropy, migration, purchase — with no coercion-based levy; the extraction that does occur is land-market externality (tenancy termination on sold estates) and cultural-hegemony drift in mixed districts, and it rises over the interval as purchases accelerate and the cultural project's infrastructure sits inside a politicizing conflict. Suppression is very low (0.12) because there is no enforcement machinery at all: participation is voluntary, and the alternatives — diaspora cultural autonomy, assimilation, other territorial schemes — remain live, which is also why accessibility_collapse is low (0.20). Theater_ratio (0.35 at end) tracks the widening gap between spiritual-center rhetoric and actual activity: early in the interval the cultural function is almost entirely real (the Hebrew revival demonstrably worked), while late in the interval a growing share of the discourse functions as legitimation for activity that has quietly become nation-building. Resistance (0.35) is moderate: ideological dissent inside Jewish life (autonomists, assimilationists) plus local Palestinian opposition to land sales. No suppression_requirement series is authored: enforcement capacity never developed in this arrangement, so the enforcement picture is static and is carried by the scalar alone. The identity_coordination typing is genuine, not cover: coordinating Jewish cultural membership and boundary maintenance around a territorial center is literally what the arrangement does; the coupling caveat is recorded in the tenant_displacement_magnitude omega, since the land substrate does concentrate costs on powerless locals.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the point. From the diaspora beneficiary seat the arrangement is a renaissance: voluntary giving, voluntary coming, culture returned multiplied. From the palestinian_tenant_farmers seat — powerless, trapped, bearing concentrated tenancy termination — the same land transfers compute as pure cost with no offsetting flow. The palestinian_arab_residents seat sits between: real exchange benefits, real displacement pressure, no exit. The reading's own moral tradition registered this gap internally (Ahad Ha'am's 1891 critique of how the first settlers treated the fellahin), which is evidence that the divergence is structural rather than an artifact of hostile observation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the diaspora communities and the rooted institutions near the beneficiary pole (low d); the tenant farmers, as declared victims with trapped exit and no power, derive near the full-target pole — correctly even within this reading's lights, since their costs are concentrated and uncompensated. One override is authored: the moderate power atom (occupied only by palestinian_arab_residents in this story) is set to d=0.58 because the role-derived value from their payer listing alone would overshoot — the story establishes them as dual-positioned co-inhabitants with substantive offsetting exchange benefits, placing them near symmetric with a slight lean toward cost-bearing. Scope amplification is modest: the arrangement operates at regional and national scopes where verification is comparatively easy, unlike planetary-scale mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification risk here runs in both directions, and the rope claim with an honest victims list is what guards the middle. Importing the sibling readings' verdicts (as the settler_colonial_reading would compute them) into this file would flatten a genuine, verifiable coordination achievement — the Hebrew revival is real, voluntary, and still functioning — into pure extraction, destroying the seat-divergence signal. Conversely, a pristine rope story with no victims declared would erase the documented tenancy harm the reading's own founders acknowledged, manufacturing a false summit of benignity. On obsolescence: the founding problem is contested rather than dead — the cultural-vitality problem persists, but the specific non-sovereign form was overtaken by the sovereignty-centered politics that outcompeted it, so the arrangement is not yet a piton (its institutions still perform their real function) nor a resolved mandate; the theater_ratio rise marks where the drift toward performance is beginning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the jewish_sovereignty_palestine kernel does a given assessment apply to? This story authors only the cultural_zionist_reading, under which sovereignty and demographic majority are not constitutive of the national project.',
    'Cross-file comparison of the five sibling stories'' victim sets, epsilon values, and axioms; the disagreement is located in the constitutive role of sovereignty, not in the underlying empirical facts alone.',
    'Assessments computed under a sibling reading (for instance the settler_colonial_reading) apply to a different constraint with a different victim set; mixing readings produces apparent epsilon instability that is actually indexical slippage between constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: this constraint is one reading of a five-way contested kernel; sibling readings are separate files.').

omega_variable(
    extraction_attribution_drift,
    'Does the measured rise in extractiveness over the interval reflect the cultural-center arrangement''s own operating logic at scale, or contamination from the political-sovereignty program that progressively absorbed the Yishuv''s activity?',
    'Compare land-purchase and institutional behavior during windows when the cultural-only covenant was explicitly operative (pre-Balfour cultural philanthropy, the Brit Shalom proposal years) against windows of open political competition, holding purchase volume constant.',
    'If contamination dominates, this reading''s epsilon stays low and the rope classification holds for the reading''s own arrangement; if the rise is inherent to scaled cultural presence, the reading drifts toward hybrid coordination-extraction even on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_attribution_drift, empirical, 'Whether the temporal extraction drift is endogenous to the cultural program or imported by the political program that outcompeted it.').

omega_variable(
    tenant_displacement_magnitude,
    'How many tenant families were actually displaced by purchases made for cultural-institutional purposes, as distinct from settlement-maximizing purchases by the political wing?',
    'Land-transfer registries, purchase-association acquisition records, and contemporaneous commission reports, cross-tabulated by purchaser affiliation and subsequent tenancy termination.',
    'Sets the weight of the palestinian_tenant_farmers victim seat; a small magnitude supports the low-epsilon profile, a large magnitude forces upward revision of extraction even within this reading''s own lights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenant_displacement_magnitude, empirical, 'Magnitude of the concentrated harm borne by the trapped payer seat.').

omega_variable(
    coinhabitation_reciprocity_status,
    'Did Palestinian residents experience the shared cultural-economic space as reciprocal exchange or as asymmetric imposition?',
    'Contemporaneous Arabic-language press (al-Karmil, Filastin), petition records, and oral histories, weighed against records of Arab participation in mixed institutions, labor markets, and land sales.',
    'If imposition dominates, the secondary beneficiary positioning of palestinian_arab_residents is nominal and their effective directionality moves toward the target pole, raising computed extraction; if substantively reciprocal, the near-symmetric placement holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coinhabitation_reciprocity_status, empirical, 'Whether the dual-positioned co-inhabitant seat''s benefit side is substantive or rhetorical.').

omega_variable(
    cs_authority_framing_ambiguity,
    'Is the authority of this reading grounded in the intelligentsia''s demonstrated moral-cultural competence (the expertise framing adopted here) or in continuity with the prophetic strand of Jewish tradition (a lineage framing)?',
    'Examine how the movement adjudicated its internal disputes: by appeal to the literary and moral standing of particular figures and arguments, or by appeal to inherited tradition and transmitted texts.',
    'Under the lineage framing, the interpretive-layer and drift computations change character — drift would read as transmission failure rather than practice departure; the expertise framing adopted here treats the severe post-statehood gap as the world leaving the reference frame, not the frame failing to transmit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing_ambiguity, conceptual, 'Two coherent framings of the reading''s authority ground yield different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t10, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(jewi_tr_t20, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(jewi_tr_t40, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(jewi_tr_t50, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(jewi_tr_t60, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(jewi_be_t10, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(jewi_be_t20, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(jewi_be_t40, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(jewi_be_t50, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 50, 0.27).
narrative_ontology:measurement(jewi_be_t60, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 60, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_sovereignty_palestine__cultural_zionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Jewish sovereignty in Palestine' covers five structurally distinct commitments and is decomposed per the epsilon-invariance principle into five linked stories, each authoring its own epsilon over the standing arrangement under contest with its own victim set and axioms. Direction of influence: this cultural reading is upstream of the liberal_nationalist_reading (the Hebrew-cultural substrate and institution-building it created were drawn on by the statehood project) and upstream of the post_zionist_reading (the binational tradition of Brit Shalom and Magnes supplies intellectual resources the civic-equality critique draws on); it is logically incompatible with the settler_colonial_reading's inherent-displacement premise. Each file links the others through affects_constraints so contamination and legitimacy propagation can be traced across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__cultural_zionist_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
