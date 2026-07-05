% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony Reading of Territorial Legitimacy
 *   domain: political/territorial/international_relations
 *
 * SUMMARY:
 *   This story instantiates the Palestinian autochthony reading of the
 *   contested territorial-legitimacy kernel: legitimacy grounded in
 *   documented continuous habitation prior to 1948/1967, the treatment of
 *   displacement as an ongoing (not historically closed) injustice, and an
 *   understanding of the right of return as a non-negotiable,
 *   generationally-transmissible individual and collective entitlement rather
 *   than a bargaining chip. This is one reading among (at least) three
 *   siblings — the zionist_refuge_reading (Israeli legitimacy grounded in
 *   persecution history, historical/religious connection, and UN partition
 *   acceptance) and the two_state_coexistence_reading (mutual recognition
 *   with 1967 lines as negotiated compromise). Per the ε-invariance
 *   principle, this story does not average or hedge across those readings; it
 *   authors one internally coherent claim with its own ε, beneficiary/victim
 *   structure, and classification, and routes the contest itself to omega
 *   variables and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - palestinian_refugees_1948: Primary target (powerless/trapped) — bears the core displacement injustice this reading exists to remedy
 *   - palestinian_refugees_1967: Secondary target (powerless/trapped) — layered displacement claim
 *   - west_bank_residents and gaza_residents: Present-tense targets (powerless/trapped) — bear ongoing territorial reduction and occupation enforcement
 *   - palestinian_citizens_of_israel: Internally displaced minority citizens (moderate/constrained) — least visible version of the same claim
 *   - israeli_state_apparatus: Primary agenda-setter and beneficiary (institutional/arbitrage) — administers the legal architecture that keeps the claim non-actionable
 *   - settlement_movement: Concentrated beneficiary (organized/mobile) — captures land whose prior habitation is documented
 *   - unrwa_and_international_bodies: Analytical/administrative observer — sustains documentary infrastructure without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.71).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.78).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony Reading of Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/territorial/international_relations").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '863f3296-443e-4b27-914c-68c247e1acb4').
narrative_ontology:cs_kernel_codification('863f3296-443e-4b27-914c-68c247e1acb4', distributed).
narrative_ontology:cs_authority_grounding('863f3296-443e-4b27-914c-68c247e1acb4', distributed).
narrative_ontology:cs_reading_relation('863f3296-443e-4b27-914c-68c247e1acb4', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('863f3296-443e-4b27-914c-68c247e1acb4', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('863f3296-443e-4b27-914c-68c247e1acb4', foundational, continuous_habitation_grounds_durable_title).
narrative_ontology:cs_axiom_status(continuous_habitation_grounds_durable_title, holdable).
narrative_ontology:cs_axiom_grounding('863f3296-443e-4b27-914c-68c247e1acb4', continuous_habitation_grounds_durable_title, deontological).
narrative_ontology:cs_axiom('863f3296-443e-4b27-914c-68c247e1acb4', foundational, displacement_claims_transmit_across_generations).
narrative_ontology:cs_axiom_status(displacement_claims_transmit_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('863f3296-443e-4b27-914c-68c247e1acb4', displacement_claims_transmit_across_generations, deontological).
narrative_ontology:cs_axiom('863f3296-443e-4b27-914c-68c247e1acb4', secondary, right_of_return_is_non_negotiable).
narrative_ontology:cs_axiom_status(right_of_return_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('863f3296-443e-4b27-914c-68c247e1acb4', right_of_return_is_non_negotiable, conventional).
narrative_ontology:cs_reference_frame('863f3296-443e-4b27-914c-68c247e1acb4', pre_1948_continuous_habitation_title).
narrative_ontology:cs_drift_state('863f3296-443e-4b27-914c-68c247e1acb4', post_oslo_stalemate_contemporary, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('863f3296-443e-4b27-914c-68c247e1acb4', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, settlement_movement).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1967).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_residents).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_residents).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, continuous_habitation_grounds_title).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, displacement_creates_durable_remedial_claim).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, right_of_return_survives_generational_transfer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced in 1948 and their descendants, now numbering in the millions across refugee camps in Lebanon, Syria, Jordan, and the West Bank/Gaza. Hold UNRWA-registered refugee status and claim title to specific villages and lands under continuous habitation prior to displacement. Cannot return under Israeli law; exit consists of permanent camp residence, resettlement in host states with varying citizenship rights, or diaspora dispersal — none of which satisfy the claim, which is specifically to the land.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Displaced in the war that produced the occupation of the West Bank and Gaza; a second, layered displacement on top of the 1948 dispossession for many families. Live predominantly in Jordan and in camps within the occupied territories themselves, administratively separated from pre-1948 lands they or their parents fled.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees_1967, payer,
    powerless, generational, trapped, regional).

% Live under military occupation, checkpoint systems, and settlement expansion that fragments contiguous Palestinian land. Assert continuous habitation as the basis of title against ongoing land confiscation and settlement construction. Movement, building permits, and water access are administratively controlled by the occupying authority; exit from the constraint means either legal challenge inside a system stacked against them or emigration that itself constitutes another displacement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_residents, payer,
    powerless, biographical, trapped, local).

% Live under blockade with population majority-composed of 1948 refugees and their descendants concentrated in a small strip after their families were expelled from what became southern and central Israel. Assert the same continuous-habitation and return claim to lands now inside Israel proper, a few kilometers away and legally unreachable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_residents, payer,
    powerless, biographical, trapped, local).

% Remained on land inside the 1948-established state and hold Israeli citizenship, but frequently on internally displaced or confiscated terms — many are 'present absentees' barred from returning to depopulated home villages within Israel's own recognized borders. Formally enfranchised but structurally treated as a demographic and security concern within a state defined as Jewish; their internal displacement claims are the least internationally visible version of the same underlying dispossession.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_citizens_of_israel, excluded).

% Administers land registry, citizenship law, the Law of Return, and military governance over the occupied territories. Determines which claims of habitation and title are legally cognizable and which are extinguished by absentee-property statutes. Controls the enforcement machinery — border control, land administration, demolition orders — that keeps the return claim non-actionable, while treating its own 1948 founding as settled and non-revisable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus, beneficiary).

% Ideologically and materially organized to establish and expand Israeli civilian presence in the West Bank, often on land whose prior Palestinian habitation is well-documented. Receives state subsidy, infrastructure, and security protection; benefits directly from the non-recognition of Palestinian continuous-habitation claims to the same parcels.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, settlement_movement, beneficiary,
    organized, generational, mobile, regional).

% Jordan, Lebanon, and Syria host large long-term refugee populations under varying degrees of enfranchisement, often citing the right of return as the reason to withhold full citizenship (to preserve the return claim rather than dissolve it through absorption). Their policy choices shape whether the refugee condition is treated as temporary or permanent but they are not party to the core sovereignty dispute.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, host_arab_states, excluded,
    institutional, generational, constrained, regional).

% Maintains refugee registration, camp administration, and the documentary record of displacement across generations. UN General Assembly Resolution 194 is repeatedly invoked but has no independent enforcement mechanism; the body sustains the claim's legal and administrative infrastructure without power to realize it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, unrwa_and_international_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the coordination function claimed is intergenerational: preserving a documented, continuous chain of habitation, title, and displacement testimony so that a remedial claim to specific land does not lapse through the passage of generations or the creation of new political facts on the ground.
% TRANSFER_FUNCTION: The land, property, and citizenship rights associated with pre-1948 and pre-1967 Palestinian habitation were transferred to the Israeli state and to individual settlers and are held there by continuing legal and military enforcement (absentee property law, land registry, checkpoint and permit systems) against the reading's claim that title survived displacement.
% ABSENT_VOICES: Individual Palestinian land-title holders and village councils whose specific claims are aggregated into the collective refugee category rarely appear as individuated legal claimants in any forum with power to adjudicate; Israeli citizens who reject the state's founding narrative are a minority voice inside Israeli political discourse and are not represented in this constraint's core stakeholder set. Host-state populations who bear the material burden of refugee hosting are also outside the core dispute yet shape its persistence.
% DISAPPEARANCE_RATIONALE: If this reading's premises were universally accepted and acted upon, the demographic, legal, and territorial architecture of Israel and the occupied territories would be reorganized around a realized right of return and land restitution — an outcome the current Israeli state apparatus and settlement movement structure their existence to prevent. Conversely, if the underlying claim were extinguished (through generational attrition, legal termination of UNRWA refugee status, or negotiated waiver), the political basis for a large share of Palestinian national mobilization would dissolve. The claim is precisely the kind of arrangement whose presence or absence rearranges concrete institutional facts.
% FOUNDING_PROBLEM: The mass displacement of Palestinian Arabs during and after the 1947-49 war (the Nakba) and the 1967 war left hundreds of thousands, later millions, without access to homes, land, and property they and their families had continuously inhabited, with no negotiated or adjudicated resolution of title or return.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the Palestinian national movement itself by UN General Assembly Resolution 194 (1948, reaffirmed annually), by Israeli historians of the 'new historian' school (e.g. documented village depopulation records), by UNRWA's continuously renewed refugee registration data, and by international human rights and humanitarian law bodies documenting ongoing occupation-related displacement. The Israeli state apparatus and much of Israeli domestic legal doctrine dispute the 'live' characterization, treating 1948 as legally and politically settled — this is itself the contested axis the omega variables below record.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at present) because, under this reading, land, citizenship, and residency rights attaching to documented pre-displacement habitation are held by the Israeli state and settlement infrastructure against a live, transmissible remedial claim — this is a transfer sustained by continuing legal and administrative machinery (absentee property law, military administration, permit regimes), not a one-time historical event. Suppression is authored even higher (0.78) because the claim's non-realization depends on active, ongoing enforcement — border control, demolition orders, checkpoint systems, denial of return — rather than on the claim's own weakness or voluntary Palestinian abandonment of it. Theater ratio is moderate and shows a bump around the Oslo period (1993-2000) when negotiation processes performed engagement with the refugee question while UNRWA registration and settlement expansion both continued underneath; it settles to a lower but nontrivial level post-2000 as the diplomatic process itself lost credibility as a vehicle for resolution. Accessibility collapse (0.58) is moderate-high but not total: legal, diplomatic, and international-forum avenues remain nominally open even though none has produced restitution, which differentiates this from a mountain-grade collapse. Resistance is authored very high (0.82) — active political mobilization, refugee-status maintenance, legal challenge, and diaspora organizing continuously contest the arrangement, which is itself evidence against treating the current territorial order as natural or settled.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of palestinian_refugees_1948/1967 and occupied-territory residents, the current territorial order is an actively enforced denial of a remedial claim that persists across generations. From the seat of the israeli_state_apparatus, 1948 is treated as legally and politically settled, and continued Palestinian assertion of the claim is read as a revisionist challenge to established sovereignty rather than as remedy for ongoing injustice. The engine computes these divergent seat-level classifications from the structural power/exit/scope data; this story does not adjudicate between them — it authors the palestinian_autochthony seat cleanly and routes the disagreement about which seat is correct to the sibling readings and to the omega variables below.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian refugee populations (1948 and 1967 cohorts) and present-day occupied-territory residents are declared victims with trapped exit options — they cannot access the land or restitution the claim concerns, and their day-to-day life is administratively constrained by the very apparatus that denies the claim. This drives their derived directionality toward the full-target end. The Israeli state apparatus is declared beneficiary and agenda-setter with arbitrage-grade exit (it sets the rules determining which claims are legally cognizable), driving its directionality toward the full-beneficiary end. The settlement movement is a concentrated organized beneficiary with mobile exit (settlers can and do relocate within a state-subsidized system) that captures land value directly tied to non-recognition of the habitation claim. Palestinian citizens of Israel occupy an intermediate position — formally enfranchised (moderate power, constrained exit) but bearing an internally-displaced version of the same dispossession, which is why they carry both payer and excluded secondary roles.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (1948/1967 displacement without adjudicated resolution) as live, not dead — which is precisely what distinguishes it from a mandatrophy-resolved reading in which the refugee question would be treated as a closed historical grievance requiring only humanitarian, not restitutive, response. The tangled_rope classification (rather than pure snare) reflects that a genuine coordination function is claimed within this reading — preserving intergenerational title and testimony against evidentiary loss — alongside the asymmetric extraction the reading identifies (land and citizenship rights held by continuing enforcement). Classifying this as pure snare would deny the coordination claim entirely (i.e., deny that documentation and memory-preservation serve any function beyond extraction of political leverage); classifying it as pure rope would deny the asymmetric cost structure the reading centers. The tangled_rope label preserves both halves as the reading's own internally coherent claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    displacement_status_live_or_settled,
    'Is the 1948/1967 Palestinian displacement a live, unresolved injustice requiring restitutive remedy, or a historically closed event whose consequences are now properly addressed only through humanitarian and negotiated-compromise mechanisms (as the two_state_coexistence_reading and zionist_refuge_reading would hold)?',
    'There is no neutral empirical adjudicator for this question — it is a normative and legal-theoretic dispute about whether remedial title claims can survive multiple generations and changed political facts on the ground. Partial resolution mechanisms include international legal rulings (ICJ advisory opinions), negotiated settlement outcomes, or long-run demographic/political shifts that make one framing practically dominant regardless of its normative correctness.',
    'If treated as settled/closed, this reading''s tangled_rope classification would likely shift toward describing residual claims as a scaffold (a transitional humanitarian arrangement with an implicit sunset) rather than an ongoing extraction structure; if treated as live and non-negotiable, the current classification and high suppression/extractiveness scores are the more defensible characterization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displacement_status_live_or_settled, conceptual, 'Whether the founding displacement is live or historically closed — the central normative fork between this reading and its siblings.').

omega_variable(
    right_of_return_operationalization,
    'Does ''right of return'' as claimed by this reading mean literal return to specific pre-1948 family land and homes (largely no longer extant in original form), or a broader remedial package (compensation, resettlement choice, symbolic acknowledgment) that could be realized without full demographic reversal?',
    'Survey and legal-textual analysis of how the claim is actually asserted across Palestinian political factions, refugee generations, and legal advocacy bodies — the claim is not monolithic even within this reading.',
    'A literal-return operationalization sustains the highest extractiveness and suppression scores (full demographic and territorial reversal is what current enforcement blocks); a compensation/choice-package operationalization might be compatible with negotiated settlement mechanisms closer to the two_state_coexistence_reading, changing the practical stakes of the tangled_rope classification without changing this story''s own authored ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_operationalization, conceptual, 'Internal ambiguity within the autochthony reading about what realizing the claim would actually require.').

omega_variable(
    coordination_versus_extraction_within_the_claim,
    'Is the intergenerational preservation of habitation documentation and refugee registration (the coordination function claimed here) separable from the claim''s use as ongoing political leverage in a live territorial conflict (the extraction-adjacent function)?',
    'Comparative analysis of other durable refugee/restitution claims (e.g. Cyprus, Bosnia, indigenous land claims in settler states) where documentation persisted for generations under negotiated eventual resolution versus cases where documentation persisted primarily to sustain political mobilization.',
    'If separable, the coordination half of the tangled_rope classification is more defensible as a genuine, non-extractive function; if inseparable, the classification moves closer to snare from this reading''s own internal vantage, since the documentation function would then be inseparable from its use as leverage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_versus_extraction_within_the_claim, conceptual, 'Whether the claim''s coordination and extraction dimensions can be structurally disentangled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.63).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2010, 0.69).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the territorial_legitimacy_dual kernel: palestinian_autochthony_reading (this file), zionist_refuge_reading, and two_state_coexistence_reading. Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure and classification, per the ε-invariance decomposition principle — the kernel is not resolved into a single averaged constraint. The autochthony reading identifies the Israeli state apparatus and settlement movement as beneficiaries and Palestinian refugee/occupied populations as victims; the zionist_refuge_reading (sibling) would identify a different beneficiary/victim structure grounded in its own premises; the two_state_coexistence_reading (sibling) treats both national claims as symmetrically legitimate and would show a materially lower extractiveness/suppression profile reflecting a negotiated-compromise framing. Network edges here record that shifts in the legal or political viability of one reading (e.g., an ICJ ruling, a negotiated settlement) structurally pressure the operating conditions of the sibling readings without foreclosing them outright.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
