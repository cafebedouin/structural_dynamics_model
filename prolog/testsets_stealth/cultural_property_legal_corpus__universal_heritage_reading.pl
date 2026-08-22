% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Doctrine of Cultural Property
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the cultural_property_legal_corpus
 *   kernel: the universal-heritage reading, under which legitimate authority
 *   over cultural artifacts rests with whatever institutions maximize
 *   preservation and universal access, regardless of geographic origin. The
 *   standing arrangement under contest is the regime in which metropolitan
 *   encyclopedic museums retain colonial-era holdings, defended by limitation
 *   periods, good-faith-purchase doctrines, and anti-seizure statutes, while
 *   claimant states must mount costly campaigns for return. Per the
 *   epsilon-invariance principle, the sibling readings
 *   (sovereign_repatriation_reading, indigenous_stewardship_reading) are
 *   separate constraints with separate epsilon values and beneficiary/victim
 *   structures; they appear here only as linked network neighbors, not as
 *   hedges inside this story. Epsilon's referent is the retention regime
 *   itself, assessed by this reading's own lights: measured against its own
 *   maximizing criterion, the arrangement concentrates custody gains in a
 *   handful of institutions while access to source publics remains thin and
 *   claimants bear the costs of contest, yielding high extraction even before
 *   any rival reading's evaluation is consulted.
 *
 * KEY AGENTS:
 *   - metropolitan_encyclopedic_museums: primary beneficiary and agenda-setter (institutional/arbitrage) — collects custody, revenue, and prestige; controls retention policy
 *   - holding_state_legal_systems: enforcement arm (institutional/constrained) — administers the limitation-period and good-faith doctrines that bar claims
 *   - colonial_successor_states: primary target (organized/constrained) — bears legal costs, diplomatic friction, and domestic political pressure per campaign
 *   - source_communities_of_removed_artifacts: deepest target (powerless/identity_locked) — bears identity and religious-practice harm with no independent procedural seat
 *   - cosmopolitan_museum_visitors and global_scholarship_community: diffuse beneficiaries (mobile) — receive access and research capital
 *   - diaspora_and_descendant_communities: excluded voice (trapped) — would contest the state-to-state framing but has no channel
 *   - unesco_intergovernmental_committee: analytical observer (institutional/analytical) — mediates without binding power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.74).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.68).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Doctrine of Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, 'd8ac4770-eef7-44da-a5a5-9dbfb8520b73').
narrative_ontology:cs_kernel_codification('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', formalized).
narrative_ontology:cs_authority_grounding('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', expertise).
narrative_ontology:cs_interpretation_layer_present('d8ac4770-eef7-44da-a5a5-9dbfb8520b73').
narrative_ontology:cs_reading_relation('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', foundational, custody_follows_access_maximization).
narrative_ontology:cs_axiom_status(custody_follows_access_maximization, holdable).
narrative_ontology:cs_axiom_grounding('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', custody_follows_access_maximization, instrumental).
narrative_ontology:cs_axiom('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', secondary, good_faith_possession_confers_legitimate_custody).
narrative_ontology:cs_axiom_status(good_faith_possession_confers_legitimate_custody, holdable).
narrative_ontology:cs_axiom_grounding('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', good_faith_possession_confers_legitimate_custody, conventional).
narrative_ontology:cs_reference_frame('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', enlightenment_universal_custody).
narrative_ontology:cs_drift_state('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', contemporary_repatriation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d8ac4770-eef7-44da-a5a5-9dbfb8520b73', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, metropolitan_encyclopedic_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, cosmopolitan_museum_visitors).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, global_scholarship_community).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, colonial_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, source_communities_of_removed_artifacts).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, universal_museum_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, non_retroactivity_principle).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, good_faith_acquisition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and display large collections assembled through colonial-era expeditions, purchases, and wartime removals. Set retention policy, decide which return requests to entertain, and shape public framing through exhibitions, catalogs, and directorial testimony. Collect admission revenue, donor support, tourism spillovers, and scholarly prestige attached to holding canonical objects. Their alternatives are unusually rich: long-term loans, touring exhibitions, digital reproduction, and franchised satellite branches extend their reach without requiring them to surrender custody of anything.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, metropolitan_encyclopedic_museums, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, metropolitan_encyclopedic_museums, beneficiary).

% Administer the statutes and case law that decide return disputes: limitation periods that bar stale claims, good-faith-purchase protections, and anti-seizure immunity for loaned objects. Apply these doctrines to whichever claims arrive, and each interpretation hardens into precedent that subsequent claimants must confront.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, holding_state_legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Govern the territories from which objects were removed and claim continuity with the dispossessed polities. Pursue return through bilateral negotiation, UNESCO mediation, and occasional litigation, absorbing legal costs, diplomatic friction, and domestic political pressure with each campaign. Leverage is limited: the objects sit abroad, the applicable law favors current possessors, and escalation risks retaliation in cultural-diplomacy channels. Several such states also hold minority heritages within their own borders under the same retention logic, a mirror position they rarely acknowledge.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, colonial_successor_states, payer,
    organized, generational, constrained, national).

% Descendant and custodial communities whose ceremonial, funerary, or communal objects sit in distant storerooms and galleries. Their connection to the objects is bound up with communal identity and religious practice; the separation persists regardless of which government negotiates on their behalf. Their practical routes run through successor-state delegations or museum outreach programs they did not design, and abandoning the connection is not a live option for them.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, source_communities_of_removed_artifacts, payer,
    powerless, generational, identity_locked, regional).

% Visit encyclopedic collections concentrated in a handful of metropoles and consume the exhibitions, catalogs, and digital surrogates those institutions produce. They gain access to world-spanning collections at low personal cost; their travel patterns concentrate in wealthy countries, so the access they enjoy is unevenly replicated for publics nearer the objects' origins.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, cosmopolitan_museum_visitors, beneficiary,
    moderate, biographical, mobile, global).

% Study the consolidated collections: comparative research, provenance work, and conservation science all benefit from objects gathered into single institutions with deep research infrastructure. Careers, publications, and curatorial authority attach to access that these institutions control and ration.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, global_scholarship_community, beneficiary,
    organized, biographical, mobile, global).

% Mediates return disputes through a standing intergovernmental committee with recommendatory power only. Convenes the parties, funds feasibility studies, and records disagreements; it cannot compel return or bind national courts, and its docket is dominated by disputes the holding institutions decline to settle bilaterally.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, unesco_intergovernmental_committee, observer,
    institutional, generational, analytical, global).

% Communities displaced by the same colonial processes that moved the objects, now living outside both the holding states and the successor states negotiating for return. Their testimony about the objects' meaning rarely enters the diplomatic record; they would contest the framing that custody questions are purely state-to-state matters, but no procedural channel exists for them to do so.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, diaspora_and_descendant_communities, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, metropolitan_encyclopedic_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates dispersed and fragile objects in professionally staffed, climate-controlled institutions; provides single-site comparative access for scholars and publics; standardizes conservation, documentation, and display practice across national boundaries.
% TRANSFER_FUNCTION: Moves custody, admission revenue, tourism spillover, scholarly prestige, and narrative authority from claimant states and source communities to holding institutions in former colonial metropoles; moves legal risk, negotiation costs, and identity harm back onto the claimants pursuing return.
% ABSENT_VOICES: Source communities and colonial-displacement diasporas are represented only indirectly, if at all: their claims are filtered through successor-state diplomacy or dismissed as particularist sentiment. Present at the table, they would reject the premise that custody by distant institutions is neutral stewardship, and would insist that communal and sacred objects are not fungible with conserved commodities.
% DISAPPEARANCE_RATIONALE: If the universal-heritage authority structure vanished overnight, several thousand canonical holdings would become newly contestable, museum acquisition and lending would reorganize around consent-based frameworks, the limitation-period and good-faith doctrines anchoring possession would lose their interpretive rationale, and source nations would recover a default presumption of possession that currently requires expensive campaigns to overcome.
% FOUNDING_PROBLEM: Artifacts looted or exported during colonial expansion and European wars lacked protection; fragile objects decayed in situ; nationalist closure threatened comparative scholarship. The encyclopedic museum and its legal corpus were built to solve preservation and cross-border access at once.
% FOUNDING_PROBLEM_CORROBORATION: Conservation scientists and UNESCO attest that live preservation risk remains for conflict-zone and fragile-material categories, supporting a residual coordination function. Post-colonial historiography, claimant-state diplomatic records, and ICOM's successive ethics revisions attest that for stable, well-documented canonical holdings the founding problem is substantially solved and the arrangement now functions as retention of custody and prestige. Attestation on both sides comes from outside the benefiting parties; nothing rests on the museums' self-description alone.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74 at interval end) because the arrangement's gains — custody, admission revenue, tourism spillover, scholarly capital — accrue to holding institutions while its costs fall on claimants, and because the commission-like margin between the doctrine's universalist promise and its delivered access is wide. Suppression (0.68) is structural: limitation periods extinguish stale claims, anti-seizure statutes protect circulating loans, and the doctrinal framing of repatriation as particularist raises the reputational price of claiming. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation. Theater (0.43) is moderate and rising: conservation and documentation are real functions, but a growing share of 'universal access' activity is rhetorical, as visitor demographics concentrate in wealthy countries and digital surrogates decouple access from physical custody. Accessibility collapse (0.55) is mid-range: within the corpus's own frame, alternative authority structures are foreclosed, but the 1970 UNESCO Convention's prospective-only regime and parallel ethical frameworks keep alternatives partly alive. Resistance (0.62) is substantial and sustained — Greek, Nigerian, Egyptian, Ethiopian, and Cambodian campaigns, plus Indigenous-led claims, meet the arrangement continuously. All three temporal series run on one shared grid (t=0..70, decade steps) so every metric is authored at every examined point; trajectories are monotonic, reflecting ratcheting enforcement and accumulating claim pressure rather than oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the museum seat, the arrangement is a civilizational service it built and maintains: consolidation, conservation, comparative access — a coordination-heavy profile with modest felt burden. From the claimant-state seat, the same structure operates as enforced retention: every route to return runs through machinery the holder controls, and the doctrine that justifies retention simultaneously delegitimizes the claimant. From the source-community seat, the structure appears as permanent separation administered by parties who negotiate over the community's heritage without the community present. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Metropolitan museums sit near the beneficiary pole (d near 0.0): the arrangement subsidizes them directly and their arbitrage-grade options mean they bear almost none of its costs. Visitors and the scholarly community hold low d as indirect beneficiaries — genuine access and research gains, diffuse costs. Colonial successor states sit near the target pole (high d): declared victims with constrained exit, they absorb the transfer's costs while collecting little. Source communities sit nearest the full-target end: declared victims whose identity_locked relationship to the objects removes even the option of walking away, which amplifies their effective extraction beyond the mobile claimant states'. The UNESCO committee is analytical and collects nothing. No directionality overrides are needed: the beneficiary/victim declarations plus exit options already place every seat correctly, and the successor states' mirror-position benefit (some hold minority heritages domestically under the same logic) is noted here rather than forced through a power-atom-keyed override that would misplace other agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-war chaos, in-situ decay, nationalist closure — is genuinely dead for the stable, well-documented canonical holdings at the center of contemporary disputes, and genuinely live for conflict-zone and fragile-material categories. The arrangement persists across both cases under a single doctrine, which is the classic mandatrophy shape: a mandate outliving its function for the flagship instances while borrowing urgency from the residual ones. Classifying this as tangled_rope rather than snare preserves the distinction the corpus needs: the conservation and access functions are real enough that abolition would impose genuine losses on scholarship and on at-risk objects, so pure-extraction treatment would mislabel the coordination core; treating it as pure rope would erase the asymmetric transfer that claimant states demonstrably bear. The R5 mismatch consumer should watch this story: founding_problem_status is contested and disappearance_verdict is world_rearranges, so no zombie flag fires yet — but if the live preservation cases continue shrinking while retention hardens, the status should flip to dead and the flag becomes the expected signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the cultural_property_legal_corpus kernel: what changes structurally if a sibling reading is adopted instead?',
    'Adoption of the sovereign_repatriation_reading would convert holding institutions'' custody into the contested act itself, shifting beneficiaries to successor states and pushing this arrangement''s profile toward pure extraction; adoption of the indigenous_stewardship_reading would relocate authority below the state level, invalidating both the museums'' and the successor states'' claims simultaneously.',
    'The epsilon referent (the standing retention arrangement) is fixed, but the beneficiary/victim sets and per-seat classifications computed from them would invert or fragment under sibling readings; classification of the same physical corpus could range from rope-like coordination to snare depending on which reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame uncertainty: which reading of the cultural-property kernel governs, and how the choice restructures beneficiaries, victims, and classification.').

omega_variable(
    preservation_necessity_ambiguity,
    'For which artifact classes does centralized metropolitan custody remain preservation-necessary (conflict-zone material, fragile organics, unstable polymers) versus merely retention-convenient (stable stone, metal, and ceramic canonical holdings)?',
    'Object-class conservation-risk assessment combined with source-nation facility track records: compare decay rates and loss incidents for comparable classes held in situ versus held abroad.',
    'If most contested canonical holdings are preservation-safe in source nations, the genuine coordination core shrinks and excess extraction rises, pushing the computed classification toward snare; if large classes remain genuinely at-risk, the rope component strengthens and the arrangement''s coordination floor is defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_necessity_ambiguity, empirical, 'Whether the preservation function justifying retention is broad or narrow across the contested corpus.').

omega_variable(
    universal_access_distribution,
    'How widely is the promised universal access actually distributed — do physical visitation and digital access reach source-nation publics in anything like the proportion enjoyed by wealthy-country publics?',
    'Visitor-origin statistics from holding institutions, regional uptake analytics for open-access digital collections, and cost-of-access studies for source-country scholars and publics.',
    'If access concentrates heavily in wealthy countries, the theater_ratio is understated and the coordination story weakens further, raising effective extraction for the excluded publics; if digital access genuinely equalizes, part of the measured theater is transitional rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_access_distribution, empirical, 'Whether the universal-access promise is delivered or performed.').

omega_variable(
    suppression_structural_or_normative,
    'Is the limited success of claimant movements attributable to structural barriers (limitation periods, seizure immunity, funding asymmetry) or to internalized cosmopolitan norms among claimant-state elites who accept the universalist frame?',
    'Compare claim outcomes across states whose diplomatic elites embrace versus contest the universalist framing, controlling for legal exposure; track whether claim intensity rises when domestic political coalitions replace elite negotiators.',
    'If suppression is substantially internalized, removing the structural barriers alone would not release claim pressure, and the arrangement''s persistence is more robust than its legal machinery suggests; if structural, doctrinal reform would rapidly change the extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_normative, empirical, 'Structural versus internalized component of the arrangement''s suppressive force on claimants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(cult_tr_t0, observed).
narrative_ontology:measurement(cult_tr_t10, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(cult_tr_t10, observed).
narrative_ontology:measurement(cult_tr_t20, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(cult_tr_t20, observed).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(cult_tr_t30, observed).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(cult_tr_t40, observed).
narrative_ontology:measurement(cult_tr_t50, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(cult_tr_t50, observed).
narrative_ontology:measurement(cult_tr_t60, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(cult_tr_t60, observed).
narrative_ontology:measurement(cult_tr_t70, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 70, 0.43).
narrative_ontology:measurement_basis(cult_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cult_be_t0, observed).
narrative_ontology:measurement(cult_be_t10, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(cult_be_t10, observed).
narrative_ontology:measurement(cult_be_t20, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(cult_be_t20, observed).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(cult_be_t30, observed).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(cult_be_t40, observed).
narrative_ontology:measurement(cult_be_t50, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement_basis(cult_be_t50, observed).
narrative_ontology:measurement(cult_be_t60, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement_basis(cult_be_t60, observed).
narrative_ontology:measurement(cult_be_t70, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 70, 0.74).
narrative_ontology:measurement_basis(cult_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(cult_su_t0, observed).
narrative_ontology:measurement(cult_su_t10, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(cult_su_t10, observed).
narrative_ontology:measurement(cult_su_t20, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(cult_su_t20, observed).
narrative_ontology:measurement(cult_su_t30, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement_basis(cult_su_t30, observed).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(cult_su_t40, observed).
narrative_ontology:measurement(cult_su_t50, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement_basis(cult_su_t50, observed).
narrative_ontology:measurement(cult_su_t60, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(cult_su_t60, observed).
narrative_ontology:measurement(cult_su_t70, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 70, 0.7).
narrative_ontology:measurement_basis(cult_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who owns culture?' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints corresponding to the three readings of the cultural_property_legal_corpus kernel. This file instantiates the universal_heritage_reading (high epsilon extracted from claimant states; holding institutions as beneficiaries). The sovereign_repatriation_reading inverts the beneficiary/victim structure around the same physical corpus, and the indigenous_stewardship_reading relocates authority below the state level entirely. The universal reading is historically upstream: it controlled the corpus and its institutions, and its doctrines (non-retroactivity, good-faith acquisition) are cited as evidence against the sibling readings' claims. Each story links the other two through affects_constraints; no story hedges across readings internally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
