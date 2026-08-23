% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [TERMINATED AT INTERVAL END — MEIJI SHINBUTSU BUNRI (1868-1871)]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Domain Partition (Partition Reading): Kami Hold Life and Purity, Buddhas Hold Death and Salvation, Without Required Ontological Unification
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This story instantiates the domain-partition reading of the shinbutsu
 *   coexistence kernel: for roughly a millennium (interval unit ~= 10.8
 *   years; t=0 ~= 900 CE, t=90 ~= 1870 CE), kami cults and Buddhist
 *   institutions operated as parallel jurisdictions — life, purity,
 *   fertility, and harvest to shrines; death, salvation, and ancestors to
 *   temples — with the boundary between them maintained by practice rather
 *   than resolved by theology. The arrangement delivered real dual-access
 *   coordination, and it carried real asymmetric costs: the death-domain
 *   monopoly was eventually monetized through compulsory parish registration
 *   (the Edo danka and terauke system), purity taboos excluded women from
 *   sacred space, and villages financed both sides. The claim/metric gap is
 *   deliberate: the reading is CLAIMED as tangled_rope on structural grounds
 *   (working coordination plus identifiable payers plus active enforcement),
 *   while the metrics are authored descriptively from the historical
 *   operation — the engine computes per-seat classifications and measures the
 *   divergence. KEY AGENTS (by structural relationship): -
 *   buddhist_temple_establishments: agenda-setter for the death/salvation
 *   domain (institutional/arbitrage) — administers parishes, collects funeral
 *   and memorial fees - shinto_shrine_networks: agenda-setter for the
 *   life/purity domain (institutional/arbitrage) — runs the festival
 *   calendar, holds purity rules - danka_households: primary payer
 *   (powerless/trapped) — compulsory parishioners bearing scheduled
 *   death-service fees - rural_cultivator_villages: collective payer with
 *   partial beneficiary position (organized/constrained) — funds festivals
 *   and absorbs levies - court_and_warrior_elites: patron-beneficiary
 *   (powerful/arbitrage) — buys legitimation, sets registration rules -
 *   women_excluded_from_sacred_sites: payer via exclusion (powerless/trapped)
 *   — bears access denial and added ritual costs - mountain_ascetic_brokers:
 *   dual-positioned intermediary (moderate/mobile) — lives off the boundary's
 *   existence - meiji_reform_bureaucrats: analytical observer
 *   (institutional/analytical) — surveys the system and terminates the
 *   interval
 *
 * KEY AGENTS:
 *   - buddhist_temple_establishments: agenda-setter and principal receipt seat (institutional/arbitrage) — administers the death domain, keeps parish registries, collects funeral and memorial payments, defends jurisdiction
 *   - shinto_shrine_networks: agenda-setter for the life/purity domain (institutional/arbitrage) — receives festival dues, maintains purity boundaries, pivoted fast when the arrangement collapsed
 *   - danka_households: primary payer seat (powerless/trapped) — compulsory registration binds them to a fee schedule attached to unavoidable life events
 *   - rural_cultivator_villages: organized collective payer with secondary beneficiary position (organized/constrained) — negotiates fees, funds the calendar the arrangement coordinates
 *   - court_and_warrior_elites: patron-beneficiary (powerful/arbitrage) — legitimation services flow up, endowments flow down, rules flow from their edicts
 *   - women_excluded_from_sacred_sites: payer through exclusion (powerless/trapped) — purity taboos deny access; funerary doctrine assigns extra ceremonial costs
 *   - mountain_ascetic_brokers: dual-positioned broker (moderate/mobile) — earns from mediating between the two domains, pays levies upward
 *   - meiji_reform_bureaucrats: observer seat (institutional/analytical) — catalogs the arrangement's functions and replaces it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.65).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.62).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Domain Partition (Partition Reading): Kami Hold Life and Purity, Buddhas Hold Death and Salvation, Without Required Ontological Unification").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '7ee42037-b8c1-473b-9420-27a097fd5633').
narrative_ontology:cs_kernel_codification('7ee42037-b8c1-473b-9420-27a097fd5633', distributed).
narrative_ontology:cs_authority_grounding('7ee42037-b8c1-473b-9420-27a097fd5633', practice).
narrative_ontology:cs_interpretation_layer_present('7ee42037-b8c1-473b-9420-27a097fd5633').
narrative_ontology:cs_reading_relation('7ee42037-b8c1-473b-9420-27a097fd5633', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ee42037-b8c1-473b-9420-27a097fd5633', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('7ee42037-b8c1-473b-9420-27a097fd5633', foundational, existential_domains_need_no_unification).
narrative_ontology:cs_axiom_status(existential_domains_need_no_unification, holdable).
narrative_ontology:cs_axiom_grounding('7ee42037-b8c1-473b-9420-27a097fd5633', existential_domains_need_no_unification, conventional).
narrative_ontology:cs_axiom('7ee42037-b8c1-473b-9420-27a097fd5633', foundational, popular_practice_is_operative_authority).
narrative_ontology:cs_axiom_status(popular_practice_is_operative_authority, holdable).
narrative_ontology:cs_axiom_grounding('7ee42037-b8c1-473b-9420-27a097fd5633', popular_practice_is_operative_authority, conventional).
narrative_ontology:cs_reference_frame('7ee42037-b8c1-473b-9420-27a097fd5633', functional_domain_dualism).
narrative_ontology:cs_drift_state('7ee42037-b8c1-473b-9420-27a097fd5633', meiji_shinbutsu_bunri_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7ee42037-b8c1-473b-9420-27a097fd5633', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrine_networks).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, court_and_warrior_elites).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, danka_households).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, rural_cultivator_villages).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, women_excluded_from_sacred_sites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, danka_households).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, rural_cultivator_villages).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, mountain_ascetic_brokers).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, mountain_ascetic_brokers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the death-and-salvation side of the arrangement: conducting funerals, memorial services, and ancestral rites, keeping parish registries, and issuing the yearly certification that civil administration requires. Affiliated households owe funeral fees, memorial-offering payments, and upkeep contributions, which temples collect directly, and temple councils defend their jurisdiction over the death domain against encroachment. Patronage ties to court nobles, warrior houses, and villages give major temples room to reposition — shifting patrons, founding branches, emphasizing different services — though village temples bound to a fixed parish have far less room to move.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_establishments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_establishments, beneficiary).

% Administer the life side: purification rites, fertility and harvest festivals, boundary consecration, and the calendrical round of village observances. They receive festival offerings, parishioner dues, and dedicated land income, and they maintain purity rules governing who may approach the sacred precincts. Their claim to the life domain is locally rooted and renewed annually through festival performance. When the wider arrangement came apart, many shrine houses recast themselves as purely Shinto institutions almost overnight.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrine_networks, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_shrine_networks, beneficiary).

% Register with a designated parish temple, buy funeral and graveside services from it, and pay scheduled memorial-offering fees across the year. The yearly certification their temple issues is required for tax and census documents, so remaining outside a parish is not a realistic option. In exchange they receive mortuary care, ancestral continuity, and a place in the village ritual order. Households that fall behind on payments risk losing burial rights and drawing official suspicion of hidden Christianity.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, danka_households, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, danka_households, beneficiary).

% Fund and staff the festival calendar, contribute corvee labor for shrine and temple construction, and absorb special levies when establishments campaign for rebuilding or when armed monastic delegations arrive demanding donations. Village councils negotiate fee amounts collectively, petition overlords, and occasionally riot. The festivals they pay for organize the agricultural year, mark communal boundaries, and provide the few large public gatherings the village has.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, rural_cultivator_villages, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, rural_cultivator_villages, beneficiary).

% Patronize both sides — endowing temples, restoring shrines — and receive ritual legitimation, curse-counteraction services, and ceremony marking rank. Their patronage decides which establishments flourish, and their edicts set registration and certification rules. When political conditions shift they redirect endowments toward whichever institutions serve the new order.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, court_and_warrior_elites, beneficiary,
    powerful, generational, arbitrage, national).

% Barred by purity taboos from many mountain sanctuaries and inner precincts, and addressed by funerary teaching that casts female embodiment as an obstacle requiring additional rites to overcome. They attend services, sponsor offerings through household budgets they often manage, and carry the extra ceremonial costs the doctrine assigns them, but they hold no office in either establishment and cannot step outside the category the taboos attach to.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, women_excluded_from_sacred_sites, payer,
    powerless, biographical, trapped, national).

% Itinerant ascetic orders working both sides of the boundary, channeling kami-cult power through Buddhist frameworks in healing, divination, and mountain rites. Their brokerage depends on the two systems remaining distinct enough to mediate between; they take fees from clients on both sides and pass levies upward to the parent establishments above them. When the boundary is policed hard, they lose the middle position that defines them.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, mountain_ascetic_brokers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, mountain_ascetic_brokers, payer).

% Survey the dual system from the 1860s onward from outside its administration, catalog its registration and record-keeping functions, and design replacements built on state-run shrine administration and civil registries. They take testimony from shrine houses, temple leaders, and nativist scholars, and their determination that the coexistence arrangement must be physically separated closes the interval.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_reform_bureaucrats, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temple_establishments).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitions existential concerns across two specialist institutions: purity, fertility, harvest, and the agricultural calendar route to kami cults; death, salvation, and ancestral continuity route to Buddhist temples. Communities obtain full life-course ritual coverage without any single institution mastering everything, and without requiring the two systems' doctrines to agree with each other.
% TRANSFER_FUNCTION: Moves offerings, scheduled fees, corvee labor, and ritual deference from lay households and villages to temple and shrine establishments: funeral and memorial payments and certification-dependent dues flow to temples, festival dues and construction levies flow to shrines, and legitimation flows upward to the elites whose patronage sustains both.
% ABSENT_VOICES: Women barred from sacred precincts held no seat in the councils that set boundary and purity rules. Hidden Christians, forced into parish registration, appear in the record only as concealment. Village delegates reached the negotiating table reactively, after levy amounts were already fixed. Nativist (kokugaku) scholars who rejected the whole arrangement operated entirely outside the temple-shrine administration whose boundaries they attacked.
% DISAPPEARANCE_RATIONALE: When the arrangement was dismantled (shinbutsu bunri and the accompanying haibutsu kishaku wave, 1868-1871), thousands of temples were destroyed or stripped, funeral and ancestral practice was disrupted for a generation, the registration and census functions embedded in parish certification lapsed until civil substitutes were built, and the entire shrine world was reconstituted under state control. Mortuary practice, festival finance, and local record-keeping all had to be rebuilt around new institutions.
% FOUNDING_PROBLEM: A continental religion carrying a complete death-and-salvation apparatus settled alongside an entrenched indigenous cult handling purity, fertility, and the agrarian year. Communities needed to use both systems without doctrinal contradiction, without institutional warfare between them, and without forcing households to choose one and forfeit the other.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from seats outside the benefiting parties: ritsuryo law codes and bakufu registration edicts attest the routing-and-control problem from the administrative seat; village complaint records and fee-dispute petitions attest the burden side; and the survival of domain-split lifecycle practice (Shinto-associated weddings, Buddhist funerals) long after the arrangement's legal destruction attests that the functional routing was real rather than institutional self-description. What lacks outside corroboration is any claim that the arrangement was doctrinally necessary — necessity attestation comes only from the establishments themselves.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from diffuse estate-era burdens (0.34) to the mature Edo parish economy (0.70): the death-domain monopoly, once backed by compulsory certification, converted unavoidable mortality into a scheduled revenue stream — the clearest rent channel in the arrangement. Suppression requirement is authored as a series deliberately, because the enforcement story is the dynamic: enforcement capacity built slowly through the medieval period (armed monastic donation campaigns, warlord-installed registers) and hardened sharply in the Edo settlement (0.44 to 0.74) when parish certification fused with anti-Christian surveillance; a flat series would falsify that ratchet. Theater ratio rises as certification becomes paperwork and prescribed memorials become rote (0.10 to 0.46) while the underlying festival and funeral functions stay real — performative share grows, functional core persists. All three series share one grid {0,15,30,45,60,75,90}; no metric is sampled off-grid. Accessibility_collapse (0.60) reflects the mature phase: leaving a parish meant losing burial rights and courting sedition suspicion, though hidden practice persisted and pre-Edo alternatives were livable. Resistance (0.50) is real but fragmented — village fee disputes, petition campaigns, occasional riots, covert Christian networks — never a sustained frontal challenge until the Meiji state arrived from outside. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently, and the structural data is what lets the engine compute that divergence. From the establishment seats the arrangement is coordination they themselves administer: they wrote the boundary rules, deliver the services, and experience the fee schedule as the price of maintaining a millennium-old dual-access system. From the danka-household seat the identical structure operates as a compulsory payment schedule welded to death — an event no household can decline — with exit legally foreclosed; identity-lock compounds the trap, since the household's ancestral line is relationally fused to its parish temple (the bodaiji bond), making exit unthinkable short of severing the ancestors. The women's seat carries a further lock: funerary doctrine did not merely bar access, it supplied the categories through which women understood their own standing, an internalized layer on top of the structural taboo. The elite seat experiences neither cost nor coordination burden — it purchases legitimation and moves patronage freely, the nearest-to-beneficiary experience in the system. Same-level divergence matters too: temple and shrine establishments hold nominally equal institutional power, but the death domain's asset specificity (parish registries, certification rights) made village temples far more exit-constrained than shrine houses, whose Meiji-era pivot to pure Shinto identity demonstrated arbitrage the parish-bound temples lacked.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The two establishment seats (agenda_setter + beneficiary, arbitrage exit) derive near the beneficiary pole — the arrangement subsidizes their institutional reproduction and they control its rules. Court and warrior elites (pure beneficiary, arbitrage) sit nearest the subsidy end: they pay nothing the arrangement forces and receive legitimation on demand. Mountain ascetic brokers (dual-positioned, mobile) derive mid-low: the boundary's existence is their livelihood. Rural cultivator villages (payer + beneficiary, organized, constrained) derive near symmetric — they fund the calendar and receive its coordination. Danka households (payer, trapped, powerless) derive near the full-target end: compulsory affiliation plus no exit amplifies their effective burden toward the maximum. Women excluded from sacred sites (payer, trapped) sit at or nearest the full-target pole — they bear costs through denial and added obligation with no positional relief anywhere in the system. Spatial scope is national for most seats, which modestly amplifies effective extraction by making verification of abuses harder; the villages' regional scope moderates theirs slightly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both characteristic errors. Read as pure rope — the pluralist-celebration error, common in textbook accounts of Japanese religiosity — the arrangement's victims disappear: the death-monopoly fee schedule fell on households with no legal exit, and that asymmetry is structural, not incidental. Read as pure snare — the anticlerical error — the millennium of working dual-access coordination vanishes, along with the reason removal was catastrophic: fixing_cost is prohibitive because the arrangement embedded mortuary care, festival finance, and census infrastructure in one fabric; the Meiji demolition (haibutsu kishaku) is the empirical demonstration of what removal cost. The R5 interview locates the arrangement correctly: the founding routing problem is contested rather than dead (attenuated domain-splitting persists in modern practice), so the dead-mandate-plus-rearrangement capture flag does not fire; the arrangement ended by external state action, not internal atrophy — it did not decay into piton, it was executed while still functional, which is precisely why the interval ends at the execution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation_ambiguity,
    'Is the domain-partition reading the correct instantiation of the shinbutsu coexistence kernel, or do the syncretic-fusion reading (ontological unity via honji suijaku) or the incoherent-bundle reading (deliberately maintained ambiguity) describe the actual standing arrangement?',
    'Compare practice-layer records (parish registers, festival ledgers, funeral contracts) against elite doctrinal texts across the same period: if routing consistently followed existential domains regardless of professed ontology, the partition reading holds; if lived religion tracked the unity doctrine, the fusion reading does; if routing was erratic and boundary disputes endemic without functional pattern, the bundle reading gains.',
    'Classification follows the resolution: partition yields the parallel-jurisdiction tangled_rope authored here; fusion relocates extraction to the doctrinal apparatus with a different beneficiary structure; bundle shifts the constraint toward a snare whose product is managed ambiguity itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation_ambiguity, conceptual, 'Which of the three readings correctly instantiates the shinbutsu coexistence kernel — the framing under-determination inherent in a distributed kernel.').

omega_variable(
    danka_compulsion_separability,
    'Was compulsory parish registration and its fee schedule intrinsic to the domain partition itself, or a separate anti-Christian security apparatus that rode on the partition''s death-domain infrastructure?',
    'Compare registration intensity across regions and timelines against anti-Christian policy milestones rather than partition milestones: if compulsion tracks the kirishitan suppression campaigns and the terauke edicts, it is separable; if fee schedules and parish binding predate and persist independently of security policy, it is intrinsic.',
    'If separable, the partition proper''s extractiveness drops materially and the arrangement trends toward rope with an appended enforcement parasite; if intrinsic, the tangled_rope classification stands with higher effective extraction on the trapped payer seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(danka_compulsion_separability, empirical, 'Whether the Edo-era compulsion layer is part of this constraint or a distinct constraint sharing its infrastructure.').

omega_variable(
    popular_versus_elite_authority_location,
    'Did the partition''s operative authority actually sit with popular practice (as this reading claims), or with elite boundary-maintenance decisions that popular practice merely complied with?',
    'Trace who adjudicated actual boundary disputes — shrine-temple jurisdiction conflicts, purity-rule challenges, festival-rights contests — and whether outcomes followed precedent from below or rulings from above.',
    'If authority sat below, enforcement weight in per-seat computations falls and the coordination function strengthens toward rope; if above, the popular-practice axiom weakens and the establishment seats'' directionalities dominate the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_versus_elite_authority_location, conceptual, 'Where the operative authority of the partition actually resided — practice layer or elite layer.').

omega_variable(
    gender_exclusion_internalization,
    'Was the suppression borne by women excluded from sacred sites structural (formal purity barriers, doctrinally assigned costs), internalized (self-understanding shaped by funerary doctrine about female embodiment), or both?',
    'Post-removal trajectory: the formal nyonin kekkai barriers were abolished in 1872; if avoidance norms, self-exclusion, and the demand for the extra corrective rites persisted among women after the barriers fell, a substantial internalized component is established.',
    'If substantially internalized, the women''s seat''s effective suppression exceeds what the structural measure suggests and travels with them past institutional exit; if predominantly structural, removing the barriers removes the burden and the seat''s directionality relaxes accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_exclusion_internalization, empirical, 'Structural versus internalized composition of the suppression on the excluded-women seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_partition_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(shinbutsu_partition_tr_t15, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(shinbutsu_partition_tr_t30, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(shinbutsu_partition_tr_t45, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 45, 0.22).
narrative_ontology:measurement(shinbutsu_partition_tr_t60, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(shinbutsu_partition_tr_t75, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement(shinbutsu_partition_tr_t90, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 90, 0.46).

% Extraction over time
narrative_ontology:measurement(shinbutsu_partition_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(shinbutsu_partition_be_t15, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(shinbutsu_partition_be_t30, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(shinbutsu_partition_be_t45, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 45, 0.47).
narrative_ontology:measurement(shinbutsu_partition_be_t60, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(shinbutsu_partition_be_t75, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(shinbutsu_partition_be_t90, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 90, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_partition_su_t0, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(shinbutsu_partition_su_t15, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 15, 0.24).
narrative_ontology:measurement(shinbutsu_partition_su_t30, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 30, 0.33).
narrative_ontology:measurement(shinbutsu_partition_su_t45, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 45, 0.36).
narrative_ontology:measurement(shinbutsu_partition_su_t60, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(shinbutsu_partition_su_t75, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 75, 0.74).
narrative_ontology:measurement(shinbutsu_partition_su_t90, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 90, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'shinbutsu shugo' (kami-buddha coexistence) covers at least three structurally distinct claims, each authored as its own story with its own epsilon and beneficiary/victim surface. THIS story authors the practice-level domain partition (epsilon ~0.65 for the standing arrangement with danka-era extraction channels, referent: the premodern dual-jurisdiction arrangement as the partition reading sees it). The syncretic_fusion_reading authors the ontological-unification claim — its referent is the doctrinal apparatus itself, with different extraction dynamics (careers, institutional authority, and interpretive offices ride on the unity doctrine). The incoherent_bundle_reading authors the meta-claim that no coherent kernel existed — its referent is the ambiguity-maintenance machinery. Upstream-downstream structure: elite fusion doctrine was cited as warrant for the partition's boundary settlements, giving the fusion story upstream influence on this one; this story's demonstration of stable functional partition creates downstream pressure on the bundle story's never-coherent premise without foreclosing it. All three files link one another through affects_constraints; orphan stories in this family would be a decomposition failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
