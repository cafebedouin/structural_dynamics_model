% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Settler-Colonial Reading: Zionist Sovereignty as Displacement Regime
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the settler-colonial reading of the contested
 *   kernel 'Jewish sovereignty in Palestine.' In this reading the standing
 *   arrangement under assessment is the Zionist settlement-and-sovereignty
 *   project as it actually operated: a European-initiated colonization of an
 *   inhabited territory, chartered by an imperial power, that transferred
 *   land, sovereignty, and demographic majority from the Palestinian
 *   population to the incoming Jewish collective — first under British
 *   auspices, then under American patronage. The reading's distinguishing
 *   move is structural: the character of the arrangement is read off
 *   land-transfer mechanics, legal instruments, and population movements
 *   rather than off the intentions, desperation, or rights-claims of the
 *   people who carried it out — hence 'regardless of intent,' a clause that
 *   places Holocaust survivors and refugees from Arab states inside the
 *   settler category without exemption. Palestinians enter as the primary
 *   victims; the metropole enters as beneficiary. Four sibling readings of
 *   the same kernel — liberal-nationalist, religious-Zionist,
 *   cultural-Zionist, post-Zionist — are separate constraint stories with
 *   their own epsilon values and victim sets; they are linked through the
 *   network, not averaged here. KEY AGENTS (by structural relationship): -
 *   israeli_state_apparatus: Agenda-setter (institutional/identity_locked) —
 *   administers land custody, residency tiers, borders, and settlement policy
 *   - jewish_immigrant_settlers: Primary beneficiary
 *   (organized/identity_locked) — receives land, housing, citizenship
 *   preference, demographic consolidation - british_mandate_establishment:
 *   Historical beneficiary (institutional/mobile) — collected strategic
 *   positioning, exited in 1948 - us_geopolitical_establishment: Current
 *   patron-beneficiary (institutional/mobile) — collects allied positioning,
 *   funds and shields the arrangement - palestinian_refugee_diaspora: Primary
 *   target (powerless/trapped) — denied return, stateless across host states
 *   - palestinians_under_occupation: Primary target (moderate/trapped) — land
 *   requisition, movement restriction, blockade -
 *   palestinian_israeli_citizens: Target with marginal mobility
 *   (moderate/constrained) — expropriated land custody, tiered admissibility
 *   - un_general_assembly_majority: Excluded voice (organized/constrained) —
 *   objects annually, locked out of enforcement by veto structure -
 *   postcolonial_scholarship: Analytical observer (analytical/analytical) —
 *   documents the land-transfer record comparatively
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.86).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.88).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Settler-Colonial Reading: Zionist Sovereignty as Displacement Regime").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '12c6c2e7-07be-4607-9ee4-337006b560b4').
narrative_ontology:cs_kernel_codification('12c6c2e7-07be-4607-9ee4-337006b560b4', formalized).
narrative_ontology:cs_authority_grounding('12c6c2e7-07be-4607-9ee4-337006b560b4', extraction).
narrative_ontology:cs_interpretation_layer_present('12c6c2e7-07be-4607-9ee4-337006b560b4').
narrative_ontology:cs_reading_relation('12c6c2e7-07be-4607-9ee4-337006b560b4', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('12c6c2e7-07be-4607-9ee4-337006b560b4', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('12c6c2e7-07be-4607-9ee4-337006b560b4', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('12c6c2e7-07be-4607-9ee4-337006b560b4', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('12c6c2e7-07be-4607-9ee4-337006b560b4', foundational, structural_displacement_regardless_of_intent).
narrative_ontology:cs_axiom_status(structural_displacement_regardless_of_intent, holdable).
narrative_ontology:cs_axiom_grounding('12c6c2e7-07be-4607-9ee4-337006b560b4', structural_displacement_regardless_of_intent, empirically_contingent).
narrative_ontology:cs_axiom('12c6c2e7-07be-4607-9ee4-337006b560b4', foundational, zero_sum_territorial_transfer).
narrative_ontology:cs_axiom_status(zero_sum_territorial_transfer, holdable).
narrative_ontology:cs_axiom_grounding('12c6c2e7-07be-4607-9ee4-337006b560b4', zero_sum_territorial_transfer, empirically_contingent).
narrative_ontology:cs_axiom('12c6c2e7-07be-4607-9ee4-337006b560b4', secondary, indigenous_return_precedence).
narrative_ontology:cs_axiom_status(indigenous_return_precedence, holdable).
narrative_ontology:cs_axiom_grounding('12c6c2e7-07be-4607-9ee4-337006b560b4', indigenous_return_precedence, deontological).
narrative_ontology:cs_reference_frame('12c6c2e7-07be-4607-9ee4-337006b560b4', precolonial_indigenous_majority_order).
narrative_ontology:cs_drift_state('12c6c2e7-07be-4607-9ee4-337006b560b4', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('12c6c2e7-07be-4607-9ee4-337006b560b4', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrant_settlers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_establishment).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, us_geopolitical_establishment).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinians_under_occupation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_israeli_citizens).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, logic_of_elimination_thesis).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, demographic_sovereignty_exclusivity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territory and its population registry: issues land allocations, controls borders and residency rights, directs settlement planning, and commands the security forces that police who may live where. Its founding documents fuse its institutional purpose with the ongoing consolidation of Jewish national presence; abandoning that purpose would dissolve the institution's reason for being. Gains pass through it as custodian of expropriated land and as recipient of patron security funding.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, identity_locked, regional).

% The waves of Jewish immigrants from Europe, the Arab world, and elsewhere who took up land, housing, citizenship preference, and military protection under the arrangements the state administers. Many arrived as refugees from persecution with few other destinations; their position in this account is defined by where they settled and what they received rather than by the circumstances of arrival. Land titles, subsidized housing, and durable demographic majorities accrue to this collective; leaving would mean renouncing the national home constituted around them.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrant_settlers, beneficiary,
    organized, generational, identity_locked, regional).

% Issued the 1917 charter endorsing a national home and administered the territory from 1920 to 1948, collecting a strategic foothold on the routes to India and the oil regions while the arriving population bore the settlement costs. Withdrew in 1948 as insurgency costs mounted; its gain was positional and its exposure ended with departure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_establishment, beneficiary,
    institutional, generational, mobile, global).

% Succeeded Britain as principal patron from the late 1960s: annual military and economic assistance, diplomatic shielding in international bodies, and intelligence cooperation, in exchange for a dependable allied presence in the eastern Mediterranean. Patronage is discretionary and revisable; the establishment absorbs diplomatic costs in parts of the world and judges the strategic return worth it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, us_geopolitical_establishment, beneficiary,
    institutional, generational, mobile, global).

% Live under military administration in the West Bank and under blockade in Gaza: land requisitioned for settlements, movement restricted by permits and barriers, house demolitions, and detention without the civil protections citizens hold. Borders are controlled by a state they do not belong to; emigration is possible for some, return is not.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinians_under_occupation, payer,
    moderate, generational, trapped, regional).

% Descendants of those expelled or fled in 1948 and 1967, registered with a dedicated UN agency, holding citizenship in few or no states, concentrated in camps in Lebanon, Jordan, Syria, and beyond. The state that governs their former towns bars their return by law; host states vary between temporary tolerance and explicit exclusion; third-country resettlement offers are scarce.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, regional).

% Hold citizenship and vote, but their communities lost most of their land to state custody in the founding period, planning regimes restrict their municipal growth, and admissibility rules privilege Jewish immigration over family reunification. Individual emigration is open to those with resources; collective equality inside the polity is the contested terrain.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_israeli_citizens, payer,
    moderate, biographical, constrained, regional).

% Passes recurring resolutions affirming return and condemning settlement expansion year after year; enforcement requires the Security Council, where permanent-member vetoes convert the majority's voice into record without effect. Its members would condition aid, recognition, and trade differently if the procedural gates opened.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, un_general_assembly_majority, excluded,
    organized, generational, constrained, global).

% Comparative historians and theorists who document land-transfer records, population movements, and legal instruments, and place the case alongside other settlement colonies to test which structural features recur. Produces the archival basis on which competing accounts of the same events are argued.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, postcolonial_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrant_settlers).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the concentration of a geographically dispersed population onto a defined territory: pooled land acquisition, mass immigration transport and absorption, shared defense, and parallel institutions (schools, hospitals, unions) that solved collective-action problems no scattered diaspora community could solve alone. Stated without evaluation of what the coordination was for.
% TRANSFER_FUNCTION: Moves land title and custodianship from Palestinian holders to state and collective institutions that allocate to the incoming population; moves residency rights along a tiered ladder favoring Jewish immigration; moves security guarantees and diplomatic cover from the patrons to the state; moves strategic positioning to the patrons; moves labor, rents, and casualties asymmetrically onto the Palestinian side.
% ABSENT_VOICES: Palestinian voices were absent from every founding instrument: the Balfour Declaration was issued over the heads of the territory's majority; the partition recommendation allocated land to a state its inhabitants did not consent to; the refugees were excluded from the armistice and later negotiation tables, and final-status questions were deferred indefinitely in 1993. In the story's present, the General Assembly majority speaks annually and is procedurally locked out of enforcement.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would trigger immediate rearrangement: the refugee diaspora's return claims become enforceable, land custody reverts to contest, the residency-tier system loses its enforcer, patron strategic planning loses its fixed regional ally, and every neighboring state's posture recalibrates. Nothing about the region's current arrangement survives the removal unchanged.
% FOUNDING_PROBLEM: As this reading reconstructs it: late-imperial Britain wanted a reliable client presence astride the eastern Mediterranean approaches, and Europe wanted rid of its Jewish population problem; the Zionist movement supplied a mechanism that addressed both by moving a population onto someone else's land. The arrangement was built to solve the patrons' strategic problem and Europe's demographic-political problem — not the inhabitants', whose objection was recorded and overridden.
% FOUNDING_PROBLEM_CORROBORATION: No beneficiary attests this genealogy. Corroboration comes from outside the benefiting parties: British Foreign Office wartime correspondence treating Palestine as a strategic asset, UN documentation of the 1948 expulsions gathered by the Count Bernadotte mission and consolidated in UNRWA registration records, International Court advisory-opinion findings on the occupation's structure, and Palestinian oral-history archive projects. The claim that the founding problem is dead is corroborated by the documented transformation of postwar European Jewish migration pressures — the expulsion-force that drove the founding waves no longer operates as it did.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored high (0.86 at interval end) because the reading assesses the standing arrangement as a zero-sum territorial transfer: land, residency tiers, and demographic majority moved from one population to another and stayed moved. Suppression (0.88) exceeds extraction because the arrangement's persistence depends on continuously denying return, policing residency, and repressing resistance — the coercive load is the load-bearing wall. Theater ratio is authored low-to-moderate (0.24 at end) because the displacement work is real and continuous; the performative layer (negotiation processes maintained past their collapse, diplomatic framing, procedural compliance) rose through the Oslo era and thinned as overt enforcement resumed. Accessibility collapse 0.62: alternatives — return, equal citizenship, the original partition line — remain articulated in international law but are collapsed in fact by force and by veto; the collapse is partial, characteristic of constructs that must be defended rather than natural limits. Resistance 0.72 reflects a century of revolt, intifada, litigation, boycott campaigns, and scholarship. Coalition capacity among the powerless victims has been repeatedly attempted (pan-Arab mobilization, the PLO, boycott coordination) and met with suppression, fragmentation across host-state jurisdictions, and internal division — the trapped diaspora seat's powerlessness is structural, not voluntary. The measurement series share one grid (seven points spanning the interval); trajectories show step-changes at the 1948 and 1967 transitions rather than smooth drift, and the series is monotonic-intensifying rather than cyclical.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats — refugees barred from return, residents under occupation — the arrangement presents as enforced dispossession with no exit. From the settler-beneficiary seat the same structures present as national home-building, refuge, and defense. From the agenda-setter seat they present as statecraft and security necessity. From the patron seats they present as a stable allied investment with acceptable diplomatic overhead. The engine computes these per-seat classifications from the structural data; the divergence between the payer seats' computed type and the beneficiary seats' computed type is the finding, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations drive high directionality: the refugee diaspora (trapped, powerless) sits nearest the full-target end; occupied residents (trapped) nearly so; citizens inside (constrained — individual emigration open, collective equality closed) somewhat lower. Beneficiary declarations drive low directionality: the settler collective (identity_locked — exit means renouncing the national home constituted around them) is subsidized by the arrangement it composes; the two patron establishments (mobile) collect strategic return at minimal personal exposure. One override is authored: institutional seats derive d=0.12. The state apparatus is declared neither beneficiary nor victim, so its derived directionality would fall to a canonical fallback near symmetric — but structurally it collects the regime's gains as custodian of expropriated land and recipient of patron funding, so the override corrects it to the beneficiary side. The same override value is consistent for the two patron establishments, whose derived beneficiary-side d it merely pins. The General Assembly majority is left to derivation; its excluded seat makes its exact d uncertain, and that uncertainty is noted rather than forced with a second override.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is acute here and the classification guards against a specific mislabel. Because the founding configuration (imperial charter plus European expulsion-pressure) is dead, an inertial reading might classify the arrangement as degraded-but-persisting — vestigial, theatrically maintained. That would be wrong in this reading's terms: the arrangement is not running on fumes; its throughput (land transfer, settlement construction, residency denial) is at its historical maximum, funded and defended at scale. A dead founding problem combined with living, intensifying extraction is the signature of an arrangement that has outlived its cover story, not of one running on inertia. The classification equally prevents the reverse mislabel: reading the arrangement as pure coordination because it visibly builds institutions, absorbs immigrants, and maintains civic infrastructure. The reading holds those functions are welded to the displacement machinery — which is why the type-boundary omega is flagged low-confidence rather than resolved by assertion, and why the coordination-type declaration (resource_allocation) carries the default floor rather than an elevated one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (settler_colonial) of the contested kernel jewish_sovereignty_palestine; four sibling readings instantiate different constraints with different victim sets, beneficiary structures, and epsilon values. Which reading a seat adopts determines the entire classification surface.',
    'Cross-reading comparison across the linked family stories; per-seat adoption data showing which reading each constituency actually holds.',
    'If the liberal_nationalist reading is adopted instead, Palestinians exit the victim set, epsilon drops toward coordination cost, and the computed type shifts toward rope. The two stories are different constraints, not one constraint measured twice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one-of-five readings; classification is reading-indexed.').

omega_variable(
    intent_structural_independence,
    'Can the displacement-regime characterization hold independently of immigrant intent, given that a large share of arrivals were refugees from genocide and expulsion with no alternative destination?',
    'Comparative structural analysis separating arrival circumstances from land-transfer mechanics: if the dispossession pattern replicates across cohorts with radically different intents, the structural claim is supported; if outcomes track intent, it fails.',
    'If intent materially modulates the structure, the ''regardless of intent'' axiom weakens and the reading converges toward hybrid accounts acknowledging legitimate-refugee origins; if robust, the axiom stands and the liberal-nationalist legitimacy claim is foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_structural_independence, empirical, 'The reading''s distinctive axiom — structure over intent — tested against refugee-cohort variation.').

omega_variable(
    metropole_net_benefit,
    'Did the imperial patrons actually collect net benefit, or did Britain pay more in insurgency and administrative costs than it gained, and does the US establishment pay more diplomatically than it receives strategically?',
    'Archival cost accounting of Mandate expenditure against strategic value delivered; longitudinal analysis of US assistance outlays against basing, intelligence, and intervention returns.',
    'If patrons were net losers, the beneficiary declaration narrows to the settler collective alone and the colonial-metropole edge of the story weakens; if net winners, the metropole-beneficiary structure is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_net_benefit, empirical, 'Tests the ''beneficiary is colonial metropole'' declaration against cost accounting.').

omega_variable(
    elimination_logic_type_boundary,
    'Is the arrangement''s internal machinery purely eliminative-displacive, or does it contain coordination components (courts, universities, hospitals, civic infrastructure) whose loss would harm their users independently of the dispossession — the boundary between pure extraction and hybrid structure?',
    'Counterfactual decomposition: identify which operating components would survive a just-resolution transition intact versus which exist only to administer displacement (settlement administration, residency-tier law, absentee-custodian regimes).',
    'If substantial separable coordination exists, the computed type trends toward tangled_rope rather than snare; if the components are fusion-welded to the displacement machinery, snare stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elimination_logic_type_boundary, conceptual, 'Type-boundary question: pure extraction versus hybrid coordination-extraction.').

omega_variable(
    comparative_pattern_fit,
    'How closely does the case track canonical settler-colonial criteria (metropolitan initiation, land-centered elimination, labor exclusion, absence of repatriation), given divergences such as the absence of a continuing mother country and the refugees-among-settlers composition of the arriving population?',
    'Structured feature-by-feature scoring of the case against the criterion set of the comparative settler-colonial literature.',
    'Weak fit would support exceptionalist national-conflict framings and soften the reading''s foundational axiom; strong fit consolidates it and strengthens foreclosure of the liberal-nationalist sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_pattern_fit, empirical, 'Empirical fit of the case to the settler-colonial criterion set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 0, 108).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t18, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 18, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t18, observed).
narrative_ontology:measurement(jewi_tr_t36, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 36, 0.14).
narrative_ontology:measurement_basis(jewi_tr_t36, observed).
narrative_ontology:measurement(jewi_tr_t54, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 54, 0.17).
narrative_ontology:measurement_basis(jewi_tr_t54, observed).
narrative_ontology:measurement(jewi_tr_t72, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 72, 0.3).
narrative_ontology:measurement_basis(jewi_tr_t72, observed).
narrative_ontology:measurement(jewi_tr_t90, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 90, 0.33).
narrative_ontology:measurement_basis(jewi_tr_t90, observed).
narrative_ontology:measurement(jewi_tr_t108, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 108, 0.24).
narrative_ontology:measurement_basis(jewi_tr_t108, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t18, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement_basis(jewi_be_t18, observed).
narrative_ontology:measurement(jewi_be_t36, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 36, 0.78).
narrative_ontology:measurement_basis(jewi_be_t36, observed).
narrative_ontology:measurement(jewi_be_t54, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 54, 0.8).
narrative_ontology:measurement_basis(jewi_be_t54, observed).
narrative_ontology:measurement(jewi_be_t72, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 72, 0.82).
narrative_ontology:measurement_basis(jewi_be_t72, observed).
narrative_ontology:measurement(jewi_be_t90, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 90, 0.84).
narrative_ontology:measurement_basis(jewi_be_t90, observed).
narrative_ontology:measurement(jewi_be_t108, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 108, 0.86).
narrative_ontology:measurement_basis(jewi_be_t108, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t18, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(jewi_su_t18, observed).
narrative_ontology:measurement(jewi_su_t36, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement_basis(jewi_su_t36, observed).
narrative_ontology:measurement(jewi_su_t54, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 54, 0.74).
narrative_ontology:measurement_basis(jewi_su_t54, observed).
narrative_ontology:measurement(jewi_su_t72, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 72, 0.8).
narrative_ontology:measurement_basis(jewi_su_t72, observed).
narrative_ontology:measurement(jewi_su_t90, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 90, 0.82).
narrative_ontology:measurement_basis(jewi_su_t90, observed).
narrative_ontology:measurement(jewi_su_t108, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 108, 0.88).
narrative_ontology:measurement_basis(jewi_su_t108, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'Zionism / the Israel-Palestine question' decomposes, per the epsilon-invariance principle, into five structurally distinct claims — one per reading of the kernel. This file authors the settler-colonial reading alone: its epsilon (0.86) is assessed over the standing arrangement BY THIS READING'S OWN LIGHTS, with Palestinians as victims and the imperial metropole as beneficiary. The liberal-nationalist sibling authors a low epsilon over the same territory because its referent assessment differs (a right exercised, a refuge provided); the two stories are different constraints sharing a kernel, linked here so cross-reading pressure and contamination propagate through the network rather than being averaged inside one story. Upstream/downstream structure: this reading influences the post-Zionist sibling (its diagnosis shapes the post-Zionist remedy design) and forecloses the liberal-nationalist sibling within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__settler_colonial_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
