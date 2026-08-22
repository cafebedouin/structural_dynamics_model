% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical-Rights Overlay on UNCLOS Maritime Entitlements (Historical-Rights Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   unclos_sovereignty_boundary: the historical-rights reading, under which
 *   long-standing usage and occupation generate sovereign maritime rights
 *   that predate and override the convention's exclusive-economic-zone
 *   provisions. The kernel decomposes into three structurally distinct
 *   constraints, linked via network.affects_constraints: the
 *   strict_eez_reading (Article 57 exclusivity, no overlay claims valid —
 *   negligible extraction, near-coordination-of-ocean-governance profile),
 *   the non_ratifier_enforcement_reading (navigation freedom as custom
 *   enforced by naval presence — moderate extraction concentrated in the
 *   enforcing coalitions' operating budgets), and this reading, which carries
 *   the highest epsilon of the family: effective control over fisheries,
 *   hydrocarbon prospectivity, and transit leverage transfers from codified
 *   holders to the asserting power. The upstream codified regime is precisely
 *   what this reading claims to override, so the family edge runs from the
 *   strict-EEZ story into this one as the thing being displaced. Time
 *   indexing: interval point 0 corresponds to 1994 (convention entry into
 *   force), point 31 to 2025; the intensification after point 15 tracks the
 *   2016 arbitral award and the subsequent fortification-and-standardization
 *   phase. The claim/metric gap is deliberate: the arrangement is CLAIMED by
 *   its principal operator as the restoration of anterior entitlement (a
 *   justice-framing), while the authored metrics describe a substantially
 *   extractive, actively enforced overlay — the engine measures that
 *   divergence; the claim is not reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - expansive_claimant_states: agenda-setter and primary beneficiary (institutional/identity_locked) — projects and administers the historical-rights overlay through coast-guard regulation, maritime-militia mobilization, fortified outposts, and standardized cartography
 *   - eez_holding_coastal_states: primary target (moderate/constrained) — hold codified 200nm zones whose exclusivity the overlay voids in practice
 *   - transiting_commercial_shipping: secondary target (organized/constrained) — navigational actors facing rising war-risk premiums, route adjustments, and escort requirements
 *   - claimant_state_fishing_fleets: secondary beneficiary (organized/mobile) — access to disputed grounds follows the enforcement umbrella
 *   - victim_state_fishing_communities: diffuse target (powerless/trapped) — artisanal fishers displaced from traditional grounds
 *   - coastal_state_energy_firms: target (powerful/constrained) — licensed blocks stalled by pressure campaigns inside their own EEZ
 *   - fonop_naval_coalitions: counter-pressure actor (institutional/mobile) — bears the contest costs the regime's existence generates
 *   - arbitral_tribunal_machinery: analytical observer — adjudicated against the broad claim; sees the full structure but commands no bailiffs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.72).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.78).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical-Rights Overlay on UNCLOS Maritime Entitlements (Historical-Rights Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '4f4cb624-d676-467b-810f-4a8dee9d8e76').
narrative_ontology:cs_kernel_codification('4f4cb624-d676-467b-810f-4a8dee9d8e76', fixed_text).
narrative_ontology:cs_authority_grounding('4f4cb624-d676-467b-810f-4a8dee9d8e76', lineage).
narrative_ontology:cs_interpretation_layer_present('4f4cb624-d676-467b-810f-4a8dee9d8e76').
narrative_ontology:cs_reading_relation('4f4cb624-d676-467b-810f-4a8dee9d8e76', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('4f4cb624-d676-467b-810f-4a8dee9d8e76', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('4f4cb624-d676-467b-810f-4a8dee9d8e76', foundational, historic_usage_generates_overriding_title).
narrative_ontology:cs_axiom_status(historic_usage_generates_overriding_title, holdable).
narrative_ontology:cs_axiom_grounding('4f4cb624-d676-467b-810f-4a8dee9d8e76', historic_usage_generates_overriding_title, empirically_contingent).
narrative_ontology:cs_axiom('4f4cb624-d676-467b-810f-4a8dee9d8e76', foundational, codified_text_cannot_extinguish_anterior_rights).
narrative_ontology:cs_axiom_status(codified_text_cannot_extinguish_anterior_rights, holdable).
narrative_ontology:cs_axiom_grounding('4f4cb624-d676-467b-810f-4a8dee9d8e76', codified_text_cannot_extinguish_anterior_rights, conventional).
narrative_ontology:cs_reference_frame('4f4cb624-d676-467b-810f-4a8dee9d8e76', anterior_custom_supremacy).
narrative_ontology:cs_drift_state('4f4cb624-d676-467b-810f-4a8dee9d8e76', post_arbitral_award_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4f4cb624-d676-467b-810f-4a8dee9d8e76', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_fishing_fleets).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, transiting_commercial_shipping).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, victim_state_fishing_communities).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, coastal_state_energy_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, fonop_naval_coalitions).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, historic_title_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, prescription_acquiescence_principle).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, nine_dash_line_maritime_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts that centuries of usage and occupation confer sovereign rights over waters and features that the convention allocates to neighbors, and administers that assertion through coast-guard regulation, maritime-militia mobilization, fortified artificial outposts, and officially standardized maps issued with every new document. Collects access to fisheries, hydrocarbon prospects, and strategic depth inside the disputed zones. Retreat from the claim would contradict the official national narrative taught in schools and embedded in state media, so stepping back from the doctrine is not a live option for the current leadership regardless of external cost.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, agenda_setter,
    institutional, generational, identity_locked, regional).

% Hold exclusive economic zones recognized by the convention and watch their exclusivity hollowed out in practice: claimant fleets enter routinely, energy exploration is blocked by shadowing vessels and diplomatic threats, and access near disputed features is physically interdicted. Remedies run through arbitration and diplomacy, but the winning award changed little on the water. Their geography cannot be relocated, and their legal victory cannot be self-executed.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    moderate, generational, constrained, regional).

% Carries a large share of global trade through the affected waters. Enforcement incidents raise war-risk insurance premiums, force route adjustments, and add escort and delay costs that pass through freight rates. No single carrier can reroute away from the region entirely; the chokepoint geography of the sea lanes bounds their discretion.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, transiting_commercial_shipping, payer,
    organized, biographical, constrained, global).

% Fish disputed grounds under coast-guard and militia escort that excludes rival vessels from the same waters. Their access expands and contracts with the enforcement umbrella rather than with license or season; when protection moves, effort moves with it.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, claimant_state_fishing_fleets, beneficiary,
    organized, biographical, mobile, regional).

% Artisanal fishers displaced from grounds their families worked for generations, now crowded out by escorted industrial fleets or barred from features by interdiction. Alternative livelihoods are scarce in coastal villages; the daily cost of the arrangement lands on them directly and they have no separate channel to contest it.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, victim_state_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Hold licenses from coastal states to explore blocks inside the licensed state's own exclusive zone, but survey ships are shadowed, drilling plans draw explicit threats, and projects stall indefinitely. Capital sits idle awaiting political-risk resolution; the firms can redirect investment to other basins, but the licensed acreage itself cannot follow.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, coastal_state_energy_firms, payer,
    powerful, biographical, constrained, regional).

% Deploy warships through the disputed waters to assert that navigation freedoms survive the claimant's perimeter. Their deployments are the principal counter-pressure, and their operating budgets, escalation exposure, and scheduling overhead are costs generated by the arrangement's existence rather than by any benefit they collect from it.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, fonop_naval_coalitions, payer,
    institutional, biographical, mobile, continental).

% Small island economies whose fiscal base depends heavily on EEZ fisheries and licensing revenue. They participated in the convention process and depend on its text holding, but carry no weight in the bilateral enforcement contests that determine what happens on the water; they would insist on strict textual boundaries if the conversation were structured to hear them.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, small_island_developing_states, excluded,
    powerless, generational, trapped, regional).

% Adjudicates law-of-the-sea disputes under the convention's compulsory procedures. Produced the 2016 award that rejected the broad historic-rights entitlement while acknowledging narrow traditional fishing. Commands no enforcement arm; its output shapes legitimacy and third-party alignment but changes conditions at sea only insofar as other seats act on it.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, arbitral_tribunal_machinery, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Accommodates long-standing, pre-convention usage patterns — artisanal migration routes, seasonal grounds, historic anchorages — that straight-line EEZ boundaries cut through, solving once what would otherwise require per-season renegotiation of every crossing of the new lines by every traditional user.
% TRANSFER_FUNCTION: Moves effective control over fisheries stocks, hydrocarbon prospectivity, and transit-route leverage from EEZ-holding coastal states and open-access navigation to the expansive claimant state, with the price paid in enforcement presence and absorbed risk rather than in adjudicated compensation.
% ABSENT_VOICES: Small island developing states and victim-state fishing communities are absent from the bilateral consultative mechanisms where the arrangement is actually managed; the strict-EEZ position appears only as litigant, not as co-designer; the arbitral award is formally in the record but excluded from the enforcement conversation, which proceeds as if it had not happened.
% DISAPPEARANCE_RATIONALE: If the historical-rights overlay vanished overnight, claimant forces would fall back to baseline positions, coastal states would resume exclusive licensing and exploration inside their zones, militia escort activity would dissolve, war-risk premiums would normalize, and freedom-of-navigation operational tempo would drop — the regional maritime order would visibly reorganize around the codified text within months.
% FOUNDING_PROBLEM: When the convention's exclusive economic zones entered into force, they drew boundaries across waters where neighboring populations had fished, traded, and administered for centuries; the arrangement was built to reconcile — or, in its expansive form, to preserve claimant access despite — that collision between anterior usage and codified lines.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the 2016 South China Sea Arbitration (PCA Case 2013-19) — an adverse forum — attests both halves, acknowledging that traditional fishing practices existed at specific features (confirming the narrow accommodation problem is real) while finding no legal basis for the broad historic-rights maritime entitlement (attesting the mandate has outrun its function). Regional historiography independently documents centuries of usage patterns. No source independent of the claimant state attests the broad override form of the doctrine.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the transfer it accomplishes — resource control and strategic position inside zones the convention allocates to others — is decoupled from any adjudicated title or compensating payment; the 2016 award found no legal basis for the broad entitlement, so the transfer rides on presence rather than right. Suppression is higher still (0.78) because the arrangement's persistence depends on continuously denying alternatives at sea: interdiction of rival fishing and survey vessels, blocking of resupply, and a domestic legal apparatus (2021 coast-guard statute authorizing force against foreign vessels in 'waters under jurisdiction') that ratchets enforcement capacity. Theater is moderate-high (0.48) and rising: nearly half of regime activity is evidentiary and rhetorical production — historical atlases, white papers, feature-naming ceremonies, museum exhibits — that the tribunal found legally inert, while the operative work is done by hulls in the water. Accessibility_collapse is moderate (0.50): alternatives persist (arbitration, freedom-of-navigation operations, coalition diplomacy, code-of-conduct talks) but each is costly and slow, so understanding the arrangement does not dissolve the choice set the way a natural limit would. Resistance is substantial (0.68): sustained litigation, allied naval assertion, and diplomatic protest meet the regime continuously. The measurement series run on one shared time grid (points 0, 5, 10, 15, 20, 25, 31) with every tracked metric authored at every point; the suppression series is included because the story specifically traces enforcement-capacity maturation — from coast-guard-only assertion to fortified garrisons and militia integration — not merely shifting extraction. Identity-lock dynamics bind the agenda-setting seat: the historical narrative is constitutive of the claimant's official national self-understanding ('inherent territory since antiquity'), an institutional-ideological fusion in which retreat would trigger a domestic legitimation crisis; if that frame broke, exit_options would shift toward constrained, suppression demand would drop, and the arrangement could relax toward an ordinary negotiable boundary dispute. Coalition check: the powerless victim seat (artisanal fisher communities) has latent coalition power only through its state's institutions; direct cross-border fisher coordination is thin, which is why resistance is carried almost entirely by the moderate and powerful seats.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from identical structural data. From the claimant seat the arrangement reads as the correction of a historical injustice — a treaty regime drafted elsewhere, applied to waters its nation used before the treaty existed; enforcement appears as the ordinary administration of inherent territory. From the coastal-state seat the identical structure reads as expropriation of codified property: zones granted by universal agreement, emptied of exclusivity by a neighbor's fleet. From the navigator's seat it reads as a spreading tax on movement — premiums, detours, escorts. The engine derives these divergent classifications from the declared positions and exits; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality follows the beneficiary/victim declarations plus exit atoms, so no overrides are needed. expansive_claimant_states sit nearest the beneficiary pole (collects the transfer, runs the rules, identity_locked exit deepens retention); claimant_state_fishing_fleets sit close behind (mobile, protected access follows the umbrella). eez_holding_coastal_states sit near the target pole (lose exclusivity, geographically immovable, remedies weakly enforceable); victim_state_fishing_communities nearer still (trapped, no alternative livelihood); coastal_state_energy_firms slightly inward (capital is mobile even where the license area is not). transiting_commercial_shipping sits at roughly symmetric-plus-target: it pays a rising risk premium but retains partial routing discretion. fonop_naval_coalitions bear real costs with modest direct gain, placing them target-leaning but less exposed than the resident victims. The arbitral machinery is analytical and feeds no directional mass.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling genuine pre-convention traditional usage with newly drawn EEZ lines that cut across it — is partly live and partly dead: the tribunal itself acknowledged traditional fishing at Scarborough Shoal even while rejecting the broad historic-rights entitlement, so a narrow accommodation mandate survives, but the arrangement has expanded far past accommodation into resource appropriation and strategic denial. This is why the classification matters in both directions: reading the regime as pure extraction erases the real accommodation function that keeps some seats (protected fishing fleets, parts of the claimant's coastal population) net-benefited; reading it as pure coordination launders appropriation behind heritage language. The R5 mismatch consumer will read founding_problem_status (contested, dead-leaning) against disappearance_verdict (world_rearranges) and flag zombie dynamics — the mandate has partially outlived its function while the arrangement grows — cross-checked against the rising theater path, which shows proxy goals (cartographic and rhetorical production) substituting for the original accommodation work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the kernel unclos_sovereignty_boundary — the historical-rights reading. What would change structurally if a sibling reading were instantiated instead?',
    'Compare compiled sibling stories: the strict_eez_reading removes the expansive claimant states from the beneficiary set entirely and restores EEZ-holding coastal states to near-symmetric position; the non_ratifier_enforcement_reading shifts the beneficiary set toward open-navigation users and makes naval coalitions the enforcement seat rather than targets.',
    'The disagreement is located in a single structural element: whether anterior custom or codified text controls maritime entitlement. Resolving it in favor of either sibling flips the beneficiary/victim sets and moves epsilon from 0.72 to near-negligible (strict-EEZ) or to a navigation-liberty profile (FON enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: this story instantiates the historical-rights reading; sibling readings are separate constraints with different beneficiary/victim structures.').

omega_variable(
    historical_record_sufficiency,
    'Does the historical record actually show continuous, acquiesced-in exercise of sovereign authority over the claimed waters at the granularity the doctrine requires?',
    'Archival research meeting tribunal-grade evidentiary standards: continuous chart series, contemporaneous administrative records, third-party (including rival-claimant and neutral) attestations of exclusive control, rather than retrospective cartography.',
    'If the record fails the standard, the doctrine loses its coordination cover and the arrangement computes toward pure extraction with the claimant state as capturer; if the record is solid for specific waters, pockets of genuine traditional-use accommodation exist and epsilon should be discounted accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_sufficiency, empirical, 'Whether the factual predicate of the historical-rights axiom holds.').

omega_variable(
    identity_lock_depth,
    'Is the claimant state''s commitment to the historical narrative identity-fused (retreat unthinkable regardless of cost) or instrumental (revisable if costs rise enough)?',
    'Observe the claimant''s response to sustained cost imposition: negotiated stand-downs after escalation episodes, willingness to shelve map displays in trade deals, domestic messaging shifts after economic shocks.',
    'If identity-fused, suppression must remain permanently high and the exit_options atom stays identity_locked, hardening the target-side computation; if instrumental, a cost threshold exists at which the doctrine becomes negotiable and the arrangement could relax toward a conventional boundary dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth, empirical, 'Depth of the identity fusion binding the agenda-setting seat to the doctrine.').

omega_variable(
    accommodation_extraction_proportion,
    'What share of the regime''s day-to-day operation is genuine traditional-use accommodation versus resource appropriation and strategic denial?',
    'Granular incident and fleet-composition data: distinguish militia-escorted industrial fleets and hydrocarbon survey interdictions from artisanal fishers exercising documented seasonal patterns.',
    'A high accommodation share supports the hybrid classification with a meaningful coordination floor; a low share collapses the coordination leg and the arrangement recomputes as pure extraction riding a rhetorical cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_extraction_proportion, empirical, 'Proportion of coordination function to appropriation in the regime''s operation.').

omega_variable(
    victim_coalition_potential,
    'Can the materially weaker victim seats (coastal states, fisher communities, energy-license holders) convert shared exposure into coalition power sufficient to raise the regime''s maintenance cost?',
    'Track incidence of joint patrols, coordinated licensing responses, collective arbitration filings, and ASEAN-style code-of-conduct progress against claimant divide-and-delay tactics.',
    'Effective coalition formation raises measured resistance above 0.68 and forces the suppression series upward faster than extraction grows, potentially dating a type transition; failed coalition attempts confirm the divide-and-manage dynamic and stabilize the current profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_coalition_potential, empirical, 'Coalition-power prospects for the powerless and moderate victim seats.').

omega_variable(
    drift_vector_dominance,
    'Which drift vector dominates the reading''s trajectory: external axiom-overriding pressure (arbitral rejection, scholarly consensus against the broad claim) or internal revival pressure (fortification, standardized maps, domestic legal codification of the claim)?',
    'Longitudinal comparison of enforcement tempo against doctrinal-production tempo, and of third-party recognition trends (how many states acknowledge the award versus accommodate the claim).',
    'If axiom-overriding dominates, the engine''s terminal attractor moves toward repudiation of the reference frame and eventual decay of the arrangement; if revival pressure dominates, the reference frame consolidates and the arrangement hardens into a durable parallel order alongside the codified regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_vector_dominance, conceptual, 'Competing drift directions on the committer axis for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 31).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t5, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(uncl_tr_t5, observed).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(uncl_tr_t10, observed).
narrative_ontology:measurement(uncl_tr_t15, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(uncl_tr_t15, observed).
narrative_ontology:measurement(uncl_tr_t20, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(uncl_tr_t20, observed).
narrative_ontology:measurement(uncl_tr_t25, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement_basis(uncl_tr_t25, observed).
narrative_ontology:measurement(uncl_tr_t31, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 31, 0.48).
narrative_ontology:measurement_basis(uncl_tr_t31, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t5, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 5, 0.57).
narrative_ontology:measurement_basis(uncl_be_t5, observed).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(uncl_be_t10, observed).
narrative_ontology:measurement(uncl_be_t15, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(uncl_be_t15, observed).
narrative_ontology:measurement(uncl_be_t20, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(uncl_be_t20, observed).
narrative_ontology:measurement(uncl_be_t25, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(uncl_be_t25, observed).
narrative_ontology:measurement(uncl_be_t31, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 31, 0.72).
narrative_ontology:measurement_basis(uncl_be_t31, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t5, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement_basis(uncl_su_t5, observed).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(uncl_su_t10, observed).
narrative_ontology:measurement(uncl_su_t15, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(uncl_su_t15, observed).
narrative_ontology:measurement(uncl_su_t20, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(uncl_su_t20, observed).
narrative_ontology:measurement(uncl_su_t25, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(uncl_su_t25, observed).
narrative_ontology:measurement(uncl_su_t31, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 31, 0.78).
narrative_ontology:measurement_basis(uncl_su_t31, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the UNCLOS sovereignty dispute' conflates three structurally distinct claims with different epsilon values. strict_eez_reading (upstream, high empirical confidence, negligible extraction — codified exclusivity functions as near-pure coordination of ocean governance) is the regime this reading claims to override, so the influence edge runs strict_eez -> historical_rights. non_ratifier_enforcement_reading shares this reading's appeal to custom-over-text but locates the controlling custom in navigation liberty rather than historic title; the two coexist in a customary-pluralist framework while competing operationally. Each member links the others via affects_constraints; epsilon differs across members because the referent arrangement differs, not because one constraint is measured multiple ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
