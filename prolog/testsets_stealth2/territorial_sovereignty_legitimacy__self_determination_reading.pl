% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Self-Determination Reading of Territorial Sovereignty Legitimacy (Standing Arrangement Assessment)
 *   domain: political_theory/international_relations
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   territorial_sovereignty_legitimacy: the self-determination reading, under
 *   which legitimate title derives from the modern principle of
 *   self-determination applied to the Arab population that formed the
 *   demographic majority with continuous residence through the modern period
 *   (19th-20th centuries). Per the kernel-reading epsilon rule, the referent
 *   of every metric is the STANDING ARRANGEMENT - the existing sovereignty
 *   and control regime over the territory - assessed by this reading's own
 *   lights: an arrangement produced through externally authored instruments
 *   (Balfour Declaration, Mandate, Partition recommendation) against the
 *   resident majority's never-consulted will, maintained since 1948 by denial
 *   of refugee return, military occupation, and differentiated legal regimes.
 *   The reading's structural deltas are authored throughout: temporal scope
 *   limited to the modern period; legitimacy requiring continuous demographic
 *   presence; partition treated as unjust external imposition; the Israeli
 *   state framed as a colonial project; right of return treated as
 *   restoration of the status quo ante. Sibling readings
 *   (covenant-continuity, existential-matrix) are separate constraints in
 *   separate files and are not averaged into this one. Claim/metric
 *   independence: claimed_type is authored from this reading's structural
 *   assessment of the standing arrangement; the metrics describe the
 *   arrangement's operation as this reading observes it; the engine computes
 *   per-seat classifications from the structural data, and divergence between
 *   claim and computed type is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - israeli_state_apparatus: agenda setter (institutional/arbitrage) - administers and enforces the arrangement, defines the frame every other party operates in
 *   - israeli_jewish_collective: primary beneficiary (powerful/identity_locked) - receives land allocation, security, and immigration privilege; identity fused to the arrangement's permanence
 *   - great_power_patron_states: secondary beneficiary (institutional/arbitrage) - collect strategic returns at minimal local cost
 *   - palestinian_refugee_diaspora: primary target (powerless/trapped) - bears dispossession and denial of return across generations
 *   - west_bank_gaza_palestinians: primary target (powerless/trapped) - bears occupation, blockade, and fragmented jurisdiction
 *   - palestinian_citizens_of_israel: partial target (moderate/constrained) - formal inclusion with structural subordination
 *   - palestinian_authority_elites: captured intermediary (organized/trapped) - collects administrative rents while subordinate to the frame
 *   - jordan_lebanon_host_states: diffuse-cost bearer (organized/constrained) - host refugee cohorts, tied to the arrangement's stability by treaty
 *   - palestinian_camp_communities_lebanon: excluded voice (powerless/trapped) - outside host-state politics and outside the negotiating rooms
 *   - international_legal_institutions: analytical observer (institutional/analytical) - records the normative ledger without enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.84).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.88).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Self-Determination Reading of Territorial Sovereignty Legitimacy (Standing Arrangement Assessment)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '61d2a048-191c-46f1-87ce-bec0d0a6bc2b').
narrative_ontology:cs_kernel_codification('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', distributed).
narrative_ontology:cs_authority_grounding('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', distributed).
narrative_ontology:cs_reading_relation('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', territorial_sovereignty_legitimacy__covenant_continuity_reading, influences).
narrative_ontology:cs_reading_relation('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', foundational, modern_continuous_residence_confers_title).
narrative_ontology:cs_axiom_status(modern_continuous_residence_confers_title, holdable).
narrative_ontology:cs_axiom_grounding('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', modern_continuous_residence_confers_title, deontological).
narrative_ontology:cs_axiom('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', secondary, external_imposition_voids_sovereign_title).
narrative_ontology:cs_axiom_status(external_imposition_voids_sovereign_title, holdable).
narrative_ontology:cs_axiom_grounding('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', external_imposition_voids_sovereign_title, deontological).
narrative_ontology:cs_axiom('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', secondary, right_of_return_restores_status_quo_ante).
narrative_ontology:cs_axiom_status(right_of_return_restores_status_quo_ante, holdable).
narrative_ontology:cs_axiom_grounding('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', right_of_return_restores_status_quo_ante, deontological).
narrative_ontology:cs_reference_frame('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', modern_period_demographic_self_determination).
narrative_ontology:cs_drift_state('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('61d2a048-191c-46f1-87ce-bec0d0a6bc2b', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_collective).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, great_power_patron_states).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_authority_elites).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, west_bank_gaza_palestinians).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_authority_elites).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, jordan_lebanon_host_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the territory's borders, customs union, military deployment, land registration, and population registry. Administers differentiated legal regimes across the areas it governs and enforces the bar on refugee return through entry-control and absentee-property mechanisms. Remits cleared tax revenue to the Palestinian Authority and withholds it during crises. Sets the rules under which every other party operates; its own exit question rarely arises because it defines the frame.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).

% The citizen body of the state. Receives land allocation through state and national institutions, preferential immigration under the Law of Return, and security guarantees unavailable to the displaced Arab population. Its national identity is constituted through the state's permanence; individual emigration is possible but reads collectively as abandonment of the national project, so the collective does not treat exit as a live option.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_collective, beneficiary,
    powerful, generational, identity_locked, regional).

% Extend military financing, diplomatic shielding in the Security Council, and technology cooperation. Receive strategic basing access, intelligence sharing, and a stable regional anchor for arms sales and alliance management. Their exposure to the arrangement's local costs is minimal; their capacity to reshape it is exercised selectively.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, great_power_patron_states, beneficiary,
    institutional, generational, arbitrage, global).

% Administer civil affairs in limited areas under negotiated limits, run a payroll employing a large share of the workforce, and depend on cleared tax revenue and donor budgets. Maintain security coordination with the administering power. Their institutional survival is contingent on the arrangement continuing; losing their position means personal and factional ruin, so exit is not realistically available.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_authority_elites, beneficiary,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_authority_elites, payer).

% Descendants of those displaced in 1948 and 1967, registered with UNRWA across Lebanon, Syria, Jordan, the West Bank, and Gaza. Barred from returning to the territory; citizenship and property rights in host states vary widely. Political representation runs through factions and a diaspora leadership whose decisions they influence unevenly. Return is the constitutive political demand; remaining abroad is the enforced default.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, global).

% Live under military occupation in the West Bank - permit-regulated movement, fragmented jurisdictions, settlement expansion onto land they hold claims to - and under blockade in Gaza. Work substantially in or depend on the Israeli economy. Vote in Palestinian Authority elections when held but not in the polity that controls their borders, water, and airspace. Emigration is available to a few with resources; for most, staying is the only option.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, west_bank_gaza_palestinians, payer,
    powerless, biographical, trapped, regional).

% Hold citizenship, vote in national elections, and receive state services. Face documented discrimination in land and planning approval, budget allocation, and constitutional status under the Nation-State Law. Community ties make emigration costly; their leverage runs through courts and electoral coalitions.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_citizens_of_israel, beneficiary).

% Host multi-generational refugee cohorts. Jordan extended citizenship to most; Lebanon restricts camp residents' access to professions and property. Bear fiscal and security costs of hosting, and manage treaty relationships - peace treaties, aid compacts - that tie their economies to the arrangement's stability.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jordan_lebanon_host_states, payer,
    organized, generational, constrained, regional).

% Live in camps with restricted employment, property, and building rights under Lebanese law, outside Lebanese citizenship and outside the negotiating rooms where final-status questions are discussed. Neither their host state nor the Palestinian factions they formally belong to represent their specific situation; they would demand immediate return implementation if seated.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_camp_communities_lebanon, excluded,
    powerless, generational, trapped, regional).

% Issue advisory opinions, resolutions, and treaty-body findings on the arrangement's legality. Record the normative ledger and supply vocabulary that advocacy movements use. Possess no enforcement arm; their pronouncements alter costs only where member states choose to act on them.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_legal_institutions, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_collective).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__self_determination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a functioning state order within its jurisdiction - security, courts, infrastructure, a single economic and customs space - and anchors a regional alliance system for its patrons. These coordination outputs are real for those admitted to the polity; the arrangement also settles the territorial-allocation question by force rather than agreement.
% TRANSFER_FUNCTION: Moves land registration, water allocations, mobility rights, and political self-rule from the territory's Arab population to the Israeli Jewish collective; moves labor and cleared tax revenue from occupied residents through the administering power; moves military financing and diplomatic protection from patron states to the state apparatus in exchange for strategic positioning.
% ABSENT_VOICES: The displaced population's consent was never solicited at any founding juncture: the Balfour Declaration, the Mandate allocation, and the Partition recommendation were decided among external powers without a plebiscite of the territory's inhabitants. Today the refugee diaspora sits outside the rooms where final-status questions are negotiated; camp communities in Lebanon are represented by no one directly; and the occupied population votes in neither the polity governing their daily lives nor a sovereign of their own.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the region's state system, alliance architecture, and the political identity of both national collectives would reorganize around the resulting vacuum: return movements, competing title claims, and patron realignment would immediately reshape borders and alignments. Nothing about the current settlement is self-enforcing in its absence.
% FOUNDING_PROBLEM: Late-imperial promise management and the European Jewish question: Britain had issued overlapping commitments (Hussein-McMahon correspondence, Sykes-Picot, Balfour Declaration) and sought a territorial resolution for a persecuted European minority, resolved at the expense of the territory's resident majority, whose wishes were never institutionally consulted.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of the Mandate period and of British imperial decision-making, working outside all party camps, attest from the archival record that the founding instruments were instruments of imperial commitment management rather than expressions of the inhabitants' will; post-colonial historiography corroborates the pattern. Party-aligned histories dispute the weighting of the imperial genealogy, which is itself signal. Corroboration for the dead status: the arrangement's current public justifications (security, deterrence, identity) no longer reference the founding problems, and the European-refuge function migrated to other instruments after 1945-1948.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.84 at interval end) because the standing arrangement, on this reading's account, transfers land registration, water, mobility, and self-rule from the territory's Arab population to the Israeli Jewish collective and its patrons, with the transfer deepening as settlement and displacement proceed. Suppression (0.88) reflects the enforcement machinery the arrangement requires - border enforcement against return, permit regimes, closures, wartime emergency measures - and is authored as a raw structural property, unscaled by power or scope; scaling belongs to the engine. Theater ratio (0.40) tracks legitimating performance: it peaks at Oslo (0.54 in 1993) when process substitutes for outcome, then declines as pretense thins. Accessibility collapse (0.60) is moderate because alternatives - armed struggle, negotiation, internationalization, emigration - remain live though each carries heavy cost, which is why resistance (0.75) has stayed high across seven decades. All three tracked series run on one shared nine-point grid (1948-2024) so every metric is authored at every examined point. The trajectories show a ratchet-with-reform-pauses pattern rather than smooth drift: extraction and enforcement dip at reform moments (1993) and resume accumulation afterward; the pauses deliver intermittent relief that resets resistance without reversing the underlying transfer, so the oscillation is partly the maintenance mechanism itself rather than noise.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the agenda-setter and beneficiary seats (state apparatus, Jewish collective, patrons), the arrangement presents as a functioning state that solved real coordination problems - security, courts, infrastructure, a single economic space - with the enforcement experienced as defense of a legitimate order against rejectionists. From the payer seats (refugee diaspora, occupied residents), the same structure presents as an imposed regime whose persistence depends on coercively denying their self-determination and return. The captured intermediary seat (PA elites) experiences both directions at once: administrative rents flowing in, subordination flowing down. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the Israeli Jewish collective (receives land, security, and immigration privilege; identity-locked to the arrangement's permanence, which sustains maintenance rather than exit), the patron states (strategic returns, minimal local cost exposure), and the PA elites (administrative rents despite formal subordination - the derivation places them near the beneficiary end, which is the capture signature). Victim declarations drive high directionality for the refugee diaspora and West Bank/Gaza residents, amplified by trapped exit; Palestinian citizens of Israel derive intermediate directionality from partial inclusion. Host states carry diffuse secondary costs. International legal institutions occupy the analytical seat and contribute no directional pull. No directionality overrides were needed: the beneficiary/victim plus exit-option data produce the intended derivations.
 *
 * MANDATROPHY ANALYSIS:
 *   On this reading's genealogy, the arrangement was built to solve problems that no longer exist in their original form: managing Britain's overlapping wartime commitments, and finding territorial refuge for a persecuted European minority - the latter transformed by 1948 statehood and by the post-1945 reshaping of European Jewry. The founding-problem interview records status dead against verdict world_rearranges, which is exactly the mismatch signature the corpus consumer reads as capture/zombie signal: the arrangement persists not because its founding problem persists but because concentrated receipts (land, security, strategic rents) and identity fusion sustain it. The classification prevents mislabeling in both directions: it blocks a pure-coordination reading because persistence depends on coercive denial of exit and return rather than participant preference; and it blocks a degraded-inertial reading because the arrangement is actively maintained with a concentrated gain_flow seat, not drifting inertially with diffuse costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_kernel_reading_indexicality,
    'This constraint is one reading of the territorial_sovereignty_legitimacy kernel; the contest turns on which structural element fixes title - the temporal scope of residence (modern-period continuity versus ancient covenant) and the source of title (demographic self-determination versus divine grant versus existential necessity). Which element does the dispute actually hinge on?',
    'Comparative structural analysis across the three sibling readings'' axioms, temporal scopes, and victim enumerations; tracing which premise each party''s practical reasoning treats as load-bearing.',
    'Adopting the covenant reading re-times title to antiquity and swaps the beneficiary/victim sets entirely; adopting the existential reading dissolves the juridical question and removes legality-based assessment altogether. This story''s epsilon, victims, and classification are indexed to the self-determination reading and do not survive translation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_kernel_reading_indexicality, conceptual, 'Reading-indexicality of the legitimacy kernel: committer structure routed here rather than folded into the constraint.').

omega_variable(
    modern_demographic_premise_empirics,
    'Does the 19th-20th century record support the reading''s premise of continuous Arab demographic majority and residence (Ottoman census fragments, 1922 and 1931 Mandate censuses, land tenure and village records), including through episodes of conquest, famine, and displacement?',
    'Archival demographic reconstruction of the territory''s population 1800-1948 with attention to continuity at village and regional levels rather than aggregate totals alone.',
    'If continuous majority weakens at the margins, the reading''s claim shifts from restoration-of-title to remediation-of-dispossession, changing the remedy structure (return versus compensation) and softening the absoluteness of the title axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_demographic_premise_empirics, empirical, 'Empirical status of the continuous-demographic-majority premise.').

omega_variable(
    self_determination_unit_ambiguity,
    'Which unit exercises the self-determination right this reading invokes - the territory''s whole population (binational state), each community separately (partition), or the majority community alone (majoritarian restoration)? The Wilsonian principle underdetermines the unit of application.',
    'Doctrinal analysis of self-determination''s unit-of-application jurisprudence (internal versus external self-determination, uti possidetis analogies, common-Article-1 ''peoples'' definitions) applied to mandate-terminated territories.',
    'The binational variant yields a different victim set (no exclusive title holder) and would likely compute as tangled_rope rather than snare; the majoritarian variant sustains this story''s assessment. The reading as instantiated here adopts the majoritarian restoration variant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_determination_unit_ambiguity, conceptual, 'Unit-of-application ambiguity inside the self-determination principle itself.').

omega_variable(
    remedy_feasibility_vs_status_quo_ante,
    'Does justice under this reading still require literal return and restoration of the status quo ante after seven decades of changed facts, or does the remedy admit substitution (compensation, sovereignty trade, negotiated admission quotas)?',
    'Negotiated-framework outcomes and restorative-justice doctrine: observe which remedy forms displaced communities and their representatives accept or refuse, and under what guarantees.',
    'A literal-return reading keeps the standing arrangement''s full dispossession on the ledger; substitution-admitting variants lower the remedial stakes and would soften the effective classification toward negotiable hybrid forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_feasibility_vs_status_quo_ante, preference, 'Remedy-form ambiguity: restoration versus substitution under the reading''s own standard.').

omega_variable(
    coordination_extraction_separability,
    'Is the state order the arrangement provides (security, courts, infrastructure, economic space) structurally separable from the exclusionary structure that maintains it, or does the service provision depend on the same mechanisms that deny return and self-rule?',
    'Counterfactual institutional analysis: whether comparable service provision persists under a rights-equal regime in comparable cases, and which specific mechanisms are dual-use.',
    'If separable, the coordination story functions as cover and the snare assessment stands; if inseparable, part of the measured burden is the price of the coordination itself and the assessment shifts toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Separability of the arrangement''s governance output from its exclusionary maintenance mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.22).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(terr_tr_t1987, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1987, 0.36).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.54).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2000, 0.46).
narrative_ontology:measurement(terr_tr_t2005, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2005, 0.5).
narrative_ontology:measurement(terr_tr_t2010, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2010, 0.47).
narrative_ontology:measurement(terr_tr_t2018, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2018, 0.43).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.71).
narrative_ontology:measurement(terr_be_t1987, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1987, 0.74).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.66).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(terr_be_t2005, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2005, 0.77).
narrative_ontology:measurement(terr_be_t2010, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(terr_be_t2018, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2018, 0.82).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2024, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.64).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(terr_su_t1987, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1987, 0.77).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.69).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(terr_su_t2005, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2005, 0.81).
narrative_ontology:measurement(terr_su_t2010, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(terr_su_t2018, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2018, 0.83).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, resource_allocation).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (territorial_sovereignty_legitimacy), three readings emitted as separate stories. The colloquial label 'who legitimately holds the land' conflates structurally distinct claims - title from divine grant plus continuous presence plus recognition, title from modern demographic self-determination, and non-juridical existential necessity - carrying different reading-indexed epsilon assessments over the same standing arrangement (OQ-26: values are reading-indexed, referent fixed). This file is the self-determination reading; siblings are linked here and back. Lineage: the covenant reading historically supplied the recognition architecture (Balfour, Mandate) that this reading contests; this reading's international-law traction now exerts downstream pressure on the covenant reading's recognition pillar.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
