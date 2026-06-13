% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Customary Freedom of Navigation Enforcement (Non-Ratifier Reading)
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   'unclos_sovereignty_boundary': the non-ratifier enforcement reading.
 *   Freedom of navigation principles are asserted by major naval powers
 *   (particularly the USA, which has not ratified UNCLOS) as customary
 *   international law independent of and superior to UNCLOS ratification.
 *   This reading treats customary doctrine as self-executing and enforceable
 *   through military presence (freedom-of-navigation operations) without
 *   coastal-state consent. The reading decouples the authority grounding from
 *   the written text of UNCLOS: rather than deriving navigation rights from
 *   UNCLOS Articles 58–66 (which exist and are bounded), this reading invokes
 *   pre-UNCLOS custom and claims universality regardless of ratification
 *   status. Naval powers benefit from the enforcement regime (military
 *   mobility, strategic leverage, resource access); coastal states claiming
 *   EEZ exclusivity bear the extraction costs (enforced openness, loss of
 *   control, diplomatic incidents). Island nations are trapped: they ratified
 *   UNCLOS expecting resource monopolies but find their EEZs transited
 *   without permission by non-ratifying powers.
 *
 * KEY AGENTS:
 *   - major_naval_powers (institutional power, arbitrage exit) — set the customary-law definition, enforce via FONOPs, collect strategic leverage
 *   - coastal_states_asserting_eez_exclusivity (moderate power, constrained exit) — attempt to enforce UNCLOS EEZ boundaries but face unilateral military transits
 *   - island_nations_with_strategic_resources (powerless, trapped exit) — claim EEZ resource rights under UNCLOS but cannot enforce against naval powers
 *   - global_trade_corridor_users (powerful, mobile exit) — benefit from enforced open routes; shipping and supply chains depend on the regime
 *   - unclos_signatories_non_ratifiers (powerful, constrained exit) — claim customary-law supremacy while declining treaty-dispute jurisdiction
 *   - regional_hegemons (excluded) — would assert alternative doctrines (historical rights, strategic defense zones) but are militarily barred
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.71).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Customary Freedom of Navigation Enforcement (Non-Ratifier Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '8e0392a2-f1d3-4926-bc4b-8e5ead2682c7').
narrative_ontology:cs_kernel_codification('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', fixed_text).
narrative_ontology:cs_authority_grounding('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', extraction).
narrative_ontology:cs_reading_relation('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', foundational, customary_law_supremacy_over_treaty).
narrative_ontology:cs_axiom_status(customary_law_supremacy_over_treaty, holdable).
narrative_ontology:cs_axiom_grounding('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', customary_law_supremacy_over_treaty, conventional).
narrative_ontology:cs_axiom('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', foundational, major_power_enforcement_authority).
narrative_ontology:cs_axiom_status(major_power_enforcement_authority, holdable).
narrative_ontology:cs_axiom_grounding('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', major_power_enforcement_authority, instrumental).
narrative_ontology:cs_axiom('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', secondary, non_ratifier_enforcement_legitimacy).
narrative_ontology:cs_axiom_status(non_ratifier_enforcement_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', non_ratifier_enforcement_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', pre_unclos_customary_freedom_of_seas).
narrative_ontology:cs_drift_state('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', post_unclos_ratification_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8e0392a2-f1d3-4926-bc4b-8e5ead2682c7', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_trade_corridor_users).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, island_nations_with_strategic_resources).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, island_nations_with_strategic_resources).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_signatories_non_ratifiers).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, customary_international_law_supremacy).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, freedom_of_seas_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The USA, NATO allies, and other major naval forces conduct freedom-of-navigation operations (FONOPs) through claimed EEZs and territorial waters without coastal-state permission, justified as enforcement of customary international law independent of UNCLOS ratification. They set the terms of what constitutes legitimate maritime freedom and enforce these terms through military presence. They collect strategic advantage: unimpeded fleet mobility, negotiating leverage with coastal states, resource access without treaty constraints. They administer the regime continuously through annual FONOP operations.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Nations that ratified UNCLOS expecting to control resource extraction and enforce security regulations within their 200-nautical-mile EEZ (China, Russia, Iran, Vietnam, Philippines, et al.) face systematic military transits by major naval powers that reject their authority. They bear the cost of enforced openness: inability to exclude foreign military vessels, loss of monopoly on resource extraction, diplomatic incidents and escalation risks, degraded capacity to regulate activities in their adjacent waters. They cannot exit their geographic position; their military capacity to resist is asymmetric against institutional naval powers.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity, payer,
    moderate, generational, constrained, national).

% Small island states (Pacific island nations, Caribbean states, island developing states) ratified UNCLOS with expectation that their EEZ would provide revenue from fisheries and seabed mineral extraction. The non-ratifier reading undermines this: naval powers conduct FONOPs, foreign fishing vessels enter their EEZs without regulation, mineral-exploration rights are contested by major powers. They benefit from some global maritime trade (regional commerce flows through their waters) but pay through inability to control their primary resource base. Geographic isolation means they cannot exit or relocate.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, island_nations_with_strategic_resources, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, island_nations_with_strategic_resources, beneficiary).

% International shipping companies, oil and liquefied-natural-gas traders, and multinational supply-chain operators benefit from enforced freedom of navigation: they move cargo through chokepoints (Strait of Malacca, South China Sea, Strait of Hormuz, Bab el-Mandeb) without coastal-state tolling, blockading, or regulatory burden. They avoid negotiating transit fees and environmental standards with every littoral state. Their exit option (rerouting around contested waters) is theoretically available but economically expensive; most depend on the shortest maritime routes for competitive shipping costs.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_trade_corridor_users, beneficiary,
    powerful, biographical, mobile, global).

% The USA and a handful of other major powers have signed UNCLOS but not ratified it. They invoke the reading to claim customary-law supremacy while declining to submit to UNCLOS dispute resolution, deep-seabed resource-sharing provisions, or the International Seabed Authority's authority. They pay in sustained diplomatic friction and coordination costs (defending the reading against UNCLOS-party challenges, managing incidents with ratifiers, maintaining the military apparatus that enforces it) but collect strategic freedom: unilateral authority to define navigation rights, no treaty obligation to share resources, ability to operate outside institutional constraints.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_signatories_non_ratifiers, payer,
    institutional, generational, constrained, global).

% The ICJ interprets international law boundaries and has rendered opinions on freedom of navigation (Nicaragua v. USA, Oil Platforms, South China Sea Arbitration cases). The court sits as a neutral arbiter between readings but cannot compel compliance from non-ratifiers and its rulings have been contested or ignored when they conflict with major-power interests. It observes the dispute analytically, producing interpretations that inform but do not determine state behavior.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% Mid-tier powers (China, Russia, Iran) that control strategic sea passages claim historical rights, strategic defense zones, or alternative customary doctrines that would give them exclusive or privileged control over adjacent waters. They are excluded from the non-ratifier reading's decision-making process: they are told what customary law requires, not invited to contest its definition. They would invoke the historical_rights_reading or assert regional alternatives if military capability were symmetric, but are structurally barred by the enforcement regime. Their exclusion is the reading's operative consequence.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, regional_hegemons, excluded,
    powerful, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains open global maritime routes and prevents coastal-state closure or selective regulation of international straits and high-traffic EEZ passages. Solves the coordination problem of preventing chokepoint blockade: if every coastal state could unilaterally regulate or exclude passage, global trade and superpower military mobility would be fragmented. The reading enforces a unified regime in which passage is universally permitted.
% TRANSFER_FUNCTION: Transfers strategic control and resource-monopoly rights from coastal states to major naval powers. Major naval powers gain unilateral authority to define and enforce navigation rights and military access; coastal states lose exclusive economic and security control of their EEZs; resource-rich island nations lose monopoly extraction rights; global traders gain cheap open routes; mid-tier regional powers lose the option to control adjacent passages.
% ABSENT_VOICES: Coastal states attempting EEZ enforcement are not in the conversation that defines the reading—they are told what customary law requires by major naval powers. Regional hegemons that would invoke historical rights or strategic defense alternatives are structurally excluded by the military enforcement regime. UNCLOS signatories that object to the non-ratifier reading are overridden by unilateral enforcement practice on the water.
% DISAPPEARANCE_RATIONALE: If the non-ratifier enforcement reading vanished, coastal states would immediately reassert full EEZ control, regional hegemons would close strategic passages (Strait of Malacca, Strait of Hormuz, South China Sea) to transit by rival powers, resource-rich island nations would monopolize fisheries and minerals, and global shipping would be forced to negotiate with every littoral state individually. A fragmented, tolled maritime system would replace the open-waters regime within months. The global trade structure depends on this reading's enforcement.
% FOUNDING_PROBLEM: Post-World War II international order required open seas for merchant shipping (global trade, economic development) and superpower naval mobility (military deterrence, power projection). Pre-UNCLOS customary international law (freedom of the seas doctrine from the Age of Sail) asserted this principle. UNCLOS (1982) attempted to institutionalize it through bounded provisions (Articles 58–66: specific rights for transit passage and high-seas navigation). The non-ratifier reading invokes the pre-UNCLOS custom to override UNCLOS-specific limitations and claim unilateral enforcement authority.
% FOUNDING_PROBLEM_CORROBORATION: Major naval powers (USA, NATO) attest the founding problem remains live: coastal-state closure threats (Strait of Hormuz blockade threats, South China Sea chokepoint control) require continuous defense of the open-seas regime. Independent shipping industry groups attest the regime is essential for global commerce. Coastal states and international law scholars attest the founding problem was institutionalized and solved by UNCLOS ratification (168 parties), and the non-ratifier reading is post-solution rent collection and power projection, not coordination maintenance. The International Court of Justice has issued mixed rulings: supporting coastal-state EEZ sovereignty in some cases (South China Sea Arbitration) but upholding freedom-of-navigation rights elsewhere (Oil Platforms). No outside authority affirms the non-ratifier reading as the binding customary-law definition; major naval powers themselves claim the authority.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.45 in 1982 (post-UNCLOS adoption, when the non-ratifier reading was emerging) and rises steadily to 0.68 by 2024. This trajectory reflects the reading's evolution from marginal interpretation to normalized enforcement practice. Early FONOPs were rare, disputed, and presented as protection of treaty-based rights; by 2024, FONOPs occur monthly across the globe (South China Sea, Black Sea, Persian Gulf) and are explicitly justified as customary law enforcement independent of UNCLOS. Suppression requirement rises in parallel (0.48 → 0.71), tracking the military and diplomatic effort needed to maintain the regime as coastal states increasingly resist and ratified-UNCLOS states invoke treaty text. Theater ratio (0.18 → 0.42) documents the shift from genuine coordination function (early FONOPs defended treaty-based open navigation) toward performative enforcement (later FONOPs defend a reading that naval powers alone author). The three series use one shared time grid aligned to the interval (1982–2024), with measurements at 6–12-year intervals. This temporal structure reveals the constraint's drift from weak coordination claim toward strong extractive enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (major naval powers) experiences the constraint as coordination they provide and maintain; the payer seats (coastal states, island nations) experience it as enforcement they cannot resist. This gap is not a measurement error—it is the signature of tangled rope: one party coordinates global trade routes (genuine function), another party pays through loss of coastal control (asymmetric extraction). The constraint's persistence depends on active enforcement (FONOPs) because coastal states would immediately reassert EEZ exclusivity without it; the coordination function alone would not sustain the arrangement (ratified UNCLOS already provides bounded freedom-of-navigation rights; the non-ratifier reading adds unilateral authority to override coastal-state wishes).
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers (agenda_setter, institutional power, arbitrage exit) compute with d near 0.0 (full beneficiary): they author the rule, enforce it unilaterally, and collect strategic mobility and resource-negotiation advantage. Coastal states attempting EEZ enforcement (moderate power, constrained exit) compute with d near 1.0 (full target): they ratified a treaty expecting resource rights, face unilateral enforcement against their wishes, and bear enforcement costs. Island nations (powerless, trapped) compute with d = 1.0: they cannot exit, lose resource monopolies, and are targets of enforced openness. Global traders (powerful, mobile) compute near d = 0.5 (symmetric): they benefit from open routes but could theoretically bypass chokepoints; their exit is constrained by economic efficiency, not structural coercion. The per-seat divergence is crucial: from a naval power seat, the constraint is legitimate coordination (preventing closure, maintaining trade). From a coastal-state seat, the same structure is coercive extraction (unilateral rule-setting, enforced openness, loss of control). The engine computes this divergence from structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII need for open sea lanes and superpower fleet mobility) was institutionalized in UNCLOS Articles 58–66: freedom of navigation embedded in treaty text, with bounded scope (innocent passage in territorial sea, rights of transit passage in straits, high-seas freedoms for all vessels). The non-ratifier enforcement reading resurrects pre-UNCLOS customary doctrine and treats it as superior to the treaty's bounded regime. This is mandatrophy: the constraint's founding function (open global routes) is solved by UNCLOS; the non-ratifier reading persists because it collects additional benefits (unilateral authority, no dispute-resolution obligation, strategic leverage) that UNCLOS would not grant. The constraint has become a vehicle for power projection rather than coordination. Temporal measurements show theater_ratio rising from 0.18 to 0.42, indicating a growing share of FONOP activity performs the reading itself (demonstrating that major powers make the rules, defending the rule against challenge) rather than solving coordination problems (defending merchant shipping, which UNCLOS already does). This rise in performative activity is the mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_content_authority,
    'Who has the legitimate authority to define what customary international law requires? The reading assumes major naval powers can author the definition unilaterally through enforcement practice; the strict_eez_reading assumes UNCLOS signatories collectively define customary law via treaty text; historical_rights_reading assumes coastal states define their own historical claims.',
    'International Court of Justice judgment on a dispute between a major naval power and a coastal state asserting EEZ exclusivity, or General Assembly resolution codifying customary law principles with majority-state participation.',
    'If an ICJ ruling held that customary law is defined by treaty-signatory consensus, the non-ratifier reading would lose authority and the constraint would collapse to UNCLOS-bounded freedom of navigation. If it held that major-power practice constitutes customary law, the non-ratifier reading would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_content_authority, conceptual, 'The locus of authority for defining customary international law content.').

omega_variable(
    non_ratifier_obligation_scope,
    'Can customary international law be binding on states that have not ratified the treaty it rests on, and can non-ratifier major powers unilaterally enforce it against ratifiers?',
    'Widespread coastal-state resistance to FONOPs coupled with diplomatic coordination; or institutional acceptance of FONOPs as legitimate, documented through lack of complaint to UN Security Council.',
    'If non-ratifiers can legitimately bind ratifiers, the non-ratifier reading stands and coastal states must accept FONOPs. If only ratifiers can reciprocally enforce customary law against ratifiers (and non-ratifiers are treated as outsiders to the covenant), the reading collapses and FONOPs become illegitimate intrusions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_ratifier_obligation_scope, preference, 'Whether customary law binds non-treaty-parties and whether non-ratifiers can enforce it.').

omega_variable(
    coordination_vs_power_projection_boundary,
    'Is the non-ratifier freedom-of-navigation reading solving a genuine coordination problem (open global routes for merchant shipping) or serving power projection (maintaining strategic military access against coastal-state resistance)? UNCLOS already solves the coordination problem; the reading adds unilateral authority.',
    'Temporal analysis: if FONOP intensity correlates with merchant-shipping volume and chokepoint closure risk, coordination motivation is present. If it correlates with geopolitical competition and coastal-state assertion of alternative claims, power-projection motivation dominates.',
    'If coordination motivation is primary, the constraint is rope with performative overhead (theater_ratio > 0.4 is noise). If power projection is primary, the constraint is snare where coordination is the cover story, and mandatrophy is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_power_projection_boundary, empirical, 'Whether the non-ratifier reading solves a real coordination problem or primarily enables power projection.').

omega_variable(
    reading_kernel_coupling_ambiguity,
    'Does the non-ratifier enforcement reading instantiate a reading of the UNCLOS text itself (the strict_eez_reading''s kernel) or does it invoke a separate, pre-UNCLOS customary-law kernel? If the latter, the readings compete for authority over different kernels, not different interpretations of the same kernel.',
    'Doctrinal analysis: examine whether the reading claims to interpret UNCLOS Articles 58–66 or to supersede them via external custom. If the former, it is a kernel reading. If the latter, it is a distinct constraint (pre_unclos_customary_navigation_doctrine).',
    'If a single kernel reading, the strict_eez_reading and this reading are in competition for interpretive authority. If distinct constraints, they are structurally independent and the network relationship is different (influence vs. foreclosure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_coupling_ambiguity, conceptual, 'Whether the reading interprets UNCLOS or invokes a separate customary-law kernel.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression (0.71) structural (military asymmetry, inability of coastal states to exclude FONOPs) or internalized (coastal states accept the reading as legitimate customary law and suppress their own objections)?',
    'Post-FONOP behavior: if coastal states lodge formal objections and demand compensation, suppression is structural. If they accept FONOPs silently and modify their own claims to align with the reading, suppression is partially internalized (identity-fusion mechanism).',
    'If internalized, the constraint''s effective suppression is higher than the structural measure—coastal states carry acceptance of the reading even when military pressure is absent. If structural, suppression decays as military presence decreases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural coercion or internalized acceptance of the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1982, 0.18).
narrative_ontology:measurement_basis(uncl_tr_t1982, observed).
narrative_ontology:measurement(uncl_tr_t1994, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1994, 0.22).
narrative_ontology:measurement_basis(uncl_tr_t1994, observed).
narrative_ontology:measurement(uncl_tr_t2004, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2004, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t2004, observed).
narrative_ontology:measurement(uncl_tr_t2014, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2014, 0.36).
narrative_ontology:measurement_basis(uncl_tr_t2014, observed).
narrative_ontology:measurement(uncl_tr_t2019, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t2019, observed).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(uncl_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1982, 0.45).
narrative_ontology:measurement_basis(uncl_be_t1982, observed).
narrative_ontology:measurement(uncl_be_t1994, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1994, 0.51).
narrative_ontology:measurement_basis(uncl_be_t1994, observed).
narrative_ontology:measurement(uncl_be_t2004, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2004, 0.58).
narrative_ontology:measurement_basis(uncl_be_t2004, observed).
narrative_ontology:measurement(uncl_be_t2014, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement_basis(uncl_be_t2014, observed).
narrative_ontology:measurement(uncl_be_t2019, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement_basis(uncl_be_t2019, observed).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(uncl_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1982, 0.48).
narrative_ontology:measurement_basis(uncl_su_t1982, observed).
narrative_ontology:measurement(uncl_su_t1994, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement_basis(uncl_su_t1994, observed).
narrative_ontology:measurement(uncl_su_t2004, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2004, 0.62).
narrative_ontology:measurement_basis(uncl_su_t2004, observed).
narrative_ontology:measurement(uncl_su_t2014, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2014, 0.68).
narrative_ontology:measurement_basis(uncl_su_t2014, observed).
narrative_ontology:measurement(uncl_su_t2019, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement_basis(uncl_su_t2019, observed).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(uncl_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, south_china_sea_nine_dash_line_claim).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, arctic_passage_sovereignty_disputes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'unclos_sovereignty_boundary'. The sibling readings (strict_eez_reading, historical_rights_reading) are separately authored constraint stories with their own ε values, beneficiary/victim structures, and classifications. They compete for interpretive authority over the same kernel text (UNCLOS, pre-UNCLOS custom) but instantiate different constraints with different extractiveness profiles. The three stories form a constraint family linked by network.affects_constraints. Decomposition rationale: the ε-invariance principle requires that different authority-grounding structures, different beneficiary sets, and different enforcement mechanisms be modeled as distinct constraints. This reading treats major-naval-power enforcement as the legitimacy ground (ε ≈ 0.68, tangled_rope); the strict_eez_reading treats UNCLOS ratifier consensus as the ground (lower ε, rope or mountain candidate); the historical_rights_reading treats coastal-state prior claims as the ground (ε depends on whose history; victim set includes major naval powers instead of coastal states). Each reading is a structurally distinct constraint, not three angles on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
