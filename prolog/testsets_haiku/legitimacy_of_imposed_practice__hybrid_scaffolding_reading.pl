% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Legitimacy of Imposed Practice (Hybrid Scaffolding Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A colonial or centralizing state imposes a new cultural practice (Western
 *   dress code, Gregorian calendar, official language) through a mechanism
 *   that combines decree with ideological messaging and elite modeling. The
 *   constraint succeeds partially: urban populations adopt the practice
 *   enthusiastically as a marker of modernity; rural populations face
 *   enforcement pressure but lack the scaffolding infrastructure (models,
 *   ideological framing, prestige incentives) that would make adoption
 *   quasi-endogenous. The result is partial displacement, visible in urban
 *   centers and hybrid practices at the boundary, while prior practices
 *   persist in rural areas where enforcement is sporadic and alternatives
 *   remain culturally embedded. This reading instantiates the scaffolding
 *   mechanism as the KEY VARIABLE that distinguishes it from pure exogenous
 *   override (decree alone) and pure endogenous climb (ideology alone without
 *   enforcement). The constraint is CLAIMED as tangled_rope because it
 *   coordinates administrative unification WHILE extracting cultural
 *   authority from indigenous practitioners. The measurement series shows
 *   extractiveness rising then plateauing (the scaffolding reaches its
 *   saturation point), suppression declining as elite adoption reduces
 *   enforcement need, and theater rising then stabilizing (the ideological
 *   performance becomes routine). This reading differs fundamentally from the
 *   exogenous_override reading (which models decree as sufficient) and the
 *   endogenous_climb reading (which models adoption as internalized
 *   regardless of enforcement) by centering the hybrid mechanism itself.
 *
 * KEY AGENTS:
 *   - colonial_administration: institutional power, agenda-setter, enforces the decree and funds the scaffolding infrastructure (modeling, messaging, incentives)
 *   - urban_elites: powerful agents, beneficiaries, voluntary adopters whose visible compliance generates ideological momentum and models for others
 *   - rural_populations: powerless agents, victims, face enforcement pressure but lack scaffolding infrastructure — their resistance is diffuse and non-coordinated
 *   - indigenous_practitioners: moderate power, identity-locked targets, whose professional authority is displaced by the new practice regime
 *   - hybrid_practitioners: moderate power, the liminal seat where the constraint's partial success is visible — they adopt publicly while maintaining prior practice privately
 *   - ideological_propagandists: organized agenda-setters who generate the messaging that transforms decree into quasi-endogenous pull
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.62).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Legitimacy of Imposed Practice (Hybrid Scaffolding Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '8c29a47b-deb7-47f8-85a6-7c58b48e3e76').
narrative_ontology:cs_kernel_codification('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', formalized).
narrative_ontology:cs_authority_grounding('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', extraction).
narrative_ontology:cs_interpretation_layer_present('8c29a47b-deb7-47f8-85a6-7c58b48e3e76').
narrative_ontology:cs_reading_relation('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', foundational, scaffolding_mechanism_necessary).
narrative_ontology:cs_axiom_status(scaffolding_mechanism_necessary, holdable).
narrative_ontology:cs_axiom_grounding('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', scaffolding_mechanism_necessary, empirically_contingent).
narrative_ontology:cs_axiom('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', foundational, quasi_endogenous_pull_possible).
narrative_ontology:cs_axiom_status(quasi_endogenous_pull_possible, holdable).
narrative_ontology:cs_axiom_grounding('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', quasi_endogenous_pull_possible, empirically_contingent).
narrative_ontology:cs_reference_frame('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', colonial_administrative_authority).
narrative_ontology:cs_drift_state('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', contemporary_postcolonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8c29a47b-deb7-47f8-85a6-7c58b48e3e76', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, colonial_administration).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, indigenous_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, hybrid_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, hybrid_practitioners).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, elite_modernization_narrative).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, cultural_progress_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees the new practice and funds enforcement machinery. Justifies the mandate as administrative rationalization, modernization, or civilizing mission. Controls access to state resources, legal penalties for non-compliance, and incentives for adoption (government contracts, access to colonial institutions). Can exit by reversing the mandate, but faces institutional pressure to demonstrate successful cultural transformation as evidence of colonial authority. Experiences the constraint as successful coordination when urban elites adopt visibly.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopt the new practice voluntarily and visibly as a marker of modernity, cosmopolitanism, and alignment with power. They benefit from prestige differentiation, access to colonial institutions, and professional advancement. They pay a social cost: disruption of prior identity anchors, potential alienation from family or community members who do not adopt. Their adoption generates the modeling and ideological momentum that scaffolds the imposition for others. They have genuine exit options (could reject adoption and remain locally powerful) but choose not to because the benefits outweigh costs.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, beneficiary,
    powerful, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, payer).

% Face the mandate through enforcement pressure and incentive structures (penalties for non-compliance, economic pressure, exclusion from state benefits). They lack the scaffolding infrastructure (elite models, ideological framing, prestige incentives) that makes adoption appear quasi-endogenous to urban populations. They experience the imposition as external coercion rather than cultural modernization. Their resistance is diffuse and non-coordinated due to geographic isolation and lack of collective voice. They bear the cost of behavioral change (disruption of established practice, loss of cultural authority for specialists) without the benefit of prestige or institutional access.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations, payer,
    powerless, biographical, constrained, local).

% The prior practice is integral to their professional identity, religious commitment, or cultural authority (e.g., traditional calendar keepers, ritual specialists, craft practitioners). The mandate targets their practice directly and threatens their structural role. Exit would require identity dissolution — abandoning the knowledge, social position, and community standing that constitute them. They resist actively but lack institutional power to prevent enforcement. They experience suppression as particularly acute because it attacks both their livelihood and their identity simultaneously.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, indigenous_practitioners, payer,
    moderate, generational, identity_locked, local).

% Adopt the new practice partially or situationally, maintaining the prior practice in restricted contexts (wearing Western dress in town, traditional dress at home; using colonial calendar for administration, traditional calendar for ritual). They gain access to colonial institutions and prestige while managing identity risk. They are neither pure adopters nor pure resisters; they exemplify the partial displacement the scaffolding achieves. They experience suppression as moderate — the constraint tolerates hybridity as long as public compliance with the new practice is visible.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, hybrid_practitioners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, hybrid_practitioners, payer).

% Produce and distribute messaging that frames the new practice as inevitable, progressive, modern, scientifically superior, or morally enlightened. They are colonial agents, aligned local actors (newspaper editors, educators, clergy, intellectuals), or intellectuals genuinely committed to the modernization narrative. They create the discourse that transforms the mandate from pure decree into a narrative pull — they are the ENGINE of the 'quasi-endogenous' mechanism. They benefit from institutional backing, prestige, and often direct compensation. They can exit by refusing to produce messaging, but career integration, ideological commitment, and institutional incentives keep them engaged.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_propagandists, agenda_setter,
    organized, biographical, mobile, national).

% Would coordinate organized resistance and articulate the claim that the imposition represents cultural violence, loss, and illegitimate authority if given institutional platforms. They are systematically barred from public discourse, media access, and political assembly. Their perspective — that the prior practice was functional, culturally grounded, and not inherently inferior; that the new practice is imposed for the benefit of colonizers, not the colonized — is excluded from the legitimacy conversation. They mount what resistance they can: private non-compliance, covert maintenance of prior practices, oral transmission of critique and memory. They lack collective voice and institutional recourse.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance_coalitions, excluded,
    powerless, biographical, trapped, local).

% Track the constraint across multiple historical cases: colonial calendar impositions, dress code mandates, language shifts, religious practice restrictions. They observe the pattern: pure decree fails or leaves surface compliance only; pure internalization through ideology alone is slow, requiring generational change; hybrid scaffolding (decree + elite modeling + ideological messaging) achieves partial displacement efficiently. They produce analytical evidence distinguishing the three readings and documenting the causal mechanisms that differentiate them.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, comparative_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform cultural markers across diverse regional populations, enabling administrative standardization, coordinated governance, and visual markers of unified political authority. The scaffolding mechanism (ideological messaging + elite modeling + prestige incentives) solves the coordination problem of how to shift practice in the absence of purely voluntary adoption — it generates quasi-endogenous pull by making compliance appear as a choice rather than pure external coercion. Urban centers adopt the practice; rural areas follow through a mix of enforcement and elite-modeling spillover. Hybrid practitioners exemplify the partial coordination achieved.
% TRANSFER_FUNCTION: Moves cultural authority and social prestige from indigenous practitioners and rural knowledge systems to colonial administration and adopting urban elites who monopolize the new practice and its ideological justification. Transfers economic benefits (access to colonial institutions, professional advancement, trade privileges, government contracts) to early adopters and compliance enforcers. Imposes costs (behavioral disruption, identity threat, loss of professional status, exclusion from state benefits) on non-adopters and indigenous specialists. The ideological messaging amplifies the extraction by framing it as progress and the indigenous alternative as backward, thus encoding extraction into the target population's own interpretive frames.
% ABSENT_VOICES: Resistance coalitions who reject the imposition as cultural violence, indigenous practitioners whose expertise and authority are delegitimized, and rural populations whose experience of enforcement contradicts the urban elite narrative of voluntary modernization adoption. These populations are systematically excluded from the discourse that legitimizes the constraint. They have no media platform, no seat at policy tables, no institutional voice to argue that the prior practice was functional and culturally grounded. Their exclusion from the ideological conversation is structural to the scaffolding mechanism — the constraint requires their silence (or invisibility as 'backward' populations) to succeed in urban centers and spread through elite modeling.
% DISAPPEARANCE_RATIONALE: If the constraint and its enforcement machinery vanished, the partial displacement already achieved (visible in urban centers, hybrid practices at boundaries) would persist because it has become institutionalized in educational systems, government administration, and elite social life. But the momentum toward complete displacement would halt. Rural areas would revert more visibly to prior practices; the ideological pull would lose institutional backing; the prestige incentives for adoption would evaporate. Administrative coordination would face immediate friction and require new mechanisms for synchronization across populations. The visible markers of unified authority (dress code, calendar system) would fragment visually.
% FOUNDING_PROBLEM: Colonial authority required visible markers of cultural transformation and administrative unification across populations with distinct practices and regional identities. Decrees alone faced passive resistance and hidden non-compliance (public compliance, private persistence). Pure ideological messaging without enforcement was too slow and reached only educated urban populations. The constraint was built to solve the problem of how to achieve sufficient visible displacement rapidly enough to consolidate colonial authority while avoiding the costs of total enforcement across rural areas.
% FOUNDING_PROBLEM_CORROBORATION: Colonial administrators, modernization advocates, and urban adopters attest that the founding problem is live: they cite the need for administrative efficiency, cultural unification, and the appearance of successful authority. Rural populations and indigenous practitioners attest that the problem is constructed: they argue the prior practices were efficient and unified within their own logic, and the 'problem' is invented to justify external imposition and serve colonial interests. Comparative historians and postcolonial scholars corroborate the contested reading, documenting that the 'coordination' problem was defined by the colonizers' administrative needs, not by the colonized populations' prior lack of coordination.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.58 endpoint) because the constraint transfers cultural authority and prestige to colonial elites and away from indigenous practitioners, even as it is presented as neutral administrative reform. The rise from 0.35 to 0.58 reflects the growing reach of the constraint as scaffolding infrastructure penetrates urban-rural boundaries; the plateau at t=35 reflects the saturation point — further extraction requires enforcement intensity that the state cannot sustain. Suppression is high initially (0.72) because pure decree meets passive resistance; it declines to 0.62 as elite adoption reduces enforcement need and makes compliance appear voluntary. Theater rises from 0.25 to 0.48 because the ideological messaging increasingly performs the coordination story, obscuring the extraction beneath. The plateau in theater at t=35+ reflects the routinization of performance — the messaging becomes ambient, the elite adoption becomes normalized, and the constraint's performative work stabilizes. The measurement grid is shared across all three metrics: every metric is authored at every time point (t=0, 7, 14, 21, 28, 35, 42, 50) to prevent misalignment. The observed/projected distinction marks the empirical record (t=0-28) and future trajectory modeling (t=35+).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (colonial administration) and the urban elites experience the constraint as successful coordination: they observe rising compliance, prestige differentiation, and administrative efficiency. The rural populations and indigenous practitioners experience it as enforced extraction: they observe pressure to abandon functional practices, loss of professional authority, and exclusion from the ideological narrative. The hybrid practitioners sit between: they experience genuine quasi-endogenous pull in urban contexts and enforcement pressure in rural contexts, depending on where they are. The engine computes per-seat type from power + exit + beneficiary/victim structural data — the same constraint produces DIFFERENT types from different seats because directionality differs: urban elites get low d (beneficiaries, arbitrage exit) → low extraction; rural populations get high d (victims, constrained exit) → high extraction. The perspectival gap is NOT a measurement error — it is the POINT of the hybrid scaffolding mechanism: to create sufficiently differentiated experiences that the constraint looks like coordination from the beneficiary seats and extraction from the victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Urban elites are structural beneficiaries: they collect prestige, access to colonial institutions, and professional advancement without surrendering identity (they adopt enthusiastically). Their directionality is low d (0.15–0.25 range) because benefits outweigh costs and exit options are real (they COULD reject adoption and remain locally powerful). Colonial administration is the agenda-setter: d near beneficiary end because it collects authority and governance efficiency. Rural populations are the targets: high d (0.75–0.85 range) because they face enforcement pressure, identity disruption, and exclusion from benefits, with constrained exit (non-compliance carries penalties). Indigenous practitioners have higher d still (0.85+) because their identity is locked in the prior practice — exit would dissolve their professional role and social position. The directionality overrides should NOT be necessary here: the structural data (beneficiary/victim declarations + power + exit_options) should derive d values that reflect the actual power asymmetries. If they do not, an override declaring the true d would signal a derivation failure to the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows NO mandatrophy in the scaffolding reading: the mandate is that the new practice should displace the prior practice, and the scaffolding mechanism is specifically designed to achieve partial displacement efficiently. The constraint's persistence is explained by its extraction value to beneficiaries (colonial elites, indigenous elites who adopt, administrative machinery) — they have every incentive to maintain it. The constraint is not an atrophied function maintained theatrically; it is an actively maintained extraction mechanism whose theater ratio rises WITH enforcement intensity, not as a substitute for function. The mandatrophy alert might fire in the EXOGENOUS_OVERRIDE reading (pure decree that fails to achieve displacement and persists only because reversing it would require administrative effort) or in a degraded post-colonial reading where the original mandate has expired but the extraction persists. But in the hybrid_scaffolding reading, the mandate is live and the mechanism is actively maintained by beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffolding_internalization_boundary,
    'At what point does scaffolding-induced quasi-endogenous adoption become genuine internalized preference indistinguishable from endogenous climb?',
    'Cohort analysis tracking adoption rates across generations: does the second generation adopt with the same enthusiasm as the first (scaffolding still present), or do adoption rates decline without continued elite modeling and messaging (quasi-endogenous remains dependent on scaffolding)?',
    'If adoption declines in later generations without scaffolding reinvestment, the reading confirms that the mechanism is scaffolding-dependent extraction, not genuine internalization. If adoption persists and accelerates, the boundary dissolves and the reading converges toward endogenous_climb.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_internalization_boundary, empirical, 'Temporal boundary between scaffolding-dependent and genuinely internalized adoption.').

omega_variable(
    hybrid_practice_stability_vs_transition,
    'Are hybrid practices (wearing traditional dress at home, Western dress in town) stable equilibria or transition points toward complete displacement?',
    'Multi-generational household and community ethnography tracking whether hybrid practices persist across generations or eventually resolve toward pure adoption of the new practice.',
    'If hybrid practices are stable, the constraint''s ''partial displacement'' is permanent and the scaffolding mechanism is equilibrium-enforcing. If they are transition points, the reading underestimates the displacement and should model the long-term trajectory as higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_practice_stability_vs_transition, empirical, 'Whether hybrid practices represent stable coexistence or transient accommodation.').

omega_variable(
    coercion_internalization_mechanism,
    'Does suppression operate primarily as external enforcement (penalties, incentives) or does it function by encoding suppression into the target population''s internalized identity rules?',
    'Post-mandate relaxation study: if suppression is purely external, compliance should decline when enforcement pressure eases; if internalized, compliance persists after enforcement pressure relaxes.',
    'If internalized, the measured suppression underestimates the constraint''s true coercive depth — the target population has absorbed the enforcement as self-regulation. If external-only, the measured suppression is accurate and shows the constraint''s dependence on active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_internalization_mechanism, empirical, 'Locus of suppression: external enforcement versus internalized self-regulation.').

omega_variable(
    scaffolding_vs_exogenous_framing,
    'Can the scaffolding mechanism be distinguished analytically from the exogenous_override reading''s ''pure decree + enforcement'', or is the difference merely one of degree rather than kind?',
    'Controlled comparison of mandates with similar enforcement intensity but different scaffolding investment: do mandates with high scaffolding (elite modeling, ideological messaging) achieve higher displacement rates than mandates with high enforcement but low scaffolding, controlling for other variables?',
    'If scaffolding proves analytically separable and causally efficacious, the reading is vindicated as a distinct mechanism. If scaffolding effects disappear when enforcement is controlled, the distinction collapses and the exogenous_override reading is correct — decree + enforcement is sufficient, and scaffolding is a secondary phenomenon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffolding_vs_exogenous_framing, empirical, 'Whether scaffolding is an analytically distinct mechanism or reducible to enforcement intensity.').

omega_variable(
    reading_contest_empirical_status,
    'Which sibling reading (exogenous_override, endogenous_climb, hybrid_scaffolding) best explains actual cases of cultural practice displacement?',
    'Comparative-historical analysis of 10+ cases (colonial calendar imposition, dress code mandates, language shifts, religious practice restrictions) coding for outcome (complete displacement, partial displacement, failure), decree presence, elite adoption rate, ideological messaging intensity, enforcement pressure. Logistic regression or qualitative pattern analysis to determine which reading''s causal model fits best.',
    'If hybrid_scaffolding reading explains variation best, it should become the canonical reading; if exogenous or endogenous readings dominate, this reading should be reclassified as a special case or reformulated. If all three readings explain different clusters of cases, the kernel contest may be genuine (coexists_with relation is correct) rather than resolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_empirical_status, empirical, 'Empirical adjudication among the three sibling readings via comparative case analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t7, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 7, 0.32).
narrative_ontology:measurement_basis(legi_tr_t7, observed).
narrative_ontology:measurement(legi_tr_t14, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement_basis(legi_tr_t14, observed).
narrative_ontology:measurement(legi_tr_t21, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 21, 0.44).
narrative_ontology:measurement_basis(legi_tr_t21, observed).
narrative_ontology:measurement(legi_tr_t28, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 28, 0.48).
narrative_ontology:measurement_basis(legi_tr_t28, observed).
narrative_ontology:measurement(legi_tr_t35, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 35, 0.5).
narrative_ontology:measurement_basis(legi_tr_t35, projected).
narrative_ontology:measurement(legi_tr_t42, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 42, 0.49).
narrative_ontology:measurement_basis(legi_tr_t42, projected).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(legi_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t7, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement_basis(legi_be_t7, observed).
narrative_ontology:measurement(legi_be_t14, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 14, 0.48).
narrative_ontology:measurement_basis(legi_be_t14, observed).
narrative_ontology:measurement(legi_be_t21, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 21, 0.54).
narrative_ontology:measurement_basis(legi_be_t21, observed).
narrative_ontology:measurement(legi_be_t28, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 28, 0.57).
narrative_ontology:measurement_basis(legi_be_t28, observed).
narrative_ontology:measurement(legi_be_t35, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 35, 0.59).
narrative_ontology:measurement_basis(legi_be_t35, projected).
narrative_ontology:measurement(legi_be_t42, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 42, 0.58).
narrative_ontology:measurement_basis(legi_be_t42, projected).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(legi_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t7, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 7, 0.7).
narrative_ontology:measurement_basis(legi_su_t7, observed).
narrative_ontology:measurement(legi_su_t14, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 14, 0.67).
narrative_ontology:measurement_basis(legi_su_t14, observed).
narrative_ontology:measurement(legi_su_t21, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 21, 0.64).
narrative_ontology:measurement_basis(legi_su_t21, observed).
narrative_ontology:measurement(legi_su_t28, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 28, 0.62).
narrative_ontology:measurement_basis(legi_su_t28, observed).
narrative_ontology:measurement(legi_su_t35, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 35, 0.61).
narrative_ontology:measurement_basis(legi_su_t35, projected).
narrative_ontology:measurement(legi_su_t42, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 42, 0.62).
narrative_ontology:measurement_basis(legi_su_t42, projected).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(legi_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'legitimacy_of_imposed_practice'. The sibling readings (exogenous_override and endogenous_climb) model alternative causal mechanisms for practice displacement. The three readings coexist in contemporary discourse: postcolonial states model themselves on exogenous authority (decree-driven); indigenous movements emphasize endogenous cultural internalization; hybrid scaffolding is the mechanism observed in most historical cases of successful displacement. Each reading has its own ε value, beneficiary/victim structure, and enforcement profile. No single reading forecloses the others logically — they compete as explanatory frameworks rather than ruling each other out. The network edges link the readings so corpus analysis can track which reading dominates in different domains and how the contest evolves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, powerful, 0.18).
constraint_indexing:directionality_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
