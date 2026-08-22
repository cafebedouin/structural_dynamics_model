% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_portuguese_exploration, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Treaty of Tordesillas—Portuguese Exploration Legitimation Reading
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The Treaty of Tordesillas (1494) represented papal division of
 *   newly-discovered territories between Portuguese and Spanish crowns,
 *   justified as confirmation of prior exploration rights and prevention of
 *   European rivalry. This reading interprets the constraint as a tangled
 *   rope: genuine coordination among European powers (shared rules, dispute
 *   adjudication) coupled with asymmetric extraction from excluded rivals
 *   through enforced trading monopoly. The constraint operates on the
 *   inter-institutional level (European monarchies) rather than on indigenous
 *   populations as primary targets—though indigenous exclusion is a
 *   structural consequence. The measurement series show rising extractiveness
 *   and theater through the 16th century (1494–1630), peaking as Portuguese
 *   monopoly enforcement hardened, then declining (1630–1650) as rival routes
 *   proliferated and enforcement capacity eroded, marking the constraint's
 *   functional decay while formal obligation persisted.
 *
 * KEY AGENTS:
 *   - portuguese_estado_da_india: institutional agenda-setter; collects monopoly rents from eastern trade
 *   - rival_european_powers (Spain west, France/England north): institutional payers; excluded from prime routes by treaty obligation
 *   - papal_authority: institutional co-agenda-setter; vindicates the division and arbitrates disputes
 *   - indigenous_rulers_and_traders: organized but excluded; their objections were unrepresented
 *   - portuguese_merchant_interests: powerful beneficiaries; realize monopoly profits
 *   - european_consumers: powerless payers; bear cost as monopoly markup on spices
 *   - analytical_observer: reads the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.71).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.62).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Treaty of Tordesillas—Portuguese Exploration Legitimation Reading").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'c18275f5-bf45-4deb-8d90-80f663fe21a5').
narrative_ontology:cs_kernel_codification('c18275f5-bf45-4deb-8d90-80f663fe21a5', formalized).
narrative_ontology:cs_authority_grounding('c18275f5-bf45-4deb-8d90-80f663fe21a5', extraction).
narrative_ontology:cs_interpretation_layer_present('c18275f5-bf45-4deb-8d90-80f663fe21a5').
narrative_ontology:cs_reading_relation('c18275f5-bf45-4deb-8d90-80f663fe21a5', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('c18275f5-bf45-4deb-8d90-80f663fe21a5', foundational, prior_exploration_grounds_monopoly_exclusion).
narrative_ontology:cs_axiom_status(prior_exploration_grounds_monopoly_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('c18275f5-bf45-4deb-8d90-80f663fe21a5', prior_exploration_grounds_monopoly_exclusion, instrumental).
narrative_ontology:cs_axiom('c18275f5-bf45-4deb-8d90-80f663fe21a5', foundational, papal_authority_coordinates_christian_monarchies).
narrative_ontology:cs_axiom_status(papal_authority_coordinates_christian_monarchies, holdable).
narrative_ontology:cs_axiom_grounding('c18275f5-bf45-4deb-8d90-80f663fe21a5', papal_authority_coordinates_christian_monarchies, deontological).
narrative_ontology:cs_reference_frame('c18275f5-bf45-4deb-8d90-80f663fe21a5', papal_authority_divides_earth_exploration_confirms_monopoly).
narrative_ontology:cs_drift_state('c18275f5-bf45-4deb-8d90-80f663fe21a5', post_1630_alternative_routes_maturation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c18275f5-bf45-4deb-8d90-80f663fe21a5', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_merchant_interests).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, european_consumers).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, european_consumers).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_authority_divides_earth).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prior_exploration_grounds_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Portuguese Crown and its maritime exploration apparatus. Sought papal confirmation of exploration rights in the Indian Ocean and eastern routes to secure trading monopoly against Spanish and other European rivals. Operates through licensed trading forts and maritime patrols; benefits from exclusive access to spice routes, Indian commerce, and established trading relationships east of the demarcation line. Justifies the monopoly as purchased through capital investment in dangerous exploration and maintenance of fortified trading stations.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, agenda_setter,
    institutional, generational, arbitrage, global).

% Spain, France, England, and other European maritime powers. Bear the cost of the demarcation through exclusion from eastern trade routes and the Indian Ocean basin, losing profitable trading opportunities they might otherwise pursue. Their exit option is geographical—they can redirect exploration westward (for Spain) or northward (for others), but the highest-value routes are foreclosed. Accept the constraint through treaty obligation and papal authority, though with mounting resistance as exploration technology improves and competing claims grow.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    powerful, generational, constrained, global).

% Ruling classes and merchant networks of India, Persia, Arabia, Southeast Asia, and East Africa. Are not parties to the demarcation treaty and have no voice in European discussions of their own territories and trade routes. The exclusion from the negotiation reflects their non-recognition as sovereign treaty partners; their actual ability to resist Portuguese monopoly is substantial but unrepresented in the formal constraint structure.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_rulers_and_traders, excluded,
    organized, generational, trapped, regional).

% The Catholic Church and Papal Curia. Issue the treaty as a formalization of divine authority to divide the earth among Christian princes based on discovery and conversion claims. Derives authority and vindicates theological claims about papal supremacy over temporal rulers and non-Christian lands. Benefits from the religious legitimacy premium attached to the division and from the commitment of Catholic monarchs to papal arbitration of territorial claims.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_authority, agenda_setter,
    institutional, generational, arbitrage, universal).

% Private trading companies, spice merchants, and financial backers of Portuguese voyages. Realize monopoly profits from exclusive access to eastern routes: Indian pepper, cloves, nutmeg, and silk command premium prices in European markets when supply is controlled. Individual merchants can relocate or diversify, but the class collectively benefits from the enforced monopoly and Crown protection.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_merchant_interests, beneficiary,
    powerful, biographical, mobile, global).

% European populations who consume spices and eastern goods. Pay elevated prices due to the monopoly—spices remain luxury goods rather than becoming mass-market commodities as would occur under competitive supply. They do benefit from increased availability and reduced scarcity relative to the pre-demarcation period of fragmented routes, but pay a monopoly markup for that improvement.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, european_consumers, payer,
    powerless, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, european_consumers, beneficiary).

% Historians, legal scholars, and international relations analysts examining the constraint from outside its operation. Assess whether the demarcation represents genuine coordination among European powers, extraction from excluded rivals, or a cover story for territorial conquest and indigenous subjugation disguised as orderly exploration.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of European maritime powers competing for the same routes and territories without a common adjudicator. Creates a uniform rule (papal division + prior exploration = monopoly right) and a dispute-resolution mechanism (papal arbitration) that prevents chaotic warfare over newly-discovered territories and converts exploration races into sanctioned monopoly assignments.
% TRANSFER_FUNCTION: Moves exclusive trading rights and territorial claim primacy from the open competition pool to Portuguese Estado da Índia in exchange for Portuguese acceptance of Spanish primacy west of the line and Portuguese commitment to papal arbitration of subsequent disputes. Extracts opportunity cost from rival European powers (lost access to eastern markets) and from indigenous rulers (who receive no compensation for the assignment of their territories to European monopoly).
% ABSENT_VOICES: Indigenous rulers, merchants, and populations of Africa, India, Southeast Asia, and the Pacific are completely absent from the treaty negotiation and have no seat at the table. They would object to being treated as non-sovereign territory ripe for European division and monopoly; their absence reflects and reinforces their non-recognition as treaty partners.
% DISAPPEARANCE_RATIONALE: If the demarcation and its enforcement vanished, rival European powers would immediately pursue eastern exploration and trade; spice prices would fall toward competitive levels; Portuguese monopoly rents would collapse; and indigenous rulers would regain negotiating power over trading relationships. The global distribution of maritime power and merchant capital would reorganize around open competition rather than sanctioned monopoly.
% FOUNDING_PROBLEM: European maritime powers competed chaotically for newly-discovered routes and territories without common rules or adjudicator, creating naval conflict risk, unclear territorial claims, and repeated disputes over priority. Papal authority and prior-exploration doctrine offered a neutral framework for assigning monopoly rights and reducing violent competition among Christian monarchs.
% FOUNDING_PROBLEM_CORROBORATION: Portuguese and Spanish crowns assert the founding problem remains live: disputes over exploration priority and claims still occur in new regions (Africa, Pacific). Rival powers (England, France) testify that the founding problem was substantially solved but the arrangement persists as rent extraction—the 16th and 17th century expansion of alternative routes and rival trading posts demonstrates that the coordination function became obsolete while the monopoly enforcement continued. Indigenous rulers and post-colonial scholars document that the founding problem was never genuinely their problem and that the constraint served only European interests.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.48) at treaty ratification because the coordination function is real—the rule does prevent chaotic competition—but the monopoly extraction is already present. It rises to 0.72 by 1630 as Portuguese enforcement tightens (maritime patrol, fort consolidation, exclusive trading compacts with Indian rulers) and the theater ratio grows (security justifications intensify even as trade function stabilizes). The peak reflects the constraint's maximum coercive reach: rival powers are actively excluded, compliance is compulsory by treaty, and enforcement is centralized. The decline to 0.62 by 1650 reflects the beginning of constraint erosion: Dutch, English, and French traders are establishing rival routes (Cape passage, northern routes), Portuguese maritime capacity is stretched thin, and the founding coordination problem becomes visibly obsolete. Suppression remains high throughout because active enforcement is required at every point—rival powers do not passively accept exclusion; they test the boundary with circumnavigation and contraband trading. Theater ratio shows the growing performative share: security reviews of Portuguese trading posts and maritime patrols become more elaborate even as their actual function in maintaining the monopoly declines relative to treaty obligation and papal authority.
 *
 * PERSPECTIVAL GAP:
 *   From the Portuguese institutional seat, the arrangement is defensive coordination: they invested capital in exploration and sought guaranteed return on that investment; the treaty is a recognition of prior accomplishment and legitimate claim. From the rival European institutional seats, the arrangement is enforced extraction: they are excluded from profitable routes not by Portuguese superiority but by papal fiat and treaty obligation; their exit option is geographical redirect rather than competitive entry. From indigenous rulers' seats (excluded from the stakeholder layer because they are not parties to the constraint), the arrangement is pure external extraction: their territories and trade routes are assigned to European monopoly without their voice or compensation. The engine computes per-seat directionality from power and exit options; these structural differences should produce divergent type classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Portuguese Estado da Índia: d ≈ 0.05 (full beneficiary). Collects monopoly rents, sets agenda, controls enforcement, has arbitrage-grade exit (could shift to Atlantic trade but chose Indian Ocean). Powerful institutional actor with generational time horizon; beneficiary status derives from collection of extraction surplus. Rival European powers: d ≈ 0.85 (near-target). Excluded from high-value routes, constrained by treaty obligation, identity-locked into Christian monarchy framework where papal arbitration binds (exit would require rejecting Christendom status or accepting papal authority failure). Institutional power and generational horizon do not reduce d because the constraint's cost structure captures the gains that would otherwise flow to them. Indigenous rulers: d ≈ 1.0 (full target) but excluded from stakeholders because they are not parties to this constraint—the demarcation constraint structures only European competitive relationships; indigenous extraction flows through a separate constraint (Portuguese territorial conquest constraint, not authored here per the ε-invariance principle and kernel decomposition). Papal authority: d ≈ 0.25 (beneficiary). Vindicates papal supremacy, derives authority premium from successful arbitration, but collects no rents directly and faces mounting delegitimacy as exploration challenges theological claims about earth division.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (European maritime competition for newly-discovered routes) was genuine in 1494 and the constraint provided real coordination benefit. By 1580, alternative routes around Africa and northward through Arctic exploration were being tested; by 1600, Dutch and English trading companies were established rivals; by 1630, Portuguese monopoly enforcement required escalating suppression (theater ratio rising) against mounting resistance (English privateering, Dutch VOC establishment, French trading posts). The constraint persists as theater and treaty obligation long after the coordination problem is functionally solved. This is not classic mandatrophy (founding problem dead + constraint persists) because the founding problem remains contested—European powers still claim Portuguese route-priority disputes arise—and because Portuguese enforcement machinery genuinely works to maintain the monopoly, not merely to maintain the appearance of enforcement. However, the rising theater ratio and the accumulating resistance against declining coordination function mark the constraint at the boundary between tangled_rope (active enforcement serving both coordination and extraction) and piton (enforcement becoming performative as coordination function atrophies). The measurement series support this boundary state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_versus_european_sovereignty,
    'Does the papal treaty establish papal authority to divide the earth and bind Christian monarchs, or is it merely a formalization of preexisting power relationships that the monarchs accepted instrumentally?',
    'Trace the frequency and pattern of subsequent European appeals to papal arbitration for territorial disputes: if European powers consistently invoke papal authority as binding, the authority is real; if they repeatedly ignore or defy papal rulings when interests diverge, papal authority is performative cover for power negotiation.',
    'If papal authority is real, the constraint''s enforcement chain runs through church legitimacy and would break if papal authority collapsed; if performative, the constraint is really a power-negotiated monopoly wearing a religious costume, and its persistence depends on European power stability, not religious legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_versus_european_sovereignty, empirical, 'Whether papal authority in the Tordesillas division is substantive or performative').

omega_variable(
    prior_exploration_versus_conquest_license,
    'Is the treaty fundamentally about confirming prior exploration rights (Portuguese reading) or about granting territorial conquest license to both powers (Spanish reading), or do both interpretations inhabit the same document simultaneously?',
    'Comparative analysis of papal bulls before and after Tordesillas, statements from Portuguese and Spanish crowns about what the treaty authorized, and subsequent enforcement patterns: do powers cite it to defend monopoly (exploration reading) or to justify territorial subjugation (conquest reading)?',
    'If the treaty carries fundamentally different meaning for the two readings, they may not coexist coherently in a single constraint—the ε-invariance principle may require decomposition into two separate constraints with different victim sets and mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prior_exploration_versus_conquest_license, conceptual, 'Whether the kernel admits multiple non-equivalent readings or conflates two structurally distinct constraints').

omega_variable(
    indigenous_sovereignty_recognition,
    'Did indigenous rulers and traders acknowledge papal authority and the demarcation, or was the constraint imposed on them without their participation or consent?',
    'Historical records of indigenous reactions, treaties, and resistance: if indigenous rulers negotiated with the demarcation''s terms, it is a constraint they are party to (though excluded from authorship); if they received no notice and had no choice, the constraint is pure imposition and the victim set is indigenous rulers, not just rival European powers.',
    'If indigenous rulers were aware but constrained, they belong in the stakeholders[] layer with roles like ''excluded'' or ''payer''; if they received no notice, they belong in an entirely separate constraint (Portuguese territorial conquest and subjugation) that is a downstream effect rather than a direct target of the demarcation constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, empirical, 'Scope of indigenous awareness and agency in relation to the demarcation').

omega_variable(
    reading_foreclosure_versus_coexistence,
    'Do the Portuguese exploration legitimation reading and the Spanish conquest legitimation reading logically foreclose each other, or do they coexist as two live interpretations of the same kernel?',
    'Test whether a unified European framework could hold both readings: if one reading''s core claim directly contradicts the other''s (e.g., papal authority is limited to exploration coordination vs. papal authority extends to territorial conquest), they foreclose; if they are held by different parties with different interests (Portuguese chose exploration framing, Spanish chose conquest framing), they coexist.',
    'If they foreclose, one reading should be marked ''forecloses'' in reading_relations; if they coexist, the relation is ''coexists_with''. This affects how the engine models the kernel''s trajectory: foreclosure implies terminal resolution of the dispute; coexistence implies stable ideological contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_versus_coexistence, conceptual, 'Logical relationship between the Portuguese and Spanish readings of the demarcation kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.12).
narrative_ontology:measurement_basis(tord_tr_t1494, observed).
narrative_ontology:measurement(tord_tr_t1530, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1530, 0.15).
narrative_ontology:measurement_basis(tord_tr_t1530, observed).
narrative_ontology:measurement(tord_tr_t1570, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1570, 0.22).
narrative_ontology:measurement_basis(tord_tr_t1570, observed).
narrative_ontology:measurement(tord_tr_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1600, 0.28).
narrative_ontology:measurement_basis(tord_tr_t1600, observed).
narrative_ontology:measurement(tord_tr_t1630, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1630, 0.35).
narrative_ontology:measurement_basis(tord_tr_t1630, observed).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1650, 0.28).
narrative_ontology:measurement_basis(tord_tr_t1650, observed).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.48).
narrative_ontology:measurement_basis(tord_be_t1494, observed).
narrative_ontology:measurement(tord_be_t1530, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1530, 0.55).
narrative_ontology:measurement_basis(tord_be_t1530, observed).
narrative_ontology:measurement(tord_be_t1570, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1570, 0.62).
narrative_ontology:measurement_basis(tord_be_t1570, observed).
narrative_ontology:measurement(tord_be_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1600, 0.68).
narrative_ontology:measurement_basis(tord_be_t1600, observed).
narrative_ontology:measurement(tord_be_t1630, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1630, 0.72).
narrative_ontology:measurement_basis(tord_be_t1630, observed).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1650, 0.62).
narrative_ontology:measurement_basis(tord_be_t1650, observed).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.55).
narrative_ontology:measurement_basis(tord_su_t1494, observed).
narrative_ontology:measurement(tord_su_t1530, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1530, 0.62).
narrative_ontology:measurement_basis(tord_su_t1530, observed).
narrative_ontology:measurement(tord_su_t1570, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1570, 0.68).
narrative_ontology:measurement_basis(tord_su_t1570, observed).
narrative_ontology:measurement(tord_su_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement_basis(tord_su_t1600, observed).
narrative_ontology:measurement(tord_su_t1630, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1630, 0.78).
narrative_ontology:measurement_basis(tord_su_t1630, observed).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1650, 0.71).
narrative_ontology:measurement_basis(tord_su_t1650, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.18).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_maritime_monopoly_enforcement).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_territorial_subjugation_iberian_americas).

% DUAL FORMULATION NOTE:
% The Tordesillas demarcation kernel has been decomposed into two constraint stories following the ε-invariance principle: portuguese_exploration_legitimation (this story, focusing on inter-European monopoly coordination with rival powers as victims) and spanish_conquest_legitimation (sibling reading, focusing on territorial conquest and indigenous subjugation). The readings share the same kernel (the papal treaty text) but differ fundamentally in what the treaty is claimed to establish, whose interests it serves, and which victim set bears extraction. They are linked as 'coexists_with' readings held by different parties rather than foreclosed alternatives. The constraint families diverge downstream: Portuguese reading feeds into maritime monopoly enforcement and trade monopoly; Spanish reading feeds into territorial conquest and indigenous subjugation. Each story carries its own ε, stakeholder analysis, and beneficiary/victim structure. Sibling readings must be studied together for full kernel comprehension; this story alone captures only the Portuguese institutional reading and should not be used for analysis of conquest or indigenous subjugation (separate constraints with separate stories).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
