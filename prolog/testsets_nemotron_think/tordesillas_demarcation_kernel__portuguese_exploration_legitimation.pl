% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__portuguese_exploration_legitimation, []).

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
 *   human_readable: Tordesillas Demarcation: Portuguese Exploration Legitimation
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The 1494 Treaty of Tordesillas, mediated by Pope Alexander VI, drew a
 *   meridian dividing the non-European world between Spain and Portugal. This
 *   reading treats the papal bull as confirmation of Portuguese prior
 *   exploration rights east of the line and as an exclusionary barrier
 *   against other European powers. The constraint operates through papal
 *   spiritual authority translated into legal title, enabling the Portuguese
 *   Estado da Índia to enforce a trade monopoly in the Indian Ocean and East
 *   Indies. The coordination function is the allocation of exploration zones
 *   to prevent conflict between the two Iberian signatories; the extraction
 *   function is the monopoly rent collected from excluded European rivals and
 *   from Asian trade networks forced into Portuguese-controlled routes. The
 *   constraint is a tangled rope: it genuinely coordinated Iberian claims
 *   while extracting from non-signatory powers and Asian merchants.
 *
 * KEY AGENTS:
 *   - portuguese_crown: Primary beneficiary (institutional/arbitrage) — receives monopoly rents, sets enforcement agenda
 *   - portuguese_estado_da_india: Beneficiary (organized/mobile) — operates the trade monopoly, collects revenue
 *   - spanish_crown: Payer for eastern zone (institutional/constrained) — excluded from east but compensated by western zone
 *   - dutch_republic: Payer (powerful/mobile) — excluded entirely, challenges by force and legal doctrine
 *   - english_crown: Payer (powerful/mobile) — excluded, later uses freedom of the seas argument
 *   - french_crown: Payer (powerful/mobile) — excluded, ignores treaty from outset
 *   - papacy: Agenda setter (institutional/analytical) — issues bull, claims spiritual authority over temporal division
 *   - indigenous_peoples: Excluded (powerless/trapped) — no voice in treaty, bear indirect extraction via monopoly pricing
 *   - asian_merchants: Payer (organized/constrained) — forced into Portuguese-controlled routes, pay higher prices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.45).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.7).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.45).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Demarcation: Portuguese Exploration Legitimation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '41647a12-3d35-4a03-bdb2-d37bef15c6fa').
narrative_ontology:cs_kernel_codification('41647a12-3d35-4a03-bdb2-d37bef15c6fa', formalized).
narrative_ontology:cs_authority_grounding('41647a12-3d35-4a03-bdb2-d37bef15c6fa', lineage).
narrative_ontology:cs_interpretation_layer_present('41647a12-3d35-4a03-bdb2-d37bef15c6fa').
narrative_ontology:cs_reading_relation('41647a12-3d35-4a03-bdb2-d37bef15c6fa', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('41647a12-3d35-4a03-bdb2-d37bef15c6fa', foundational, portuguese_exploration_priority_east_of_line).
narrative_ontology:cs_axiom_status(portuguese_exploration_priority_east_of_line, holdable).
narrative_ontology:cs_axiom_grounding('41647a12-3d35-4a03-bdb2-d37bef15c6fa', portuguese_exploration_priority_east_of_line, conventional).
narrative_ontology:cs_axiom('41647a12-3d35-4a03-bdb2-d37bef15c6fa', secondary, exclusion_of_rival_europeans_from_east).
narrative_ontology:cs_axiom_status(exclusion_of_rival_europeans_from_east, holdable).
narrative_ontology:cs_axiom_grounding('41647a12-3d35-4a03-bdb2-d37bef15c6fa', exclusion_of_rival_europeans_from_east, conventional).
narrative_ontology:cs_reference_frame('41647a12-3d35-4a03-bdb2-d37bef15c6fa', treaty_as_binding_law_of_nations).
narrative_ontology:cs_drift_state('41647a12-3d35-4a03-bdb2-d37bef15c6fa', contemporary_early_modern, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('41647a12-3d35-4a03-bdb2-d37bef15c6fa', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, spanish_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_republic).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_crown).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, asian_merchants).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_authority_over_new_world).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, treaty_as_law_of_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the treaty, receives the eastern zone as confirmation of prior exploration. Collects 20% royal fifth on all trade revenue from Estado da Índia. Can shift capital and diplomatic focus globally; exit from the constraint would mean abandoning the legal basis for their monopoly, which they never do.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter).

% Operates the carrack fleet, fortified factories, and customs houses that enforce the monopoly. Collects the revenue that flows to the Crown. Officials can rotate posts or return to Portugal; the institution itself is mobile across the Indian Ocean but dependent on the treaty's legal cover.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, beneficiary,
    organized, biographical, mobile, continental).

% Signatory to the treaty; receives the western zone. In this reading, they are excluded from the eastern zone and cannot legally trade or settle there. Their exit is constrained by the treaty they signed — challenging it risks the western zone. They occasionally sponsor Portuguese voyages to the east (e.g., Magellan) but mostly accept the division.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, spanish_crown, payer,
    institutional, generational, constrained, global).

% Non-signatory rising naval power. Rejects papal authority and the treaty. Enters the Indian Ocean by force (1595 onward), seizes Portuguese forts, establishes VOC. Their exit from the constraint is military and legal (Grotius's mare liberum). They bear the cost of exclusion until they break the monopoly.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_republic, payer,
    powerful, biographical, mobile, global).

% Non-signatory. Initially seeks northwest passage to avoid treaty zones. Later establishes EIC (1600) and challenges Portuguese in India. Uses legal arguments against papal donation. Exit is mobile — they can and do ignore the treaty, but pay in conflict costs until Portuguese power wanes.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_crown, payer,
    powerful, biographical, mobile, global).

% Non-signatory. Francis I famously demanded to see 'the clause in Adam's will' excluding France. Establishes French East India Company (1664). Ignores treaty from outset; exit is immediate non-recognition. Bears costs only in competitive disadvantage until they establish their own presence.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_crown, payer,
    powerful, biographical, mobile, global).

% Issues the bull Inter caetera (1493) and ratifies Tordesillas (1494). Claims spiritual authority to divide temporal dominion. Does not collect revenue but gains diplomatic leverage over Catholic monarchs. Exit is analytical — the Papacy never abandons the claim, but its enforcement capacity evaporates after the Reformation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy, agenda_setter,
    institutional, civilizational, analytical, universal).

% No voice in treaty negotiations. Bear indirect extraction: Portuguese monopoly raises prices for Asian goods, disrupts existing trade networks, and enables militarized factories that extract tribute. In this reading they are not the primary target (unlike the Spanish reading), but they are structurally excluded from any benefit and bear downstream costs.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_peoples, excluded,
    powerless, generational, trapped, regional).

% Gujarati, Arab, Chinese, and Southeast Asian merchants forced to buy Portuguese cartazes (passes) or face seizure. Some collaborate, some resist, some shift routes. Exit is constrained by Portuguese naval control of chokepoints (Hormuz, Malacca). They pay the monopoly markup but retain some agency through evasion and bribery.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, asian_merchants, payer,
    organized, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocated exploration and trade zones between the two Iberian powers to prevent military conflict over newly encountered lands, providing a legal framework for discovery claims.
% TRANSFER_FUNCTION: Moves monopoly rent from excluded European competitors and Asian trading networks to the Portuguese Crown and Estado da Índia, via enforced exclusion from the eastern zone and mandatory cartaz system.
% ABSENT_VOICES: Indigenous rulers and Asian merchant communities had no representation at Tordesillas. The Pope claimed authority over their lands without consultation. Non-Iberian European powers (France, England, Netherlands) were excluded from the negotiation and never consented.
% DISAPPEARANCE_RATIONALE: If the treaty vanished in 1500, the Portuguese monopoly would lose its legal title, opening the Indian Ocean to unrestricted European competition decades earlier. The Estado da Índia would lose its primary revenue basis. The law of nations would develop without the papal donation precedent, altering colonial legal history.
% FOUNDING_PROBLEM: After Columbus's 1492 voyage, Spain and Portugal faced imminent conflict over competing claims in the Atlantic and Indian Oceans. The papal mediation was sought to prevent war between the two Catholic powers.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Iberian conflict prevention) is attested as dead by multiple independent historians (e.g., Boxer, Subrahmanyam) who note that by 1580 the Iberian Union made the division moot, and by 1600 non-Iberian powers had rendered it obsolete. The Portuguese Crown's own correspondence shifts from conflict-avoidance to monopoly-defense language by 1520.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).
:- end_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the monopoly generates significant revenue but is partially offset by enforcement costs and leakage. Suppression is high (0.7) because maintaining the monopoly requires naval patrols, fortification, and legal-diplomatic efforts against rival powers. Theater ratio starts low (0.15) when the treaty reflects actual power, but rises as the treaty becomes a legal fiction ignored by rising powers. Accessibility collapse is moderate (0.6): once the treaty is accepted as law of nations, alternative legal frameworks for open access are marginalized, though de facto alternatives (piracy, privateering) persist. Resistance is high (0.7) from non-signatory European powers who never accepted papal authority over their sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the Portuguese seat, the constraint is a rope: it coordinates with Spain and legitimates their monopoly. From the Dutch/English/French seats, it is a snare: an illegitimate exclusion enforced by superior naval power. From the Papal seat, it is a mountain: a divine allocation of stewardship. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Portuguese Crown and Estado da Índia are structural beneficiaries (d near 0.0) — they collect the monopoly rent and control the enforcement apparatus. Spanish Crown is a payer for the eastern zone (d near 0.7) but a beneficiary for the western zone; in this reading we treat them as a victim of the eastern exclusion. Non-signatory European powers are payers (d near 0.9) — they bear the full cost of exclusion with no compensating benefit. Asian merchants are payers (d near 0.8) — they face monopoly pricing but have some exit via alternative ports. Indigenous peoples are excluded (d not computed) — they are not parties to the constraint but bear downstream effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing Iberian conflict over new discoveries) was live in 1494 but dead by 1600 as other powers rendered the bilateral division obsolete. The constraint persisted because it became the legal basis for Portuguese monopoly extraction — a classic mandatrophy where the coordination function atrophied but the extraction function intensified. The treaty's transformation from coordination tool to extraction instrument is documented in the rising theater ratio and shifting suppression targets from Iberian compliance to non-European resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the papal treaty function as a genuine coordination mechanism allocating exploration zones, or as a cover for Portuguese trade monopoly extraction?',
    'Comparative analysis of Portuguese Estado da Índia revenue records vs. enforcement costs; counterfactual modeling of rival European entry absent the treaty.',
    'If coordination, the constraint is a rope with moderate extraction; if cover, it is a snare with the treaty as legitimation theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the treaty''s primary structure is coordination or extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of rival European powers structural (naval patrols, legal claims) or internalized (acceptance of papal authority as binding)?',
    'Track compliance of non-signatory powers: if they ignore the treaty without internalized guilt, suppression is structural; if they seek papal dispensation, internalized.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint carries its enforcement inside the agents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of rival European powers.').

omega_variable(
    indigenous_population_exclusion,
    'Are indigenous populations entirely absent from the victim set of this reading, or do they bear indirect extraction through the trade monopoly?',
    'Quantify extractive flows from indigenous labor and resources into the Portuguese trade monopoly enabled by the treaty''s exclusion of rivals.',
    'If indirect extraction is substantial, the victim set expands and the constraint may reclassify toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_population_exclusion, empirical, 'Whether indigenous populations are indirect victims of the trade monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tordesillas_portuguese_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.15).
narrative_ontology:measurement(tordesillas_portuguese_tr_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1520, 0.2).
narrative_ontology:measurement(tordesillas_portuguese_tr_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1550, 0.25).
narrative_ontology:measurement(tordesillas_portuguese_tr_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1600, 0.35).
narrative_ontology:measurement(tordesillas_portuguese_tr_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1650, 0.45).
narrative_ontology:measurement(tordesillas_portuguese_tr_t1700, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1700, 0.55).
narrative_ontology:measurement(tordesillas_portuguese_tr_t1800, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1800, 0.7).

% Extraction over time
narrative_ontology:measurement(tordesillas_portuguese_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.25).
narrative_ontology:measurement(tordesillas_portuguese_be_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1520, 0.4).
narrative_ontology:measurement(tordesillas_portuguese_be_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1550, 0.5).
narrative_ontology:measurement(tordesillas_portuguese_be_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1600, 0.48).
narrative_ontology:measurement(tordesillas_portuguese_be_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1650, 0.42).
narrative_ontology:measurement(tordesillas_portuguese_be_t1700, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1700, 0.35).
narrative_ontology:measurement(tordesillas_portuguese_be_t1800, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1800, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(tordesillas_portuguese_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.8).
narrative_ontology:measurement(tordesillas_portuguese_su_t1520, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1520, 0.75).
narrative_ontology:measurement(tordesillas_portuguese_su_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1550, 0.7).
narrative_ontology:measurement(tordesillas_portuguese_su_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1600, 0.6).
narrative_ontology:measurement(tordesillas_portuguese_su_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1650, 0.5).
narrative_ontology:measurement(tordesillas_portuguese_su_t1700, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(tordesillas_portuguese_su_t1800, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1800, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.15).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india_monopoly).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_east_india_company_formation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, freedom_of_the_seas_doctrine).

% DUAL FORMULATION NOTE:
% This constraint and its sibling 'spanish_conquest_legitimation' form a constraint family decomposing the Tordesillas kernel. The Portuguese reading has lower extractiveness (trade monopoly) and victims = rival Europeans; the Spanish reading has higher extractiveness (land conquest, indigenous subjugation) and victims = indigenous populations. Both share the same treaty text but instantiate different constraints with different ε, beneficiary/victim sets, and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, institutional, 0.15).
constraint_indexing:directionality_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
