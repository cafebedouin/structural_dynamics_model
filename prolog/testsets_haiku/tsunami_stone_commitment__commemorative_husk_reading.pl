% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__commemorative_husk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: tsunami_stone_commitment__commemorative_husk_reading
 *   human_readable: Tsunami Stone Commitment as Commemorative Husk (Symbolic Artifact Reading)
 *   domain: disaster_anthropology/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the commemorative_husk_reading of a
 *   contested kernel: the meaning and force of tsunami stone inscriptions
 *   (石碑) erected in Edo-period and Meiji-era Japan as warnings against
 *   catastrophic coastal flooding. Under this reading, the stones decay from
 *   live behavioral constraints—actively interpreted and transmitted across
 *   generations as guides for evacuation and settlement patterns—into
 *   symbolic artifacts whose primary function becomes ceremonial
 *   commemoration and historical tourism rather than protection. The
 *   inscription persists but its normative force erodes: future generations
 *   inherit the stone as monument to past disasters rather than as actionable
 *   warning. Compliance with the warning (avoidance of hazard zones for
 *   settlement) becomes coincidental to economic logic rather than enforced
 *   through intergenerational norm transmission. Development interests
 *   benefit from this decay because it allows coastal land to be
 *   commercialized without the friction of active safety constraints. Future
 *   generations face extraction: they inherit the theatrical marker of hazard
 *   awareness (the stone itself) while occupying newly built structures in
 *   precisely the zones the stone was meant to protect against. The 2011
 *   Tōhoku tsunami provided empirical test for the readings: communities
 *   where stone-based norms remained behaviorally live showed markedly lower
 *   mortality in the inundation zones; communities where the stone had
 *   decayed to symbol showed casualties consistent with development-driven
 *   displacement of residents into hazard zones.
 *
 * KEY AGENTS:
 *   - coastal_development_interests: Investors, planners, and municipal governments seeking to commercialize hazard-zone land; benefit from the stone's transformation into monument rather than enforcer
 *   - pre-1900_stone_erecting_communities: Descendants of those who encoded the warning; potential knowledge-holders for transmitted norms, though intergenerational transmission often interrupted
 *   - post-1950_coastal_residents: Populations resettled or built into former hazard zones by economic development; bear extraction via non-protection
 *   - scholars_and_preservation_advocates: Claim the behavioral_competence_reading; interpret stone decay as loss rather than transformation
 *   - anthropological_observer: Sits outside both framings; measures which reading the community instantiates via ethnographic observation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, 0.82).
domain_priors:suppression_score(tsunami_stone_commitment__commemorative_husk_reading, 0.71).
domain_priors:theater_ratio(tsunami_stone_commitment__commemorative_husk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__commemorative_husk_reading, "Tsunami Stone Commitment as Commemorative Husk (Symbolic Artifact Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__commemorative_husk_reading, "disaster_anthropology/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__commemorative_husk_reading, '24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081').
narrative_ontology:cs_kernel_codification('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', fixed_text).
narrative_ontology:cs_authority_grounding('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', lineage).
narrative_ontology:cs_interpretation_layer_present('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081').
narrative_ontology:cs_reading_relation('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', tsunami_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', foundational, stone_as_historical_artifact).
narrative_ontology:cs_axiom_status(stone_as_historical_artifact, holdable).
narrative_ontology:cs_axiom_grounding('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', stone_as_historical_artifact, conventional).
narrative_ontology:cs_axiom('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', foundational, behavioral_norm_transmission_attenuated).
narrative_ontology:cs_axiom_status(behavioral_norm_transmission_attenuated, holdable).
narrative_ontology:cs_axiom_grounding('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', behavioral_norm_transmission_attenuated, empirically_contingent).
narrative_ontology:cs_reference_frame('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', memorial_commemoration_framework).
narrative_ontology:cs_drift_state('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', contemporary_post_1950_development_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24e1a7ac-fceb-41e6-bb7b-8f00c1d8a081', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, pre_1900_stone_erecting_communities).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, knowledge_holder_descendants).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, pre_1900_stone_erecting_communities).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, post_resettlement_coastal_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Municipal governments, real-estate investors, and infrastructure planners who control or influence coastal land-use decisions. They benefit from reframing the stone as historical monument rather than active safety constraint, as it allows commercialization of hazard-zone land without friction from enforced settlement restrictions. They set the framing narrative and control which interpretation of the stone gains institutional legitimacy—steering toward commemoration and away from behavioral transmission. They could enforce the stone-based norm if they chose; instead they engineer its transformation into symbol.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_interests, agenda_setter,
    institutional, biographical, arbitrage, national).

% Descendants of the fishing and farming communities that erected the stones as warnings based on lived experience of prior tsunamis. They hold cultural authority to interpret the stones and maintain the tradition; they are beneficiaries of the commemorative framing insofar as it elevates the stone to historical importance and attracts scholarly attention. They also bear extraction in that their authority is ceremonial without enforcement power—the stone is kept alive as symbol but the lived norm (settlement pattern guidance) has been displaced.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, pre_1900_stone_erecting_communities, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__commemorative_husk_reading, pre_1900_stone_erecting_communities, payer).

% Populations resettled or newly built into the very hazard zones the stones were meant to restrict. They inhabit land marketed as safe or desirable precisely because the stone's behavioral force has eroded—they experience the extraction directly as hazard exposure. They cannot exit (sunk costs in housing, local employment, kinship ties). They inherit both the stone as marker that hazard was known and the hazard itself because enforcement of the restriction has atrophied. The 2011 tsunami disproportionately killed residents in these zones.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, post_resettlement_coastal_residents, payer,
    powerless, immediate, trapped, local).

% Family and community lineages that maintain specialized knowledge of stone placement, original meaning, and the behavioral norms encoded in the inscriptions. They benefit from the scholarly revaluation of the stones as historical artifacts (recognition, cultural authority). They also bear extraction through the loss of enforceability—their knowledge is treated as folklore rather than actionable guidance.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, knowledge_holder_descendants, beneficiary,
    moderate, generational, identity_locked, local).

% Academics, preservation societies, and advocacy groups that study and promote the behavioral_competence_reading. They frame stone decay as loss and advocate for transmission of the original safety-norm interpretation. They have no direct stake in settlement or development but hold epistemic authority to interpret the stones' meaning. They remain external to the extraction structure but generate pressure to reframe the constraint.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, scholars_preservation_advocates, observer,
    organized, generational, mobile, national).

% Analytical seat to measure which reading is instantiated in practice through ethnographic observation and historical analysis. Not a party to the constraint; measures how it operates.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, anthropological_observer, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone was built to solve a coordination problem among coastal communities: how to encode and transmit knowledge of hazard patterns and safe settlement zones across generations without centralized authority or written records. The stone inscribed the knowledge in durable form and tied it to burial ground + village ceremony, creating a feedback loop: mourning ritual reinforced memory; memory reinforced settlement pattern; settlement pattern reinforced the collective understanding that the zone was dangerous. Under this reading, that coordination function has atrophied.
% TRANSFER_FUNCTION: The husk reading describes a transfer from future coastal residents to coastal development interests: future residents inherit commitment-to-memorial (the obligation to treat the stone as historical artifact, to preserve it, to revere it) while losing protection-from-development (the benefit of the norm-based settlement restriction). Coastal developers gain land-use freedom and monetizable historical narrative. The transfer is not monetary; it is authority-to-interpret and risk-displacement.
% ABSENT_VOICES: The primary excluded voice is the future residents who will face hazard in the zones the stone marks. They would object to the commemorative reading (if they could object in advance, they would argue for behavioral re-transmission and settlement-pattern enforcement), but they are not present in the institutional deliberation about how to interpret the stone. The behavioral_competence_reading also remains partially excluded—scholars advocate for it, but institutional power settles on the husk reading.
% DISAPPEARANCE_RATIONALE: If the stone and its interpretive framing disappeared overnight, coastal development would accelerate (the last cultural marker cautioning against settlement would be gone), but real protection would remain unaffected (it derives from building codes, emergency response, and other independent mechanisms, not from the stone). The hazard itself would not change; the coordination mechanism that once guided settlement away from it would be entirely displaced by economic logic. The arrangement structures who bears the hazard and who benefits from treating it as a solved historical problem rather than a live management challenge.
% FOUNDING_PROBLEM: Coastal communities in Edo-period Japan (1600s–1800s) experienced catastrophic tsunamis at intervals of 100–200 years. The founding problem was: how do you transmit warning across a gap longer than individual lifespans, such that descendants 150 years later know to evacuate when the earth shakes? The stone inscription encoded the warning durably and tied it to ceremony, creating a mnemonic and behavioral loop.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's persistence is corroborated by the 2011 Tōhoku tsunami, which killed ~20,000 people concentrated precisely in zones marked by older stones. Tsunami scientists and disaster anthropologists (outside the benefiting parties) confirm that the hazard is not solved—it is only managed by post-event response. Some communities where the behavioral norm remained live (e.g., Aneyoshi, Iwate) showed dramatically lower casualty rates, providing empirical evidence that the norm was protective when enforced. However, institutional memory holders and government planners largely attest that the founding problem is 'solved' via modern building codes and warning systems—attesting that the stone itself is no longer necessary. This dispute is structurally the contestation between the two readings.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__commemorative_husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__commemorative_husk_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) under this reading because the constraint operates asymmetrically: the stone transfers commitment-to-commemoration (a symbolic good, costless to produce in volume) while displacing real protection to uncoordinated market decisions. Suppression is high (0.71) because maintaining the husk reading requires actively suppressing intergenerational transmission of the behavioral norm—ceremonial performances, tourism narratives, and academic reinterpretation all work to keep the stone as symbol and forestall its reactivation as guide. Theater is the highest metric (0.68) because the constraint's primary function becomes performative: annual rituals, monument plaques, academic papers about the stone all reinforce its role as historical artifact while economic decisions proceed independently. Accessibility_collapse is low (0.45) because the decay is not inevitable—alternative paths (maintaining the norm, building enforcement mechanisms, designing settlements outside hazard zones) remain technically and socially accessible; the husk outcome reflects choice, not structural inevitability. Resistance is moderate (0.58): descendants of stone-erecting communities sometimes reassert the norm, scholars advocate for behavioral re-interpretation, but coastal development interests have sufficient institutional power and economic incentive to maintain the symbolic reading. The measurement series tracks the constraint's evolution over 30 years: extractiveness increases as development accelerates in protected zones; theater increases as the stone becomes tourist destination; suppression increases as economic interests successfully reframe the constraint from warning-system to historical-artifact.
 *
 * PERSPECTIVAL GAP:
 *   The coastal_development_interests seat and the post-resettlement-residents seat compute dramatically differently. From the developer/planner seat, the stone's decay to symbol is not extraction but liberation—it removes a friction on rational economic use of land; they experience the constraint as weakening coordination (good coordination would be one clear ownership/use rule; the decayed stone leaves ambiguity). From the residents' seat, the same decay is pure extraction: they inherit both the stone (symbolic marker that the hazard was known) and the hazard (because the norm-based settlement restriction eroded). The engine computes this divergence from power (developers are institutional, residents are moderate), exit_options (developers have mobile/arbitrage routes, residents are trapped), and directionality (d rises toward 1.0 for residents, falls toward 0.0 for developers).
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal_development_interests hold d ≈ 0.15–0.20 (beneficiary end): they benefit from the constraint's decay, have exit options (other development markets, capital flight), and hold institutional power. Post_coastal_residents hold d ≈ 0.85–0.90 (target end): they bear extraction via settlement in hazard zones, have no exit (trapped by property ownership, kinship, employment), and lack institutional countervailing power. Stone-erecting-descendants sit near d ≈ 0.55–0.65 (moderate, contested): they hold cultural authority over the norm's interpretation but lack economic power to enforce it against development interests. The directionality_overrides array is empty—the structural derivation captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (provide warning and enforce settlement restriction for known tsunami hazard zones) was live and well-understood at the time of stone-erection (1600s–1800s). Under this reading, the founding problem remains demonstrably live: the 2011 Tōhoku tsunami killed ~20,000 people, concentrated in precisely the zones marked by older stones. The founding_problem_status is 'live' (the hazard persists). BUT the constraint that instantiated the solution (the stone as behavioral warning, enforced through norm transmission) has atrophied: mandatrophy is resolved. The stone persists as monument, but the protection function it was built to serve operates outside the constraint now—protection is displaced onto uncoordinated private choices, building codes that bear no reference to the stone, and post-event emergency response. This is the diagnostic signature of piton: the founding problem is unsolved, the constraint persists, but the primary function has atrophied into performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the stone inscription a live behavioral constraint that communities actively interpret and enforce, or a decayed symbolic artifact that performs memory theater while real protection derives from other sources?',
    'Post-event ethnographic survey of survivor communities: behavioral coding of stone-referential discourse, decision-making in evacuation/rebuilding phases, intergenerational transmission of warning-transmission practices. If survivors cite the stone as decision-relevant in real-time crisis, it is behaviorally live; if invoked only in retrospect or public ceremony, it is commemorative husk.',
    'If the constraint is behaviorally live (sibling behavioral_competence_reading correct), ε drops sharply (~0.15–0.25) because extraction on future generations is minimal — the norm is self-enforcing through transmission. If the husk reading holds, ε stays high (0.82+) because the decayed constraint extracts commitment to a non-protective artifact while real protection is displaced onto uncoordinated private decisions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, empirical, 'Whether the stone operates as live norm or symbolic artifact.').

omega_variable(
    extraction_mechanism_specificity,
    'Who specifically benefits from the stone''s transformation into symbolic artifact rather than live warning? Is the extraction intentional institutional capture, passive diffusion of authority into ceremony, or emergent outcome of urbanization displacing oral transmission?',
    'Documentary history of land-use decisions in the two decades after the 1700s–1800s tsunamis: permits issued for development in preserved hazard zones; government statements defending preservation vs. promoting development; interviews with descendants of original stone-erecting communities and modern planners.',
    'Intentional capture (developers and planners choosing theater over enforcement) establishes the beneficiary set as agenda_setter. Passive diffusion suggests no concentrated beneficiary — the constraint becomes tangled_rope with weak enforcement leakage. Emergent urban displacement makes this a scaffold with failed sunset (the original function abandoned without formal replacement).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism_specificity, empirical, 'Whether non-protection is structural advantage or unintended consequence.').

omega_variable(
    competing_interpretation_traditions,
    'Does the behavioral_competence reading represent a live tradition within descendant communities, or is it a scholarly retrospective attribution imposing a coherent norm onto fragmentary, contested intergenerational record?',
    'Genealogical tracing of stone-interpretation discourse in village records, oral histories, and transmitted practices. Direct testimony from knowledge-holders in the original stone-erecting communities; comparison with parallel tsunami-warning stone sites across Japan to assess consistency of norm transmission.',
    'If behavioral competence is a live tradition held by descendants, the readings coexist_with each other (different parties in different communities hold different framings). If it is scholarly interpretation imposed onto fragmentary record, this reading (husk) is the reading the community actually instantiates, and the behavioral reading is a counterfactual scholarly hypothesis rather than a sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_interpretation_traditions, conceptual, 'Whether the behavioral-competence reading is live tradition or retrospective scholarly attribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__commemorative_husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(tsun_tr_t15, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(tsun_tr_t30, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(tsun_be_t15, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(tsun_be_t30, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(tsun_su_t15, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(tsun_su_t30, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__commemorative_husk_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__commemorative_husk_reading, 0.12).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel decomposes into two structurally distinct constraints: (1) behavioral_competence_reading instantiates the stone as live norm with low extraction (~0.15–0.25 ε) via active intergenerational transmission; (2) commemorative_husk_reading instantiates the stone as symbolic artifact with high extraction (~0.82 ε) via displacement of protection to uncoordinated market decisions. The readings coexist under different institutional framings and in different communities. The 2011 Tōhoku tsunami provided empirical test: communities maintaining behavioral transmission showed substantially lower mortality in marked hazard zones. The readings are linked by the same fixed kernel text (the inscriptions) but diverge in authority_grounding (behavioral_competence grounds in lineage + practice; husk grounds in institutional + development authority). Each story carries its own ε, beneficiary set, and classification; they are related via network.affects_constraints, not folded into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
