% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Practice Displacement via Internalization: Bottom-Up Adoption as Legitimacy Condition
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A modernizing state (colonial or revolutionary) decrees the displacement
 *   of an established cultural practice (e.g., lunar calendar adoption in
 *   Islamic societies under secular nationalism, European dress codes during
 *   Meiji Restoration or Soviet campaigns) with the intent to create
 *   territorial cultural unification. This reading frames practice
 *   displacement as dependent on bottom-up internalization — the state's
 *   mandate alone cannot generate lasting adoption because individuals and
 *   communities must actively accept the new practice as meaningful to their
 *   own identity and lived experience. The constraint's persistence depends
 *   on continued enforcement (suppression) precisely because internalization
 *   has failed. Communities preserve the prior practice in private/domestic
 *   contexts while exhibiting surface compliance in public/administered
 *   spaces, creating a theater_ratio that rises over time as the gap between
 *   reported and actual adoption widens. The state experiences this as
 *   victimization of its modernization timeline; communities experience it as
 *   preservation of autonomy against erasure. ε = 0.68 at interval end
 *   reflects high extraction of cultural authority coupled with high
 *   uncertainty about whether compliance is genuine or performative.
 *
 * KEY AGENTS:
 *   - State Modernization Apparatus (institutional, agenda-setter): sets and enforces the mandate; bears the cost of failed displacement
 *   - Communities Preserving Autonomy (moderate power, organized networks): retain prior practice in private contexts; benefit structurally from the constraint's failure to internalize
 *   - Enforcement Bureaucracy (institutional, identity-locked): administers the mandate; their institutional survival depends on reported compliance masking actual non-adoption
 *   - Urban Adopters (powerful, mobile): partially adopt the new practice as status signal; internalization is incomplete and identity-dependent
 *   - Peripheral Populations (powerless, trapped): subject to mandate but with sparse enforcement visibility; practice displacement fails where enforcement is thin
 *   - Intellectual Reformers (excluded, moderate power): would advocate gradual adoption with ideological scaffolding; excluded from implementation structure
 *   - Next Generation (excluded, powerless, identity-locked): vector for generational internalization; their preferences unknown and contested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.71).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Practice Displacement via Internalization: Bottom-Up Adoption as Legitimacy Condition").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '08c1fa10-ce86-4aef-8460-0eefc69f8fb1').
narrative_ontology:cs_kernel_codification('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', fixed_text).
narrative_ontology:cs_authority_grounding('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', extraction).
narrative_ontology:cs_interpretation_layer_present('08c1fa10-ce86-4aef-8460-0eefc69f8fb1').
narrative_ontology:cs_reading_relation('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', foundational, internalization_is_prerequisite_for_stable_displacement).
narrative_ontology:cs_axiom_status(internalization_is_prerequisite_for_stable_displacement, holdable).
narrative_ontology:cs_axiom_grounding('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', internalization_is_prerequisite_for_stable_displacement, empirically_contingent).
narrative_ontology:cs_axiom('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', foundational, coercion_alone_cannot_generate_lasting_practice_change).
narrative_ontology:cs_axiom_status(coercion_alone_cannot_generate_lasting_practice_change, holdable).
narrative_ontology:cs_axiom_grounding('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', coercion_alone_cannot_generate_lasting_practice_change, empirically_contingent).
narrative_ontology:cs_reference_frame('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', community_autonomous_practice_preservation).
narrative_ontology:cs_drift_state('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', post_mandate_enforcement_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08c1fa10-ce86-4aef-8460-0eefc69f8fb1', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_traditionalists).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, peripheral_populations).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_change_requires_endogenous_adoption).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, imposed_practice_without_internalization_is_unstable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees a new practice (e.g., solar calendar adoption, European dress codes) as part of modernization program. Enforces the mandate through administrative penalties, credential denial, or social shaming. Measures success by compliance rate on the surface (adoption metrics in urban centers, reported calendar use). Bears the cost of enforcement infrastructure and the failure cost when rural/peripheral populations continue prior practices despite the mandate.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Retain lunar calendar or prior dress norms in private/domestic contexts despite public compliance with state mandate. They experience the state's constraint as an attempt to erase identity; their preservation of prior practice is an autonomous choice to maintain cultural continuity. Exit from the state system itself (migration, withdrawal from administered territory) is possible but costly.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy, beneficiary,
    moderate, generational, mobile, regional).

% Form networks and institutions (literary circles, religious congregations, family councils) that preserve and transmit the prior practice as an identity marker and source of cultural authority. They frame internalization of the state-mandated practice as cultural loss. Their resistance to internalization is the structural constraint on the state's displacement project.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_traditionalists, beneficiary,
    organized, generational, constrained, national).

% Professional, merchant, and administrative classes who adopt the new practice (solar calendar, European dress) as a signal of affiliation with the modernizing state. They pay the cost of identity rupture and intergenerational cultural discontinuity. They also benefit from career advancement and social status within the state's new hierarchy. Their partial internalization creates a wedge between urban and rural practice adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters, payer,
    powerful, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters, beneficiary).

% Administers the mandate through school systems, credential gates, administrative courts, and social monitoring. Their careers and institutional legitimacy depend on reported compliance rates. They face persistent resistance from communities and must escalate enforcement or accept surface compliance masking private non-compliance. Their institutional survival rides on the mandate's success, creating incentive for theatrical metrics over genuine internalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, enforcement_bureaucracy, agenda_setter,
    institutional, biographical, identity_locked, national).

% Rural, remote, or economically marginal populations subject to the mandate but with minimal visibility to the state enforcement apparatus. They retain prior practices in daily life because enforcement is sparse and because the new practice (e.g., solar calendar) has no functional advantage in their economic context. When enforcement reaches them, they face social penalties and restricted access to administrative services; exit requires abandoning ancestral territory.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, peripheral_populations, payer,
    powerless, generational, trapped, regional).

% The external or distant institutional power that initiated or legitimated the modernization program (e.g., colonial administration, revolutionary center). Operates via mandate and symbolic authority; actual enforcement delegated to local bureaucracy. Observes compliance patterns and adjusts policy based on reports, often blind to the gap between reported and actual internalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, colonial_or_centralizing_authority, agenda_setter,
    institutional, generational, analytical, global).

% Advocates who argue the new practice is superior on rational, scientific, or moral grounds. They are excluded from the actual implementation because the state prefers mandate and enforcement over persuasion. If included, they would propose gradual adoption, ideological messaging campaigns, and scaffolding rather than pure decree. Their exclusion is the structural feature that makes this a tangled rope rather than a rope.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, intellectual_reformers, excluded,
    moderate, biographical, mobile, national).

% Children and youth born during or after the mandate. They are subject to schooling in the new practice and may internalize it; their eventual preferences would determine whether the displacement succeeds generationally. They are excluded from this moment's decision structure but are the vector through which internalization either succeeds or fails. Their internalization depends on whether the prior practice remains accessible and valued in family/community contexts.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, next_generation, excluded,
    powerless, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state's modernization mandate attempts to coordinate cultural practice across a territory toward a unified standard (solar calendar, dress code, language usage). The coordination problem the state imagines it is solving: shared temporal reference and visible status markers to enable bureaucratic administration, national market formation, and cultural homogenization as a foundation for state capacity.
% TRANSFER_FUNCTION: Moves cultural authority and temporal/identity sovereignty from communities and traditionalist institutions to the state apparatus. The state collects the authority to define 'modern' practice; communities and traditionalists bear the cost of cultural discontinuity, identity rupture, and loss of autonomous meaning-making. Urban professional classes pay an identity cost but gain state-aligned status.
% ABSENT_VOICES: Intellectual reformers who would argue for gradual adoption and ideological persuasion are structurally excluded; the state prefers decree over dialogue. Next-generation voices are muted because their preferences have not yet formed. Communities' own development priorities and adaptive evolution of practice within their cultural frameworks are not heard; the state frames the mandate as a non-negotiable modernization, not a contested change.
% DISAPPEARANCE_RATIONALE: From the state's perspective, if the mandate disappeared the administrative apparatus would lose a key legitimation tool and the path to modernization would stall. From the communities' perspective, if the mandate disappeared the prior practice would immediately resurface without organizational cost — the constraint is the state's imposed structure, not a learned coordination. The parties dispute whether the mandate's disappearance would leave a gap or a restoration.
% FOUNDING_PROBLEM: Territorial state formation requires visible cultural unification across diverse populations to enable administration, taxation, and military conscription. The prior practice (lunar calendar, diverse dress norms) fragments temporal reference and social signaling across the territory. The modernizing state decrees a single standard to solve the administrative fragmentation problem.
% FOUNDING_PROBLEM_CORROBORATION: State administrators and colonial authorities attest the founding problem is live and the mandate necessary for state capacity. Communities and traditionalists attest the problem was invented by the state to justify cultural erasure and that prior practices were functionally adequate for local coordination and trade. Academic historians of state formation document that modern states routinely cite administrative fragmentation as justification for cultural mandates; whether the fragmentation posed a genuine barrier or was a pretext for centralization remains under scholarly dispute.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval because the state deepens its investment in enforcement infrastructure (bureaucratic expansion, credential gates, social monitoring) even as actual internalization stagnates. The lack of voluntary adoption signals forces escalation. Theater_ratio rises from 0.28 to 0.58 because urban centers report high compliance (calendars in public use, dress conformity visible) while rural and private contexts retain the prior practice — the gap between administered appearance and actual behavior becomes the dominant feature of the constraint's operation. Suppression requirement stabilizes at 0.71 (t=25 onward) because the state reaches the enforcement frontier: further escalation produces diminishing returns (peripheral populations remain trapped but marginally compliant; traditionalist networks adapt evasion strategies; next-generation internalization depends on household-level transmission, not bureaucratic reach). Accessibility_collapse is moderate (0.42) because communities have real exit options — migration, withdrawal to remote areas, or private practice preservation — even though the exit cost is high. Resistance remains high (0.74) throughout because traditionalist networks are organized, articulate, and sustained by cultural meaning, not by economic interest alone. The constraint is a tangled rope from the state's perspective (genuine coordination problem + asymmetric extraction of authority) and a snare from communities' perspective (mandate imposed, alternatives structurally suppressed, authority extracted). The engine computes this divergence from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat: the constraint solves a real coordination problem (administrative fragmentation) and communities should internalize the new practice through education and social modeling; failure is attributed to traditionalist resistance or insufficient enforcement. From the communities' seat: the constraint is pure extraction of cultural sovereignty disguised as modernization; the prior practice was adequate and the mandate is an assault on autonomy. From urban adopters' seat: partial adoption is rational status-seeking in a new institutional environment; full internalization is unnecessary and identity-rupture is a bearable cost. From the enforcement bureaucracy's seat: reported compliance is success; the gap between reported and actual adoption is not their problem (institutional separation of measurement and ground truth). The engine's per-seat classification should diverge sharply across these positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus is the structural agenda-setter (d near 1.0: imposes the constraint, bears the cost of failed displacement, must escalate enforcement). Communities preserving autonomy are beneficiaries (d near 0.0: the constraint's failure to internalize is their victory; they retain cultural authority). Urban adopters are mixed — they pay an identity cost but gain institutional status (d near 0.5: neither full target nor full beneficiary, but positioned in the ambiguous middle where internalization is incomplete). Enforcement bureaucracy is identity-locked institutional actors (d dependent on measurement capture: from their operative perspective, reported compliance = success, so d may be moderate; but from the structural perspective they are frontline implementers of an extractive mandate, so d should be higher). Peripheral populations are trapped targets (d near 1.0: subject to mandate with minimal agency, sparse but present enforcement, no exit available). Intellectual reformers are excluded, so directionality is not applicable; they would have d near 0.5 (symmetric cost/benefit if admitted). Next-generation is trapped and identity-locked (d near 1.0 at this moment, but futures depend on internalization success).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — administrative fragmentation requiring unified cultural practice — may be dead, contested, or attenuated. The state believes it is live and invokes it to justify continued enforcement. Communities believe it is artificial and invented to justify erasure. Academic sources suggest the fragmentation was real in early state formation but became a pretext for centralization as the state's administrative capacity grew. This reading frames mandatrophy as the core dynamic: the state's institutional survival depends on continued enforcement even as the genuine coordination need declines. If the founding problem has died (coordination achievable without cultural displacement), the constraint shifts from tangled_rope to snare — extraction riding on a dead mandate, persisting only through coercion. The theater_ratio rise signals this decay: if the new practice were genuinely adopted and internalized, theater should be near zero (real compliance); instead, theater rises because the state must increasingly fake success (report adoption, hide non-compliance, escalate enforcement of the appearance of compliance). The constraint has NOT yet transitioned to piton (there is still active enforcement, not merely theatrical maintenance), but the trajectory suggests that path. Mandatrophy is live and contested; the mismatch consumer should flag this constraint for institutional-decay monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_surface_compliance,
    'Is the measured non-displacement a sign of genuine non-internalization (communities consciously rejecting the new practice as inauthentic), or of measurement-blindness (the new practice is internalized privately/cognitively but not visible to the state''s enforcement apparatus)?',
    'Multi-generational ethnographic study tracking household practice, identity markers, and meaning-attribution across urban/rural divides and cohorts. Intergenerational interview data testing whether next generation retains prior practice or has internalized the mandated practice as natural. Exit-and-return studies: do emigrants/exiles who leave the mandate territory resume prior practice, suggesting it was merely suppressed rather than replaced?',
    'If non-internalization is genuine, the constraint is a snare (extraction without stable adoption; coercion required indefinitely). If measurement-blindness is primary, the constraint may be shifting toward piton (de facto internalization while theater maintains the appearance of resistance). The reading''s validity depends on this distinction: endogenous_climb asserts genuine non-internalization is possible and persistent; exogenous_override would claim surface compliance IS internalization (behavioral conformity = success).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalization_vs_surface_compliance, empirical, 'Whether non-adoption is a conscious rejection or an artifact of measurement separation').

omega_variable(
    generational_internalization_vector,
    'Will next-generation children who grow up with both mandated practice (schooling, public administration) and retained practice (family, community) eventually internalize one or both as authentic, or remain caught in permanent identity bifurcation?',
    'Cohort studies tracking generational shifts in practice preference, identity attachment, and meaning-attribution. Historical evidence from similar state mandates (e.g., Chinese language standardization, European language replacement of indigenous languages) showing whether displacement succeeded inter-generationally or failed. School-vs-home transmission tracking the site where internalization either succeeds or stalls.',
    'If next generation internalizes the mandated practice, displacement succeeds despite current-moment non-internalization — the constraint was always a temporary tangled rope, succeeding via inter-generational scaffolding. If next generation retains prior practice or bifurcates identity, the constraint is structurally locked into coercive maintenance — piton or degraded snare. This distinction determines whether mandatrophy is real or whether the state''s founding problem is alive across timescales longer than this interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_internalization_vector, empirical, 'Whether practice displacement succeeds inter-generationally or fails across cohorts').

omega_variable(
    excluded_scaffolding_counterfactual,
    'If intellectual reformers (ideological messaging advocates) had been INCLUDED in the implementation structure instead of excluded, would gradual adoption with persuasion campaigns have succeeded where pure decree failed, or is the divergence between readings a false dichotomy (both scaffolded and decreed approaches fail absent some unmeasured structural condition)?',
    'Historical comparison with cases where scaffolding was attempted (Soviet campaigns with mass media + decree, Chinese educational modernization with persuasion + enforcement). Did scaffolded approaches achieve faster or more stable internalization? Natural experiments where scaffold components succeeded or failed independently.',
    'If scaffolding would have succeeded, this reading (pure endogenous climb, excluding reform advocates) misdiagnoses the problem: the failure is not that internalization is impossible, but that the state chose enforcement over persuasion. The true distinction would be between scaffolded_rope (genuine coordination with ideological buy-in) and coercive_snare (pure extraction). If scaffolding also fails, all three sibling readings converge on the same empirical failure (displacement is unstable regardless of mechanism), and the reading divergence is conceptual rather than empirical (different normative judgments about coercion vs persuasion, not different predictions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_scaffolding_counterfactual, conceptual, 'Whether endogenous_climb reading identifies a real constraint or a choice between two equivalent failure modes').

omega_variable(
    kernel_contest_framing_ambiguity,
    'Does the legitimacy_of_imposed_practice kernel concern the STATE''S legitimacy to impose, or the IMPOSED PRACTICE''S legitimacy once imposed, or both? If both, do they collapse into one axis or remain distinct?',
    'Textual analysis of the kernel codification (laws, decrees, policy statements, reform advocates'' writings). Do authorities frame this as ''state authority to mandate'' (exogenous_override axis) or ''practice acceptance by population'' (endogenous_climb axis) or ''procedurally legitimate mixture'' (hybrid_scaffolding axis)? Where the kernel is ambiguous, what do sibling readings assume about the framing?',
    'If the kernel is primarily about state authority, exogenous_override reading dominates and endogenous_climb is a disobedience frame, not a legitimacy frame. If the kernel is primarily about practice acceptance, endogenous_climb dominates. If the kernel genuinely concerns both, the three readings are not commensurate competitors but different aspects of a larger legitimacy question — each reading captures one dimension. This affects whether the engine''s per-seat classifications will converge or remain irreducibly divergent across seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_framing_ambiguity, conceptual, 'Whether the kernel frames legitimacy as state authority, population acceptance, or both').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(legi_tr_t8, observed).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(legi_tr_t16, observed).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement_basis(legi_tr_t25, observed).
narrative_ontology:measurement(legi_tr_t37, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 37, 0.56).
narrative_ontology:measurement_basis(legi_tr_t37, observed).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement_basis(legi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(legi_be_t8, observed).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(legi_be_t16, observed).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(legi_be_t25, observed).
narrative_ontology:measurement(legi_be_t37, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 37, 0.68).
narrative_ontology:measurement_basis(legi_be_t37, observed).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(legi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(legi_su_t8, observed).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(legi_su_t16, observed).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(legi_su_t25, observed).
narrative_ontology:measurement(legi_su_t37, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 37, 0.71).
narrative_ontology:measurement_basis(legi_su_t37, observed).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(legi_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel legitimacy_of_imposed_practice. The endogenous_climb_reading asserts that practice displacement requires bottom-up internalization — the state's decree alone cannot generate lasting adoption because individuals and communities must actively accept the new practice as meaningful. Sibling readings (exogenous_override, hybrid_scaffolding) are separate constraint stories with distinct ε values, different beneficiary/victim structures, and different classifications. All three stories are linked via this network field; the decomposition follows the ε-invariance principle (OQ-26): the readings measure the same kernel arrangement under different interpretive lenses, each with its own ε (this reading's ε=0.68 reflects extraction coupled with failed internalization; sibling readings would author different ε values reflecting their own framing of what displacement success looks like). See commentary.kernel_context for the full kernel contest statement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, institutional, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
