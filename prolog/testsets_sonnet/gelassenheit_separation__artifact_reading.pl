% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Ordnung Ban on Worldly-Resembling Artifacts (Visible-Distinction Reading)
 *   domain: religious/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the artifact reading of the gelassenheit
 *   separation kernel: technology is judged by whether it visually resembles
 *   worldly (English) artifacts, independent of function or degree of
 *   entanglement with outside systems. Under this reading, a functionally
 *   self-contained solar panel or synthetic work fabric is forbidden purely
 *   because it looks modern, while a disguised or repackaged device with
 *   identical function to a forbidden one may be approved because it looks
 *   plain. This is a distinct constraint from the principle reading (which
 *   asks only whether technology creates structural entanglement) and the
 *   consequence reading (which asks only about effects on visiting and mutual
 *   aid) — the three readings can rule oppositely on the same device, which
 *   is why they are authored as three separate constraint stories rather than
 *   one story with a measurement parameter.
 *
 * KEY AGENTS:
 *   - bishops_and_ministers: agenda_setter (institutional/constrained) — administer case-by-case appearance rulings
 *   - visible_boundary_maintainers: beneficiary (organized/constrained) — status and investment protected by the appearance standard
 *   - young_off_grid_households: payer (powerless/trapped) — bear cost of appearance rule despite already satisfying non-entanglement
 *   - small_family_farm_operators: payer (powerless/trapped) — economic disadvantage from appearance-based bans
 *   - members_with_disabilities: payer (powerless/trapped) — functional need overridden by appearance standard
 *   - consequence_reading_communities: excluded (organized/constrained) — alternative reading circulates but has no standing here
 *   - ordnung_compliance_researchers: observer (analytical) — documents cross-district divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.71).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.87).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Ordnung Ban on Worldly-Resembling Artifacts (Visible-Distinction Reading)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/technology_governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, '21a952bd-ae93-4744-9f81-ae5866da413c').
narrative_ontology:cs_kernel_codification('21a952bd-ae93-4744-9f81-ae5866da413c', distributed).
narrative_ontology:cs_authority_grounding('21a952bd-ae93-4744-9f81-ae5866da413c', practice).
narrative_ontology:cs_interpretation_layer_present('21a952bd-ae93-4744-9f81-ae5866da413c').
narrative_ontology:cs_reading_relation('21a952bd-ae93-4744-9f81-ae5866da413c', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('21a952bd-ae93-4744-9f81-ae5866da413c', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('21a952bd-ae93-4744-9f81-ae5866da413c', foundational, visibility_is_the_measure_of_separation).
narrative_ontology:cs_axiom_status(visibility_is_the_measure_of_separation, holdable).
narrative_ontology:cs_axiom_grounding('21a952bd-ae93-4744-9f81-ae5866da413c', visibility_is_the_measure_of_separation, conventional).
narrative_ontology:cs_axiom('21a952bd-ae93-4744-9f81-ae5866da413c', foundational, appearance_governs_independent_of_function).
narrative_ontology:cs_axiom_status(appearance_governs_independent_of_function, holdable).
narrative_ontology:cs_axiom_grounding('21a952bd-ae93-4744-9f81-ae5866da413c', appearance_governs_independent_of_function, conventional).
narrative_ontology:cs_reference_frame('21a952bd-ae93-4744-9f81-ae5866da413c', visible_plain_distinctiveness_standard).
narrative_ontology:cs_drift_state('21a952bd-ae93-4744-9f81-ae5866da413c', contemporary_off_grid_technology_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21a952bd-ae93-4744-9f81-ae5866da413c', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, bishops_and_ministers).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, visible_boundary_maintainers).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, young_off_grid_households).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, small_family_farm_operators).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, members_with_disabilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the Ordnung's artifact-appearance rules at council (Gmee) meetings, ruling on individual technology petitions case by case. They decide whether a device 'looks Englisch' regardless of what it does — a battery-charged appliance styled to look plain may pass where a functionally identical but chrome-finished one fails. Their authority rests on maintaining a bright, visible line others can see and be judged against.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, bishops_and_ministers, agenda_setter,
    institutional, generational, constrained, regional).

% Established households who have already built their operations around appearance-compliant equipment (horse-drawn implements retrofitted with disguised engines, non-electric-looking fixtures) benefit from a rule that makes their existing investment the visible standard others must match. Their social status as models of separation is reinforced every time a neighbor's technology is rejected for looking too worldly.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, visible_boundary_maintainers, beneficiary,
    organized, generational, constrained, regional).

% Starting new farms or businesses entirely disconnected from the public grid, they would adopt solar panels or synthetic-fabric work clothing purely for function and cost, with zero entanglement in worldly utility systems. The artifact-reading Ordnung forbids these anyway because they visually resemble English equipment, forcing costlier, less efficient substitutes even though the principle of non-entanglement is already satisfied. Leaving means leaving the community and often the extended family network.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, young_off_grid_households, payer,
    powerless, biographical, trapped, local).

% Compete economically against non-plain farms and reduced-labor English operations, but are barred from adopting functionally isolated technology (battery-stored solar irrigation, modern lightweight tools) solely because of appearance. They bear real income loss for a distinction that has no bearing on community entanglement, only on how the equipment looks to onlookers.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, small_family_farm_operators, payer,
    powerless, biographical, trapped, local).

% Mobility aids, hearing devices, and medical equipment are sometimes rejected or require costly disguise/modification because their design resembles standard English medical technology, even when the device is entirely self-contained and creates no dependency on outside systems. They must petition ministers individually and often wait or improvise.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, members_with_disabilities, payer,
    powerless, biographical, trapped, local).

% Neighboring church districts that evaluate technology by its effect on visiting patterns and mutual aid rather than by appearance would approve devices this Ordnung forbids. They are not part of this district's decision-making and their alternative reading carries no formal weight here, though it circulates informally and creates visible inconsistency between districts.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, consequence_reading_communities, excluded,
    organized, generational, constrained, regional).

% Scholars of Anabaptist material culture and technology governance document how different districts adjudicate the same devices differently, tracing which readings of separation produce which rulings and at what cost to affected households.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, ordnung_compliance_researchers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a legible, shared visual boundary between the community and the surrounding society, allowing members and outsiders alike to recognize community identity at a glance and reinforcing in-group commitment through consistent, visible markers.
% TRANSFER_FUNCTION: Moves the cost of maintaining appearance-based distinctiveness onto households whose technology needs are least served by appearance-driven rules — young off-grid families, small farms competing economically, and members needing functional medical or mobility devices — while concentrating interpretive authority and status in the ministers and already-compliant established households.
% ABSENT_VOICES: Districts practicing the consequence reading or principle reading would approve much of what this Ordnung forbids; they are not represented in this district's council and their alternative interpretations are not treated as evidence in individual rulings. Disabled members petitioning for medical equipment often have their functional argument set aside in favor of the appearance standard.
% DISAPPEARANCE_RATIONALE: If the artifact-appearance standard vanished and only functional entanglement mattered, several households would immediately adopt solar power, modern fabrics, and standard-appearance medical devices; the visible-distinctiveness function that currently signals community boundary to outsiders would need a different mechanism (dress, dialect, worship practice) to persist, and the ministers' case-by-case interpretive authority over appearance would lose its object.
% FOUNDING_PROBLEM: Historically, the Ordnung's technology restrictions arose to prevent the community's assimilation into surrounding society and to preserve practices of mutual aid, visiting, and non-dependence on entangling worldly systems (insurance, public utilities, easy travel) that could erode communal bonds.
% FOUNDING_PROBLEM_CORROBORATION: Ministers and established households attest the visible-distinction standard is still necessary to prevent gradual assimilation. Scholars of Amish technology adoption (outside the benefiting parties) and members from consequence-reading districts attest that appearance-based rules have, in numerous documented cases, diverged from the founding concern with entanglement — rejecting functionally isolated devices that create no dependency on outside systems, while approving disguised or repackaged equipment that is functionally identical to forbidden versions. This divergence is corroborated by comparative studies across districts using different interpretive standards.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.71) because the appearance standard imposes real economic and functional costs (foregone efficiency, forced disguise costs, blocked medical devices) that are structurally decoupled from the stated coordination goal of avoiding worldly entanglement — the young off-grid household example shows the ban firing even when entanglement is zero. Suppression is very high (0.87) because compliance is enforced through excommunication/shunning risk (Meidung) and social visibility itself is the enforcement mechanism — everyone can see a forbidden-looking object, making evasion costly. Theater ratio is substantial and rising (0.62 at T=40) because much enforcement activity has shifted toward appearance-policing (does it look plain enough) rather than the founding concern with actual entanglement — a classic Goodhart substitution where a visible proxy (appearance) replaces the harder-to-observe target (structural dependency).
 *
 * PERSPECTIVAL GAP:
 *   From the ministers' and established households' seat, the rule is coherent coordination: a legible, defensible boundary that has worked for generations. From the powerless payer seats — off-grid households, small farms, disabled members — the same rule computes as extraction with no coordination payoff, since their situations already satisfy the founding non-entanglement concern. The engine should compute divergent seat classifications from this asymmetry; the claim of tangled_rope reflects that both a genuine (if attenuated) coordination function and identifiable, structurally asymmetric victims coexist in this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Bishops and ministers set and enforce the rule and are its primary interpretive beneficiaries (d near beneficiary end) — their authority is the mechanism, not merely a byproduct. Visible boundary maintainers benefit from a rule that formalizes their existing compliant investment as the community standard (d beneficiary-leaning). The three payer groups are structurally targeted: they bear the cost of a rule that fires independent of their actual entanglement status, and their exit options are trapped by community and family ties (identity-locked in practice, though not overridden here since the derivation from victim declaration + trapped exit already captures this). Excluded consequence-reading communities are neither beneficiaries nor victims of this district's Ordnung but illustrate the counterfactual: under their reading, several forbidden devices would pass.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabelings: treating this purely as extraction (ignoring the real, if partial, coordination function of visible group boundary maintenance that the founding problem genuinely addressed) and treating it purely as legitimate coordination (ignoring the asymmetric cost imposed on powerless members whose situations already satisfy the underlying non-entanglement rationale). The founding problem is contested-status rather than flatly dead: boundary maintenance against assimilation remains a live concern for the community as a whole, but the artifact-appearance operationalization of that concern has drifted from the founding rationale in ways corroborated by outside scholarship and by sibling districts using different readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appearance_vs_entanglement_boundary_ambiguity,
    'Is visible resemblance to worldly artifacts a legitimate independent marker of separation, or is it a proxy that has drifted from the founding concern with structural entanglement?',
    'Comparative case analysis across districts: track rulings on functionally identical devices (e.g., battery-stored solar irrigation) across artifact-reading, principle-reading, and consequence-reading districts, and correlate outcomes with actual entanglement/assimilation measures (attendance, out-marriage rates, retention) over multi-decade horizons.',
    'If appearance tracks entanglement closely, the artifact reading is a reasonable operationalization of a genuine coordination concern (closer to rope/tangled_rope with modest extraction). If appearance and entanglement diverge substantially — as the off-grid and disability cases suggest — the appearance standard functions increasingly as extraction/status-maintenance riding on the founding problem''s legitimacy, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appearance_vs_entanglement_boundary_ambiguity, conceptual, 'Whether visible appearance is a valid proxy for, or a drift away from, the founding non-entanglement rationale.').

omega_variable(
    committer_reading_selection,
    'Why does this district hold the artifact reading rather than the principle or consequence reading, and what would change if it shifted?',
    'Oral history and Ordnung revision records: trace which bishops introduced appearance-based rulings, when, and in response to what perceived assimilation pressure; compare to districts that shifted readings after generational leadership change.',
    'If the artifact reading was adopted defensively during a period of perceived rapid assimilation and has persisted past that pressure by institutional inertia, this supports a founding_problem_status of contested-trending-dead and increases confidence the current ruling pattern is extraction riding on residual legitimacy. If the artifact reading tracks a continuously live and severe assimilation threat, the coordination function is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_selection, conceptual, 'The historical and institutional basis for this district''s specific reading selection within the kernel contest.').

omega_variable(
    disability_exemption_precedent_ambiguity,
    'Do medical/mobility device exemptions granted case-by-case constitute a genuine principled carve-out, or do they demonstrate that the appearance rule is negotiable for anyone with sufficient standing to petition effectively?',
    'Audit approval/denial rates and wait times for medical device petitions against petitioner social standing (established vs. newer households) within the district''s council records.',
    'If exemption rates correlate with petitioner standing rather than need, this strengthens the tangled_rope reading (rule enforcement is asymmetric by power, not by principle) and suggests theater_ratio understates actual arbitrariness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disability_exemption_precedent_ambiguity, empirical, 'Whether case-by-case exemptions are principled or power-correlated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__artifact_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__artifact_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__artifact_reading, theater_ratio, 24, 0.53).
narrative_ontology:measurement(gela_tr_t32, gelassenheit_separation__artifact_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__artifact_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gela_be_t8, gelassenheit_separation__artifact_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(gela_be_t16, gelassenheit_separation__artifact_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(gela_be_t24, gelassenheit_separation__artifact_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(gela_be_t32, gelassenheit_separation__artifact_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__artifact_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(gela_su_t8, gelassenheit_separation__artifact_reading, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(gela_su_t16, gelassenheit_separation__artifact_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(gela_su_t24, gelassenheit_separation__artifact_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(gela_su_t32, gelassenheit_separation__artifact_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__artifact_reading, suppression_requirement, 40, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__artifact_reading, 0.08).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three ε-invariant constraints decomposed from the colloquial single concept 'Amish separation from the world' (gelassenheit_separation kernel). The artifact reading (this story) measures substantially higher extractiveness (0.71) and suppression (0.87) than the principle reading is expected to show, because it forbids functionally isolated technology purely on appearance grounds — a strictly broader and more costly prohibition than a pure non-entanglement test. It is expected to diverge further still from the consequence reading, which evaluates technology by community-practice effects rather than appearance or abstract entanglement. All three share the founding problem narrative but operationalize 'separation' through different observables, which is exactly the condition under which the ε-invariance principle requires decomposition into linked sibling stories rather than one parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
